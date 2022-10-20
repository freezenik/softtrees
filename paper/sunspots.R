## Required packages
library("softtrees")
library("disttree")
library("gamlss.dist")
stopifnot(requireNamespace("bamlss"))
library("forecast")

set.seed(123)

## Save newest data set as data.frame.
if(!file.exists("sunspots.rds")) {
  d <- read.csv2("https://www.sidc.be/silso/DATA/SN_ms_tot_V2.0.csv", header = FALSE)
  raw <- read.csv2("https://www.sidc.be/silso/DATA/SN_m_tot_V2.0.csv", header = FALSE)
  x <- ts(as.numeric(d[, 4]), start = c(1749, 1), freq = 12)
  x[x < 0] <- NA
  raw <- ts(as.numeric(raw[, 4]), start = c(1749, 1), freq = 12)
  raw[raw < 0] <- NA

  plot(cbind(raw, x), plot.type = "single", col = c(1, 2), lwd = 2)

  sunspots <- data.frame(
    "raw" = as.numeric(raw),
    "counts" = as.numeric(x),
    "trend" = time(x),
    "year" = trunc(time(x)),
    "mon" =  cycle(x)
  )

  sunspots <- subset(sunspots, trend >= 1833.833)

  x <- ts(sunspots$counts, start = c(sunspots$year[1], sunspots$mon[1]), freq = 12)
  plot(x)

  saveRDS(sunspots, file = "sunspots.rds")

  ## Download NASA forecasts.
  df <- list()
  for(j in 1999:2022) {
    for(mon in tolower(month.abb)) {
      url1 <- paste0("https://www.nasa.gov/sites/default/files/atoms/files/", mon, j, "ssn.txt")
      url2 <- paste0("https://www.nasa.gov/sites/default/files/atoms/files/", mon, j, "ssn_prd.txt")
      nasa <- try(read.table(url1, skip = 7), silent = TRUE)
      if(inherits(nasa, "try-error")) {
        nasa <- try(read.table(url2, skip = 7), silent = TRUE)
      }
      if(!inherits(nasa, "try-error")) {
        cat("..", j, "-", mon, "\n")
        nasa <- nasa[, 1:5]
        names(nasa) <- c("date", "mon", "95%", "50%", "5%")
        nasa$year <- trunc(nasa$date)
        foo <- Vectorize(function(x) {
          switch(x,
          "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, "JUL" = 7, "AUG" = 8,
          "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12
          )
        })
        nasa$mon <- foo(nasa$mon)
        df[[paste0(j, "-", mon)]] <- ts(nasa[["50%"]], start = c(nasa$year[1], nasa$mon[1]), freq = 12)
      }
    }
  }
  nasa <- do.call("cbind", df)
  saveRDS(nasa, file = "nasa.rds")
}

## Load full data.
sunspots <- readRDS("sunspots.rds")

## Load NASA forecasts.
nasa <- readRDS("nasa.rds")

sunspots_data <- function(y, p, P = 1, scale.inputs = TRUE, ...)
{
  yname <- deparse(substitute(y))
  if(length(y) < 3) {
    stop("Not enough data to fit a model")
  }

  ## Check for NAs in y.
  if(any(is.na(y))) {
    stop("Missing values in y!")
  }

  m <- max(round(frequency(y)), 1L)

  ys <- y

  ## Set up lagged matrix.
  n <- length(y)
  y <- as.ts(y)
  if(m == 1) {
    if(missing(p)) {
      stop("need to specify p!")
    }
    if(p >= n) {
      warning("Reducing number of lagged inputs due to short series")
      p <- n - 1
    }
    lags <- 1:p
    if (P > 1) {
      warning("Non-seasonal data, ignoring seasonal lags")
    }
    P <- 0
  } else {
    if(missing(p)) {
      stop("need to specify p!")
    }
    if(any(p >= n)) {
      warning("Reducing number of lagged inputs due to short series")
      p <- n - 1
    }
    if(P > 0 && n >= m * P + 2) {
      lags <- sort(unique(c(1:p, m * (1:P))))
    } else {
      lags <- 1:p
      if (P > 0) {
        warning("Series too short for seasonal lags")
        P <- 0
      }
    }
  }

  maxlag <- max(lags)
  nlag <- length(lags)

  yr <- range(y, na.rm = TRUE)
  my <- mean(y, na.rm = TRUE)
  sdy <- sd(y, na.rm = TRUE)

  getX <- function(y) {
    n <- length(y)
    X <- matrix(NA_real_, ncol = nlag + 1L, nrow = n - maxlag)
    X[, 1L] <- 1.0
    for(i in 1:nlag) {
      X[, i + 1L] <- y[(maxlag - lags[i] + 1):(n - lags[i])]
      # X[, i + 1L] <- (X[, i + 1L] - abs(yr[1])) / diff(yr) * 2 - 1
      X[, i + 1L] <- (X[, i + 1L] - my) / sdy
    }
    colnames(X) <- c("(Intercept)", paste0("L", lags))
    return(X)
  }

  X <- getX(y)[, -1, drop = FALSE]
  y <- y[-(1:maxlag)]
  
  rval <- cbind(data.frame("y" = y, as.data.frame(X)))

  attr(rval, "getX") <- getX
  attr(rval, "y") <- ys

  return(rval)
}

model_forecast <- function(object, data, h = 10, family)
{
  y <- ys <- attr(data, "y")

  n <- length(y)

  getX <- attr(data, "getX")

  par <- NULL

  q5 <- q95 <- NULL
  for(ii in 1:h) {
    X <- as.data.frame(getX(y))
    if(inherits(object, "srt"))
      p <- predict(object, newdata = X[nrow(X), , drop = FALSE], type = "parameter", drop = FALSE)
    else
      p <- predict(object, newdata = X[nrow(X), , drop = FALSE], type = "parameter")
    par <- rbind(par, p)
    if(is.null(family$q)) {
      p <- q5 <- q95 <- p[[1]]
    } else {
      q5 <- c(q5, family$q(0.05, p))
      q95 <- c(q95, family$q(1 - 0.05, p))
      p <- family$q(0.5, p)
    }
    y <- c(y, p)
  }

  q5 <- zoo::as.zoo(ts(c(rep(NA, length(ys)), q5),
    frequency = frequency(ys), start = start(ys)))
  q95 <- zoo::as.zoo(ts(c(rep(NA, length(ys)), q95),
    frequency = frequency(ys), start = start(ys)))
  y <- zoo::as.zoo(ts(y, frequency = frequency(ys), start = start(ys)))
  ysub <- rep(TRUE, length(y))
  ysub[1:n] <- FALSE
  y <- subset(y, subset = ysub)
  q5 <- subset(q5, subset = ysub)
  q95 <- subset(q95, subset = ysub)

  rval <- cbind("5%" = q5, "Median" = y, "95%" = q95)
  rval <- zoo::as.zoo(ts(rval, start = start(y), frequency = frequency(y)))

  attr(rval, "parameters") <- par

  return(rval)
}

eval <- function(cn, plot = FALSE) {
  x_nasa <- nasa[, cn]
  x_nasa <- na.omit(x_nasa)

  y <- ts(sunspots$counts, start = c(sunspots$year[1], sunspots$mon[1]), freq = 12)

  raw <- ts(sunspots$raw, start = c(sunspots$year[1], sunspots$mon[1]), freq = 12)

  y0 <- window(y, start = start(y), end = start(x_nasa))
  y0 <- ts(y0[-length(y0)], start = start(y0), freq = 12)

  y1 <- window(y, start = start(x_nasa), end = end(x_nasa))
  x_nasa <- window(x_nasa, start = start(x_nasa), end = end(y1))
  y1_raw <- window(raw, start = start(x_nasa), end = end(y1))
  raw <- window(raw, start = start(y0), end = end(y1))

  y_raw <- window(raw, start = start(y0), end = end(y0))

  df <- sunspots_data(sqrt(y_raw + 0.001), p = 2*12, P = 35)

  f <- as.formula(paste("y~", paste(names(df)[-1], collapse = "+")))

  f <- list(f, update(f, NULL ~ .), update(f, NULL ~ .), update(f, NULL ~ .))

  families <- list(
    "NO" = NO(),
    "GA" = GA(),
    "TF" = TF()
  )

  err <- list()
  for(j in names(families)) {
    cat(".. .. family", j, "\n")

    yl <- c(-Inf, Inf)
    if(!is.null(families[[j]]$y.valid)) {
      if(!families[[j]]$y.valid(-1))
        yl[1] <- 0
    }

    lambdas <- c(1, 5, 10, 50, 100, 500, 1000)
    p1 <- par1 <- list()
    mse1 <- crps1 <- NULL
    for(l in seq_along(lambdas)) {
      b1 <- try(srt(f, data = df, K = log(nrow(df)), family = families[[j]], lambda = lambdas[l], plot = FALSE, verbose = FALSE))
      if(!inherits(b1, "try-error")) {
        p <- model_forecast(b1, data = df, h = length(x_nasa), family = family(b1))
        par1[[l]] <- attr(p, "parameters")
        p1[[l]] <- ts((p[, "Median"])^2 - 0.001, start = start(y1), freq = 12)
        mse1 <- c(mse1, mean((p1[[l]] - y1_raw)^2, na.rm = TRUE))
        tcrps <- try(bamlss:::.CRPS(sqrt(y1_raw + 0.001), par1[[l]], family(b1), yl))
        if(!inherits(tcrps, "try-error"))
          crps1 <- c(crps1, mean(tcrps, na.rm = TRUE))
        else
          crps1 <- c(crps1, NA)
      } else {
        p1[[l]] <- NA
        crps1 <- c(crps1, NA)
        mse1 <- c(mse1, NA)
      }
    }
    if(all(is.na(mse1)) | all(is.na(crps1)))
      next

    p1 <- p1[[which.min(crps1)]]
    par1 <- par1[[which.min(crps1)]]
    lambda1 <- lambdas[which.min(crps1)]

    b2 <- try(distforest(f[[1]], data = df, ntree = 100, family = families[[j]]))

    if(!inherits(b2, "try-error")) {
      p2 <- model_forecast(b2, data = df, h = length(x_nasa), family = family(b1))
      par2 <- attr(p2, "parameters")
      p2 <- ts((p2[, "Median"])^2 - 0.001, start = start(y1), freq = 12)
    } else {
      p2 <- NA
    }

    if(all(is.na(p1)) & all(is.na(p2)))
      next

    if(plot) {
      scd <- zoo::as.zoo(cbind(raw, y0, y1, p1, p2, x_nasa))
      col <- c(1, 2, 2, 4, 5, 6)
      n <- nrow(scd)
      k <- n - 3*11*12
      plot(scd[k:n, , drop = FALSE], plot.type = "single", col = col, lwd = 2, ylab = "#Sunspots")
      legend("topright", c("Mean counts", "Smoothed counts", "srt", "distforest", "NASA"),
        lty = 1, lwd = 2, col = c(1, 2, 4, 5, 6), bty = "n")
    }

    mse <- c(
      "nasa" = mean((x_nasa - y1_raw)^2, na.rm = TRUE),
      "srt" = mean((p1 - y1_raw)^2, na.rm = TRUE),
      "distforest" = mean((p2 - y1_raw)^2, na.rm = TRUE)
    )

    smape <- c(
      "nasa" = mean(abs(x_nasa - y1_raw) / (abs(p2) + abs(y1_raw))),
      "srt" = mean(abs(p1 - y1_raw) / (abs(p1) + abs(y1_raw))),
      "distforest" = mean(abs(p2 - y1_raw) / (abs(p2) + abs(y1_raw)))
    )

    ll <- c(
      "nasa" = NA,
      "srt" = sum(family(b1)$d(sqrt(y1_raw + 0.001), par1, log = TRUE), na.rm = TRUE),
      "distforest" = sum(family(b1)$d(sqrt(y1_raw + 0.001), par2, log = TRUE), na.rm = TRUE)
    )

    crps1 <- try(bamlss:::.CRPS(sqrt(y1_raw + 0.001), par1, family(b1), yl))
    crps2 <- try(bamlss:::.CRPS(sqrt(y1_raw + 0.001), par2, family(b1), yl))

    if(inherits(crps1, "try-error")) {
      crps1 <- NA
    } else {
      crps1 <- mean(crps1, na.rm = TRUE)
    }

    if(inherits(crps2, "try-error")) {
      crps2 <- NA
    } else {
      crps2 <- mean(crps2, na.rm = TRUE)
    }

    crps <- c(
      "nasa" = NA,
      "srt" = crps1,
      "distforest" = crps2
    )

    r1 <- y1_raw - p1
    r2 <- y1_raw - p2
    r3 <- y1_raw - x_nasa

    err[[j]] <- data.frame(
      "Date" = cn,
      "Distribution" = j,
      "Model" = names(mse),
      "MSE" = mse,
      "SMAPE" = smape,
      "logLik" = ll,
      "CRPS" = crps,
      "h" = length(y1_raw),
      "lambda" = lambda1,
      "dm_srt_distforest" = dm.test(r1, r2, alternative = "less")$p.value,
      "dm_srt_nasa" = dm.test(r1, r3, alternative = "less")$p.value,
      "dm_distforest_nasa" = dm.test(r2, r3, alternative = "less")$p.value
    )
  }

  err <- do.call("rbind", err)

  return(err)
}

## Run model evaluation.
if(!file.exists("sc_eval.rds")) {
  library("parallel")

  sc_eval <- mclapply(colnames(nasa), eval, mc.cores = 50)
  sc_eval <- do.call("rbind", sc_eval)

  saveRDS(sc_eval, file = "sc_eval.rds")
}

if(!file.exists("figures/sunspots_data.png")) {
  library("zoo")

  x <- read.csv2("https://www.sidc.be/silso/DATA/SN_m_tot_V2.0.csv", header = FALSE)
  x <- ts(as.numeric(x[, 4]), start = c(1749, 1), freq = 12)
  x0 <- x
  x <- window(x, start = c(1833, 11), end = end(x))
  x <- as.zoo(x)

  png("figures/sunspots_data.png", units = "in", res = 150, width = 10, height = 3.8)
  par(mar = c(4.1, 4.1, 0.1, 0.1))
  plot(x, xlab = "Time", ylab = "#Sunspots", col = NA,
    ylim = c(0, max(x, na.rm = TRUE) * 1.1), axes = FALSE)
  box()
  axis(1)
  axis(2, at = c(0, 100, 200, 300))
  lines(x0, col = rgb(0, 0, 0.3, alpha = 0.7))

  cycles <- c("1755-02", "1766-06", "1775-06", "1784-09", "1798-04", "1810-08",
    "1823-05", "1833-11", "1843-07", "1855-12", "1867-03", "1878-12", "1890-03",
    "1902-01", "1913-07", "1923-08", "1933-09", "1944-02", "1954-04", "1964-10",
    "1976-03", "1986-09", "1996-08", "2008-12", "2019-12")

  cycles <- zoo::as.yearmon(cycles)

  abline(v = cycles, col = 1, lty = 2)
  cp <- cycles[-length(cycles)] + diff(cycles)/2
  cp[7] <- cp[7] + 1.5
  text(cp, max(x, na.rm = TRUE) * 1.07, paste("Cycle", 1:(length(cycles) - 1)), cex = 0.55)
  dev.off()
}

if(!file.exists("sunspots_model.rds")) {
  library("parallel")

  x_nasa <- na.omit(ts(nasa[, ncol(nasa)], start = start(nasa), freq = 12))
  y_raw <- y_raw0 <- ts(sunspots$raw, start = c(sunspots$year[1], sunspots$mon[1]), freq = 12)

  df <- sunspots_data(sqrt(y_raw + 0.001), p = 2*12, P = 35)

  f <- as.formula(paste("y~", paste(names(df)[-1], collapse = "+")))

  f <- list(f, update(f, NULL ~ .), update(f, NULL ~ .), update(f, NULL ~ .))

  b <- srt(f, data = df, K = log(nrow(df)), family = GA,
    lambda = 2, ntrees = 2000, cores = 50)

  saveRDS(b, file = "sunspots_model.rds")

  p <- model_forecast(b, data = df, h = length(x_nasa), family = family(b))
  p <- p^2 - 0.001

  saveRDS(p, file = "sunspots_model_forecast.rds")

  if(!file.exists("sunspots_model_forecast_distforest.rds")) {
    set.seed(456)
    m <- distforest(f[[1]], data = df, family = GA, ntree = 2000)
    pm <- model_forecast(m, data = df, h = length(x_nasa), family = family(b))
    pm <- pm^2 - 0.001
    pm <- pm[, 2]

    saveRDS(pm, file = "sunspots_model_forecast_distforest.rds")
  }

  png("figures/sunspots_forecast.png", units = "in", res = 150, width = 10, height = 3.8)
  par(mar = c(4.1, 4.1, 0.1, 0.1))
  scd <- zoo::as.zoo(cbind(zoo::as.zoo(y_raw0), p, zoo::as.zoo(x_nasa)))
  cycles <- c("1964-10", "1976-03", "1986-09", "1996-08",
    "2008-12", "2019-12", "2032-01", "2042-03")
  cycles <- zoo::as.yearmon(cycles)
  n <- nrow(scd)
  k <- n - 7*11*12-10
  craw <- rgb(0, 0, 0.3, alpha = 0.7)
  plot(scd[k:n, 1, drop = FALSE], plot.type = "single", col = craw,
    lwd = 2, ylab = "#Sunspots", xlab = "Time", lty = c(1, 2, 1, 2, 1),
    ylim = c(0, max(scd[,1], na.rm = TRUE) - 40), axes = FALSE)
  axis(1, at = as.numeric(cycles), labels = as.character(cycles))
  axis(2)
  pol <- na.omit(scd[, c("5%", "Median", "95%")])
  tp <- as.numeric(time(pol))
  pol <- as.matrix(pol)
  colnames(pol) <- c("2.5%", "Mean", "97.5%")
  bamlss::plot2d(pol ~ tp, add = TRUE, col.lines = 1, lwd = 3)
  #lines(pm, col = 4, lwd = 3, lty = 3)
  lines(scd[k:n, 5, drop = FALSE], col = 2, lwd = 3, lty = 2)
  abline(v = cycles, col = 1, lty = 2)
  cp <- cycles[-length(cycles)] + diff(cycles)/2
  text(cp, max(scd[,1], na.rm = TRUE) - 50, paste("Cycle", 20:26))
  #abline(v = as.numeric(zoo::as.yearmon("2024-09"))) ## 25: NASA+srt
  #abline(v = as.numeric(zoo::as.yearmon("2034-11"))) ## 26: NASA
  #abline(v = as.numeric(zoo::as.yearmon("2036-09"))) ## 26: srt
  dev.off()

  legend("top", c("#Sunspots", "#Forecast srt", "#Forecast NASA"),
    lty = 1, lwd = 2, col = c(1, 4, 2), bty = "n")
}

if(!file.exists("figures/lambdas.png")) {
  d <- readRDS("sc_eval.rds")

  d$year <- sapply(strsplit(as.character(d$Date), "-", fixed = TRUE), function(x) {
    as.numeric(x[1])
  })
  d$hf <- cut(d$h, breaks = seq(0, 300, by = 100), inlcude.lowest = TRUE,
    labels = c("short", "medium", "long"))

  mse <- aggregate(MSE ~ Distribution + Model + hf, data = d, FUN = mean, na.rm = TRUE)
  smape <- aggregate(SMAPE ~ Distribution + Model + hf, data = d, FUN = mean, na.rm = TRUE)
  crps <- aggregate(CRPS ~ Distribution + Model + hf, data = d, FUN = mean, na.rm = TRUE)

  get_min <- function(object, i = "CRPS") {
    res <- NULL
    for(j in levels(object$hf)) {
      a <- subset(object, hf == j)
      a <- a[order(a[[i]]), ]
      a$Distribution <- as.character(a$Distribution)
      a$Distribution[a$Model == "nasa"] <- "NA"
      a <- data.frame(
        "h" = j,
        "1st" = paste0(a$Model[1], "-", a$Distribution[1], ": ", formatC(a[[i]][1], width = 5)),
        "2nd" = paste0(a$Model[2], "-", a$Distribution[2], ": ", formatC(a[[i]][2], width = 5)),
        "3rd" = paste0(a$Model[3], "-", a$Distribution[3], ": ", formatC(a[[i]][3], width = 5)),
      check.names = FALSE)
      res <- rbind(res, a)
    }
    res
  }

  get_min(crps, "CRPS")
  get_min(mse, "MSE")
  get_min(smape, "SMAPE")

  png("figures/lambdas.png", units = "in", res = 150, width = 7.5, height = 3)
  par(mar = rep(0, 4), mfrow = c(1, 3), oma = c(4, 4, 4, 3))
  ylim <- c(0, 3500)
  dn <- subset(d, Model == "nasa" & Distribution == "NO")
  for(j in c("GA", "NO", "TF")) {
    d2 <- subset(d, Model == "srt" & Distribution == j)
    plot(MSE ~ h, data = d2, col = as.integer(as.factor(d2$lambda)),
      xlab = "Horizon [months]", pch = as.integer(as.factor(d2$lambda)),
      main = "", ylim = ylim, axes = FALSE)
    df <- subset(d, Model == "distforest" & Distribution == j)
    points(df$h, df$MSE, col = rgb(0.4, 0.4, 0, alpha = 0.7), pch = 8, cex = 0.7)
    points(dn$h, dn$MSE, pch = 16, cex = 1, col = gray(0.7))
    points(d2$h, d2$MSE, col = as.integer(as.factor(d2$lambda)), pch = as.integer(as.factor(d2$lambda)))
    ll <- levels(as.factor(d2$lambda))
    col <- colorspace::rainbow_hcl(length(ll))
    llexp <- lapply(paste0("lambda==", ll), function(x) parse(text = x))
    llexp <- do.call("c", llexp)
    box()
    if(j == "GA") {
      legend("topleft", llexp, col = 1:length(ll),
        pch = 1:length(ll), horiz = FALSE, ncol = 2, bty = "n")
      axis(1)
      axis(2)
    }
    if(j == "NO") {
      legend("topleft", c("NASA", "distforest"), col = c(gray(0.7), rgb(0.4, 0.4, 0, alpha = 0.7)),
        pch = c(16, 8), bty = "n")
    }
    if(j == "TF") {
      axis(4)
      axis(1)
    }
    mtext(j, side = 3, line = 1, font = 2)
  }
  mtext("Horizon [months]", side = 1, line = 2, outer = TRUE, cex = 0.8)
  mtext("MSE", side = 2, line = 2.5, outer = TRUE, cex = 0.8)
  dev.off()
}

