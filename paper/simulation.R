## Required packages.
library("bamlss")
library("disttree")
library("parallel")
library("mlbench")
library("gamlss.dist")
library("softtrees")

## Generate mlbench predictors.
make_eta <- function(n = 1000) {
  d1 <- mlbench.friedman1(n, sd = -1)
  d2 <- mlbench.friedman2(n, sd = -1)

  x <- as.data.frame(d1$x)
  z <- as.data.frame(d2$x)

  names(x) <- paste0("x", 1:ncol(x))
  names(z) <- paste0("z", 1:ncol(z))

  d <- list()
  d$mu <- d1$y
  d$sigma <- d2$y
  d$mu <- (d$mu - 1.5) / 26.48 * 2 + 1
  d$sigma <- (d$sigma - 7.96) / 1736.85 * 2 - 2.5

  for(j in names(x))
    x[[j]] <- scale(x[[j]])
  for(j in names(z))
    z[[j]] <- scale(z[[j]])

  d <- cbind(d, x, z)

  return(d)
}

## Eval data.
i <- round(runif(1, 10000, 1000000))
set.seed(123)
nd <- make_eta(10000)

##  Normal distribution.
nd$y <- rNO(nrow(nd), mu = nd$mu, sigma = exp(nd$sigma))
saveRDS(nd, file = "srt_eval_data_NO.rds")

## Gumbel distribution.
nd$y <- rGU(nrow(nd), mu = nd$mu, sigma = exp(nd$sigma))
saveRDS(nd, file = "srt_eval_data_GU.rds")

## Negative binomial distribution.
nd$y <- rNBI(nrow(nd), mu = exp(nd$mu), sigma = exp(nd$sigma))
saveRDS(nd, file = "srt_eval_data_NBI.rds")
set.seed(i)

## Error functions.
mape <- function(y, f) {
  f[abs(f) < 1e-10] <- NA
  median(abs(y - f)/abs(f), na.rm = TRUE)
}

mse <- function(y, f) {
  mean((y - f)^2, na.rm = TRUE)
}

## Simulation functions.
eval_NO <- function(n = 1000, ...) {
  d <- make_eta(n)
  d$y <- rNO(n, mu = d$mu, sigma = exp(d$sigma))

  nd <- readRDS("srt_eval_data_NO.rds")

  f <- y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z1 + z2 + z3 + z4

  f1 <- list(
    f,
    update(f, NULL ~ .)
  )

  f2 <- all.vars(f1[[2]])
  f2 <- as.formula(paste("y~", paste0("s(", f2, ")", collapse = "+")))
  f2 <- list(
    f2,
    update(f2, NULL ~ .)
  )

  b1 <- srt(f1, data = d, K = 2, family = NO, lambda = 5, plot = FALSE)
  b2 <- srt(f1, data = d, K = 2, family = NO, lambda = 2, plot = FALSE, ntrees = 100)
  b3 <- bamlss(f2, data = d, family = "gaussian")
  b4 <- distforest(f, data = d, family = NO, ntree = 1000)

  p1 <- predict(b1, newdata = nd)
  p2 <- predict(b2, newdata = nd)
  p3 <- predict(b3, newdata = nd)
  p4 <- predict(b4, newdata = nd)
  
  err.mu <- err.sigma <- list()
  for(f in c("mape", "mse")) {
    err.mu[[f]] <- list()
    err.mu[[f]]$srt <- eval(parse(text = paste0(f, '(nd$mu, p1$mu)')))
    err.mu[[f]]$srf <- eval(parse(text = paste0(f, '(nd$mu, p2$mu)')))
    err.mu[[f]]$bamlss <- eval(parse(text = paste0(f, '(nd$mu, p3$mu)')))
    err.mu[[f]]$distforest <- eval(parse(text = paste0(f, '(nd$mu, p4[, 1])')))
    err.mu[[f]] <- do.call("c", err.mu[[f]])

    err.sigma[[f]] <- list()
    err.sigma[[f]]$srt <- eval(parse(text = paste0(f, '(nd$sigma, p1$sigma)')))
    err.sigma[[f]]$srf <- eval(parse(text = paste0(f, '(nd$sigma, p2$sigma)')))
    err.sigma[[f]]$bamlss <- eval(parse(text = paste0(f, '(nd$sigma, p3$sigma)')))
    err.sigma[[f]]$distforest <- eval(parse(text = paste0(f, '(nd$sigma, log(p4[, 2]))')))
    err.sigma[[f]] <- do.call("c", err.sigma[[f]])
  }

  err.mu <- do.call("cbind", err.mu)
  err.sigma <- do.call("cbind", err.sigma)
  err.mu <- as.data.frame(err.mu)
  err.sigma <- as.data.frame(err.sigma)
  names(err.mu) <- toupper(names(err.mu))
  names(err.sigma) <- toupper(names(err.sigma))

  fam <- family(b3)

  p1$sigma <- exp(p1$sigma)
  p2$sigma <- exp(p2$sigma)
  p3$sigma <- exp(p3$sigma)

  crps <- c(
    "srt" = mean(fam$crps(nd$y, p1), na.rm = TRUE),
    "srf" = mean(fam$crps(nd$y, p2), na.rm = TRUE),
    "bamlss" = mean(fam$crps(nd$y, p3), na.rm = TRUE),
    "distforest" = mean(fam$crps(nd$y, p4), na.rm = TRUE)
  )

  err.mu$SCORE <- NA
  err.sigma$SCORE <- as.numeric(crps)

  err.mu$Model <- rownames(err.mu)
  err.sigma$Model <- rownames(err.sigma)
  err.mu$Parameter <- "mu"
  err.sigma$Parameter <- "sigma"

  rval <- rbind(err.mu, err.sigma)
  rval$Sim <- "NO"
  rval$Nobs <- n
  rownames(rval) <- NULL

  return(rval)
}

eval_GU <- function(n = 1000, ...) {
  d <- make_eta(n)
  d$y <- rGU(n, mu = d$mu, sigma = exp(d$sigma))

  nd <- readRDS("srt_eval_data_GU.rds")

  f <- y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z1 + z2 + z3 + z4

  f1 <- list(
    f,
    update(f, NULL ~ .)
  )

  f2 <- all.vars(f1[[2]])
  f2 <- as.formula(paste("y~", paste0("s(", f2, ")", collapse = "+")))
  f2 <- list(
    f2,
    update(f2, NULL ~ .)
  )

  b1 <- srt(f1, data = d, K = 2, family = GU, lambda = 5, plot = FALSE)
  b2 <- srt(f1, data = d, K = 2, family = GU, lambda = 2, plot = FALSE, ntrees = 100)
  b3 <- bamlss(f2, data = d, family = GU)
  b4 <- distforest(f, data = d, family = GU, ntree = 1000)

  p1 <- predict(b1, newdata = nd)
  p2 <- predict(b2, newdata = nd)
  p3 <- predict(b3, newdata = nd)
  p4 <- predict(b4, newdata = nd)

  crps_GU <- function(y, par) {
    rval <- try(bamlss:::.CRPS(y, as.data.frame(par), family(b2), c(-Inf, Inf)), silent = TRUE)
    if(inherits(rval, "try-error"))
      rval <- 9999
    return(rval)
  }

  err.mu <- err.sigma <- list()
  for(f in c("mape", "mse")) {
    err.mu[[f]] <- list()
    err.mu[[f]]$srt <- eval(parse(text = paste0(f, '(nd$mu, p1$mu)')))
    err.mu[[f]]$srf <- eval(parse(text = paste0(f, '(nd$mu, p2$mu)')))
    err.mu[[f]]$bamlss <- eval(parse(text = paste0(f, '(nd$mu, p3$mu)')))
    err.mu[[f]]$distforest <- eval(parse(text = paste0(f, '(nd$mu, p4[, 1])')))
    err.mu[[f]] <- do.call("c", err.mu[[f]])

    err.sigma[[f]] <- list()
    err.sigma[[f]]$srt <- eval(parse(text = paste0(f, '(nd$sigma, p1$sigma)')))
    err.sigma[[f]]$srf <- eval(parse(text = paste0(f, '(nd$sigma, p2$sigma)')))
    err.sigma[[f]]$bamlss <- eval(parse(text = paste0(f, '(nd$sigma, p3$sigma)')))
    err.sigma[[f]]$distforest <- eval(parse(text = paste0(f, '(nd$sigma, log(p4[, 2]))')))
    err.sigma[[f]] <- do.call("c", err.sigma[[f]])
  }

  err.mu <- do.call("cbind", err.mu)
  err.sigma <- do.call("cbind", err.sigma)
  err.mu <- as.data.frame(err.mu)
  err.sigma <- as.data.frame(err.sigma)
  names(err.mu) <- toupper(names(err.mu))
  names(err.sigma) <- toupper(names(err.sigma))

  p1$sigma <- exp(p1$sigma)
  p2$sigma <- exp(p2$sigma)
  p3$sigma <- exp(p3$sigma)

  crps <- c(
    "srt" = mean(crps_GU(nd$y, p1), na.rm = TRUE),
    "srf" = mean(crps_GU(nd$y, p2), na.rm = TRUE),
    "bamlss" = mean(crps_GU(nd$y, p3), na.rm = TRUE),
    "distforest" = mean(crps_GU(nd$y, p4), na.rm = TRUE)
  )

  err.mu$SCORE <- NA
  err.sigma$SCORE <- as.numeric(crps)

  err.mu$Model <- rownames(err.mu)
  err.sigma$Model <- rownames(err.sigma)
  err.mu$Parameter <- "mu"
  err.sigma$Parameter <- "sigma"

  rval <- rbind(err.mu, err.sigma)
  rval$Sim <- "GU"
  rval$Nobs <- n
  rownames(rval) <- NULL

  return(rval)
}

eval_NBI <- function(n = 1000, ...) {
  d <- make_eta(n)
  d$y <- rNBI(n, mu = exp(d$mu), sigma = exp(d$sigma))

  nd <- readRDS("srt_eval_data_NBI.rds")

  f <- y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + z1 + z2 + z3 + z4

  f1 <- list(
    f,
    update(f, NULL ~ .)
  )

  f2 <- all.vars(f1[[2]])
  f2 <- as.formula(paste("y~", paste0("s(", f2, ")", collapse = "+")))
  f2 <- list(
    f2,
    update(f2, NULL ~ .)
  )

  b1 <- srt(f1, data = d, K = 2, family = NBI, lambda = 4, plot = FALSE)
  b2 <- srt(f1, data = d, K = 2, family = NBI, lambda = 2, plot = FALSE, ntrees = 100)
  b3 <- bamlss(f2, data = d, family = NBI)
  b4 <- distforest(f, data = d, family = NBI, ntree = 1000)

  p1 <- predict(b1, newdata = nd)
  p2 <- predict(b2, newdata = nd)
  p3 <- predict(b3, newdata = nd)
  p4 <- predict(b4, newdata = nd)

  err.mu <- err.sigma <- list()
  for(f in c("mape", "mse")) {
    err.mu[[f]] <- list()
    err.mu[[f]]$srt <- eval(parse(text = paste0(f, '(nd$mu, p1$mu)')))
    err.mu[[f]]$srf <- eval(parse(text = paste0(f, '(nd$mu, p2$mu)')))
    err.mu[[f]]$bamlss <- eval(parse(text = paste0(f, '(nd$mu, p3$mu)')))
    err.mu[[f]]$distforest <- eval(parse(text = paste0(f, '(nd$mu, log(p4[, 1]))')))
    err.mu[[f]] <- do.call("c", err.mu[[f]])

    err.sigma[[f]] <- list()
    err.sigma[[f]]$srt <- eval(parse(text = paste0(f, '(nd$sigma, p1$sigma)')))
    err.sigma[[f]]$srf <- eval(parse(text = paste0(f, '(nd$sigma, p2$sigma)')))
    err.sigma[[f]]$bamlss <- eval(parse(text = paste0(f, '(nd$sigma, p3$sigma)')))
    err.sigma[[f]]$distforest <- eval(parse(text = paste0(f, '(nd$sigma, log(p4[, 2]))')))
    err.sigma[[f]] <- do.call("c", err.sigma[[f]])
  }

  err.mu <- do.call("cbind", err.mu)
  err.sigma <- do.call("cbind", err.sigma)
  err.mu <- as.data.frame(err.mu)
  err.sigma <- as.data.frame(err.sigma)
  names(err.mu) <- toupper(names(err.mu))
  names(err.sigma) <- toupper(names(err.sigma))

  logs <- function(y, par, ymin = 1L, ymax = max(max(y), 100L)) {
    fam <- family(b2)
    return(sum(-fam$d(y, par, log = TRUE), na.rm = TRUE))
  }

  log_scores <- c(
    "srt" = logs(nd$y, list("mu" = exp(p1$mu), "sigma" = exp(p1$sigma))),
    "srf" = logs(nd$y, list("mu" = exp(p2$mu), "sigma" = exp(p2$sigma))),
    "bamlss" = logs(nd$y, list("mu" = exp(p3$mu), "sigma" = exp(p3$sigma))),
    "distforest" = logs(nd$y, list("mu" = p4$mu, "sigma" = p4$sigma))
  )

  err.mu$SCORE <- NA
  err.sigma$SCORE <- as.numeric(log_scores)

  err.mu$Model <- rownames(err.mu)
  err.sigma$Model <- rownames(err.sigma)
  err.mu$Parameter <- "mu"
  err.sigma$Parameter <- "sigma"

  rval <- rbind(err.mu, err.sigma)
  rval$Sim <- "NBI"
  rval$Nobs <- n
  rownames(rval) <- NULL

  return(rval)
}

seval <- function(nrep = 50, nobs = c(500, 1000, 5000), cores = 50) {
  grid <- expand.grid("rep" = 1:nrep, "nobs" = nobs)

  ind <- 1:nrow(grid)
  ind <- split(ind, factor(sort(rep(1:cores, length.out = nrep))))

  eval_funs <- list(eval_NO, eval_GU, eval_NBI)

  parallel_fun <- function(i) {
    cat("..starting section", i, "\n")

    res <- list()
    k <- 1
    for(j in ind[[i]]) {
      for(fun in eval_funs) {
        res[[k]] <- fun(grid[j, "nobs"])
        k <- k + 1
      }
    }

    do.call("rbind", res)
  }

  res <- mclapply(1:length(ind), parallel_fun, mc.cores = cores)
  res <- do.call("rbind", res)

  res
}

if(!file.exists("sim_results.rds")) {
  set.seed(123)

  sim_results <- seval(nrep = 100, nobs = c(500, 1000, 5000, 10000), cores = 50)

  saveRDS(sim_results, file = "sim_results.rds")
}
