#softforest <- function(formula, data, ntree = 1, size = 0.63, cores = 1, ...)
#{
#  stopifnot(requireNamespace("parallel"))
#  
#  n <- nrow(data)
#  ind <- 1:n

#  foo <- function(i) {
#    cat(".. .. starting tree", i, "\n")
#    di <- data[sample(ind, replace = FALSE, size = floor(size * n)), , drop = FALSE]
#    softtree(formula, data = di, plot = FALSE, ...)
#  }

#  rval <- parallel::mclapply(1:ntree, foo, mc.cores = cores)

#  class(rval) <- "softforest"

#  return(rval)
#}

#predict.softforest <- function(object, newdata = NULL, FUN = mean, ...)
#{
#  p <- NULL
#  for(j in 1:length(object)) {
#    p <- cbind(p, predict(object[[j]], newdata = newdata))
#  }
#  p <- apply(p, 1, FUN)
#  return(p)
#}

#softtree <- function(formula, data = NULL, scale.x = TRUE, lambda = c(1e-2, 1e-10),
#  smin = 100, K = NULL, maxit = NULL, verbose = TRUE, plot = FALSE, ...)
#{
#  stopifnot(requireNamespace("mgcv"))

#  if(!is.list(formula))
#    formula <- list(formula, formula)
#  formula[[2]] <- update(formula[[2]], NULL ~ .)

#  rval <- mgcv::gam(formula[[1]], family = gaussian(), data = data, method = "REML")
#  y <- model.response(model.frame(rval), "numeric")
#  rval$lm <- stats::lm(update(formula[[2]], y ~ .), data = data)
#  x <- model.matrix(rval$lm)
#  rval$lm$model <- NULL
#  rval$lm$terms <- terms(formula[[2]])

#  if(scale.x) {
#    scalex <- list()
#    for(j in colnames(x)) {
#      if(j != "(Intercept)") {
#        scalex[[j]] = c("mean" = mean(x[, j]), "sd" = sd(x[, j]))
#        x[, j] <- (x[, j] - scalex[[j]]["mean"]) / scalex[[j]]["sd"]
#      }
#    }
#    rval$scalex <- scalex
#  }

#  rval$softtree <- softtree.fit(x, y, offset = offset, lambda = lambda,
#    smin = smin, K = K, maxit = maxit, verbose = verbose, plot = plot,
#    eta = rval$fitted.values, edf = sum(rval$edf))

#  class(rval) <- c("softtree", class(rval))

#  return(rval)
#}

#predict.softtree <- function(object, newdata = NULL, ...)
#{
#  eta <- mgcv::predict.gam(object, newdata)

#  if(length(object$softtree$coefficients) > 0) {
#    x <- model.matrix(object$lm, data = newdata)
#    if(!is.null(object$scalex)) {
#      for(j in colnames(x)) {
#        if(j != "(Intercept)") {
#          x[, j] <- (x[, j] - object$scalex[[j]]["mean"]) / object$scalex[[j]]["sd"]
#        }
#      }
#    }
#    coef <- object$softtree$coefficients
#    P <- data.frame(rep(1.0, NROW(x)), check.names = FALSE, fix.empty.names = FALSE)
#    for(i in 1:length(coef)) {
#      j <- coef[[i]]$split
#      w <- coef[[i]]$coefficients[-c(1:2)]
#      beta <- coef[[i]]$coefficients[1:2]
#      prob <- sig(drop(x %*% w))
#      eta <- eta + P[, j] * (prob * beta[1] + (1 - prob) * beta[2])
#      P[, j] <- cbind(prob, 1 - prob) * P[, j]
#      P <- as.data.frame(as.matrix(P), make.names = FALSE)
#    }
#  }
#  return(eta)
#}

#fitted.softtree <- function(object, ...)
#{
#  return(object$softtree$fitted.values)
#}

#sig <- function(x) {
#  sigmoid(x)
#}

#sig_deriv <- function(x) {
#  f <- sigmoid(x)
#  f * (1 - f)
#}

#find_split <- function(x, y, eta, p, lambda)
#{
#  lambda <- rep(lambda, length.out = 2)

#  fn <- function(parameters) {
#    w <- parameters[-c(1:2)]
#    beta <- parameters[1:2]
#    prob <- sig(x %*% w)
#    eta <- eta + p * (prob * beta[1] + (1 - prob) * beta[2])
#    sum((y - eta)^2) + lambda[2] * sum(w^2) + lambda[1] * sum(beta^2)
#  }

##  gr <- function(parameters) {
##    w <- parameters[-c(1:2)]
##    beta <- parameters[1:2]
##    xw <- drop(x %*% w)
##    prob <- sig(xw)

##    s <- cbind(
##      -(2 * (p/(1 + exp(-xw)) * (y - eta - beta[1] * p/(1 + exp(-xw)) - beta[2] * p * (1 - 1/(1 + exp(-xw)))))),
##      -(2 * (p * (1 - 1/(1 + exp(-xw))) * (y - eta - beta[1] * p/(1 + exp(-xw)) - beta[2] * p * (1 - 1/(1 + exp(-xw)))))),
##      -(2 * ((beta[1] * p * exp(-xw)/(1 + exp(-xw))^2 - beta[2] * p * (exp(-xw)/(1 + exp(-xw))^2)) *
##        (y - eta - beta[1] * p/(1 + exp(-xw)) - beta[2] * p * (1 - 1/(1 + exp(-xw)))))) * x
##    )

#### D(expression((y - eta - b1*p/(1+exp(-(w0 + w1*z))) - b2*p*(1-1/(1+exp(-(w0 + w1*z)))))^2), "w0")

##    g <- colSums(s, na.rm = TRUE) - diag(c(lambda[1], lambda[1], rep(lambda[2], length(w)))) %*% parameters

##    g
##  }

#  rval <- optim(c(1, 1, 1, rep(0.1, NCOL(x) - 1L)), fn = fn, gr = NULL, method = "BFGS")

#  w <- rval$par[-c(1:2)]
#  beta <- rval$par[1:2]
#  prob <- drop(sig(x %*% w))
#  rval$fitted.values <- drop(p * (prob * beta[1] + (1 - prob) * beta[2]))
#  rval$prob <- prob

#  return(rval)
#}

#softtree.fit <- function(x, y, offset = NULL,
#  smin = 50, K = NULL, maxit = NULL,
#  verbose = TRUE, eta = NULL, edf = 0, plot = TRUE, ...)
#{
#  ia <- interactive()
#  if(!ia)
#    plot <- FALSE
#  if(plot) {
#    opar <- par(no.readonly = TRUE)
#    on.exit(par(opar))
#  }
#  coef <- list()
#  if(is.null(eta))
#    eta <- rep(0, nrow(x))
#  vars <- colnames(x)
#  vars <- vars[!grepl("(Intercept)", vars)]
#  k <- length(vars)
#  edf <- edf + 1
#  iter <- 1
#  if(k > 0) {
#    N <- NROW(x)
#    if(N < smin)
#      stop("not enough data, set smin!")
#    if(is.null(K))
#      K <- log(N)
#    P <- data.frame(rep(1.0, N), check.names = FALSE, fix.empty.names = FALSE)
#    do <- TRUE
#    res <- y - eta
#    ll <- 0.5 * (0.0 - N * (log(2 * pi) + 1 - log(N) + log(sum(res^2))))
#    aic <- aic00 <- -2 * ll + K * edf
#    eta00 <- eta
#    if(!is.null(maxit)) {
#      if(maxit < 1) {
#        do <- FALSE
#      }
#    }
#    while(do) {
#      ic <- rep(NA, NCOL(P))
#      sj <- list()
#      err0 <- sum((y - eta)^2)
#      for(j in 1:NCOL(P)) {
#        if(sum(P[, j] > 1e-03) < smin) {
#          ic[j] <- Inf
#        } else {
#          sj[[j]] <- find_split(x, y, eta, P[, j], ...)
#          ic[j] <- sj[[j]]$value
#        }
#      }
#      if(all(!is.finite(ic)))
#        break
#      if(all(abs(ic - err0) <= 1e-07))
#        break
#      j <- which.min(ic)
#      coef[[iter]] <- list("split" = j, "coefficients" = sj[[j]]$par)
#      eta <- eta + fitted(sj[[j]])
#      P[, j] <- cbind(sj[[j]]$prob, 1 - sj[[j]]$prob) * P[, j]
#      P <- as.data.frame(as.matrix(P), make.names = FALSE)
#      res <- y - eta
#      ll <- c(ll, 0.5 * (0.0 - N * (log(2 * pi) + 1 - log(N) + log(sum(res^2)))))
#      aic <- c(aic, -2 * ll[iter] + K * (ncol(P) + edf))
#      if(plot & ia) {
#        par(mfrow = c(1, 2))
#        plot(aic, type = "l", xlab = "Iteration", ylab = "-2 * logLik + K * df", lwd = 2)
#        plot(eta, y, xlab = "Fitted values", ylab = "Response")
#        abline(0, 1, lwd = 2, col = 4)
#      }
#      if(is.null(maxit)) {
#        if((aic[length(aic)] >= aic[length(aic) - 1L]) & (iter > 1)) {
#          if(all(aic > aic00)) {
#            aic <- aic00
#            coef <- list()
#            eta <- eta00
#          } else {
#            coef <- coef[-length(coef)]
#            aic <- aic[length(aic) - 1L]
#          }
#          do <- FALSE
#        }
#      } else {
#        if(iter >= maxit)
#          do <- FALSE
#      }
#      if((iter > 1) & ia)
#        cat("\r")
#      if(verbose) {
#        pt <- paste(".. .. iter =", iter, "AIC =", aic[length(aic)], if(!ia) "\n" else NULL)
#        cat(pt)
#      }
#      iter <- iter + 1
#    }

#    if(ia & verbose)
#      cat("\n")

#    rval <- list(
#      "fitted.values" = eta,
#      "coefficients" = coef,
#      "AIC" = aic,
#      "splits" = length(coef)
#    )
#  } else {
#    rval <- list("fitted.values" = eta, "coefficients" = coef)
#  }
#  return(rval)
#}


srt <- function(formula, family = NULL, data = NULL,
  weights = NULL, subset = NULL, offset = NULL, contrasts = NULL,
  classic = FALSE, ntrees = 1, prob = 0.63, cores = 1, k = 10, lambda = 0.1, aic = TRUE,
  maxs = 100, plot = TRUE, verbose = TRUE, model = TRUE, x = FALSE, y = FALSE, 
  scale.x = FALSE, method = c("adaptive", "full"), ...)
{
  if(is.null(family))
    family <- Gaussian
  if(is.function(family))
    family <- family()
  if(inherits(family, "gamlss.family"))
    family <- tF(family)
  family <- complete_family(family)
  if(!is.list(formula))
    formula <- list(formula)
  if(length(formula) < length(family$names)) {
    for(j in (length(formula) + 1):length(family$names))
      formula[[j]] <- ~ 1
  }
  formula <- rep(formula, length = length(family$names))
  names(formula) <- family$names

  ret.x <- x
  ret.y <- y

  method <- match.arg(method)

  tree_index <- list(...)$tree_index
  if(!is.null(tree_index))
    ntrees <- length(tree_index)
  if(ntrees > 1) {
    if(!is.list(lambda))
      lambda <- rep(list(lambda), ntrees)
  }

  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action",  "offset"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf$na.action <- na.fail
  mf[c("offset", "weights")] <- NULL
  mf[[1L]] <- quote(model.frame)

  mfd <- list("X" = list(), "model" = list(), "terms" = list())

  if(scale.x)
    mfd$scaled <- list()

  prc <- list(...)$prc
  if(is.null(prc))
    prc <- FALSE
  rank <- list(...)$rank

  if(prc)
    mfd$prc <- list()

  offset <- as.list(substitute(offset))
  weights <- as.list(substitute(weights))
  if(length(offset) > 1) {
    offset <- offset[-1]
  } else {
    offset <- rep(offset, length.out = length(formula))
  }
  if(length(weights) > 1) {
    weights <- weights[-1]
  } else {
    weights <- rep(weights, length.out = length(formula))
  }
  nx <- names(formula)
  names(offset) <- nx[1:length(offset)]
  names(weights) <- nx[1:length(weights)]

  nas <- NULL
  for(i in seq_along(formula)) {
    mf[[2]] <- as.call(formula[[i]])
    if(length(offset)) {
      if(!is.null(offset[[nx[i]]]))
        mf$offset <- as.name(as.character(offset[[nx[i]]]))
    }
    if(length(weights)) {
      if(!is.null(weights[[nx[i]]]))
        mf$weights <- as.name(as.character(weights[[nx[i]]]))
    }
    mfd$model[[i]] <- eval(mf, parent.frame())
    mf[c("offset", "weights")] <- NULL
    nas <- c(nas, which(rowSums(is.na(mfd$model[[i]])) > 0))
  }
  if(length(nas) > 0) {
    for(i in names(formula)) {
      mfd$model[[i]] <- mfd$model[[i]][-nas, , drop = FALSE]
    }
  }

  names(mfd$model) <- nx

  for(i in names(formula)) {
    mt <- attr(mfd$model[[i]], "terms")
    mfd$terms[[i]] <- mt

    mfd$X[[i]] <- model.matrix(mt, mfd$model[[i]], contrasts.arg = contrasts)

    sok <- TRUE
    if(ncol(mfd$X[[i]]) < 2) {
      if(length(unique(mfd$X[[i]][, 1])) < 2)
        sok <- FALSE
    }

    if(sok) {
      vn <- colnames(mfd$X[[i]])
      nu <- apply(mfd$X[[i]], 2, function(x) length(unique(x)))
      vn <- vn[nu > 2]
      if(scale.x) {
        mfd$scaled[[i]] <- list()
        for(j in vn) {
          sx <- list(
            "mean" = mean(mfd$X[[i]][, j], na.rm = TRUE),
            "sd" = sd(mfd$X[[i]][, j], na.rm = TRUE)
          )
          mfd$X[[i]][, j] <- (mfd$X[[i]][, j] - sx$mean) / sx$sd

#          sx <- list()
#          sx$min <- min(mfd$X[[i]][, j], na.rm = TRUE)
#          sx$max <- max(mfd$X[[i]][, j], na.rm = TRUE)

#          mfd$X[[i]][, j] <- (mfd$X[[i]][, j] - sx$min) / (sx$max - sx$min)

          mfd$scaled[[i]][[j]] <- sx
        }
      }

      if(prc) {
        if(ncol(mfd$X[[i]]) > 1L) {
          j <- grep("(Intercept)", colnames(mfd$X[[i]]), fixed = TRUE)
          rank2 <- if(is.null(rank)) {
            ncol(mfd$X[[i]]) - 1L
          } else rank
          mfd$prc[[i]] <- prcomp(mfd$X[[i]][, -j, drop = FALSE],
            center = TRUE, scale = TRUE, rank = rank2)
          mfd$X[[i]] <- cbind("(Intercept)" = 1, mfd$prc[[i]]$x)
        }
      }
    }

    if(!is.null(model.offset(mfd$model[[i]])))
      mfd$offset[[i]] <- model.offset(mfd$model[[i]])
    if(!is.null(model.offset(mfd$weights[[i]])))
      mfd$weights[[i]] <- model.weights(mfd$model[[i]])

    if(nrow(mfd$X[[i]]) < 1)
      mfd$X[[i]] <- cbind("(Intercept)" = rep(1, nrow(mfd$model[[1L]])))
    attr(mfd$X[[i]], "offset") <- offset[[i]]
  }

  mfd$y <- model.response(mfd$model[[names(formula)[1L]]])
  if(!is.null(ar.start <- list(...)$ar.start))
  mfd$y <- cbind("Y" = mfd$y, "ar.start" = ar.start)

  if(!is.null(list(...)$ret.mfd)) {
    mfd$family <- family
    mfd$formula <- formula
    return(mfd)
  }

  if(!is.null(vd <- list(...)$validdata)) {
    mfd$family <- family
    mfd$formula <- formula
    mfd$call <- cl
    mfd$contrasts <- contrasts
    class(mfd) <- "srt"
    mfd$vX <- model.matrix.srt(mfd, data = vd)
    mfd$vy <- model.response(model.frame.srt(mfd, data = vd)[[names(formula)[1L]]])
  }

  .fit.fun <- if(method == "adaptive") .srt.fit else .srt.fit2

  if(ntrees > 1) {
    N <- nrow(mfd$model[[1L]])

    parallel_fun <- function(i) {
      cat(".. starting tree", i, "\n")
      if(is.null(tree_index))
        index <- sample(1:N, size = floor(N * prob), replace = FALSE)
      else
        index <- tree_index[[i]]
      .fit.fun(mfd$X, mfd$y,
        family = family, k = k, lambda = lambda[[i]], aic = aic,
        plot = FALSE, verbose = verbose, maxs = maxs, index = index,
        classic = classic, offset = mfd$offset, weights = mfd$weights,
        vX = mfd$vX, vy = mfd$vy, ...)
    }

    rval <- parallel::mclapply(1:ntrees, parallel_fun, mc.cores = cores)
    names(rval) <- paste0("tree", 1:length(rval))

    class(rval) <- "srf"
  } else {
    rval <- .fit.fun(mfd$X, mfd$y,
      family = family, k = k, lambda = lambda, aic = aic,
      plot = plot, verbose = verbose, maxs = maxs,
      classic = classic, offset = mfd$offset, weights = mfd$weights,
      vX = mfd$vX, vy = mfd$vy, ...)

    class(rval) <- "srt"
  }

  rval$call <- cl
  rval$family <- family
  rval$formula <- formula
  rval$terms <- mfd$terms
  rval$contrasts <- contrasts
  rval$method <- method

  if(scale.x)
    rval$scaled <- mfd$scaled
  if(prc)
    rval$prc <- mfd$prc

  if(model)
    rval$model <- mfd$model
  if(ret.x)
    rval$x <- mfd$X
  if(ret.y)
    rval$y <- mfd$y

  return(rval)
}


family.srt <- family.srf <- family.srtboost <- family.srtm <- function(object, ...)
{
  return(object$family)
}

erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1

abs2 <- function(x) {
  sqrt(x^2 + 1e-05)
}

exp2 <- function(x) {
  x[x > 20] <- 20
  x <- exp(x)
  x
}

sigmoid <- function(x) {
  1 / (1 + exp2(-x))
}

sigmoid_deriv <- function(x) {
  f <- sigmoid(x)
  f * (1 - f)
}

#sigmoid <- function(x) {
#  (x / sqrt(1 + x^2) + 1) * 0.5
#}

#sigmoid <- function(x) {
#  (erf(sqrt(pi) * 0.5 * x) + 1) * 0.5
#}

#sigmoid <- function(x) {
#  tanh(x) * 0.5 + 0.5
#}

#sigmoid <- function(x) {
#  pnorm(x)
#}

#sigmoid_deriv <- function(x) {
#  dnorm(x)
#}

node_fun <- function(w, x, weights, y, I, lambda, eta, i, family, Y, W) {
  beta <- w[1:2]
  w <- w[-c(1:2)]
  g <- sigmoid(x %*% w)
  G <- cbind(g, 1 - g) * weights
  eta[[i]] <- eta[[i]] + drop(G %*% beta)
  ll <- family$loglik(Y, family$map2par(eta))
  -1 * ll + lambda * sum(w[-1]^2) + I[1] * sum(beta^2) ##+ 0.5 * lambda * sum(abs2(w[-1]))
}

node_fun_grad <- function(w, x, weights, y, I, lambda, eta, i, family, Y, W) {
  beta <- w[1:2]
  w <- w[-c(1:2)]

  xw <- drop(x %*% w)
  g <- sigmoid(xw)
  G <- cbind(g, 1 - g) * weights

  eta[[i]] <- eta[[i]] + drop(G %*% beta)

  score <- process_derivs(family$score[[i]](Y, family$map2par(eta), id = i), is.weight = FALSE)

  s <- cbind(score * G, score * weights * (beta[1] - beta[2]) * sigmoid_deriv(xw) * x)

  grad <- colSums(s) - diag(c(I[1], I[1], 0, rep(lambda, length(w) - 1))) %*% c(beta, w)

  -grad
}

node_fun_hard <- function(w, x, weights, y, I, eta, i, family, Y, mx, W) {
  g <- sigmoid(mx * (x - w))
  G <- cbind(g, 1 - g) * weights
  GW <- G * W
  beta <- solve(crossprod(GW, G) + I, t(GW) %*% y)
  eta[[i]] <- eta[[i]] + drop(G %*% beta)
  ll <- family$loglik(Y, family$map2par(eta))
  -1 * ll
}

init.eta <- function(eta, y, family, nobs)
{
  if(is.null(family$initialize))
    return(eta)
  for(j in family$names) {
    if(!is.null(family$initialize[[j]])) {  
      linkfun <- make.link2(family$links[j])$linkfun
      eta[[j]] <- linkfun(family$initialize[[j]](y))
      eta[[j]] <- rep(eta[[j]], length.out = nobs)
    }
  }

  return(eta)
}


extract_nnet_weights <- function(x)
{
  if(inherits(x, "nnet"))
    x <- coef(x)
  nw <- nw0 <- names(x)
  nw0 <- sapply(strsplit(nw0, "->", fixed = TRUE), function(x) { x[2] })
  nw0 <- paste0("->", nw0)
  nw <- grep("->h", nw, fixed = TRUE, value = TRUE)
  nw <- sapply(strsplit(nw, "->", fixed = TRUE), function(x) { x[2] })
  nw <- unique(nw)
  wts <- NULL
  for(j in nw) {
    xtmp <- x[nw0 == paste0("->", j)]
    wts <- cbind(wts, xtmp)
  }
  rownames(wts) <- NULL
  colnames(wts) <- nw
  return(wts)
}


## Formatting for printing.
fmt <- Vectorize(function(x, width = 8, digits = 2) {
  txt <- formatC(round(x, digits), format = "f", digits = digits, width = width)
  if(nchar(txt) > width) {
    txt <- strsplit(txt, "")[[1]]
    txt <- paste(txt[1:width], collapse = "", sep = "")
  }
  txt
})


## Fitting function.
.srt.fit <- function(X, y, family, k = 10, lambda = 0.1, aic = TRUE,
  K = NULL, plot = TRUE, verbose = TRUE, maxit = Inf, index = NULL,
  eps = 1e-05, find.lambda = FALSE, classic = FALSE,
  initialize = TRUE, maxs = Inf, select = FALSE, linear = FALSE, full = FALSE,
  hard = FALSE, offset = NULL, weights = NULL, start = NULL,
  vX = NULL, vy = NULL, vFUN = NULL, ...)
{
  nx <- names(X)

  const <- list(...)$const
  if(is.null(const))
    const <- 1e-5

  do_optim <- list(...)$do_optim
  if(is.null(do_optim))
    do_optim <- FALSE

  valid <- !is.null(vX)

  linear <- rep(linear, length.out = length(nx))
  names(linear) <- nx

  if(!is.null(index)) {
    for(i in nx) {
      X[[i]] <- X[[i]][index, , drop = FALSE]
    }
    if(is.null(dim(y))) {
      y <- y[index]
    } else {
      y <- y[index, , drop = FALSE]
    }
  }

  ia <- interactive()

  gamma <- list(...)$gamma
  if(is.null(gamma))
    gamma <- 0.1
  force <- list(...)$force
  flush <- list(...)$flush
  if(is.null(flush))
    flush <- TRUE
  edf2 <- list(...)$edf2
  if(is.null(edf2))
    edf2 <- FALSE

  if(is.null(force))
    force <- TRUE
  if(hard)
    classic <- TRUE

  faster <- list(...)$faster
  if(is.null(faster))
    faster <- FALSE

  discrete <- FALSE
  if(!is.null(family$type)) {
    if(tolower(family$type) == "discrete")
      discrete <- TRUE
  }
  if(family$family %in% c("binomial", "nbinom"))
    discrete <- TRUE

  N <- nrow(X[[1L]])
  if(is.null(K))
    K <- log(N)

  lw <- eta <- G <- crit <- beta <- tcoef <- coef <- index <- eta_lin <- cl <- vlw <- vG <- list()
  is_factor <- Pl <- list()
  for(i in nx) {
    lw[[i]] <- data.frame(rep(1, N))
    if(valid)
      vlw[[i]] <- data.frame(rep(1, nrow(vX[[1L]])))
    eta[[i]] <- rep(0, N)
    if(linear[i]) {
      eta_lin[[i]] <- rep(0, N)
      cl[[i]] <- rep(0, ncol(X[[i]]))
      Pl[[i]] <- diag(const, ncol(X[[i]]))
      names(cl[[i]]) <- colnames(X[[i]])
    }
    coef[[i]] <- list()
    is_factor[[i]] <- apply(X[[i]], 2, function(x) {
      length(unique(x)) == 2L
    })
  }

  if(valid) {
    veta <- list()
    for(i in nx)
      veta[[i]] <- rep(0, nrow(vX[[1L]]))
  }

  eta0 <- init.eta(eta, y, family, N)
  for(i in nx) {
    if(!is.null(offset[[i]])) {
      eta[[i]] <- eta[[i]] + offset[[i]]
    } else {
      eta[[i]] <- eta0[[i]]
    }
  }

  if(initialize & !any(linear) & is.null(start)) {
    eta0 <- init.eta(eta, y, family, N)

    Xi <- cl2 <- list()
    for(i in nx) {
      Xi[[i]] <- X[[i]][, "(Intercept)", drop = FALSE]
      cl2[[i]] <- 0
    }

    if(verbose)
      cat(".. initializing parameters ..\n")

    if(do_optim) {
      obj_fun <- function(beta) {
        eta <- list()
        for(i in nx)
          eta[[i]] <- rep(beta[i], N)
        ll <- family$loglik(y, family$map2par(eta))
        return(-ll)
      }

      opt <- optim(unlist(cl2), fn = obj_fun, method = "L-BFGS-B")
      cl2 <- as.list(opt$par)
      eta <- list()
      for(i in nx)
        eta[[i]] <- rep(cl2[[i]], N)
      if(verbose)
        cat(".. logLik =", -1 * opt$value, "..\n")
    } else {
      eps0 <- eps + 1L
      k2 <- 0

      while((1e-04 < eps0) & (k2 < 400)) {
        eta0 <- eta
        for(i in nx) {
          ll0 <- family$loglik(y, family$map2par(eta))

          peta <- family$map2par(eta)
          hess <- process_derivs(family$hess[[i]](y, peta, id = i), is.weight = TRUE)
          score <- process_derivs(family$score[[i]](y, peta, id = i), is.weight = FALSE)
          z <- eta[[i]] + 1 / hess * score

          b0 <- cl2[[i]]

         if(is.null(weights[[i]])) {
            XW <- Xi[[i]] * hess
         } else {
            XW <- Xi[[i]] * hess * weights[[i]]
          }

          cl2[[i]] <- 1/(sum(XW, na.rm = TRUE) + 1e-20) * sum(XW * z, na.rm = TRUE)

          eta[[i]] <- drop(Xi[[i]] * cl2[[i]])

          ll1 <- family$loglik(y, family$map2par(eta))

          if(ll1 < ll0) {
            fnu <- function(nu) {
              b <- nu * cl2[[i]] + (1 - nu) * b0
              eta[[i]] <- drop(Xi[[i]] * b)
              return(family$loglik(y, family$map2par(eta)))
            }
            objfun0 <- function(nu) {
              ll0 - fnu(nu)
            }
            nu <- optimize(objfun0, c(0, 1))$minimum
            cl2[[i]] <- nu * cl2[[i]] + (1 - nu) * b0
            eta[[i]] <- drop(Xi[[i]] * cl2[[i]])
          }
        }
        eps0 <- do.call("cbind", eta)
        eps0 <- mean(abs((eps0 - do.call("cbind", eta0)) / eps0), na.rm = TRUE)

        if(verbose) {
          cat(if(ia & flush) "\r" else if(k2 > 1) "\n" else NULL)
          cat(".. logLik =", fmt(family$loglik(y, family$map2par(eta))), "eps =", fmt(eps0, digits = 5),
            "iter =", formatC(k2, width = 5), if(!ia) "..\n" else NULL)
        }

        k2 <- k2 + 1
      }

      if(verbose)
        cat("\n")
    }

    if(valid) {
      for(i in nx) {
        veta[[i]] <- drop(vX[[i]][, "(Intercept)"] * cl2[[i]])
      }
    }

    for(i in nx) {
      coef[[i]][[1L]] <- list("w" = "root", "beta" = as.numeric(cl2[[i]]))
    }

    if(length(nx) < 2) {
      if(ncol(X[[1L]]) < 2) {
        return(list("fitted.values" = eta, coefficients = coef, "index" = list(1)))
      }
    }
  }

  if(!is.null(start)) {
    start <- as.numeric(unlist(start))
    for(i in seq_along(nx)) {
      coef[[i]][[1L]] <- list("w" = "root", "beta" = as.numeric(start[i]))
      eta[[nx[i]]] <- rep(start[i], N)
      if(valid)
        veta[[nx[i]]] <- rep(start[i], nrow(vX[[1L]]))
    }
  }

  if(any(linear)) {
    eps0 <- eps + 1L
    k2 <- 0

    if(verbose)
      cat(".. estimate linear model ..\n")

    if(do_optim) {

      obj_fun_l <- function(beta) {
        eta <- list()
        for(i in nx) {
          b <- beta[grep(paste0(i, "."), names(beta), fixed = TRUE)]
          eta[[i]] <- drop(X[[i]] %*% b)
        }
        ll <- family$loglik(y, family$map2par(eta))
        return(-ll)
      }

      opt <- optim(unlist(cl), fn = obj_fun_l, method = "L-BFGS-B")

      eta <- cl <- list()
      for(i in nx) {
        cl[[i]] <- opt$par[grep(paste0(i, "."), names(opt$par), fixed = TRUE)]
        eta[[i]] <- drop(X[[i]] %*% cl[[i]])
      }
      if(verbose)
        cat(".. logLik =", -1 * opt$value, "..\n")

    } else {
      while((1e-04 < eps0) & (k2 < 400)) {
        eta0 <- eta
        for(i in nx) {
          ll0 <- family$loglik(y, family$map2par(eta))
          if(linear[i]) {
            peta <- family$map2par(eta)
            hess <- process_derivs(family$hess[[i]](y, peta, id = i), is.weight = TRUE)
            score <- process_derivs(family$score[[i]](y, peta, id = i), is.weight = FALSE)
            z <- eta[[i]] + 1 / hess * score

            b0 <- cl[[i]]

            if(is.null(weights[[i]])) {
              XW <- X[[i]] * hess
            } else {
              XW <- X[[i]] * hess * weights[[i]]
            }

            cl[[i]] <- drop(chol2inv(chol(crossprod(XW, X[[i]]) + Pl[[i]])) %*% crossprod(XW, z))

            eta[[i]] <- drop(X[[i]] %*% cl[[i]])

            ll1 <- family$loglik(y, family$map2par(eta))

            if(ll1 < ll0) {
              fnu <- function(nu) {
                b <- nu * cl[[i]] + (1 - nu) * b0
                eta[[i]] <- drop(X[[i]] %*% b)
                return(family$loglik(y, family$map2par(eta)))
              }
              objfun1 <- function(nu) {
                ll0 - fnu(nu)
              }
              nu <- optimize(objfun1, c(0, 1))$minimum
              cl[[i]] <- nu * cl[[i]] + (1 - nu) * b0
              eta[[i]] <- drop(X[[i]] %*% cl[[i]])
            }
          }
        }
        eps0 <- do.call("cbind", eta)
        eps0 <- mean(abs((eps0 - do.call("cbind", eta0)) / eps0), na.rm = TRUE)

        if(verbose) {
          cat(if(ia & flush) "\r" else if(k2 > 1) "\n" else NULL)
          cat(".. logLik =", fmt(family$loglik(y, family$map2par(eta))),
            "eps =", fmt(eps0, digits = 5), "iter =", formatC(k2, width = 5), if(!ia) "..\n" else NULL)
        }

        k2 <- k2 + 1
      }
      for(i in nx)
        names(cl[[i]]) <- colnames(X[[i]])

      if(valid) {
        for(i in nx)
          veta[[i]] <- drop(vX[[i]] %*% cl[[i]])
      }
    }
  }

  eta2 <- eta

  I <- diag(gamma, 2L)
  ic <- logLik <- edf_save <- size_save <- vic <- vlogLik <- NULL

  do <- TRUE
  iter <- 1

  jj <- rep(NA, length(nx))
  names(jj) <- nx

  lambda <- rep(lambda, length.out = length(nx))
  names(lambda) <- nx

  eps0 <- eps + 1

  if(!ia)
    plot <- FALSE
  if(plot) {
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
  }

  while(do & (iter < maxit) & (eps0 > eps)) {
    eta0 <- eta
    if(select)
      ll0 <- family$loglik(y, family$map2par(eta))
    for(i in nx) {
      if(ncol(X[[i]]) < 2) {
        objfun3 <- function(par) {
          eta[[i]] <- rep(par, length = N)
          if(is.null(weights[[i]])) {
            ll <- family$loglik(y, family$map2par(eta))
          } else {
            d <- family$d(y, family$map2par(eta)) * weights
            ll <- sum(d, na.rm = TRUE)
          }
          return(ll)
        }
    
        gradfun3 <- function(par) {
          eta[[i]] <- rep(par, length = N)
          peta <- family$map2par(eta)
          score <- process_derivs(family$score[[i]](y, peta, id = i), is.weight = FALSE)
          if(is.null(weights[[i]])) {
            grad <- sum(score)
          } else {
            grad <- sum(score * weights[[i]])
          }
          return(grad)
        }

        opt <- optim(mean(eta[[i]]), fn = objfun3, gr = gradfun3,
          method = "L-BFGS-B", control = list(fnscale = -1))

        eta[[i]] <- rep(opt$par, N)

        if(valid)
          veta[[i]] <- rep(opt$par, nrow(vX[[1L]]))

        coef[[i]][[length(coef[[i]]) + 1L]] <- list("w" = "root", "beta" = opt$par)
      } else {
        peta <- family$map2par(eta)

        if(!select)
          ll0 <- family$loglik(y, peta)

        eta2 <- eta

        nc <- ncol(X[[i]])
        G[[i]] <- beta[[i]] <- tcoef[[i]] <- list()
        if(valid)
          vG[[i]] <- list()

        edf <- ncol(lw[[i]])

        if(classic & !full) {
          if(ncol(X[[i]]) > 1) {
            pvals <- rep(NA, ncol(X[[i]]))
            for(j in 2:ncol(X[[i]])) {
              pvals[j] <- coin::pvalue(coin::independence_test(score ~ X[[i]][, j]))
            }
            jj[i] <- which.min(pvals)
          } else {
            jj[i] <- 1
          }
        }

        if(faster) {
          if(ncol(lw[[i]]) < 2) {
            kki <- 1
          } else {
            kki <- apply(lw[[i]], 2, function(x) {
              sum(x * score) - sum(x)
            })
            kki <- which.min(kki)
          }
        } else {
          kki <- 1:ncol(lw[[i]])
        }

        crit[[i]] <- rep(NA, ncol(lw[[i]]))

        ## Note, former y = e in optim().
        for(j in kki) {
          if(classic) {
            if(hard) {
              mx <- max(abs(X[[i]][, jj[i]])) * 100
              opt <- optimize(node_fun_hard, interval = range(X[[i]][, jj[i]]) + c(-1, 1),
                x = X[[i]][, jj[i]], weights = lw[[i]][, j], y = NULL, I,
                eta = eta, i = i, family = family, Y = y, mx = mx,
                W = weights[[i]])
              opt <- list("par" = c(-opt$minimum * mx, mx))
            } else {
              if(full) {
                llf <- rep(NA, ncol(X[[i]]))
                fpar <- list()
                for(l in 2:ncol(X[[i]])) {
                  opt <- optim(c(0.1, 0.2, 0.9, 0.9), fn = node_fun, gr = node_fun_grad,
                    x = X[[i]][, c(1L, l)], weights = lw[[i]][, j], y = NULL, I = I, lambda = lambda[i],
                    eta = eta, i = i, family = family, Y = y,
                    W = weights[[i]],
                    method = "L-BFGS-B")
                  llf[l] <- opt$value
                }
                jj[i] <- which.min(llf)
              }
              opt <- optim(c(0.1, 0.2, 0.9, 0.9), fn = node_fun, gr = node_fun_grad,
                x = X[[i]][, c(1L, jj[i])], weights = lw[[i]][, j], y = NULL, I = I, lambda = lambda[i],
                eta = eta, i = i, family = family, Y = y,
                W = weights[[i]],
                method = "L-BFGS-B")
            }
            g <- sigmoid(X[[i]][, c(1L, jj[i])] %*% opt$par[-c(1:2)])
          } else {
            opt <- try(optim(c(0.1, 0.2, rep(0.9, ncol(X[[i]]))), fn = node_fun, gr = node_fun_grad,
              x = X[[i]], weights = lw[[i]][, j], y = NULL, I = I, lambda = lambda[i],
              eta = eta, i = i, family = family, Y = y, W = weights[[i]],
              method = "L-BFGS-B"), silent = TRUE)
            if(inherits(opt, "try-error"))
              opt <- list("par" = rep(0, ncol(X[[i]]) + 2)) 
            g <- sigmoid(X[[i]] %*% opt$par[-c(1:2)])
          }

          tcoef[[i]][[j]] <- opt$par[-c(1:2)]
          G[[i]][[j]] <- cbind(g, 1 - g) * lw[[i]][, j]
          if(valid) {
            vg <- sigmoid(vX[[i]] %*% opt$par[-c(1:2)])
            vG[[i]][[j]] <- cbind(vg, 1 - vg) * vlw[[i]][, j]
          }
          beta[[i]][[j]] <- opt$par[c(1:2)] #drop(chol2inv(chol(crossprod(GW, G[[i]][[j]]) + I)) %*% t(GW) %*% e)
          eta2[[i]] <- eta[[i]] + drop(G[[i]][[j]] %*% beta[[i]][[j]])
          ll <- family$loglik(y, family$map2par(eta2))
          crit[[i]][j] <- ll - ll0
        }

        next_eps <- all(abs(crit[[i]] / ll0) <= eps)
        if((all(crit[[i]] <= 0) | next_eps) & force) {
          if(verbose)
            cat(if(ia) "\n" else NULL, "..", i, "no improvements, next ..\n")
        } else {
          ##i <- nx[which.max(sapply(crit, max))]
          if(!select) {
            j <- which.max(crit[[i]])
            eta[[i]] <- eta[[i]] + drop(G[[i]][[j]] %*% beta[[i]][[j]])
            lw[[i]][, j] <- G[[i]][[j]]
            lw[[i]] <- as.data.frame(as.matrix(lw[[i]]))
            coef[[i]][[length(coef[[i]]) + 1L]] <- list("w" = tcoef[[i]][[j]], "beta" = beta[[i]][[j]])

            if(valid) {
              veta[[i]] <- veta[[i]] + drop(vG[[i]][[j]] %*% beta[[i]][[j]])
              vlw[[i]][, j] <- vG[[i]][[j]]
              vlw[[i]] <- as.data.frame(as.matrix(vlw[[i]]))
            }

            if(classic) {
              index[[i]] <- rbind(index[[i]], c(j, jj[i]))
            } else {
              index[[i]] <- c(index[[i]], j)
            }
          }
        }
      }
    }

    if(select) {
      if(all(unlist(lapply(crit, function(x) { x <= 0 }))))
        break

      i <- nx[which.max(sapply(crit, max, na.rm = TRUE))]
      j <- which.max(crit[[i]])

      eta[[i]] <- eta[[i]] + drop(G[[i]][[j]] %*% beta[[i]][[j]])
      lw[[i]][, j] <- G[[i]][[j]]
      lw[[i]] <- as.data.frame(as.matrix(lw[[i]]))

      if(valid) {
        veta[[i]] <- veta[[i]] + drop(vG[[i]][[j]] %*% beta[[i]][[j]])
        vlw[[i]][, j] <- vG[[i]][[j]]
        vlw[[i]] <- as.data.frame(as.matrix(vlw[[i]]))
      }

      coef[[i]][[length(coef[[i]]) + 1L]] <- list("w" = tcoef[[i]][[j]], "beta" = beta[[i]][[j]])

      if(classic) {
        index[[i]] <- rbind(index[[i]], c(j, jj[i]))
      } else {
        index[[i]] <- c(index[[i]], j)
      }
    }

    edf <- size <- 0
    for(ii in nx) {
      ncwi <- ncol(lw[[ii]])
      edf <- edf + ncwi + if(edf2) ncwi / 2 * ncol(X[[ii]]) else 0
      size <- size + ncwi
      if(linear[ii])
        edf <- edf + ncol(X[[ii]])
    }

    ll <- family$loglik(y, family$map2par(eta))

    logLik <- c(logLik, ll)
    ic <- c(ic, -2 * ll + edf * K)
    edf_save <- c(edf_save, edf)
    size_save <- c(size_save, size)

    if(valid) {
      vpar <- family$map2par(veta)
      vll <- family$loglik(vy, vpar)
      vlogLik <- c(vlogLik, vll)
      if(is.null(vFUN)) {
        vic <- c(vic, -2 * vll)# + edf * K)
      } else {
        vic <- c(vic, vFUN(vy, vpar, ...))
      }
    }

    if(plot) {
      par(mfrow = c(2, 2), mar = c(4.5, 4.5, 1.1, 1.1))
      plot(ic, type = "l", lwd = 2, xlab = "Iteration", ylab = "-2 * logLik + K * edf")
      if(valid) {
        par(new = TRUE)
        plot(vic, lwd = 2, type = "n", axes = FALSE, xlab = "", ylab = "")
        lines(vic, lwd = 2, col = 4)
        par(new = TRUE)
        plot(c(0, 1), c(0, 1), type = "n", axes = FALSE, xlab = "", ylab = "")
        text(0.7, 0.7, round(vic[length(vic)], 4), col = 4)
      }
      par <- family$map2par(eta)
      if(!is.null(family$p)) {
        if(discrete) {
          ymin <- min(y, na.rm = TRUE)
          a <- family$p(ifelse(y == ymin, y, y - 1), par)
          a <- ifelse(y == ymin, 0, a)
          b <- family$p(y, par)
          u <- runif(length(y), a, b)
          u <- ifelse(u > 0.999999, u - 1e-10, u)
          u <- ifelse(u < 1e-06, u + 1e-10, u)
          res <- qnorm(u)
        } else {
          prob <- family$p(y, par)
          thres <- 0.999999999999999
          prob[prob > thres] <- thres
          prob[prob < (1 - thres)] <- 1 - thres
          res <- qnorm(prob)
        }
        res2 <- qnorm(ppoints(length(res)))
        qqnorm(res, main = "")
        lines(res2, res2, col = 4, lwd = 2)
      }
      if(is.null(family$mean) & is.null(family$q)) {
        plot(par[[1L]], if(inherits(y, "Surv")) y[, "time"] else if(inherits(y, "matrix") ) y[, 1] else y,
          ylab = "Response", xlab = "Fitted values")
        abline(0, 1, col = 4, lwd = 2)
        hist((if(inherits(y, "Surv")) y[, "time"] else if(inherits(y, "matrix") ) y[, 1] else y) - par[[1L]],
          freq = FALSE, col = "lightgray", main = "", xlab = "Residuals")
      } else {
        my <- if(is.null(family$mean)) {
          family$q(0.5, par)
        } else {
          try(family$mean(par), silent = TRUE)
        }
        if(inherits(my, "try-error"))
          my <- family$q(0.5, par)
        if(all(!is.finite(my)))
          my <- par[[1L]]
        plot(my, if(inherits(y, "Surv")) y[, "time"] else if(inherits(y, "matrix") ) y[, 1] else y,
          ylab = "Response", xlab = "Fitted values")
        abline(0, 1, col = 4, lwd = 2)
        hist((if(inherits(y, "Surv")) y[, "time"] else if(inherits(y, "matrix") ) y[, 1] else y) - my,
          freq = FALSE, col = "lightgray", main = "", xlab = "Residuals")
      }
    }

    eps0 <- do.call("cbind", eta)
    eps0 <- mean(abs((eps0 - do.call("cbind", eta0)) / eps0), na.rm = TRUE)

    if(verbose) {
      cat(if(ia & flush) "\r" else if(iter > 1) "\n" else NULL)
      cat("AIC = ", fmt(ic[length(ic)]),
        " logLik = ", fmt(ll),
        " size = ", formatC(size, width = 3),
        " eps = ", fmt(eps0, digits = 5),
        if(select) paste0(" par = ", i) else NULL,
        if(!ia) "\n" else NULL, sep = "")
    }

    if(aic) {
      l <- length(ic)
      if(valid & FALSE) {
        if(l > 1) {
          do <- vic[l] <= vic[l - 1]
        } else {
          do <- TRUE
        }
      } else {
        if(l > 1) {
          do <- ic[l] <= ic[l - 1]
        } else {
          do <- TRUE
        }
      }
      if(!do)
        eta <- eta0
    } else {
      do <- edf < k
    }

    if(size > maxs)
      do <- FALSE

    iter <- iter + 1
  }

  if(aic) {
    if(!is.finite(maxs)) {
      ic <- ic[-length(ic)]
      logLik <- logLik[-length(logLik)]
      if(valid) {
        vic <- vic[-length(vic)]
        vlogLik <- vlogLik[-length(vlogLik)]
      }
      edf_save <- edf_save[-length(edf_save)]
      size_save <- size_save[-length(size_save)]
      for(i in nx) {
        if(length(coef[[i]]) > 1) {
          coef[[i]] <- coef[[i]][-length(coef[[i]])]
          if(!is.character(coef[[i]][[1L]]$w)) {
            if(classic) {
              index[[i]] <- index[[i]][-nrow(index[[i]]), , drop = FALSE]
            } else {
              index[[i]] <- index[[i]][-length(index[[i]])]
            }
          }
        }
      }
    }
  }

  if(verbose & ia) {
    cat("\n.. final model ..\n")
    cat("AIC = ", fmt(ic[length(ic)]),
      " logLik = ", fmt(logLik[length(logLik)]),
      " size = ", formatC(size_save[length(size_save)], width = 3)
    )
    cat("\n")
  }

  rval <- list("coefficients" = coef, "index" = index, "fitted.values" = eta,
    "size" = size_save[length(size_save)], "criterion" = ic, "K" = K)
  
  if(valid)
    rval$valid <- list("criterion" = vic, "logLik" = vlogLik)

  if(any(linear))
    rval$coef_lin <- cl

  rval
}


model.frame.srt <- model.frame.srf <- function(formula, ...)
{
  nx <- names(formula$formula)
  dots <- list(...)
  nargs <- dots[match(c("data", "na.action", "subset"), names(dots), 0)]

  if(length(nargs) || is.null(formula$model)) {
    fcall <- formula$call
    m <- match(c("formula", "data", "subset", "weights", "na.action", "offset"),
      names(fcall), 0L)
    fcall <- fcall[c(1L, m)]
    offset <- as.list(fcall$offset)
    if(length(offset) > 1) {
      offset <- offset[-1]
    } else {
      offset <- rep(offset, length.out = length(nx))
    }
    names(offset) <- nx
    fcall[c("offset", "weights")] <- NULL
    fcall$drop.unused.levels <- FALSE
    fcall[[1L]] <- quote(stats::model.frame)

    model <- list()
    for(i in nx) {
      fcall$formula <- terms(formula$formula[[i]])
      if(!is.null(dots$del.y)) {
        if(dots$del.y)
          fcall$formula <- delete.response(fcall$formula)
      }
      fcall[names(nargs)] <- nargs
      if(length(offset)) {
        if(!is.null(offset[[i]]))
          fcall$offset <- as.name(as.character(offset[[i]]))
      }
      env <- environment(formula$terms[[i]])
      if(is.null(env)) 
        env <- parent.frame()
      model[[i]] <- eval(fcall, env)
    }

    return(model)
  } else {
    return(formula$model)
  }
}

model.frame.srtboost <- function(formula, ...)
{
  class(formula) <- "srt"
  model.frame(formula, ...)
}


model.matrix.srt <- model.matrix.srf <- function(object, ...)
{
  if(n_match <- match("x", names(object), 0L)) {
    return(object[[n_match]])
  } else {
    nx <- names(object$formula)
    data <- model.frame(object, ..., del.y = TRUE)
    offset <- list()
    for(i in nx) {
      offset[[i]] <- model.offset(data[[i]])
    }
    dots <- list(...)
    dots$data <- dots$contrasts.arg <- NULL
    X <- list()
    for(i in nx) {
      X[[i]] <- model.matrix(delete.response(object$terms[[i]]),
        data[[i]], contrasts.arg = object$contrasts)
      if(nrow(X[[i]]) < 1)
        X[[i]] <- cbind("(Intercept)" = rep(1, nrow(data[[1L]])))
      attr(X[[i]], "offset") <- offset[[i]]
    }
    return(X)
  }
}

model.matrix.srtboost <- function(object, ...)
{
  class(object) <- "srt"
  model.matrix(object, ...)
}


predict.srt <- function(object, newdata, model = NULL,
  type = c("link", "parameter"), k = NULL, ...)
{
  if(object$method == "full") {
    return(predict.srt2(object, newdata = newdata, model = model, type = type, k = k, ...))
  }
  nx <- names(object$formula)
  if(missing(newdata) || is.null(newdata)) {
    X <- model.matrix(object)
  } else {
    X <- model.matrix(object, data = newdata, ...)
  }

  if(length(object$scaled)) {
    for(i in nx) {
      if(length(object$scaled[[i]])) {
        for(j in names(object$scaled[[i]])) {
          sx <- object$scaled[[i]][[j]]
          X[[i]][, j] <- (X[[i]][, j] - sx$mean) / sx$sd
          #X[[i]][, j] <- (X[[i]][, j] - sx$min) / (sx$max - sx$min)
        }
      }
    }
  }

  if(length(object$prc)) {
    for(i in nx) {
      if(length(object$prc[[i]])) {
        j <- grep("(Intercept)", colnames(X[[i]]), fixed = TRUE)
        X[[i]] <- cbind("(Intercept)" = 1,
          predict(object$prc[[i]], newdata = X[[i]][, -j, drop = FALSE]))
      }
    }
  }

  type <- match.arg(type)
  coef <- coef(object)
  coef_lin <- object$coef_lin
  index <- object$index
  if(length(index) > 0)
    classic <- is.matrix(index[[1L]])
  else
    classic <- FALSE
  if(is.null(model)) {
    model <- nx
  } else {
    if(!all(model %in% nx))
      stop("argument model is specified wrong!")
  }
  N <- nrow(X[[1L]])
  pred <- lw <- list()
  ret.N <- NULL
  if(!is.null(list(...)$ret.N))
    ret.N <- list()
  for(i in model) {
    lw[[i]] <- data.frame(rep(1, N))
    if(!is.null(ret.N))
      ret.N[[i]] <- list()
    if(!is.null(coef_lin[[i]])) {
      pred[[i]] <- drop(X[[i]] %*% coef_lin[[i]])
    } else {
      if(length(coef[[i]][[1L]]$beta) < 2L) {
        pred[[i]] <- rep(coef[[i]][[1L]]$beta, N)
        coef[[i]] <- coef[[i]][-1L]
      } else {
        pred[[i]] <- rep(0, N)
      }
    }
    if(length(coef[[i]])) {
      for(j in 1:length(coef[[i]])) {
        if(!is.character(coef[[i]][[j]]$w)) {
          w <- coef[[i]][[j]]$w
          if(classic) {
            g <- sigmoid(X[[i]][, c(1L, index[[i]][j, 2L])] %*% w)
            G <- cbind(g, 1 - g) * lw[[i]][, index[[i]][j, 1L]]
            lw[[i]][, index[[i]][j, 1L]] <- G
          } else {
            g <- sigmoid(X[[i]] %*% w)
            G <- cbind(g, 1 - g) * lw[[i]][, index[[i]][j]]
            lw[[i]][, index[[i]][j]] <- G
          }
          lw[[i]] <- as.data.frame(as.matrix(lw[[i]]))
          pred[[i]] <- pred[[i]] + drop(G %*% coef[[i]][[j]]$beta)
          if(!is.null(ret.N))
            ret.N[[i]][[j + 1]] <- G
        } else {
          pred[[i]] <- rep(coef[[i]][[j]]$beta, N)
          if(!is.null(ret.N))
            ret.N[[i]][[j + 1]] <- rep(1, N)
        }
        if(!is.null(k)) {
          if(j == k)
            break
        }
      }
    }

    if(!is.null(attr(X[[i]], "offset"))) {
      pred[[i]] <- pred[[i]] + attr(X[[i]], "offset")
    }

    if(type != "link") {
      links <- object$family$links[nx]
      if(length(links) > 0) {
        if(links[i] != "identity") {
          linkinv <- make.link2(links[i])$linkinv
          pred[[i]] <- linkinv(pred[[i]])
        }
      } else {
        warning(paste("could not compute predictions on the scale of parameter",
          ", predictions on the scale of the linear predictor are returned!", sep = ""))
      }
    }
  }

  drop <- list(...)$drop
  if(is.null(drop))
    drop <- TRUE

  if(!is.null(ret.lw <- list(...)$ret.lw)) {
    if((length(lw) < 2) & drop)
      lw <- lw[[1L]]
    if(ret.lw) {
      return(lw)
    }
  }

  if(!is.null(ret.N)) {
    for(i in model)
      ret.N[[i]][[1L]] <- matrix(1, nrow = N, ncol = 1)
    if((length(ret.N) < 2) & drop)
      ret.N <- ret.N[[1L]]
    return(ret.N)
  }

  if((length(pred) < 2L) & drop) {
    return(pred[[1L]])
  } else {
    return(as.data.frame(pred))
  }
}


predict.srf <- function(object, newdata, model = NULL,
  type = c("link", "parameter"), FUN = mean, ...)
{
  if(object$method == "full") {
    return(predict.srf2(object, newdata = newdata, model = model, type = type, FUN = FUN, ...))
  }
  nx <- names(object$formula)
  if(missing(newdata) || is.null(newdata)) {
    X <- model.matrix(object, contrasts.arg = object$contrasts)
  } else {
    X <- model.matrix(object, data = newdata, contrasts.arg = object$contrasts, ...)
  }
  if(length(object$scaled)) {
    for(i in nx) {
      if(length(object$scaled[[i]])) {
        for(j in names(object$scaled[[i]])) {
          sx <- object$scaled[[i]][[j]]
          X[[i]][, j] <- (X[[i]][, j] - sx$mean) / sx$sd
          #X[[i]][, j] <- (X[[i]][, j] - sx$min) / (sx$max - sx$min)
        }
      }
    }
  }
  if(is.null(FUN))
    FUN <- mean
  type <- match.arg(type)
  coef <- coef(object)
  index <- object$index
  if(is.null(model)) {
    model <- nx
  } else {
    if(!all(model %in% nx))
      stop("argument model is specified wrong!")
  }
  N <- nrow(X[[1L]])
  pred <- list()
  lw <- list()
  trees <- grep("tree", names(object), value = TRUE)
  nsamp <- list(...)$nsamp
  if(!is.null(nsamp))
    trees <- sample(trees, size = nsamp)
  for(i in model) {
    pred[[i]] <- list()
    for(ti in trees) {
      coef <- coef(object[[ti]])
      coef_lin <- object[[ti]]$coef_lin
      index <- object[[ti]]$index
      if(length(index) < 1)
        classic <- FALSE
      else
        classic <- is.matrix(index[[1L]])
      lw <- data.frame(rep(1, nrow(X[[1]])))
      if(!is.null(coef_lin[[i]])) {
        pred[[i]][[ti]] <- drop(X[[i]] %*% coef_lin[[i]])
      } else {
        if(length(coef[[i]][[1L]]$beta) < 2L) {
          pred[[i]][[ti]] <- rep(coef[[i]][[1L]]$beta, N)
          coef[[i]] <- coef[[i]][-1L]
        } else {
          pred[[i]][[ti]] <- rep(0, N)
        }
      }
      if(length(coef[[i]])) {
        for(j in 1:length(coef[[i]])) {
          if(!is.character(coef[[i]][[j]]$w)) {
            w <- coef[[i]][[j]]$w
            if(classic) {
              g <- sigmoid(X[[i]][, c(1L, index[[i]][j, 2L])] %*% w)
              G <- cbind(g, 1 - g) * lw[, index[[i]][j, 1L]]
              lw[, index[[i]][j, 1L]] <- G
            } else {
              g <- sigmoid(X[[i]] %*% w)
              G <- cbind(g, 1 - g) * lw[, index[[i]][j]]
              lw[, index[[i]][j]] <- G
            }
            lw <- as.data.frame(as.matrix(lw))
            pred[[i]][[ti]] <- pred[[i]][[ti]] + drop(G %*% coef[[i]][[j]]$beta)
          } else {
            pred[[i]][[ti]] <- rep(coef[[i]][[j]]$beta, N)
          }
        }
      }
    }
    pred[[i]] <- do.call("cbind", pred[[i]])

    if(!is.null(attr(X[[i]], "offset"))) {
      pred[[i]] <- pred[[i]] + attr(X[[i]], "offset")
    }

    if(type != "link") {
      links <- object$family$links[nx]
      if(length(links) > 0) {
        if(links[i] != "identity") {
          linkinv <- make.link2(links[i])$linkinv
          pred[[i]] <- linkinv(pred[[i]])
        }
      } else {
        warning(paste("could not compute predictions on the scale of parameter",
          ", predictions on the scale of the linear predictor are returned!", sep = ""))
      }
    }

    pred[[i]] <- apply(pred[[i]], 1, FUN, ...)
    if(!is.null(dim(pred[[i]])))
      pred[[i]] <- t(pred[[i]])
  }
  drop <- list(...)$drop
  if(is.null(drop))
    drop <- TRUE
  if((length(pred) < 2L) & drop) {
    return(pred[[1L]])
  } else {
    return(as.data.frame(pred))
  }
}


fitted.srt <- function(object, ...) {
  predict.srt(object, ...)
}


fitted.srf <- function(object, ...) {
  predict.srf(object, ...)
}


residuals.srt <- residuals.srf <- residuals.srtboost <- function(object, type = c("quantile", "response"), ...)
{
  family <- family(object)

  if(!is.null(family$residuals)) {
    res <- family$residuals(object, type = type, ...)
    if(length(class(res)) < 2) {
      if(inherits(res, "numeric"))
        class(res) <- c("srt_residuals", class(res))
    }
  } else {
    type <- match.arg(type)

    object$model <- NULL

    object$y <- model.response(model.frame(object, ...)[[names(object$formula)[1L]]])

    if(is.null(object$y))
      stop("response variable is missing, cannot compute residuals!")

    nobs <- nrow(object$y)
    y <- if(is.data.frame(object$y)) {
      if(ncol(object$y) < 2) {
        object$y[[1]]
      } else object$y
    } else {
      object$y
    }

    par <- predict(object, drop = FALSE, ...)
    for(j in family$names)
      par[[j]] <- make.link2(family$links[j])$linkinv(par[[j]])

    if(type == "quantile") {
      if(is.null(family$p)) {
        type <- "response"
        warning(paste("no $p() function in family '", family$family,
          "', cannot compute quantile residuals, computing response resdiuals instead!", sep = ""))
      } else {
        discrete <- FALSE
        if(!is.null(family$type)) {
          if(tolower(family$type) == "discrete")
            discrete <- TRUE
        }
        if(family$family == "binomial")
          discrete <- TRUE
        if(discrete) {
          ymin <- min(y, na.rm = TRUE)

          a <- family$p(ifelse(y == ymin, y, y - 1), par)
          a <- ifelse(y == ymin, 0, a)
          b <- family$p(y, par)
          u <- runif(length(y), a, b)
          u <- ifelse(u > 0.999999, u - 1e-16, u)
          u <- ifelse(u < 1e-06, u + 1e-16, u)
          res <- qnorm(u)
#	  a <- family$p(y - 1, par)
#	  b <- family$p(y, par)
#	  u <- runif(n = length(y), min = a, max = b)
#	  res <- qnorm(u)
        } else {
          prob <- family$p(y, par)
          thres <- 0.999999999999999
          prob[prob > thres] <- thres
          prob[prob < (1 - thres)] <- 1 - thres
          res <- qnorm(prob)
          if(any(isnf <- !is.finite(res))) {
            warning("non finite quantiles from probabilities, set to NA!")
            res[isnf] <- NA
          }
        }
        attr(res, "type") <- "Quantile"
      }
    }

    if(type == "response") {
      mu <- if(is.null(family$mu)) {
        function(par, ...) { par[[1]] }
      } else family$mu
      res <- y - mu(par)
      attr(res, "type") <- "Response"
    }
   
    class(res) <- c("srt_residuals", class(res))
  }

  if(any(j <- !is.finite(res)))
    res[j] <- NA

  return(res)
}


plot.srt_residuals <- function(x, which = c("hist-resid", "qq-resid"), spar = TRUE, ...)
{
  ## What should be plotted?
  which.match <- c("hist-resid", "qq-resid")
  if(!is.character(which)) {
    if(any(which > 2L))
      which <- which[which <= 2L]
    which <- which.match[which]
  } else which <- which.match[pmatch(tolower(which), which.match)]
  if(length(which) > length(which.match) || !any(which %in% which.match))
    stop("argument which is specified wrong!")

  if(is.null(dim(x)))
    x <- matrix(x, ncol = 1)
  nc <- ncol(x)
  cn <- colnames(x)

  if(nc > 10) {
    nc <- 1
    cn <- NULL
  }

  if(spar) {
    op <- par(no.readonly = TRUE)
    on.exit(par(op))
    par(mfrow = n2mfrow(length(which) * nc))
  }

  type <- attr(x, "type")

  for(j in 1:nc) {
    for(w in which) {
      args <- list(...)
      if(w == "hist-resid") {
        rdens <- density(as.numeric(x), na.rm = TRUE)
        rh <- hist(as.numeric(x), plot = FALSE)
        args$ylim <- c(0, max(c(rh$density, rdens$y)))
#        if(is.null(args$xlim)) {
#          args$xlim <- range(x[is.finite(x)], na.rm = TRUE)
#          args$xlim <- c(-1, 1) * max(args$xlim)
#        }
        args$freq <- FALSE
        args$x <- as.numeric(x)
        args <- delete.args("hist.default", args, package = "graphics", not = c("xlim", "ylim"))
        if(is.null(args$xlab))
          args$xlab <- if(is.null(type)) "Residuals" else paste(type, "residuals")
        if(is.null(args$ylab))
          args$ylab <- "Density"
        if(is.null(args$main)) 
          args$main <- paste("Histogram and density", if(!is.null(cn[j])) paste(":", cn[j]) else NULL)
        ok <- try(do.call("hist", args))
        if(!inherits(ok, "try-error"))
          lines(rdens)
        box()
      }
      if(w == "qq-resid") {
        if(ncol(x) > 10) {
          x <- t(apply(x, 1, c95))

          args$x <- NULL
          args$plot.it <- FALSE
          args <- delete.args("qqnorm.default", args, package = "stats", not = c("col", "pch"))
          if(is.null(args$main))
            args$main <- paste("Normal Q-Q Plot", if(!is.null(cn[j])) paste(":", cn[j]) else NULL)

          args$y <- x[, "Mean"]
          mean <- do.call(qqnorm, args)
          args$y <- x[, "2.5%"]
          lower <- do.call(qqnorm, args)
          args$y <- x[, "97.5%"]
          upper <- do.call(qqnorm, args)

          ylim <- range(c(mean$y, lower$y, upper$y), na.rm = TRUE)
          args$plot.it <- TRUE
          args$ylim <- ylim
          args$y <- x[, "Mean"]
          mean <- do.call(qqnorm, args)

          if(is.null(args$ci.col))
            args$ci.col <- 4
          if(is.null(args$ci.lty))
            args$ci.lty <- 2

          lines(lower$x[order(lower$x)], lower$y[order(lower$x)],
            lty = args$ci.lty, col = args$ci.col)
          lines(upper$x[order(upper$x)], upper$y[order(upper$x)],
            lty = args$ci.lty, col = args$ci.col)

          args$y <- x[, "Mean"]
          qqline(args$y)
        } else {
          args$y <- x
#          if(is.null(args$ylim)) {
#            args$ylim <- range(x[is.finite(x)], na.rm = TRUE)
#            args$ylim <- c(-2.5, 2.5) * max(args$ylim)
#          }
#          if(is.null(args$xlim)) {
#            args$xlim <- range(x[is.finite(x)], na.rm = TRUE)
#            args$xlim <- c(-2.5, 2.5) * max(args$xlim)
#          }
          args$x <- NULL
          args <- delete.args("qqnorm.default", args, package = "stats", not = c("col", "pch", "xlim", "ylim"))
          if(is.null(args$main))
            args$main <- paste("Normal Q-Q Plot", if(!is.null(cn[j])) paste(":", cn[j]) else NULL)
          ok <- try(do.call(qqnorm, args))

          if(!inherits(ok, "try-error"))
            lines(x, x, col = 4, lwd = 2)
        }
      }
    }
  }

  return(invisible(NULL))
}


delete.args <- function(fun = NULL, args = NULL, not = NULL, package = NULL)
{
  if(is.character(fun) & !is.null(package))
    fun <- eval(parse(text = paste(package, paste(rep(":", 3), collapse = ""), fun, sep = "")))
  nf <- names(formals(fun))
  na <- names(args)
  for(elmt in na)
    if(!elmt %in% nf) {
      if(!is.null(not)) {
        if(!elmt %in% not)
          args[elmt] <- NULL
      } else args[elmt] <- NULL
    }

  return(args)
}

delete.NULLs <- function(x.list) 
{
  x.list[unlist(lapply(x.list, length) != 0)]
}


plot.srt <- plot.srf <- plot.srtboost <- function(x,
  which = c("criterion", "hist-resid", "qq-resid", "response-fitted"), spar = TRUE, ...) {
  if(spar) {
    par(mfrow = n2mfrow(length(which)))
  }
  i <- pmatch(which, "criterion")
  i <- na.omit(i)
  if(length(i)) {
    if(is.null(x$criterion)) {
      if(!is.null(x$logLik)) {
        plot(x$logLik, type = "l", lwd = 2, xlab = "Iteration",
          ylab = paste0("logLik"),
          main = "logLik path")
      
      }
    } else {
      plot(x$criterion, type = "l", lwd = 2, xlab = "Iteration",
        ylab = paste0("-2 * logLik + ", round(x$K, 2), " * edf"),
        main = "Information criterion")
    }
  }
  i <- pmatch(which, "response-fitted")
  i <- na.omit(i)
  if(length(i)) {
    par <- predict(x, type = "parameter")
    fam <- x$family
    if(!is.null(fam$mean)) {
      my <- if(is.null(fam$mean)) {
        fam$q(0.5, par)
      } else {
        try(fam$mean(par), silent = TRUE)
      }
      if(inherits(my, "try-error"))
        my <- fam$q(0.5, par)
      if(all(!is.finite(my)))
        my <- par[[1L]]
    } else {
      my <- par[[1L]]
    }
    y <- model.response(model.frame(x)[[1]])
    plot(my, if(inherits(y, "Surv")) y[, "time"] else if(inherits(y, "matrix") ) y[, 1] else y,
      ylab = "Response", xlab = "Fitted values", main = "Response vs. fitted")
    abline(0, 1, col = 4, lwd = 2)
  }
  i <- pmatch(which, c("hist-resid", "qq-resid"))
  i <- na.omit(i)
  if(length(i))
    plot(residuals(x, ...), which = c("hist-resid", "qq-resid")[i], spar = FALSE)
}


logLik.srt <- logLik.srf <- logLik.srtboost <- function(object, ...) {
  nd <- list(...)$newdata
  if(is.null(nd)) {
    par <- predict(object, type = "parameter", drop = FALSE)
    Y <- model.response(model.frame(object)[[1L]])
  } else {
    par <- predict(object, newdata = nd, type = "parameter", drop = FALSE)
    Y <- model.response(model.frame(object, data = nd)[[1L]])
  }
  val <- family(object)$loglik(Y, par)
  if(inherits(object, c("srf", "srtboost"))) {
    p <- NA
  } else {
    p <- object$size
  }
  attr(val, "df") <- p
  class(val) <- "logLik"
  val
}


srtboost <- function(..., nu = 0.1  , k = 4, lambda = 1e-05, n.iter = 400,
  index = NULL, K = NULL, classic = FALSE, select = FALSE, eps = .Machine$double.eps^0.3,
  verbose = TRUE, plot = TRUE, linear = FALSE)
{
  mfd <- srt(..., ret.mfd = TRUE)

  ia <- interactive()

  k0 <- k
  X <- mfd$X
  y <- mfd$y
  family <- mfd$family

  nx <- names(X)

  if(!is.null(index)) {
    for(i in nx) {
      X[[i]] <- X[[i]][index, , drop = FALSE]
    }
    if(is.null(dim(y))) {
      y <- y[index]
    } else {
      y <- y[index, , drop = FALSE]
    }
  }

  if(!ia)
    plot <- FALSE
  if(plot) {
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
  }

  discrete <- FALSE
  if(!is.null(family$type)) {
    if(tolower(family$type) == "discrete")
      discrete <- TRUE
  }
  if(family$family == "binomial")
    discrete <- TRUE

  N <- nrow(X[[1L]])
  if(is.null(K))
    K <- log(N)

  lw <- eta <- G <- crit <- beta <- tcoef <- coef <- index <- list()
  is_factor <- list()
  for(i in nx) {
    lw[[i]] <- data.frame(rep(1, N))
    eta[[i]] <- rep(0, N)
    coef[[i]] <- list()
    is_factor[[i]] <- apply(X[[i]], 2, function(x) {
      length(unique(x)) == 2L
    })
  }

  eta0 <- init.eta(eta, y, family, N)
  for(i in nx) {
    if(!is.null(mfd$offset[[i]])) {
      eta[[i]] <- eta[[i]] + mfd$offset[[i]]
    } else {
      eta[[i]] <- eta0[[i]]
    }
  }

  Xi <- cl2 <- list()
  for(i in nx) {
    Xi[[i]] <- X[[i]][, "(Intercept)", drop = FALSE]
    cl2[[i]] <- 0
  }

  eps0 <- eps + 1L
  k2 <- 0
  while((eps < eps0) & (k2 < 100)) {
    eta0 <- eta
    for(i in nx) {
      ll0 <- family$loglik(y, family$map2par(eta))

      peta <- family$map2par(eta)
      hess <- process_derivs(family$hess[[i]](y, peta, id = i), is.weight = TRUE)
      score <- process_derivs(family$score[[i]](y, peta, id = i), is.weight = FALSE)
      z <- eta[[i]] + 1 / hess * score

      b0 <- cl2[[i]]

      if(is.null(mfd$weights[[i]])) {
        XW <- Xi[[i]] * hess
      } else {
        XW <- Xi[[i]] * hess * mfd$weights[[i]]
      }

      cl2[[i]] <- drop(chol2inv(chol(crossprod(XW, Xi[[i]]) + 1e-20)) %*% crossprod(XW, z))

      cl2[[i]] <- 1/(sum(XW, na.rm = TRUE) + 1e-20) * sum(XW * z, na.rm = TRUE)

      eta[[i]] <- drop(Xi[[i]] * cl2[[i]])

      ll1 <- family$loglik(y, family$map2par(eta))

      if(ll1 < ll0) {
        fnu <- function(nu2) {
          b <- nu2 * cl2[[i]] + (1 - nu2) * b0
          eta[[i]] <- drop(Xi[[i]] * b)
          return(family$loglik(y, family$map2par(eta)))
        }
        nu2 <- 1
        while((fnu(nu2) < ll0) & (.Machine$double.eps < nu2)) {
          nu2 <- nu2 / 2
        }
        cl2[[i]] <- nu2 * cl2[[i]] + (1 - nu2) * b0
        eta[[i]] <- drop(Xi[[i]] * cl2[[i]])
      }
    }
    eps0 <- do.call("cbind", eta)
    eps0 <- mean(abs((eps0 - do.call("cbind", eta0)) / eps0), na.rm = TRUE)

    k2 <- k2 + 1
  }

  for(i in nx) {
    coef[[i]][[1L]] <- list("w" = "root", "beta" = cl2[[i]])
  }

  eta2 <- eta

  lambda <- rep(lambda, length.out = length(nx))
  names(lambda) <- nx

  nu <- rep(nu, length.out = length(nx))
  names(nu) <- nx

  fam0 <- Gaussian0()
  names(X) <- rep("mu", length(X))

  ll <- NULL

  coef <- list()
  for(i in nx) {
    coef[[i]] <- list()
  }

  npar <- length(nx)
  bsel <- list()
  ll_sel <- rep(0, length = npar)
  names(ll_sel) <- nx

  eps0 <- eps + 1L

  for(k in 1:n.iter) {
    eta0 <- eta
    for(i in 1:npar) {
      peta <- family$map2par(eta)
      score <- process_derivs(family$score[[nx[i]]](y, peta, id = nx[i]), is.weight = FALSE)
      hess <- process_derivs(family$hess[[i]](y, peta, id = i), is.weight = TRUE)

      if(!is.null(mfd$weights[[nx[i]]]))
        hess <- hess * mfd$weights[[nx[i]]]

      b <- .srt.fit(X[i], score/hess,
        family = fam0, k = k0, lambda = lambda[i], aic = FALSE,
        plot = FALSE, verbose = FALSE, maxs = Inf, K = K, classic = classic,
        initialize = TRUE, oname = nx[i], linear = linear)

      if(!select) {
        eta[[nx[i]]] <- eta[[nx[i]]] + nu[i] * b$fitted.values$mu ## !boosting family
        coef[[nx[i]]][[k]] <- list("coefficients" = b$coefficients, "index" = b$index, "coef_lin" = b$coef_lin)
      } else {
        eta0[[nx[i]]] <- eta[[nx[i]]] + nu[i] * b$fitted.values$mu
        ll_sel[i] <- family$loglik(y, family$map2par(eta0))
        bsel[[i]] <- b
        eta0[[nx[i]]] <- eta[[nx[i]]]
      }
    }

    if(select) {
      eta[[nx[i]]] <- eta[[nx[i]]] + nu[i] * bsel[[i]]$fitted.values$mu
      coef[[nx[i]]][[k]] <- list("coefficients" = bsel[[i]]$coefficients, "index" = bsel[[i]]$index,
        "coef_lin" = bsel[[i]]$coef_lin)
      for(ii in nx[nx != nx[i]]) {
        coef[[ii]][[k]] <- list("coefficients" = NA, "index" = NA)
      }
    }

    eps0 <- do.call("cbind", eta)
    eps0 <- mean(abs((eps0 - do.call("cbind", eta0)) / eps0), na.rm = TRUE)

    ll <- c(ll, family$loglik(y, family$map2par(eta)))

    if(verbose) {
      cat(if(ia) "\r" else if(k > 1) "\n" else NULL)
      cat(
        " iter = ", formatC(k, width = 5),
        " logLik = ", fmt(ll[k]),
        " eps = ", fmt(round(eps0, 5)),
        if(select) paste0(" par = ", nx[i]) else NULL,
        if(!ia) "\n" else NULL, sep = "")
    }

    if(plot) {
      par(mfrow = c(2, 2), mar = c(4.5, 4.5, 1.1, 1.1))
      plot(ll, type = "l", lwd = 2, xlab = "Iteration", ylab = "logLik")
      par <- family$map2par(eta)
      if(!is.null(family$p)) {
        if(discrete) {
          ymin <- min(y, na.rm = TRUE)
          a <- family$p(ifelse(y == ymin, y, y - 1), par)
          a <- ifelse(y == ymin, 0, a)
          b <- family$p(y, par)
          u <- runif(length(y), a, b)
          u <- ifelse(u > 0.999999, u - 1e-10, u)
          u <- ifelse(u < 1e-06, u + 1e-10, u)
          res <- qnorm(u)
        } else {
          prob <- family$p(y, par)
          thres <- 0.999999999999999
          prob[prob > thres] <- thres
          prob[prob < (1 - thres)] <- 1 - thres
          res <- qnorm(prob)
        }
        res2 <- qnorm(ppoints(length(res)))
        qqnorm(res, main = "")
        lines(res2, res2, col = 4, lwd = 2)
      }
      if(is.null(family$mean) & is.null(family$q)) {
        plot(par[[1L]], if(inherits(y, "Surv")) y[, "time"] else if(inherits(y, "matrix") ) y[, 1] else y,
          ylab = "Response", xlab = "Fitted values")
        abline(0, 1, col = 4, lwd = 2)
        hist((if(inherits(y, "Surv")) y[, "time"] else if(inherits(y, "matrix") ) y[, 1] else y) - par[[1L]],
          freq = FALSE, col = "lightgray", main = "", xlab = "Residuals")
      } else {
        my <- if(is.null(family$mean)) {
          family$q(0.5, par)
        } else {
          family$mean(par)
        }
        if(all(!is.finite(my)))
          my <- par[[1L]]
        plot(my, if(inherits(y, "Surv")) y[, "time"] else if(inherits(y, "matrix") ) y[, 1] else y,
          ylab = "Response", xlab = "Fitted values")
        abline(0, 1, col = 4, lwd = 2)
        hist((if(inherits(y, "Surv")) y[, "time"] else if(inherits(y, "matrix") ) y[, 1] else y) - my,
          freq = FALSE, col = "lightgray", main = "", xlab = "Residuals")
      }
    }

    if(eps0 < eps)
      break

    if(k > 1) {
      if(ll[k] < ll[k - 1L]) {
        for(i in nx) {
          coef[[i]] <- coef[[i]][-k]
        }
        break
      }
    }
  }

  if(verbose & ia)
    cat("\n")

  coef <- list("intercept" = cl2, "trees" = coef)

  rval <- list("fitted.values" = eta, "coefficients" = coef, "formula" = mfd$formula,
    "family" = family, "terms" = mfd$terms, "contrasts" = mfd$contrasts, "call" = match.call(),
    "nu" = nu, "n.iter" = k,
    "scaled" = mfd$scaled)

  class(rval) <- "srtboost"
 
  rval
}

mstop <- function(x) {
  UseMethod("mstop")
}

mstop.default <- function(x) {
  x$n.iter
}

mstop.srtboost <- function(x) {
  x$n.iter
}


predict.srtboost <- function(object, newdata, model = NULL,
  type = c("link", "parameter"), mstop = NULL, matrix = FALSE, ...)
{
  nx <- names(object$formula)
  if(missing(newdata) || is.null(newdata)) {
    X <- model.matrix(object, contrasts.arg = object$contrasts)
  } else {
    X <- model.matrix(object, data = newdata, contrasts.arg = object$contrasts, ...)
  }
  type <- match.arg(type)
  if(is.null(model)) {
    model <- nx
  } else {
    if(!all(model %in% nx))
      stop("argument model is specified wrong!")
  }
  nu <- object$nu
  n.iter <- length(object$coefficients$trees[[1L]])
  if(!is.null(mstop)) {
    if(mstop > n.iter) {
      warning("argument mstop set too large, set to last boosting iteration!")
      mstop <- n.iter
    }
    n.iter <- mstop
  }
  pred <- list()
  N <- nrow(X[[1L]])
  for(i in model) {
    pred[[i]] <- object$coefficients$intercept[[i]]
    for(k in 1:n.iter) {
      lw <- data.frame(rep(1, N))
      coef <- object$coefficients$trees[[i]][[k]]$coefficients[[1L]]
      coef_lin <- object$coefficients$trees[[i]][[k]]$coef_lin
      index <- object$coefficients$trees[[i]][[k]]$index[[1L]]
      classic <- is.matrix(index)
      if(!all(is.na(coef[[1L]]))) {
        if(length(coef[[1L]]$beta) < 2) {
          fit <- rep(coef[[1L]]$beta, N)
          coef <- coef[-1L]
        } else fit <- rep(0, N)
        if(length(coef)) {
          for(j in 1:length(coef)) {
            w <- coef[[j]]$w
            if(classic) {
              g <- sigmoid(X[[i]][, c(1L, index[j, 2L])] %*% w)
              G <- cbind(g, 1 - g) * lw[, index[j, 1L]]
              lw[, index[j, 1L]] <- G
            } else {
              g <- sigmoid(X[[i]] %*% w)
              G <- cbind(g, 1 - g) * lw[, index[j]]
              lw[, index[j]] <- G
            }
            lw <- as.data.frame(as.matrix(lw))
            fit <- fit + drop(G %*% coef[[j]]$beta)
          }
        }
        if(!matrix) {
          pred[[i]] <- pred[[i]] + nu[i] * fit
        } else {
          pred[[i]] <- cbind(pred[[i]], nu[i] * fit)
        }
      } else {
        if(!matrix) {
          pred[[i]] <- pred[[i]] + rep(0, N)
        } else {
          pred[[i]] <- cbind(pred[[i]], rep(0, N))
        }
      }
    }

    if(type != "link") {
      links <- object$family$links[nx]
      if(length(links) > 0) {
        if(links[i] != "identity") {
          linkinv <- make.link2(links[i])$linkinv
          pred[[i]] <- linkinv(pred[[i]])
        }
      } else {
        warning(paste("could not compute predictions on the scale of parameter",
          ", predictions on the scale of the linear predictor are returned!", sep = ""))
      }
    }
  }

  if(!matrix) {
    pred <- as.data.frame(pred)
  } else {
    for(i in model)
      pred[[i]] <- t(apply(pred[[i]], 1, cumsum))
  }

  drop <- list(...)$drop
  if(is.null(drop))
    drop <- TRUE
  if((length(pred) < 2L) & drop) {
    return(pred[[1L]])
  } else {
    return(pred)
  }

  pred
}


c95 <- function(x)
{
  qx <- quantile(x, probs = c(0.025, 0.975), na.rm = TRUE)
  return(c(qx[1], "Mean" = mean(x, na.rm = TRUE), qx[2]))
}


scale2 <- function (x, lower = -1.5, upper = 1.5) 
{
  x <- if (length(unique(x)) > 1) {
    (x - min(x, na.rm = TRUE))/diff(range(x, na.rm = TRUE)) * (upper - lower) + lower
  } else x
  x
}


Crazy <- function (n = 1000) 
{
  d <- data.frame(x = runif(n, -3, 3))
  d$eta <- sin(20 * exp(scale2(d$x, 0, 1))) * scale2(d$x, 0, 1)^2
  d$eta[d$x >= -1] <- d$eta[d$x >= -1] - 1.5
  d$eta[d$x <= -2] <- -1.5
  d$eta[d$x >= -2 & d$x <= -1] <- 0
  d$y <- d$eta + rnorm(n, sd = 0.1)
  return(d)
}

process_derivs <- function(x, is.weight = FALSE) 
{
  .Call("process_derivs", as.numeric(x), as.logical(is.weight), PACKAGE = "softtrees")
}

summary.srt <- function(object, ...)
{
  
}

CRPS <- function(object, newdata = NULL, interval = c(-Inf, Inf)) {
  yname <- response_name(object)
  fam <- family(object)
  if(is.null(fam$p))
    stop("no p() function in family object!")
  if(is.null(newdata))
    newdata <- model.frame(object)
  par <- as.data.frame(predict(object, newdata = newdata, type = "parameter", drop = FALSE))
  crps <- .CRPS(newdata[[yname]], par, fam, interval)
  return(crps)
}

.CRPS <- function(y, par, family, interval = c(-Inf, Inf)) {
  if(is.function(family))
    family <- family()
  if(inherits(family, "gamlss.family"))
    family <- tF(family)
  family <- complete_family(family)
  n <- length(y)
  crps <- rep(0, n)
  for(i in 1:n) {
    foo <- function(x) {
      (family$p(x, par[i, , drop = FALSE]) - 1 * (x >= y[i]))^2
    }
    crps[i] <- integrate(foo, lower = interval[1L], upper = interval[2L])$value
  }
  return(crps)
}

## Get response name.
response.name <- function(formula, hierarchical = TRUE, keep.functions = FALSE, na.rm = FALSE)
{
  rn <- NA
  if(inherits(formula, "bamlss.frame")) {
    formula$formula <- as.formula(formula$formula)
    if(!is.null(formula$formula)) {
      if(!is.null(attr(formula$formula, "response.name")))
        return(attr(formula$formula, "response.name"))
    }
    formula <- terms(model.frame(formula))
  }
  if(!is.null(attr(formula, "terms")))
    formula <- attr(formula, "terms")
  if(inherits(formula, "formula")) {
    f <- as.Formula(formula)
    f <- formula(f, lhs = TRUE, rhs = FALSE)
    if(keep.functions) {
      cf <- as.character(formula)
      rn <- if(length(cf) < 3) character(0) else cf[2]
    } else {
      rn <- all.vars(f)
      if(any(grepl("|", rn, fixed = TRUE)))
        rn <- all.vars(as.Formula(f))
    }
  } else {
    if(inherits(formula, "list")) {
      rn <- NULL
      for(i in seq_along(formula)) {
        if(is.null(formula[[i]]$formula) & inherits(formula[[i]], "list")) {
          for(j in seq_along(formula[[i]])) {
            if(!hierarchical & (j > 1)) {
              next
            } else {
              tf <- if(is.null(formula[[i]][[j]]$formula)) {
                formula[[i]][[j]]
              } else formula[[i]][[j]]$formula
              rn <- c(rn, response.name(tf, keep.functions = keep.functions))
            }
          }
        } else {
          rn <- c(rn , response.name(formula[[i]]$formula, keep.functions = keep.functions))
        }
      }
    }
  }
  if(!length(rn))
    rn <- NA
  if(na.rm)
    rn <- rn[!is.na(rn)]
  rn
}

response_name <- function(object, ...)
{
  return(response.name(object, ...))
}

## Multi step time series.
#get_data <- function(y, h = 10, p, P, predictions = NULL, ...) {
#  m <- max(round(frequency(y)), 1L)
#  ## Set up lagged matrix.
#  n <- length(y)
#  y <- as.ts(y)
#  if(m == 1) {
#    if(missing(p)) {
#      stop("need to specify p!")
#    }
#    if(p >= n) {
#      warning("Reducing number of lagged inputs due to short series")
#      p <- n - 1
#    }
#    lags <- 1:p
#    if (P > 1) {
#      warning("Non-seasonal data, ignoring seasonal lags")
#    }
#    P <- 0
#  } else {
#    if(missing(p)) {
#      stop("need to specify p!")
#    }
#    if(any(p >= n)) {
#      warning("Reducing number of lagged inputs due to short series")
#      p <- n - 1
#    }
#    if(P > 0 && n >= m * P + 2) {
#      lags <- sort(unique(c(1:p, m * (1:P))))
#    } else {
#      lags <- 1:p
#      if (P > 0) {
#        warning("Series too short for seasonal lags")
#        P <- 0
#      }
#    }
#  }

#  maxlag <- max(lags)
#  nlag <- length(lags)

#  getX <- function(y) {
#    n <- length(y)
#    X <- matrix(NA_real_, ncol = nlag, nrow = n - maxlag)
#    for(i in 1:nlag)
#      X[, i] <- y[(maxlag - lags[i] + 1):(n - lags[i])]
#    return(X)
#  }

#  X <- getX(y)
#  y <- y[-(1:maxlag)]

#  if(h > 0)
#    yh <- c(y[-(1:h)], rep(NA, h))
#  else
#    yh <- y

#  df <- as.data.frame(X)
#  names(df) <- paste0("L", lags)
#  df$y <- yh
#  if(!is.null(predictions)) {
#    #predictions <- predictions[-c(1:maxlag), , drop = FALSE]
#    pNA <- as.data.frame(matrix(NA, nrow = h - 1, ncol = ncol(predictions)))
#    names(pNA) <- names(predictions)
#    predictions <- rbind(predictions, pNA)
#    names(predictions) <- paste0("pred_", names(predictions))
#    df <- cbind(df, predictions)
#  }
#  df <- na.omit(df)

#  return(df)
#}

#get_formula <- function(data, family) {
#  if(is.function(family))
#    family <- family()
#  if(inherits(family, "gamlss.family"))
#    family <- tF(family)

#  f <- names(data)
#  f <- f[f != "y"]
#  pred <- grep("pred_", f, fixed = TRUE, value = TRUE)
#  f <- f[!grepl("pred_", f, fixed = TRUE)]
#  f <- paste(f, collapse = "+")
#  nx <- family$names
#  formula <- list()
#  for(i in nx) {
#    formula[[i]] <- f
#    if(length(pred))
#      formula[[i]] <- paste(formula[[i]], "+", paste0("pred_", i))
#    formula[[i]] <- as.formula(paste("~", formula[[i]]))
#  }
#  formula[[1L]] <- update(formula[[1L]], y ~ .)

#  return(formula)
#}

#make_model_srt <- function(y, h = 10, p, P, family, ...) {
#  df <- get_data(y, h = h, p = p, P = P, ...)
#  f <- get_formula(df, family)
#  b <- srt(f, data = df, ...)
#  return(b)
#}

#srtms <- function(y, h = 132, p, P, ...) {
#  models <- list()
#  predictions <- NULL
#  for(j in 1:h) {
#    cat(".. horizon", j, "\n")
#    models[[j]] <- make_model_srt(y, h = j, p = p, P = P, predictions = predictions, ...)
#    predictions <- predict(models[[j]])
#  }
#  class(models) <- "srtms"
#  attr(models, "y") <- y
#  attr(models, "setup") <- list("p" = p, "P" = P, "h" = h)
#  return(models)
#}

#make_model_df <- function(y, h = 10, p, P, family, ...) {
#  df <- get_data(y, h = h, p = p, P = P)
#  f <- get_formula(df, family)
#  b <- distforest(f[[1]], data = df, ...)
#  return(b)
#}

#dfms <- function(y, h = 132, p, P, ...) {
#  models <- list()
#  for(j in 1:h) {
#    cat(".. horizon", j, "\n")
#    models[[j]] <- make_model_df(y, h = j, p = p, P = P, ...)
#  }
#  class(models) <- "dfms"
#  attr(models, "y") <- y
#  attr(models, "setup") <- list("p" = p, "P" = P, "h" = h)
#  return(models)
#}

#predict.srtms <- function(object, FUN = NULL, ...) {
#  y <- attr(object, "y")
#  setup <- attr(object, "setup")

#  nd <- get_data(y, h = 0, p = setup$p, P = setup$P)
#  nd <- nd[nrow(nd), , drop = FALSE]

#  if(is.null(FUN)) {
#    FUN <- function(par) {
#      family(object[[1L]])$q(0.5, par)
#    }
#  }

#  fit <- list()
#  for(i in 1:length(object)) {
#    par <- predict(object[[i]], newdata = nd, type = "parameter")
#    predictions <- predict(object[[i]], newdata = nd, type = "link")
#    names(predictions) <- paste0("pred_", names(predictions))
#    nd[names(predictions)] <- predictions
#    fit[[i]] <- FUN(par)
#  }

#  fit <- do.call("rbind", fit)

#  return(fit)
#}
