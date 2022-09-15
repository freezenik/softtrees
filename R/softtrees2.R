node_fun2 <- function(w, x, weights, y, I, lambda, gamma, eta, i, j, family, Y, W, linear) {
  g <- sigmoid(x %*% w)
  G <- cbind(g, 1 - g) * weights[, j]

  weights[, j] <- G

  X <- as.matrix(weights)
  if(linear)
    X <- cbind(x[, -1L, drop = FALSE], X)
  X <- cbind(1, X)

  XW <- X * W

  beta <- solve(crossprod(XW, X) + diag(gamma, ncol(X)), t(XW) %*% y)

  eta[[i]] <- drop(X %*% beta)

  ll <- family$loglik(Y, family$map2par(eta))

  -1 * ll + lambda * sum(w^2)
}

node_fun_grad2 <- function(w, x, weights, y, I, lambda, gamma, eta, i, j, family, Y, W, linear) {
  xw <- drop(x %*% w)
  g <- sigmoid(x %*% w)
  G <- cbind(g, 1 - g) * weights[, j]

  w0 <- weights[, j]
  weights[, j] <- G

  X <- as.matrix(weights)
  if(linear)
    X <- cbind(x[, -1L, drop = FALSE], X)
  X <- cbind(1, X)

  XW <- X * W

  beta <- drop(solve(crossprod(XW, X) + diag(gamma, ncol(X)), t(XW) %*% y))

  eta[[i]] <- drop(X %*% beta)

  score <- process_derivs(family$score[[i]](Y, family$map2par(eta), id = i), is.weight = FALSE)

  beta <- tail(beta, 2)

  s <- score * w0 * (beta[1] - beta[2]) * sigmoid_deriv(xw) * x

  grad <- colSums(s) - diag(rep(lambda, length(w))) %*% w

  -grad
}

.srt.fit2 <- function(X, y, family, k = 10, lambda = 0.001, gamma = 0.001, aic = TRUE,
  K = NULL, plot = TRUE, verbose = TRUE, maxit = Inf, index = NULL,
  eps = 1e-05,
  initialize = TRUE, maxs = Inf, select = FALSE, linear = FALSE,
  offset = NULL, weights = NULL, ...)
{
  nx <- names(X)

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

  force <- list(...)$force
  flush <- list(...)$flush

  if(is.null(flush))
    flush <- TRUE
  if(is.null(force))
    force <- TRUE

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

  G <- lw <- eta <- Xi <- crit <- beta <- tcoef <- coef <- index <- list()

  for(i in nx) {
    lw[[i]] <- data.frame(rep(1, N))
    eta[[i]] <- rep(0, N)
    coef[[i]] <- list()
  }

  for(i in nx) {
    eta[[i]] <- rep(0, N)
    if(!is.null(offset[[i]])) {
      eta[[i]] <- eta[[i]] + offset[[i]]
    }
  }

  eta2 <- eta

  ic <- logLik <- edf_save <- size_save <- NULL

  do <- TRUE
  iter <- 1

  jj <- rep(NA, length(nx))
  names(jj) <- nx

  lambda <- rep(lambda, length.out = length(nx))
  names(lambda) <- nx
  gamma <- rep(gamma, length.out = length(nx))
  names(gamma) <- nx

  eps0 <- eps + 1

  ## Initialize intercepts.
  eta0 <- init.eta(eta, y, family, N)

  Xi <- cl2 <- list()
  for(i in nx) {
    Xi[[i]] <- X[[i]][, "(Intercept)", drop = FALSE]
    cl2[[i]] <- 0
  }

  if(verbose)
    cat(".. initializing parameters ..\n")

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
        XW <- Xi[[i]] * hess * weights
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
        objfun1 <- function(nu) {
          ll0 - fnu(nu)
        }
        nu <- optimize(objfun1, c(0, 1))$minimum
        cl2[[i]] <- nu * cl2[[i]] + (1 - nu) * b0
        eta[[i]] <- drop(Xi[[i]] * cl2[[i]])
      }
    }
    eps0 <- do.call("cbind", eta)
    eps0 <- mean(abs((eps0 - do.call("cbind", eta0)) / eps0), na.rm = TRUE)

    if(verbose) {
      cat(if(ia & flush) "\r" else if(k2 > 1) "\n" else NULL)
      cat("logLik =", family$loglik(y, family$map2par(eta)), "eps =", eps0,
        "iter =", k2, if(!ia) "\n" else NULL)
    }

    k2 <- k2 + 1
  }

  if(!ia)
    plot <- FALSE
  if(plot) {
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
  }

  eps0 <- eps + 1L

  while(do & (iter < maxit) & (eps0 > eps)) {
    eta0 <- eta
    if(select)
      ll0 <- family$loglik(y, family$map2par(eta))
    edf <- size <- 0
    for(i in nx) {
      if(ncol(X[[i]]) < 2) {
        objfun2 <- function(par) {
          eta[[i]] <- rep(par, length = N)
          if(is.null(weights[[i]])) {
            ll <- family$loglik(y, family$map2par(eta))
          } else {
            d <- family$d(y, family$map2par(eta)) * weights
            ll <- sum(d, na.rm = TRUE)
          }
          return(ll)
        }
    
        gradfun2 <- function(par) {
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

        opt <- optim(mean(eta[[i]]), fn = objfun2, gr = gradfun2,
          method = "L-BFGS-B", control = list(fnscale = -1))

        eta[[i]] <- rep(opt$par, N)

        coef[[i]][[length(coef[[i]]) + 1L]] <- list("w" = "root", "beta" = opt$par)
      } else {
        peta <- family$map2par(eta)
        hess <- process_derivs(family$hess[[i]](y, peta, id = i), is.weight = TRUE)
        score <- process_derivs(family$score[[i]](y, peta, id = i), is.weight = FALSE)
        z <- eta[[i]] + 1 / hess * score

        if(!select)
          ll0 <- family$loglik(y, peta)

        eta2 <- eta

        nc <- ncol(X[[i]])
        G[[i]] <- Xi[[i]] <- beta[[i]] <- tcoef[[i]] <- list()

        kk <- ncol(lw[[i]])
        crit[[i]] <- rep(NA, ncol(lw[[i]]))

        for(j in 1:kk) {
          opt <- try(optim(rep(cl2[[i]], ncol(X[[i]])), fn = node_fun2, gr = node_fun_grad2,
            x = X[[i]], weights = lw[[i]], y = z, lambda = lambda[i], gamma = gamma[i],
            eta = eta, i = i, j = j, family = family, Y = y,
            W = if(is.null(weights[[i]])) hess else hess * weights[[i]], linear = linear[i],
            method = "L-BFGS-B"), silent = TRUE)
          if(!inherits(opt, "try-error")) {
            g <- sigmoid(X[[i]] %*% opt$par)

            tcoef[[i]][[j]] <- opt$par

            G[[i]][[j]] <- cbind(g, 1 - g) * lw[[i]][, j]

            Xi1 <- lw[[i]]
            Xi1[, j] <- G[[i]][[j]]

            Xi1 <- as.matrix(Xi1)
            if(linear[i])
              Xi1 <- cbind(X[[i]][, -1L, drop = FALSE], Xi1)
            Xi1 <- cbind(1, Xi1)
            Xi[[i]][[j]] <- Xi1

            if(is.null(weights[[i]])) {
              Xi1W <- Xi1 * hess
            } else {
              Xi1W <- Xi1 * hess * weights[[i]]
            }

            beta[[i]][[j]] <- drop(solve(crossprod(Xi1W, Xi1) + diag(gamma[i], ncol(Xi1)), t(Xi1W) %*% z))

            eta2[[i]] <- drop(Xi1 %*% beta[[i]][[j]])
            ll <- family$loglik(y, family$map2par(eta2))

            crit[[i]][j] <- ll - ll0
          } else {
            crit[[i]][j] <- -Inf
          }
        }

        next_eps <- all(abs(crit[[i]] / ll0) <= eps)

        if((all(crit[[i]] <= 0) | next_eps) & force) {
          if(verbose)
            cat(if(ia) "\n" else NULL, "..", i, "no improvements, next ..\n")
        } else {
          j <- which.max(crit[[i]])

          if(is.null(weights[[i]])) {
            Xi1W <- Xi[[i]][[j]] * hess
          } else {
            Xi1W <- Xi[[i]][[j]] * hess * weights[[i]]
          }

          XWX <- crossprod(Xi1W, Xi[[i]][[j]])
          Xz <- t(Xi1W) %*% z

          objfun3 <- function(tau, ret.b = FALSE, ret.edf = FALSE) {
            S <- XWX + diag(tau, ncol(Xi[[i]][[j]]))
            b <- drop(solve(S, Xz))
            if(ret.b)
              return(b)
            eta2[[i]] <- drop(Xi[[i]][[j]] %*% b)
            ll <- family$loglik(y, family$map2par(eta2))
            edf <- sum(diag(XWX %*% chol2inv(chol(S))), na.rm = TRUE)
            if(ret.edf)
              return(edf)
            -2 * ll + edf * K
          }

          gamma[i] <- optimize(objfun3, lower = 1e-10, upper = 1e+10)$minimum

          beta[[i]][[j]] <- objfun3(gamma[i], ret.b = TRUE)
          edf <- edf + objfun3(gamma[i], ret.edf = TRUE)
          eta[[i]] <- drop(Xi[[i]][[j]] %*% beta[[i]][[j]])
          lw[[i]][, j] <- G[[i]][[j]]
          lw[[i]] <- as.data.frame(as.matrix(lw[[i]]))
          coef[[i]][[length(coef[[i]]) + 1L]] <- list("w" = tcoef[[i]][[j]], "beta" = beta[[i]][[j]])
          index[[i]] <- c(index[[i]], j)
          cl2[[i]] <- beta[[i]][[j]][1L]
          #edf <- edf + ncol(Xi[[i]][[j]])
        }
      }
    }

    size <- sum(sapply(lw, ncol))

    ll <- family$loglik(y, family$map2par(eta))

    logLik <- c(logLik, ll)
    ic <- c(ic, -2 * ll + edf * K)
    edf_save <- c(edf_save, edf)
    size_save <- c(size_save, size)

    if(plot) {
      par(mfrow = c(2, 2), mar = c(4.5, 4.5, 1.1, 1.1))
      plot(ic, type = "l", lwd = 2, xlab = "Iteration", ylab = "-2 * logLik + K * edf")
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
      cat("AIC = ", ic[length(ic)],
        " logLik = ", ll,
        " size = ", size,
        " edf = ", round(edf, 2),
        " eps = ", round(eps0, 5),
        if(select) paste0(" par = ", i) else NULL,
        if(!ia) "\n" else NULL, sep = "")
    }

    if(aic) {
      l <- length(ic)
      if(l > 1) {
        do <- ic[l] <= ic[l - 1]
      } else {
        do <- TRUE
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
      edf_save <- edf_save[-length(edf_save)]
      size_save <- size_save[-length(size_save)]
      for(i in nx) {
        if(length(coef[[i]]) > 1) {
          coef[[i]] <- coef[[i]][-length(coef[[i]])]
          if(!is.character(coef[[i]][[1L]]$w)) {
            index[[i]] <- index[[i]][-length(index[[i]])]
          }
        }
      }
    }
  }

  if(verbose & ia) {
    cat("\n.. final model ..\n")
    cat("AIC = ", ic[length(ic)],
      " logLik = ", logLik[length(logLik)],
      " size = ", size_save[length(size_save)],
      " edf = ", round(edf_save[length(edf_save)], 2)
    )
    cat("\n")
  }

  rval <- list("coefficients" = coef, "index" = index, "fitted.values" = eta,
    "size" = size_save[length(size_save)], "criterion" = ic, "K" = K, "linear" = linear)

  rval
}

predict.srt2 <- function(object, newdata, model = NULL,
  type = c("link", "parameter"), k = NULL, ...)
{
  nx <- names(object$formula)
  if(missing(newdata) || is.null(newdata)) {
    X <- model.matrix(object)
  } else {
    X <- model.matrix(object, data = newdata, ...)
  }

  linear <- object$linear

  if(length(object$scaled)) {
    for(i in nx) {
      if(length(object$scaled[[i]])) {
        for(j in names(object$scaled[[i]])) {
          sx <- object$scaled[[i]][[j]]
          X[[i]][, j] <- (X[[i]][, j] - sx$mean) / sx$sd
          ##X[[i]][, j] <- (X[[i]][, j] - sx$min) / (sx$max - sx$min)
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
  index <- object$index
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
    if(length(coef[[i]][[1L]]$beta) < 2L) {
      pred[[i]] <- rep(coef[[i]][[1L]]$beta, N)
      coef[[i]] <- coef[[i]][-1L]
    } else {
      pred[[i]] <- rep(0, N)
    }
    if(length(coef[[i]])) {
      for(j in 1:length(coef[[i]])) {
        if(!is.character(coef[[i]][[j]]$w)) {
          w <- coef[[i]][[j]]$w
          g <- sigmoid(X[[i]] %*% w)
          G <- cbind(g, 1 - g) * lw[[i]][, index[[i]][j]]
          lw[[i]][, index[[i]][j]] <- G
          lw[[i]] <- as.data.frame(Xi <- as.matrix(lw[[i]]))
          if(linear[i])
            Xi <- cbind(X[[i]][, -1L, drop = FALSE], Xi)
          Xi <- cbind(1, Xi)
          pred[[i]] <- drop(Xi %*% coef[[i]][[j]]$beta)
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

predict.srf2 <- function(object, newdata, model = NULL,
  type = c("link", "parameter"), FUN = mean, ...)
{
  nx <- names(object$formula)
  if(missing(newdata) || is.null(newdata)) {
    X <- model.matrix(object, contrasts.arg = object$contrasts)
  } else {
    X <- model.matrix(object, data = newdata, contrasts.arg = object$contrasts, ...)
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
  linear <- object$linear
  for(i in model) {
    pred[[i]] <- list()
    for(ti in trees) {
      coef <- coef(object[[ti]])
      linear <- object[[ti]]$linear
      index <- object[[ti]]$index
      lw <- data.frame(rep(1, nrow(X[[1]])))
      if(length(coef[[i]][[1L]]$beta) < 2L) {
        pred[[i]][[ti]] <- rep(coef[[i]][[1L]]$beta, N)
        coef[[i]] <- coef[[i]][-1L]
      } else {
        pred[[i]][[ti]] <- rep(0, N)
      }
      if(length(coef[[i]])) {
        for(j in 1:length(coef[[i]])) {
          if(!is.character(coef[[i]][[j]]$w)) {
            w <- coef[[i]][[j]]$w
            g <- sigmoid(X[[i]] %*% w)
            G <- cbind(g, 1 - g) * lw[, index[[i]][j]]
            lw[, index[[i]][j]] <- G
            lw <- as.data.frame(Xi <- as.matrix(lw))
            if(linear[i])
              Xi <- cbind(X[[i]][, -1L, drop = FALSE], Xi)
            Xi <- cbind(1, Xi)
            pred[[i]][[ti]] <- drop(Xi %*% coef[[i]][[j]]$beta)
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

