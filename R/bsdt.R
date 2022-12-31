## Soft Distributional Regression Tree Machine.
bsdt <- function(..., k = 4, maxit = 100, batch_ids = NULL,
  nu = 0.1, lambda = 0.0001, omin = 50, faster = TRUE,
  plot = TRUE, verbose = TRUE, flush = TRUE, eps_logLik = Inf)
{
  mf <- srt(..., ret.mfd = TRUE)
  if(nrow(mf$X[[1]]) < omin)
    stop("argument omin is too large!")
  fit <- .bsdt.fit(mf$X, mf$y, family = mf$family, offset = mf$offset,
    k = k, maxit = maxit, batch_ids = batch_ids, faster = faster,
    nu = nu, plot = plot, verbose = verbose, flush = flush, lambda = lambda,
    omin = omin, eps_logLik = eps_logLik)
  mf[names(fit)] <- fit
  mf$call <- match.call()
  class(mf) <- c("bsdt", "srt")
  return(mf)
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

.bsdt.fit <- function(X, y, family, k, maxit, batch_ids, nu, start, offset,
  plot, verbose, flush, digits = 4L, lambda, faster, omin, eps_logLik)
{
  nx <- names(X)
  N <- nrow(X[[1L]])

  if(is.null(batch_ids))
    batch_ids <- rep(list(seq_len(N)), maxit)

  maxit <- length(batch_ids)

  trees <- list()
  for(i in nx)
    trees[[i]] <- list()
  eta <- etas <- vector("mode" = "list", length = length(nx))
  names(eta) <- nx

  ia <- interactive()
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

  ll_save <- NULL
  m <- 1
  ptm <- proc.time()
  for(iter in seq_len(maxit)) {
    if(is.null(dim(y))) {
      yi <- y[batch_ids[[iter]]]
    } else {
      yi <- y[batch_ids[[iter]], , drop = FALSE]
    }
    for(i in nx) {
      if(iter > 1) {
        eta[[i]] <- 0
        for(j in 1:length(trees[[i]])) {
          eta[[i]] <- eta[[i]] + nu * predict(trees[[i]][[j]],
            data = X[[i]][batch_ids[[iter]], , drop = FALSE])
        }
      } else {
        eta[[i]] <- rep(0, length(batch_ids[[iter]]))
      }
      if(!is.null(offset[[i]])) {
        eta[[i]] <- eta[[i]] + offset[[i]][batch_ids[[iter]]]
      }
    }

    ll0 <- family$loglik(yi, family$map2par(eta))

    if(length(lambda) != length(nx)) {
      lambda <- rep(lambda, length.out = length(nx))
      names(lambda) <- nx
    }
    if(is.null(names(lambda)))
      names(lambda) <- nx

    ttrees <- list()
    for(i in nx) {
      ttrees[[i]] <- .fit.stree(x = X[[i]][batch_ids[[iter]], , drop = FALSE],
        y = yi, eta = eta, family = family, i = i, k = k, lambda = lambda[i],
        faster = faster, omin = omin)
      eta[[i]] <- eta[[i]] + nu * predict(ttrees[[i]],
        data = X[[i]][batch_ids[[iter]], , drop = FALSE])
    }

    ll1 <- family$loglik(yi, family$map2par(eta))
    eps <- abs((ll1 - ll0) / ll0)

    if((ll1 > ll0) & (eps < eps_logLik)) {
      ll_save <- c(ll_save, ll1)

      for(i in nx) {
        trees[[i]][[m]] <- ttrees[[i]]
      }
      m <- m + 1L
    }

    if(plot & (m > 1)) {
      par(mfrow = c(2, 2), mar = c(4.5, 4.5, 1.1, 1.1))
      plot(ll_save, type = "l", lwd = 2, xlab = "#Trees", ylab = "logLik")
      par <- family$map2par(eta)
      if(!is.null(family$p)) {
        if(discrete) {
          ymin <- min(yi, na.rm = TRUE)
          a <- family$p(ifelse(yi == ymin, yi, yi - 1), par)
          a <- ifelse(yi == ymin, 0, a)
          b <- family$p(yi, par)
          u <- runif(length(yi), a, b)
          u <- ifelse(u > 0.999999, u - 1e-10, u)
          u <- ifelse(u < 1e-06, u + 1e-10, u)
          res <- qnorm(u)
        } else {
          prob <- family$p(yi, par)
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
        plot(par[[1L]], if(inherits(yi, "Surv")) yi[, "time"] else if(inherits(yi, "matrix") ) yi[, 1] else yi,
          ylab = "Response", xlab = "Fitted values")
        abline(0, 1, col = 4, lwd = 2)
        hist((if(inherits(yi, "Surv")) yi[, "time"] else if(inherits(yi, "matrix") ) yi[, 1] else yi) - par[[1L]],
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
        plot(my, if(inherits(yi, "Surv")) yi[, "time"] else if(inherits(yi, "matrix") ) yi[, 1] else yi,
          ylab = "Response", xlab = "Fitted values")
        abline(0, 1, col = 4, lwd = 2)
        hist((if(inherits(yi, "Surv")) yi[, "time"] else if(inherits(yi, "matrix") ) yi[, 1] else yi) - my,
          freq = FALSE, col = "lightgray", main = "", xlab = "Residuals")
      }
    }

    if(verbose) {
      cat(if(ia & flush) "\r" else if(iter > 1) "\n" else NULL)
      cat("logLik = ", fmt(ll_save[length(ll_save)], width = 8, digits = digits),
        " eps = ", fmt(eps, width = 6, digits = digits + 2),
        " iter = ", iter,
        if(!ia) "\n" else NULL, sep = "")
    }
  }

  elapsed <- c(proc.time() - ptm)[3]

  if(verbose & ia) {
    cat("\n")
  }

  if(verbose) {
    et <- if(elapsed > 60) {
      paste(formatC(format(round(elapsed / 60, 2), nsmall = 2), width = 5), "min", sep = "")
    } else paste(formatC(format(round(elapsed, 2), nsmall = 2), width = 5), "sec", sep = "")
    cat("elapsed time: ", et, "\n", sep = "")
  }

  rval <- list("trees" = trees, "maxit" = maxit,
    "logLik" = ll_save, "nu" = nu, "k" = k, "lambda" = lambda,
    "elapsed" = elapsed)

  return(rval)
}

.fit.stree <- function(x, y, eta, family, i, k, lambda, faster, omin)
{
  coef <- list()

  if(ncol(x) > 1) {
    fn <- function(par, x, weights) {
      beta <- par[1:2]
      w <- par[-c(1:2)]
      g <- sigmoid(x %*% w)
      G <- cbind(g, 1 - g) * weights
      eta[[i]] <- eta[[i]] + drop(G %*% beta)
      ll <- family$d(y, family$map2par(eta), log = TRUE)
      ll[(ll < -50) | !is.finite(ll)] <- NA
      ll <- sum(ll, na.rm = TRUE)
      ll <- ll - lambda * sum(w^2)
      return(-1 * ll)
    }

    gr <- function(par, x, weights) {
      beta <- par[1:2]
      w <- par[-c(1:2)]
      xw <- drop(x %*% w)
      g <- sigmoid(xw)
      G <- cbind(g, 1 - g) * weights
      eta[[i]] <- eta[[i]] + drop(G %*% beta)
      score <- process_derivs(family$score[[i]](y, family$map2par(eta), id = i), is.weight = FALSE)
      s <- cbind(score * G, score * weights * (beta[1] - beta[2]) * sigmoid_deriv(xw) * x)
      grad <- colSums(s) - diag(c(0, 0, lambda, lambda)) %*% c(beta, w)
     -grad
    }

    N <- data.frame("r" = rep(1, nrow(x)))

    K <- 1
    while(K <= k) {
      vn <- colnames(x)
      vn <- vn[vn != "(Intercept)"]
      val_j <- list()
      peta <- family$map2par(eta)
      ll0 <- family$loglik(y, peta)
      if(faster) {
        score <- process_derivs(family$score[[i]](y, peta, id = i), is.weight = FALSE)
        hess <- process_derivs(family$hess[[i]](y, peta, id = i), is.weight = TRUE)
        z <- score / hess
        for(l in 1:ncol(N)) {
          if(sum(N[, l] > 0.0001) >= omin) {
            pval <- rep(NA, length(vn))
            for(j in seq_along(vn)) {
              xj <- x[, vn[j]]
              ok <- FALSE
              if((uxj <- length(unique(xj))) > 7) {
                breaks <- unique(quantile(x[, vn[j]], prob = seq(0, 1, length = 5)))
                if(length(breaks) > 2L) {
                  xj <- cut(xj, breaks = breaks, include.lowest = TRUE)
                  ok <- TRUE
                }
              } else {
                if(uxj > 1) {
                  xj <- as.factor(xj)
                  ok <- TRUE
                }
              }
              if(ok) {
                mj <- lm(z ~ xj, weights = hess * N[, l])
                fstat <- summary(mj)$fstatistic
                if(!is.null(fstat))
                  pval[j] <- pf(fstat[1L], fstat[2L], fstat[3L], lower.tail = FALSE)
              }
            }
            j <- which.min(pval)
            val_j[[l]] <- optim(c(0.001, 0.001, 0.001, 1), fn = fn, gr = gr,
              x = x[, c("(Intercept)", vn[j])], weights = N[, l], method = "L-BFGS-B",
              control = list("pgtol"= 0.00001))
            val_j[[l]]$index <- l
            val_j[[l]]$variable <- vn[j]
            val_j[[l]]$contrib <- -1 * val_j[[l]]$value - ll0
          } else {
            val_j[[l]] <- list("value" = Inf, "par" = rep(0, 4), "index" = 1L)
          }
        }
        j <- which.min(sapply(val_j, function(x) x$value))
        vj <- val_j[[j]]$variable
      } else {
        for(j in vn) {
          val_l <- list()
          for(l in 1:ncol(N)) {
            if(sum(N[, l] > 0.0001) >= omin) {
              val_l[[l]] <- optim(c(0.1, 0.1, 1, 1), fn = fn, gr = gr,
                x = x[, c("(Intercept)", j)], weights = N[, l], method = "L-BFGS-B",
                control = list("pgtol"= 0.00001))
            } else {
              val_l[[l]] <- list("value" = Inf, "par" = rep(0, 4), "index" = 1L)
            }
          }
          l <- which.min(sapply(val_l, function(x) x$value))
          val_j[[j]] <- val_l[[l]]
          val_j[[j]]$index <- l
          val_j[[j]]$contrib <- -1 * val_j[[l]]$value - ll0
        }
        j <- which.min(sapply(val_j, function(x) x$value))
        vj <- vn[j]
      }
      coef[[K]] <- list(
        "coefficients" = val_j[[j]]$par,
        "value" = -1 * val_j[[j]]$value,
        "variable" = vj,
        "index" = val_j[[j]]$index,
        "contrib" = val_j[[j]]$contrib
      )
      beta <- coef[[K]]$coefficients[1:2]
      w <- coef[[K]]$coefficients[-c(1:2)]
      g <- sigmoid(x[, c("(Intercept)", coef[[K]]$variable)] %*% w)
      G <- cbind(g, 1 - g) * N[, coef[[K]]$index]
      eta[[i]] <- eta[[i]] + drop(G %*% beta)
      N[, coef[[K]]$index] <- G
      N <- as.data.frame(as.matrix(N))
      K <- ncol(N)
    }
  } else {
    fn2 <- function(par) {
      eta[[i]] <- eta[[i]] + par
      ll <- family$loglik(y, family$map2par(eta))
      return(-1 * ll)
    }

    gr2 <- function(par) {
      eta[[i]] <- eta[[i]] + par
      score <- process_derivs(family$score[[i]](y, family$map2par(eta), id = i), is.weight = FALSE)
      -sum(score)
    }

    val <- optim(1, fn = fn2, gr = gr2, method = "L-BFGS-B")

    coef[[1L]] <- list(
      "coefficients" = val$par,
      "value" = -1 * val$value,
      "variable" = "(Intercept)",
      "index" = 1
    )
  }

  class(coef) <- c("stree", "list")

  return(coef)
}

predict.stree <- function(object, data, ...)
{
  fit <- rep(0, nrow(data))
  if(length(object) < 2L) {
    fit <- fit + object[[1]]$coefficients
  } else {
    N <- data.frame("r" = rep(1, nrow(data)))
    for(j in 1:length(object)) {
      beta <- object[[j]]$coefficients[1:2]
      w <- object[[j]]$coefficients[-c(1:2)]
      g <- sigmoid(data[, c("(Intercept)", object[[j]]$variable)] %*% w)
      G <- cbind(g, 1 - g) * N[, object[[j]]$index]
      fit <- fit + drop(G %*% beta)
      N[, object[[j]]$index] <- G
      N <- as.data.frame(as.matrix(N))
    }
  }
  return(fit)
}

variables <- function(x)
{
  vars <- NULL
  K <- 0
  if(length(x$trees)) {
    nx <- names(x$trees)
    vars <- list()
    for(i in nx) {
      for(j in 1:length(x$trees[[i]])) {
        for(k in 1:length(x$trees[[i]][[j]])) {
          vars[[i]] <- c(vars[[i]], x$trees[[i]][[j]][[k]]$variable)
          K <- K + 1
        }
      }
      vars[[i]] <- sort(table(vars[[i]]), decreasing = TRUE)
    }
    for(i in nx)
      vars[[i]] <- vars[[i]] / K
  }
  class(vars) <- "bsdt_variables"
  return(vars)
}

contrib <- function(x) {
  vars <- contrib <- NULL
  res <- list()
  K <- 0
  if(length(x$trees)) {
    nx <- names(x$trees)
    vars <- contrib <- list()
    for(i in nx) {
      for(j in 1:length(x$trees[[i]])) {
        for(k in 1:length(x$trees[[i]][[j]])) {
          vars[[i]] <- c(vars[[i]], x$trees[[i]][[j]][[k]]$variable)
          cj <- x$trees[[i]][[j]][[k]]$contrib
          if(is.null(cj))
            cj <- 0
          contrib[[i]] <- c(contrib[[i]], cj)
          K <- K + 1
        }
      }
      res[[i]] <- data.frame("variable" = vars[[i]], "contrib" = contrib[[i]])
    }
  }

  class(res) <- "bsdt_contrib"
  return(res)
}

plot.bsdt_contrib <- function(x, model = NULL, legend = TRUE, ...) {
  if(!is.null(model))
    model <- names(x)[pmatch(model, names(x))]
  else
    model <- names(x)
  x <- x[model]
  res <- list()
  for(i in names(x)) {
    vn <- x[[i]]$variable
    vc <- list()
    for(j in vn) {
      cd <- x[[i]][x[[i]]$variable == j, , drop = FALSE]
      vc[[j]] <- sum(cd$contrib)
    }
    vc <- sort(unlist(vc), decreasing = TRUE)
    res[[i]] <- vc
  }

  vn <- unique(unlist(lapply(res, names)))

  res2 <- list()
  for(j in vn) {
    for(i in names(res)) {
      res2[[j]] <- c(res2[[j]], res[[i]][j])
    }
    names(res2[[j]]) <- names(res)
  }
  res2 <- do.call("cbind", res2)
  res2[is.na(res2)] <- 0

  j <- apply(res2, 2, sum)
  res2 <- res2 #/ max(j)
  res2 <- res2[, order(j, decreasing = TRUE), drop = FALSE]

  plot <- list(...)$plot
  if(is.null(plot))
    plot <- TRUE

  if(isTRUE(plot)) {
    N <- nrow(res2)
    col <- if(N < 2) "lightgray" else colorspace::rainbow_hcl(N)

    plt <- barplot(res2, col = col, ylab = "logLik contribution",
        xaxt = "n")

    text(plt, par("usr")[3], labels = colnames(res2),
      srt = 50, adj = c(1.1, 1.1), xpd = TRUE, cex = 1)

    if(!isFALSE(legend) & (N > 1)) {
      pos <- list(...)$pos
      if(is.null(pos))
        pos <- "topright"
      bty <- list(...)$bty
      if(is.null(bty))
        bty <- "n"
      legend <- names(x)
      if(length(pos) > 1) {
        legend(pos[1], pos[2], legend = legend, fill = col, bty = bty, title = "Parameter")
      } else {
        legend(pos, legend = legend, fill = col, bty = bty, title = "Parameter")
      }
    }
  }

  invisible(res2)
}

plot.bsdt_variables <- function(x, legend = TRUE, ...)
{
  res <- NULL

  if(!is.null(x)) {
    main <- list(...)$main
    ylab <- list(...)$ylab
    if(is.null(ylab))
      ylab <- "% selected"
    vn <- unique(unlist(lapply(x, names)))
    res <- NULL
    for(j in vn) {
      rj <- NULL
      for(i in 1:length(x)) {
        rj <- c(rj, x[[i]][j])
      }
      res <- cbind(res, rj)
    }
    colnames(res) <- vn
    rownames(res) <- names(x)
    res[is.na(res)] <- 0

    cs <- apply(res, 2, sum)
    res <- res[, order(cs, decreasing = TRUE), drop = FALSE]

    plot <- list(...)$plot
    if(is.null(plot))
      plot <- TRUE

    if(isTRUE(plot)) {
      col <- colorspace::rainbow_hcl(length(x))

      plt <- barplot(res * 100, col = col, ylab = ylab, main = main,
        xaxt = "n")

      text(plt, par("usr")[3], labels = colnames(res),
           srt = 50, adj = c(1.1, 1.1), xpd = TRUE, cex = 1)

      if(!isFALSE(legend)) {
        pos <- list(...)$pos
        if(is.null(pos))
          pos <- "topright"
        bty <- list(...)$bty
        if(is.null(bty))
          bty <- "n"
        legend <- names(x)
        if(length(pos) > 1) {
          legend(pos[1], pos[2], legend = legend, fill = col, bty = bty, title = "Parameter")
        } else {
          legend(pos, legend = legend, fill = col, bty = bty, title = "Parameter")
        }
      }
    }
  }

  invisible(res)
}

predict.bsdt <- function(object, newdata, model = NULL,
  type = c("link", "parameter"), ...)
{
  nx <- names(object$formula)
  nu <- object$nu
  type <- match.arg(type)
  verbose <- list(...)$verbose
  pmatrix <- isTRUE(list(...)$pmatrix)
  mstop <- list(...)$mstop
  ia <- interactive()
  if(is.null(verbose))
    verbose <- TRUE
  if(missing(newdata) || is.null(newdata)) {
    X <- model.matrix(object)
  } else {
    X <- model.matrix(object, data = newdata, ...)
  }
  if(is.null(model)) {
    model <- nx
  } else {
    if(!all(model %in% nx))
      stop("argument model is specified wrong!")
  }
  pred <- list()
  for(i in model) {
    if(verbose) {
      cat(".. predictions for parameter", i, "\n")
    }
    pred[[i]] <- 0
    end <- if(is.null(mstop)) length(object$trees[[i]]) else mstop
    for(j in 1:end) {
      if(verbose) {
        if(ia & (j > 1))
          cat("\r")
        cat(".. .. tree", j, if(!ia) "\n" else NULL)
      }
      if(pmatrix) {
        pred[[i]] <- cbind(pred[[i]],  nu * predict(object$trees[[i]][[j]], data = X[[i]]))
      } else {
        pred[[i]] <- pred[[i]] + nu * predict(object$trees[[i]][[j]], data = X[[i]])
      }
    }
    if(verbose & ia)
      cat("\n")
    if(pmatrix)
      pred[[i]] <- t(apply(pred[[i]], 1, cumsum))
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
  if((length(pred) < 2L) & drop) {
    return(pred[[1L]])
  } else {
    return(if(pmatrix) pred else as.data.frame(pred))
  }
}

update.bsdt <- function(object, batch_ids = NULL, maxit = 10, lambda = 0.0001,
  verbose = TRUE, flush = TRUE, plot = TRUE, digits = 4, ...)
{
  family <- object$family

  discrete <- FALSE
  if(!is.null(family$type)) {
    if(tolower(family$type) == "discrete")
      discrete <- TRUE
  }
  if(family$family == "binomial")
    discrete <- TRUE

  ia <- interactive()
  if(!ia)
    plot <- FALSE
  if(plot) {
    opar <- par(no.readonly = TRUE)
    on.exit(par(opar))
  }

  eps_logLik <- list(...)$eps_logLik
  if(is.null(eps_logLik))
    eps_logLik <- 1

  X <- model.matrix(object, ...)
  y <- model.response(model.frame(object, ...)[[family$names[1]]])

  nu <- object$nu
  N <- nrow(X[[1L]])
  nx <- names(X)

  trees <- list()

  if(is.null(batch_ids))
    batch_ids <- rep(list(seq_len(N)), 1L)

  ll_save <- NULL

  for(iter in seq_len(maxit)) {
    for(i in nx) {
      for(j in 1:length(object$trees[[i]])) {
        ind <- batch_ids[[sample(1:length(batch_ids), size = 1L)]]

        if(is.null(dim(y))) {
          yi <- y[ind]
        } else {
          yi <- y[ind, , drop = FALSE]
        }

        eta <- list()
        for(ii in nx) {
          eta[[ii]] <- 0
          for(jj in 1:length(object$trees[[ii]]))
            eta[[ii]] <- eta[[ii]] + nu * predict(object$trees[[ii]][[jj]], data = X[[ii]][ind, , drop = FALSE])
        }

        eta2 <- eta

        ll0 <- family$loglik(yi, family$map2par(eta))

        eta2[[i]] <- eta2[[i]] - nu * predict(object$trees[[i]][[j]], data = X[[i]][ind, , drop = FALSE])
        ttree <- try(.refit.stree(object$trees[[i]][[j]], X[[i]][ind, , drop = FALSE], yi, eta2, family, i, lambda), silent = TRUE)


        if(!inherits(ttree, "try-error")) {
          eta2[[i]] <- eta2[[i]] + nu * predict(ttree, data = X[[i]][ind, , drop = FALSE])

          ll1 <- family$loglik(yi, family$map2par(eta2))

          eps <- abs((ll1 - ll0) / ll0)

          if(eps < eps_logLik) {
            ll_save <- c(ll_save, ll1)
            object$trees[[i]][[j]] <- ttree
            eta[[i]] <- eta2[[i]]
            if(verbose) {
              cat(if(ia & flush) "\r" else if(iter > 1) "\n" else NULL)
              cat(".. .. refitting", i, "tree", j, " eps = ", fmt(eps, width = 6, digits = digits + 2))
            }
          }
        }

        eta2[[i]] <- eta[[i]]
      }
    }

    trees[[iter]] <- object$trees

    if(verbose) {
      cat(if(ia & flush) "\r" else if(iter > 1) "\n" else NULL)
      cat("min. logLik = ", fmt(min(ll_save), width = 8, digits = digits),
        " max. logLik = ", fmt(max(ll_save), width = 8, digits = digits),
        " iter = ", iter,
        if(!ia) "\n" else NULL, sep = "")
    }

    if(plot & (iter > 1)) {
      par(mfrow = c(1, 1), mar = c(4.5, 4.5, 1.1, 1.1))
      plot(ll_save, type = "l", lwd = 2, xlab = "Iteration", ylab = "logLik")
    }
  }

  if(verbose)
    cat("\n")

  object$trees <- trees
  object$logLik <- ll_save

  class(object) <- c("bsdtr", "srt")

  return(object)
}

predict.bsdtr <- function(object, newdata, model = NULL,
  type = c("link", "parameter"), FUN = median, ...)
{
  nx <- names(object$formula)
  nu <- object$nu
  type <- match.arg(type)
  if(missing(newdata) || is.null(newdata)) {
    X <- model.matrix(object)
  } else {
    X <- model.matrix(object, data = newdata, ...)
  }
  if(is.null(model)) {
    model <- nx
  } else {
    if(!all(model %in% nx))
      stop("argument model is specified wrong!")
  }
  predi <- list()
  for(iter in 1:length(object$trees)) {
    pred <- list()
    for(i in model) {
      pred[[i]] <- 0
      for(j in 1:length(object$trees[[iter]][[i]])) {
        pred[[i]] <- pred[[i]] + nu * predict(object$trees[[iter]][[i]][[j]], data = X[[i]])
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
      predi[[i]] <- cbind(predi[[i]], pred[[i]])
    }
  }
  if(!is.null(FUN)) {
    for(i in model) {
      predi[[i]] <- apply(predi[[i]], 1, FUN, ...)
    }
  }
  drop <- list(...)$drop
  if(is.null(drop))
    drop <- TRUE
  if((length(predi) < 2L) & drop) {
    return(predi[[1L]])
  } else {
    return(predi)
  }
}

.refit.stree <- function(tree, x, y, eta, family, i, lambda)
{
  coef <- list()
  k <- length(tree)

  if(ncol(x) > 1) {
    fn <- function(par, x, weights) {
      beta <- par[1:2]
      w <- par[-c(1:2)]
      g <- sigmoid(x %*% w)
      G <- cbind(g, 1 - g) * weights
      eta[[i]] <- eta[[i]] + drop(G %*% beta)
      ll <- family$loglik(y, family$map2par(eta)) - lambda * sum(w^2)
      return(-1 * ll)
    }

    gr <- function(par, x, weights) {
      beta <- par[1:2]
      w <- par[-c(1:2)]
      xw <- drop(x %*% w)
      g <- sigmoid(xw)
      G <- cbind(g, 1 - g) * weights
      eta[[i]] <- eta[[i]] + drop(G %*% beta)
      score <- process_derivs(family$score[[i]](y, family$map2par(eta), id = i), is.weight = FALSE)
      s <- cbind(score * G, score * weights * (beta[1] - beta[2]) * sigmoid_deriv(xw) * x)
      grad <- colSums(s) - diag(c(0, 0, lambda, lambda)) %*% c(beta, w)
     -grad
    }

    N <- data.frame("r" = rep(1, nrow(x)))

    K <- 1
    while(K <= k) {
      j <- tree[[K]]$variable

      ll0 <- -1 * fn(tree[[K]]$coefficients, x[, c("(Intercept)", j)], N[, tree[[K]]$index])

      val <- optim(tree[[K]]$coefficients, fn = fn, gr = gr,
        x = x[, c("(Intercept)", j)], weights = N[, tree[[K]]$index], method = "L-BFGS-B",
        control = list("pgtol"= 0.0001))

      ll1 <- -1 * val$value

      if(ll1 > ll0) {
        tree[[K]] <- list(
          "coefficients" = val$par,
          "value" = -1 * val$value,
          "variable" = j,
          "index" = tree[[K]]$index
        )
      }

      beta <- tree[[K]]$coefficients[1:2]
      w <- tree[[K]]$coefficients[-c(1:2)]
      g <- sigmoid(x[, c("(Intercept)", tree[[K]]$variable)] %*% w)
      G <- cbind(g, 1 - g) * N[, tree[[K]]$index]
      eta[[i]] <- eta[[i]] + drop(G %*% beta)
      N[, tree[[K]]$index] <- G
      N <- as.data.frame(as.matrix(N))
      K <- ncol(N)
    }
  } else {
    fn2 <- function(par) {
      eta[[i]] <- eta[[i]] + par
      ll <- family$loglik(y, family$map2par(eta))
      return(-1 * ll)
    }

    gr2 <- function(par) {
      eta[[i]] <- eta[[i]] + par
      score <- process_derivs(family$score[[i]](y, family$map2par(eta), id = i), is.weight = FALSE)
      -sum(score)
    }

    val <- optim(tree[[1L]]$coefficients, fn = fn2, gr = gr2, method = "L-BFGS-B")

    tree[[1L]] <- list(
      "coefficients" = val$par,
      "value" = -1 * val$value,
      "variable" = "(Intercept)",
      "index" = 1
    )
  }

  class(tree) <- c("stree", "list")

  return(tree)
}

importance <- function(x) {
  sf <- plot(variables(x), plot = FALSE)
  cx <- plot(contrib(x), plot = FALSE)
  cx <- cx / sum(cx)
  vn <- colnames(sf)
  res <- list()
  for(j in vn) {
    cj <- sum(cx[, j])
    sj <- sum(sf[, j])
    res[[j]] <- cj * sj
  }
  res <- sort(unlist(res), decreasing = TRUE)
  res <- res / max(res)
  return(res)
}

logLik.bsdt <- function(object, ...) {
  return(object$logLik)
}

AIC.bsdt <- function(object, ..., k = 2) {
  ll <- logLik(object)
  df <- list()
  for(i in names(b$trees)) {
    df[[i]] <- rep(1, length(object$trees[[i]]))
  }
  df <- do.call("cbind", df)
  df <- rowSums(df)
  df <- cumsum(df)
  aic <- -2 *ll + k * df
  class(aic) <- "bsdt_aic"
  return(aic)
}

plot.bsdt_aic <- function(x) {
  x <- as.numeric(x)
  plot.default(x, type = "l", xlab = "Iteration", ylab = "AIC", lwd = 2)
  iter <- 1:length(x)
  b <- loess(x ~ iter, span = 0.2)
  p <- predict(b)
  lines(p, col = 4, lwd = 2)
  wm <- which.min(p)
  abline(v = wm, lty = 2, col = "lightgray")
  axis(3, at = wm, labels = paste("mstop =", wm))
  invisible(return(c("mstop" = wm)))
}

mstop.bsdt <- function(object, ...) {
  x <- AIC(object, ...)
  x <- as.numeric(x)
  iter <- 1:length(x)
  b <- loess(x ~ iter, span = 0.2)
  p <- predict(b)
  wm <- which.min(p)
  return(c("mstop" = wm))
}

