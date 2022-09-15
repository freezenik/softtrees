## Second make.link function.
make.link2 <- function(link)
{
  if(is.null(link))
    link <- "identity"
  link0 <- link
  if(link0 == "tanhalf"){
    rval <- list(
      "linkfun" = function (mu) {
        tan(mu/2)},
      "linkinv" = function(eta) {
        2 * atan(eta)},
      "mu.eta" = function(eta) {
        2 / (eta^2 + 1)},
      "mu.eta2" = function(eta) {
        (-4 * eta ) / (eta^2 + 1)^2},
      "valideta" = function(eta) TRUE,
      "name" = "tanhalf"
      )
  } else {
    mu.eta2 <- function(x) {
      if(link0 == "identity") {
        x$mu.eta2 <- function(eta) rep.int(0, length(eta))
        return(x)
      }
      if(link0 == "log") {
        x$mu.eta2 <- function(eta) exp(eta)
        return(x)
      }
      if(link0 == "logit") {
        x$mu.eta2 <- function(eta) {
          eta <- exp(eta)
          return(-eta * (eta - 1) / (eta + 1)^3)
        }
        return(x)
      }
      if(link0 == "probit") {
        x$mu.eta2 <- function(eta) {
          -eta * dnorm(eta, mean = 0, sd = 1)
        }
        return(x)
      }
      if(link0 == "inverse") {
        x$mu.eta2 <- function(eta) {
          2 / (eta^3)
        }
        return(x)
      }
      if(link0 == "1/mu^2") {
        x$mu.eta2 <- function(eta) {
          0.75 / eta^(2.5)
        }
        return(x)
      }
      if(link0 == "sqrt") {
        x$mu.eta2 <- function(eta) { rep(2, length = length(eta)) }
        return(x)
      }
      x$mu.eta2 <- function(eta) rep.int(0, length(eta))
      ## warning(paste('higher derivatives of link "', link, '" not available!', sep = ''))
      return(x)
    }

    if(link %in% c("logit", "probit", "cauchit", "cloglog", "identity",
                   "log", "sqrt", "1/mu^2", "inverse")) {
      rval <- make.link(link)
    } else {
      rval <- switch(link,
        "rhogit" = list(
          "linkfun" = function(mu) { mu / sqrt(1 - mu^2) },
          "linkinv" = function(eta) {
              rval <- eta / sqrt(1 + eta^2)
              rval <- (abs(rval) - .Machine$double.eps) * sign(rval)
              rval
          },
          "mu.eta" = function(eta) { 1 / (1 + eta^2)^1.5 }
        ),
        "cloglog2" = list(
          "linkfun" = function(mu) { log(-log(mu)) },
          "linkinv" = function(eta) {
            pmax(pmin(1 - expm1(-exp(eta)), .Machine$double.eps), .Machine$double.eps)
          },
          "mu.eta" = function(eta) {
            eta <- pmin(eta, 700)
            pmax(-exp(eta) * exp(-exp(eta)), .Machine$double.eps)
          }
        ),
        "sigmoid" = list(
          "linkfun" = function(mu) {
            i <- mu <= -1
            if(any(i))
              mu[i] <- mu[i] <- -0.9999
            i <- mu >= 1
            if(any(i))
              mu[i] <- mu[i] <- 0.9999 
            -log(2/(mu + 1) - 1)
          },
          "linkinv" = function(eta) {
            tanh(eta/2)
          },
          "mu.eta" = function(eta) {
            0.5 / cosh(eta * 0.5)^2
          },
          "mu.eta2" = function(eta) {
            eta2 <- eta * 0.5
            -(0.5 * (2 * (sinh(eta2) * 0.5 * cosh(eta2)))/(cosh(eta2)^2)^2)
          }
        )
      )
    }

    rval <- mu.eta2(rval)
  }
  rval$name <- link
  rval
}

parse_links <- function(links, default.links, ...)
{
  dots <- list(...)
  nl <- names(default.links)
  if(length(dots))
    links <- as.character(dots)
  if(is.null(names(links)))
    names(links) <- rep(nl, length.out = length(links))
  links <- as.list(links)
  for(j in nl) {
    if(is.null(links[[j]]))
      links[[j]] <- default.links[j]
  }
  links <- links[nl]
  links <- as.character(links)
  names(links) <- nl
  links
}



tF <- function(x, ...)
{
  if(is.function(x))
    x <- x()
  if(!inherits(x, "gamlss.family"))
    stop('only "gamlss.family" objects can be transformed!')

  args <- list(...)
  bd <- if(is.null(args$bd)) 1 else args$bd
  args$bd <- NULL
  pr <- args$range
  check_range <- function(par) {
    for(j in names(par)) {
      if(!is.null(pr[[j]])) {
        par[[j]][par[[j]] < min(pr[[j]])] <- min(pr[[j]])
        par[[j]][par[[j]] > max(pr[[j]])] <- max(pr[[j]])
      }
    }
    par
  }
  nx <- names(x$parameters)
  score <- hess <- initialize <- list()

  make_call <- function(fun) {
    fn <- deparse(substitute(fun), backtick = TRUE, width.cutoff = 500)
    nf <- names(formals(fun))
    if(length(nf) < 1) {
      call <- paste(fn, "()", sep = "")
    } else {
      call <- paste(fn, "(", if("y" %in% nf) "y," else "", sep = "")
      np <- nx[nx %in% nf]
      if(!length(np)) {
        call <- paste(fn, "(y", sep = "")
      } else {
        call <- paste(call, paste(np, '=', 'par$', np, sep = '', collapse = ','), sep = "")
      }
      if("bd" %in% nf) {
        call <- paste(call, ",bd=", bd, sep = "")
      }
    }
    call <- parse(text = paste(call, ")", sep = ""))
    return(call)
  }

  if("mu" %in% nx) {
    mu.link <- make.link2(x$mu.link)
    mu.cs <- make_call(x$dldm)
    mu.hs <- make_call(x$d2ldm2)
    score$mu  <- function(y, par, ...) {
      par <- check_range(par)
      res <- eval(mu.cs) * mu.link$mu.eta(mu.link$linkfun(par$mu))
      if(!is.null(dim(res)))
        res <- res[, 1]
      res
    }
    hess$mu <- function(y, par, ...) {
      par <- check_range(par)
      score <- eval(mu.cs)
      hess <- -1 * eval(mu.hs)
      eta <- mu.link$linkfun(par$mu)
      res <- drop(score * mu.link$mu.eta2(eta) + hess * mu.link$mu.eta(eta)^2)
      if(!is.null(dim(res)))
        res <- res[, 1]
      res
    }
    if(!is.null(x$mu.initial)) {
      initialize$mu <- function(y, ...) {
        if(!is.null(attr(y, "contrasts"))) {
          if(!is.null(dim(y)))
            y <- y[, ncol(y)]
        }
        if(!is.null(bd))
          bd <- rep(bd, length.out = if(!is.null(dim(y))) nrow(y) else length(y))
        res <- eval(x$mu.initial)
        if(!is.null(dim(res)))
          res <- res[, 1]
        res
      }
    }
  }

  if("sigma" %in% nx) {
    sigma.link <- make.link2(x$sigma.link)
    sigma.cs <- make_call(x$dldd)
    sigma.hs <- make_call(x$d2ldd2)
    score$sigma  <- function(y, par, ...) {
      par <- check_range(par)
      res <- eval(sigma.cs) * sigma.link$mu.eta(sigma.link$linkfun(par$sigma))
      if(!is.null(dim(res)))
        res <- res[, 1]
      res
    }
    hess$sigma <- function(y, par, ...) {
      par <- check_range(par)
      score <- eval(sigma.cs)
      hess <- -1 * eval(sigma.hs)
      eta <- sigma.link$linkfun(par$sigma)
      res <- drop(score * sigma.link$mu.eta2(eta) + hess * sigma.link$mu.eta(eta)^2)
      if(!is.null(dim(res)))
        res <- res[, 1]
      res
    }
    if(!is.null(x$sigma.initial)) {
      initialize$sigma <- function(y, ...) {
        if(!is.null(bd))
          bd <- rep(bd, length.out = if(!is.null(dim(y))) nrow(y) else length(y))
        res <- eval(x$sigma.initial)
        if(!is.null(dim(res)))
          res <- res[, 1]
        res
      }
    }
  }

  if("nu" %in% nx) {
    nu.link <- make.link2(x$nu.link)
    nu.cs <- make_call(x$dldv)
    nu.hs <- make_call(x$d2ldv2)
    score$nu  <- function(y, par, ...) {
      par <- check_range(par)
      res <- eval(nu.cs) * nu.link$mu.eta(nu.link$linkfun(par$nu))
      if(!is.null(dim(res)))
        res <- res[, 1]
      res
    }
    hess$nu <- function(y, par, ...) {
      par <- check_range(par)
      score <- eval(nu.cs)
      hess <- -1 * eval(nu.hs)
      eta <- nu.link$linkfun(par$nu)
      res <- drop(score * nu.link$mu.eta2(eta) + hess * nu.link$mu.eta(eta)^2)
      if(!is.null(dim(res)))
        res <- res[, 1]
      res
    }
    if(!is.null(x$nu.initial)) {
      initialize$nu <- function(y, ...) {
        if(!is.null(bd))
          bd <- rep(bd, length.out = if(!is.null(dim(y))) nrow(y) else length(y))
        res <- eval(x$nu.initial)
        if(!is.null(dim(res)))
          res <- res[, 1]
        res
      }
    }
  }

  if("tau" %in% nx) {
    tau.link <- make.link2(x$tau.link)
    tau.cs <- make_call(x$dldt)
    tau.hs <- make_call(x$d2ldt2)
    score$tau  <- function(y, par, ...) {
      par <- check_range(par)
      res <- eval(tau.cs) * tau.link$mu.eta(tau.link$linkfun(par$tau))
      if(!is.null(dim(res)))
        res <- res[, 1]
      res
    }
    hess$tau <- function(y, par, ...) {
      par <- check_range(par)
      score <- eval(tau.cs)
      hess <- -1 * eval(tau.hs)
      eta <- tau.link$linkfun(par$tau)
      res <- drop(score * tau.link$mu.eta2(eta) + hess * tau.link$mu.eta(eta)^2)
      if(!is.null(dim(res)))
        res <- res[, 1]
      res
    }
    if(!is.null(x$tau.initial)) {
      initialize$tau <- function(y, ...) {
        if(!is.null(bd))
          bd <- rep(bd, length.out = if(!is.null(dim(y))) nrow(y) else length(y))
        res <- eval(x$tau.initial)
        if(!is.null(dim(res)))
          res <- res[, 1]
        res
      }
    }
  }

  dfun <- get(paste("d", x$family[1], sep = ""))
  pfun <- try(get(paste("p", x$family[1], sep = "")), silent = TRUE)
  qfun <- try(get(paste("q", x$family[1], sep = "")), silent = TRUE)
  rfun <- try(get(paste("r", x$family[1], sep = "")), silent = TRUE)

  nf <- names(formals(dfun))
  bdc <- "bd" %in% nf

  dc <- parse(text = paste('dfun(y,', paste(paste(nx, 'par$', sep = "="),
    nx, sep = '', collapse = ','), ',log=log,...',
    if(bdc) paste0(",bd=", bd) else NULL, ")", sep = ""))
  pc <- parse(text = paste('pfun(q,', paste(paste(nx, 'par$', sep = "="),
    nx, sep = '', collapse = ','), ',log=log,...',
    if(bdc) paste0(",bd=", bd) else NULL, ")", sep = ""))
  qc <- parse(text = paste('qfun(p,', paste(paste(nx, 'par$', sep = "="),
    nx, sep = '', collapse = ','), ',log=log,...',
    if(bdc) paste0(",bd=", bd) else NULL, ")", sep = ""))
  rc <- parse(text = paste('rfun(n,', paste(paste(nx, 'par$', sep = "="),
    nx, sep = '', collapse = ','), ',...',
    if(bdc) paste0(",bd=", bd) else NULL, ")", sep = ""))

  rval <- list(
    "family" = x$family[1],
    "names" = nx,
    "links" = unlist(x[paste(nx, "link", sep = ".")]),
    "score" = score,
    "hess" = hess,
    "d" = function(y, par, log = FALSE, ...) {
       par <- check_range(par)
       eval(dc)
    },
    "p" = if(!inherits(pfun, "try-error")) function(q, par, log = FALSE, ...) {
      par <- check_range(par)
      eval(pc)
    } else NULL,
    "q" = if(!inherits(qfun, "try-error")) function(p, par, log = FALSE, ...) {
      par <- check_range(par)
      eval(qc)
    } else NULL,
    "r" = if(!inherits(rfun, "try-error")) function(n, par, ...) {
      par <- check_range(par)
      eval(rc)
    } else NULL
  )
  names(rval$links) <- nx
  rval$valid.response <- x$y.valid
  rval$initialize <- initialize
  rval$type <- tolower(x$type)

  if(!is.null(x$mean)) {
    meanc <- make_call(x$mean)
    rval$mean  <- function(par, ...) {
      par <- check_range(par)
      res <- eval(meanc)
      if(!is.null(dim(res)))
        res <- res[, 1]
      res
    }
  }

  if(!is.null(x$variance)) {
    varc <- make_call(x$variance)
    rval$variance  <- function(par, ...) {
      par <- check_range(par)
      res <- eval(varc)
      if(!is.null(dim(res)))
        res <- res[, 1]
      res
    }
  }

  class(rval) <- "family.bamlss"
  rval
}


complete_family <- function(family)
{
  if(is.null(names(family$links)))
    names(family$links) <- family$names

  linkinv <- linkfun <- mu.eta <- list()
  for(j in family$names) {
    link <- make.link2(family$links[j])
    linkinv[[j]] <- link$linkinv
    linkfun[[j]] <- link$linkfun
    mu.eta[[j]] <- link$mu.eta
  }

  if(is.null(family$map2par)) {
    family$map2par <- function(eta) {
      for(j in family$names) {
        eta[[j]] <- linkinv[[j]](eta[[j]])
        eta[[j]][is.na(eta[[j]])] <- 0
        if(any(jj <- eta[[j]] == Inf))
          eta[[j]][jj] <- 10
        if(any(jj <- eta[[j]] == -Inf))
          eta[[j]][jj] <- -10
      }
      return(eta)
    }
  }

  if(is.null(family$mu.eta)) {
    family$mu.eta <- mu.eta
  }

  if(is.null(family$mu)) {
    family$mu <- function(par) { par[[1]] }
  }

  if(is.null(family$mean)) {
    family$mean <- function(par) { par[[1]] }
  }

  if(is.null(family$loglik)) {
    if(!is.null(family$d)) {
      family$loglik <- function(y, par, ...) {
        logdens <- family$d(y, par, log = TRUE)
        if(any(i <- !is.finite(logdens))) {
          logdens[i] <- -100
        }
        return(sum(logdens, na.rm = TRUE))
      }
    }
  }

  err01 <- .Machine$double.eps^(1/3)
  err02 <- err01 * 2
  err11 <- .Machine$double.eps^(1/4)
  err12 <- err11 * 2

  if(is.null(family$score) & !is.null(family$d))
    family$score <- list()
  for(i in family$names) {
    if(is.null(family$score[[i]]) & !is.null(family$d)) {
      fun <- c(
        "function(y, par, ...) {",
        paste("  eta <- linkfun[['", i, "']](par[['", i, "']]);", sep = ""),
        paste("  par[['", i, "']] <- linkinv[['", i, "']](eta + err01);", sep = ""),
        "  d1 <- family$d(y, par, log = TRUE);",
        paste("  par[['", i, "']] <- linkinv[['", i, "']](eta - err01);", sep = ""),
        "  d2 <- family$d(y, par, log = TRUE);",
        "  return((d1 - d2) / err02)",
        "}"
      )
      family$score[[i]] <- eval(parse(text = paste(fun, collapse = "")))
      attr(family$score[[i]], "dnum") <- TRUE
    }
  }

  if(is.null(family$hess) & !is.null(family$d))
    family$hess <- list()
  for(i in family$names) {
    if(is.null(family$hess[[i]]) & !is.null(family$d)) {
      fun <- if(!is.null(attr(family$score[[i]], "dnum"))) {
        c(
          "function(y, par, ...) {",
          paste("  eta <- linkfun[['", i, "']](par[['", i, "']]);", sep = ""),
          paste("  par[['", i, "']] <- linkinv[['", i, "']](eta + err11);", sep = ""),
          paste("  d1 <- family$score[['", i, "']](y, par, ...);", sep = ""),
          paste("  par[['", i, "']] <- linkinv[['", i, "']](eta - err11);", sep = ""),
          paste("  d2 <- family$score[['", i, "']](y, par, ...);", sep = ""),
          "  return(-1 * (d1 - d2) / err12)",
          "}"
        )
      } else {
        c(
          "function(y, par, ...) {",
          paste("  eta <- linkfun[['", i, "']](par[['", i, "']]);", sep = ""),
          paste("  par[['", i, "']] <- linkinv[['", i, "']](eta + err01);", sep = ""),
          paste("  d1 <- family$score[['", i, "']](y, par, ...);", sep = ""),
          paste("  par[['", i, "']] <- linkinv[['", i, "']](eta - err01);", sep = ""),
          paste("  d2 <- family$score[['", i, "']](y, par, ...);", sep = ""),
          "  return(-1 * (d1 - d2) / err02)",
          "}"
        )
      }
      family$hess[[i]] <- eval(parse(text = paste(fun, collapse = "")))
    }
  }

  return(family)
}


print.srt_family <- function(x, full = TRUE, ...)
{
  cat("Family:", x$family, if(!is.null(x$full.name)) paste0("(", x$full.name, ")") else NULL,  "\n")
  links <- paste(names(x$links), x$links, sep = " = ")
  links <- paste(links, collapse = ", ")
  cat(if(length(links) > 1) "Link functions:" else "Link function:", links, sep = " ")
  cat("\n")
  if(full) {
    nfun <- names(x[c("transform", "optimizer", "sampler", "results", "predict")])
    if(!all(is.na(nfun))) {
      nfun <- nfun[!is.na(nfun)]
      cat("---\nFamily specific functions:\n")
      for(j in nfun)
        cat(" ..$ ", j, "\n", sep = "")
    }
    nfun <- names(x[c("score", "hess")])
    if(!all(is.na(nfun))) {
      nfun <- nfun[!is.na(nfun)]
      cat("---\nDerivative functions:\n")
      for(j in nfun) {
        cat(" ..$ ", j, "\n", sep = "")
        for(i in names(x[[j]]))
          cat(" .. ..$ ", i, "\n", sep = "")
      }
    }
  }
}


Gaussian <- function(...)
{
  links <- c(mu = "identity", sigma = "log")

  rval <- list(
    "family" = "gaussian",
    "names" = c("mu", "sigma"),
    "links" = parse_links(links, c(mu = "identity", sigma = "log"), ...),
    "score" = list(
      "mu" = function(y, par, ...) { drop((y - par$mu) / (par$sigma^2)) },
      "sigma" = function(y, par, ...) { drop(-1 + (y - par$mu)^2 / (par$sigma^2)) }
    ),
    "hess" = list(
      "mu" = function(y, par, ...) { drop(1 / (par$sigma^2)) },
      "sigma" = function(y, par, ...) { rep(2, length(y)) }
    ),
    "loglik" = function(y, par, ...) {
      sum(dnorm(y, par$mu, par$sigma, log = TRUE))
    },
    "mu" = function(par, ...) {
      par$mu
    },
    "d" = function(y, par, log = FALSE) {
      dnorm(y, mean = par$mu, sd = par$sigma, log = log)
    },
    "p" = function(y, par, ...) {
      pnorm(y, mean = par$mu, sd = par$sigma, ...)
    },
    "r" = function(n, par) {
      rnorm(n, mean = par$mu, sd = par$sigma)
    },
    "q" = function(p, par) {
      qnorm(p, mean = par$mu, sd = par$sigma)
    },
    "crps" = function(y, par, ...) {
      sum(scoringRules::crps_norm(y, mean = par$mu, sd = par$sigma), na.rm = TRUE)
    },
    "initialize" = list(
      "mu"    = function(y, ...) { (y + mean(y)) / 2 },
      "sigma" = function(y, ...) { rep(sd(y), length(y)) }
    ),
    "mean"      = function(par) par$mu,
    "variance"  = function(par) par$sigma^2,
    "valid.response" = function(x) {
      if(is.factor(x) | is.character(x))
        stop("the response should be numeric!")
      return(TRUE)
    }
  )

  class(rval) <- "srt_family"
  rval
}


Gaussian0 <- function(...)
{
  rval <- list(
    "family" = "Gaussian0",
    "names" = "mu",
    "links" = c(mu = "identity"),
    "score" = list(
      "mu" = function(y, par, ...) { y - par$mu }
    ),
    "hess" = list(
      "mu" = function(y, par, ...) { rep(1, length(par$mu)) }
    ),
    "loglik" = function(y, par, ...) {
      sum(dnorm(y, par$mu, 1, log = TRUE))
    },
    "d" = function(y, par, log = FALSE, ...) {
      dnorm(y, par$mu, 1, log = log)
    },
    "p" = function(y, par, ...) {
      pnorm(y, mean = par$mu, sd = 1, ...)
    },
    "initialize" = list(
      "mu" = function(y, ...) { (y + mean(y)) / 2 }
    ),
    "valid.response" = function(x) {
      if(is.factor(x) | is.character(x))
        stop("the response should be numeric!")
      return(TRUE)
    }
  )

  rval$map2par <- function(eta) eta

  class(rval) <- "srt_family"
  rval
}


Weibull <- function(...)
{
## delta * ((y - mu) / exp(sigma) - exp(sigma) - exp( (y - mu) / exp(sigma) ))
## (1 - delta) * -exp((y - mu) / exp(sigma))
  rval <- list(
    "family" = "Weibull",
    "names" = c("mu", "sigma"),
    "links" = c(mu = "identity", sigma = "log"),
    "d" = function(y, par, log = FALSE, ...) {
      delta <- y[, "status"]
      y <- log(y[, "time"])
      yms <- (y - par$mu) / par$sigma
      fy <- delta * (yms - par$sigma - exp(yms))
      Sy <- (1 - delta) * -exp(yms)
      d <- fy + Sy
      if(!log)
        d <- exp(d)
      return(d)
    },
    "p" = function(y, par, ...) {
      delta <- y[, "status"]
      y <- log(y[, "time"])
      p1 <- 1 - exp(-exp((y - par$mu) / par$sigma))
      p2 <- runif(length(y), p1, 1)
      prob <- ifelse(delta > 0, p1, p2)
      return(prob)
    },
    "q" = function(p, par, ...) {
      lambda <- exp(-par$mu/par$sigma)
      alpha <- 1 / par$sigma
      q <- lambda * (-log(1 - p))^(1 / alpha)
      return(q)
    },
    "score" = list(
      "mu" = function(y, par, ...) {
        delta <- y[, "status"]
        y <- log(y[, "time"])
        eyms <- exp((y - par$mu)/par$sigma)
        s1 <- 1/par$sigma
        eymss1 <- eyms * s1
        a <- -(delta * (s1 - eymss1))
        b <- (1 - delta) * (eymss1)
        return(a + b)
      },
      "sigma" = function(y, par, ...) {
        delta <- y[, "status"]
        y <- log(y[, "time"])
        yms <- (y - par$mu)/par$sigma
        eyms <- exp(yms)
        eyms2 <-  eyms * yms
        a <- -(delta * (yms + par$sigma - eyms2))
        b <- (1 - delta) * eyms2
        return(a + b)
      }
    ),
    "hess" <- list(
      "mu" = function(y, par, ...) {
        delta <- y[, "status"]
        y <- log(y[, "time"])
        eyms <- exp((y - par$mu)/par$sigma) * 1/par$sigma^2
        a <- -(delta * eyms)
        b <- -((1 - delta) * eyms)
        return(-(a + b))
      },
      "sigma" = function(y, par, ...) {
        delta <- y[, "status"]
        y <- log(y[, "time"])
        yms <- (y - par$mu)/par$sigma
        eyms <- exp(yms)
        a <- -(delta * (-yms + par$sigma - (eyms * (-yms - eyms * yms^2))))
        b <- (1 - delta) * (eyms * -yms - eyms * yms^2)
        return(-(a + b))
      }
    ),
    "valid.response" = function(x) {
      if(!inherits(x, "Surv"))
        stop("the response should be a survival object!")
      return(TRUE)
    }
  )

  class(rval) <- "family.bamlss"

  rval
}

