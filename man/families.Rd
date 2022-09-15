\name{stree.families}
\alias{family.stree}
\alias{Gaussian}
\alias{Gaussian0}
\alias{Weibull}
\alias{stree_family}

\title{
  Distribution Families in \pkg{softtrees}
}

\description{
  Family objects in \pkg{softtrees} specify the information that is needed for estimation, i.e.,
  the parameter names and corresponding link functions, the
  density function, derivatives of the log-likelihood w.r.t. the predictors, and so
  forth.
}

\usage{
## Family objects in softtrees:
Gaussian(...)
Gaussian0(...)
Weibull(...)
}

\arguments{
  \item{\dots}{arguments passed to functions that are called within the family object.}
}

\details{
  The following lists the minimum requirements on a \pkg{softtrees} family object to be used with
  \code{\link{srt}} and \code{\link{srtboost}}:
  \itemize{
    \item The family object must return a \code{\link{list}} of class \code{"stree_family"}.
    \item The object must contain the family name as a character string. 
    \item The object must contain the names of the parameters as a character string, as well as
      the corresponding link functions as character string.
  }
  It is assumed that the density function in a family object has the following arguments:

  \code{d(y, par, log = FALSE, ...)}

  where argument \code{y} is the response (possibly a matrix) and \code{par} is a named list
  holding the evaluated parameters of the distribution, e.g., using a normal distribution \code{par}
  has two elements, one for the mean \code{par$mu} and one for the standard deviation
  \code{par$sigma}. The dots argument is for passing special internally used objects, depending
  on the type of model this feature is usually not needed.

  The family object holds derivative functions evaluating
  derivatives of the log-likelihood w.r.t. the predictors (or expectations of derivatives).
  For each parameter, these functions also hold the following arguments:
 
  \code{score(y, par, ...)}

  for computing the first derivative of the log-likelihood w.r.t. a predictor and

  \code{hess(y, par, ...)}

  for computing the negative second derivatives. Within the family object these functions
  are organized in a named list, see the examples below.

  In addition, for the cumulative distribution function (\code{p(y, par, ...)}), for the quantile
  function (\code{q(y, par, ...)}) or for creating random numbers (\code{r(n, par, ...)}) the same
  structure is assumed. See, e.g., the code of function \code{Gaussian()}.

  Some model fitting engines can initialize the distributional parameters which oftentimes
  leads to much faster convergence. The initialize functions are again organized within a named list,
  one entry for each parameter, similar to the \code{score} and \code{hess} functions, e.g.,
  see the code of family object \code{Gaussian()}.

  The examples below illustrate this setup.
}

\seealso{
  \code{\link{srt}}, \code{\link{srtboost}}
}

\examples{
\dontrun{## New family object for the normal distribution,
## can be used srt() or srtboost().
Normal <- function(...) {
  f <- list(
    "family" = "Normal",
    "names" = c("mu", "sigma"),
    "links" = c("identity", "log"),
    "d" = function(y, par, log = FALSE) {
      dnorm(y, mean = par$mu, sd = par$sigma, log = log)
    },
    "score" = list(
      "mu" = function(y, par, ...) {
        drop((y - par$mu) / (par$sigma^2))
      },
      "sigma" = function(y, par, ...) {
        drop(-1 + (y - par$mu)^2 / (par$sigma^2))
      }
    ),
    "hess" = list(
      "mu" = function(y, par, ...) {
        drop(1 / (par$sigma^2))
      },
      "sigma" = function(y, par, ...) { 
        rep(2, length(y))
      }
    )
  )
  class(f) <- "srt_family"
  return(f)
}

## Generate some data.
set.seed(123)
n <- 1000
x <- sort(runif(n, -3, 3))
y <- sin(x) + rnorm(n, sd = exp(-2 + cos(x)))

## Model formula.
f <- list(y ~ x, ~ x)

## Estimate softtree using the BIC.
b <- srt(f, family = Normal, K = log(n), lambda = 0.1)

## Plot estimated functions.
pmu <- predict(b, model = "mu")
psigma <- predict(b, model = "sigma")

par(mfrow = c(1, 2))
plot(x, y, main = expression(mu))
lines(pmu ~ x, col = 4, lwd = 2)
lines(sin(x) ~ x, col = 2, lwd = 2)
plot(psigma ~ x, col = 4, lwd = 2, main = expression(log(sigma)), type = "l")
lines(I(-2 + cos(x)) ~ x, col = 2, lwd = 2)
}}

\keyword{regression}
\keyword{models}
\keyword{distribution}

