\name{residuals.srt}
\alias{residuals.srt}
\alias{residuals.srf}
\alias{residuals.srtboost}
\alias{plot.srt_residuals}

\title{
  Compute Soft Distributional Regression Tree Residuals
}

\description{
  Function to compute quantile and response residuals.
}

\usage{
## Single soft distributional tree.
\method{residuals}{srt}(object, type = c("quantile", "response"), ...)

## Soft distributional forests.
\method{residuals}{srf}(object, type = c("quantile", "response"), ...)

## Boosted distributional tree.
\method{residuals}{srtboost}(object, type = c("quantile", "response"), ...)

## A plotting method.
\method{plot}{srt_residuals}(x, which = c("hist-resid", "qq-resid"),
  spar = TRUE, ...)
}

\arguments{
  \item{object}{an object of class \code{"srt"}, \code{"srf"} or \code{"srtboost"}.}
  \item{type}{character, the type of residuals wanted, possible types are
    \code{"quantile"} residuals and \code{"response"} residuals.}
  \item{x}{object returned from function \code{srt()}, or \code{srtboost}.}
  \item{which}{character or integer, should a histogram with kernel density estimates
    be plotted or a qq-plot?}
  \item{spar}{logical, should graphical parameters be set by the plotting function?}
  \item{\dots}{for method \code{residuals()} arguments passed to possible
    \code{$residuals()} functions that may be part of \code{\link{family.stree}}. For function
    \code{plot.srt_residuals()} arguments passed to function
    \code{\link[graphics]{hist.default}} and \code{\link[stats]{qqnorm.default}}.}
}

\details{
  Response residuals are the raw residuals, i.e., the response data minus the fitted distributional
  mean. If the \code{\link{family.stree}} object contains a function \code{$mu(par, \dots)}, then
  raw residuals are computed with \code{y - mu(par)} where \code{par} is the named list of fitted
  values of distributional parameters. If \code{$mu(par, ...)} is missing, then the fitted values
  of the first distributional parameter are used.

  Randomized quantile residuals are based on the cumulative distribution function of the
  \code{\link{family.stree}} object, i.e., the \code{$p(y, par, ...)} function.
}

\value{
  A vector or matrix of residuals.
}

\references{
  Dunn P. K., and Smyth G. K. (1996). Randomized Quantile Residuals.
    \emph{Journal of Computational and Graphical Statistics} \bold{5}, 236-244. 
}

\seealso{
  \code{\link{srt}}, \code{\link{srtboost}}
}

\examples{
\dontrun{## Generate some data.
set.seed(123)
n <- 1000
x <- sort(runif(n, -3, 3))
y <- sin(x) + rnorm(n, sd = exp(-2 + cos(x)))

## Model formula, a list, one formula
## for each parameter.
f <- list(y ~ x, ~ x)

## Estimate softtree using the BIC.
b <- srt(f, family = Gaussian, K = log(n), lambda = 0.1)

## Extract quantile residuals.
e <- residuals(b, type = "quantile")

## Plot.
plot(e)
}
}

\keyword{regression}
\keyword{models}

