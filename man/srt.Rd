\name{srt}
\alias{srt}
\alias{plot.srt}

\title{
  Soft Distributional Regression Trees and Forests
}

\description{
  With this function soft regression trees and forests can be estimated.
}

\usage{
srt(formula, family = NULL, data = NULL, weights = NULL,
  subset = NULL, offset = NULL, contrasts = NULL,
  classic = FALSE, ntrees = 1, prob = 0.63, cores = 1, k = 10,
  lambda = 0.1, aic = TRUE, maxs = Inf, plot = TRUE,
  verbose = TRUE, model = TRUE, x = FALSE, y = FALSE,
  scale.x = FALSE, method = c("adaptive", "full"), ...)

\method{plot}{srt}(x,
  which = c("criterion", "hist-resid", "qq-resid", "response-fitted"),
  spar = TRUE, ...)
}

\arguments{
  \item{formula}{list, a list of formulae, one formula for each parameter of the distribution,
    see the example.}
  \item{family}{a family object, can be either a family object of the
    \pkg{gamlss.dist} package, or from the \pkg{bamlss} package.}
  \item{data}{a data frame.}
  \item{weights}{not yet supported.}
  \item{subset}{not yet supported.}
  \item{offset}{not yet supported.}
  \item{contrasts}{an optional list. See the \code{contrasts.arg} of \code{model.matrix.default}.}
  \item{classic}{logical, should classic decision tree split covariate selection be applied?
    In this case the algorithm applies an independence test for selection using the \pkg{coin}
    package. If \code{classic = FALSE}, the algorithm uses all covariates under the sigmoid
    function for growing a smooth tree.}
  \item{ntrees}{integer, how many trees should be estimated?}
  \item{prob}{numeric in \code{(0, 1)}, the fraction that should be drawn randomly to estimate
    one tree. Note if \code{ntrees = 1} no sampling is applied.}
  \item{cores}{how many cores should be used for estimating the \code{ntrees}.}
  \item{k}{integer, the maximum size of the tree. Note that this is suppressed if \code{aic = TRUE}.}
  \item{lambda}{numeric, a penalty parameter that controls the overall smoothness of the tree.}
  \item{aic}{If \code{aic = TRUE}, then the maximum size of the tree will be selected by
    the AIC. Note that an extra argument \code{K} can be supplied, which controls the parmeter
    penalty in the AIC. The default is \code{K = log(nobs)}, which represents the BIC for
    selection of the tree size.}
  \item{maxs}{integer, the maximum allowed size of the tree, also overwrites any
    switches by AIC internally.}
  \item{plot}{logical, should additional information be plotted during estimation. Only available
    if \code{ntrees = 1}}
  \item{verbose}{logical, should additional information be printed?}
  \item{model}{logical, should the mode frame be part of the return value?}
  \item{x}{logical, should the mode matrix be part of the return value?}
  \item{y}{logical, should the response be part of the return value?}
  \item{scale.x}{logical, should the input data be scaled?}
  \item{method}{\code{method = "adaptive"} uses the plain adaptive version
    of the softtree, \code{method = "full"} estimates the softtree using the
    full resulting design matrix including a shrinkage penalty.}
  \item{which}{character, which plot should be created?}
  \item{spar}{logical, should graphical parameters be set by the plotting function?}
  \item{\dots}{arguments to be passed to \code{model.frame} and \code{model.matrix}.}
}

\value{
  A list of class \code{"srt"} if \code{ntrees = 1} or of class \code{"srf"}.
}

\seealso{
  \code{\link{srtboost}}, \code{\link{predict.srt}}
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

## Diagnostic plots.
plot(b)

## Plot estimated functions.
pmu <- predict(b, model = "mu")
psigma <- predict(b, model = "sigma")

par(mfrow = c(1, 2))
plot(x, y, main = expression(mu))
lines(pmu ~ x, col = 4, lwd = 2)
lines(sin(x) ~ x, col = 2, lwd = 2)
legend("topleft", c("truth", "estimate"), col = c(2, 4), lwd = 2, bty = "n")
plot(psigma ~ x, col = 4, lwd = 2, main = expression(log(sigma)), type = "l")
lines(I(-2 + cos(x)) ~ x, col = 2, lwd = 2)
}
}

\keyword{models}
\keyword{regression}

