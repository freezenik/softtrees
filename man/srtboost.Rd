\name{srtboost}
\alias{srtboost}
\alias{mstop}

\title{
  Boosting Soft Regression Trees
}

\description{
  This function estimates soft distributional regression trees using a boosting type algorithm.
}

\usage{
## The boosted version of soft regression trees.
srtboost(..., nu = 0.1, k = 4, lambda = 1e-05, n.iter = 400,
  index = NULL, K = NULL, classic = FALSE, select = FALSE,
  eps = .Machine$double.eps^0.3, verbose = TRUE,
  plot = TRUE, linear = FALSE)

## Extract the last boosting iteration.
mstop(x)
}

\arguments{
  \item{\dots}{arguments like \code{formula}, \code{data}, see function \code{\link{srt}}.}
  \item{nu}{numeric, the step length control parameter.}
  \item{k}{integer, the maximum size of the sof regression tree that should
    be used in each boosting iteration.}
  \item{lambda}{numeric, a penalty parameter that controls the overall smoothness of the tree.}
  \item{n.iter}{integer, the number of boosting iterations.}
  \item{index}{integer, integer vector slecting the observations that should be used for estimation.}
  \item{K}{not used yet.}
  \item{classic}{logical, should classic decision tree split covariate selection be applied?
    In this case the algorithm applies an independence test for selection using the \pkg{coin}
    package. If \code{classic = FALSE}, the algorithm uses all covariates under the sigmoid
    function for growing a smooth tree.}
  \item{select}{logical, should the best fitting parameter be selected for update only?}
  \item{eps}{numeric, a stopping criterion.}
  \item{verbose}{logical, should additional information be printed?}
  \item{plot}{logical, should additional information be plotted during estimation. Only available
    if \code{ntrees = 1}}
  \item{linear}{logical, should linear effects be estimated prior estimation of the tree.}
  \item{x}{an object returned from \code{srtboost}.}
}

\value{
  A list of class \code{"srtboost"}.
}

\seealso{
  \code{\link{srt}}, \code{\link{predict.srt}}
}

\examples{
\dontrun{## Generate some data.
set.seed(123)
n <- 1000
x <- sort(runif(n, -3, 3))
y <- sin(x) + rnorm(n, sd = exp(-2 + cos(x)))

## Model formula.
f <- list(y ~ x, ~ x)

## Estimate softtree using the BIC.
b <- srtboost(f, family = Gaussian, nu = 0.1, lambda = 0.001)

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

## Now only for mu over all boosting iterations.
par(mfrow = c(1, 1))
plot(x, y, main = expression(mu))
for(i in 1:mstop(b)) {
  pmu <- predict(b, model = "mu", mstop = i)
  lines(pmu ~ x, col = rgb(0.1, 0.1, 0.1, alpha = 0.4), lwd = 2)
}
lines(pmu ~ x, col = 4, lwd = 2)
}
}

\keyword{models}
\keyword{regression}

