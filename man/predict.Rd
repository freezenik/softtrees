\name{predict.srt}
\alias{predict.srt}
\alias{predict.srf}
\alias{predict.srtboost}

\title{
  Prediction Methods for Soft Distributional Regression Trees
}

\description{
  Takes a fitted object as returned from function \code{\link{srt}} or \code{\link{srtboost}}
  and computes predictions for the parameters of the modeled distribution.
}

\usage{
## Single soft distributional tree.
\method{predict}{srt}(object, newdata, model = NULL,
  type = c("link", "parameter"), k = NULL, ...)

## Soft distributional forests.
\method{predict}{srf}(object, newdata, model = NULL,
  type = c("link", "parameter"), FUN = mean, ...)

## Boosted distributional tree.
\method{predict}{srtboost}(object, newdata, model = NULL,
  type = c("link", "parameter"), mstop = NULL, matrix = FALSE, ...)
}

\arguments{
  \item{object}{an object of class \code{"srt"}, \code{"srf"} or \code{"srtboost"}.}
  \item{newdata}{a data frame or list containing the values of the model
    covariates at which predictions are required.}
  \item{model}{character or integer, specifies the model for which predictions should be computed.}
  \item{type}{character, if \code{type = "link"} the predictor of the corresponding \code{model}
    is returned. If \code{type = "parameter"} predictions on the distributional parameter scale
    are returned.}
  \item{k}{integer, for a single tree argument \code{k} specifies the maximum size of the
    tree to be used for prediction.}
  \item{FUN}{a function that can be used to aggregete the results of a fitted
    soft distributional regression forest.}
  \item{mstop}{integer, the stopping iteration of the boosted tree that should be
    used for prediction.}
  \item{matrix}{logical, should all single predicted values for all boosting iterations be
    returned as a matrix?.}
  \item{\dots}{arguments passed \code{FUN}.}
}

\value{
  Depending on arguments \code{model}, \code{FUN}, a list of predictions or simple vectors
  or matrices of predictions.
}

\seealso{
  \code{link{srt}}, \code{link{srtboost}}.
}

\examples{
\dontrun{## Generate some data.
set.seed(123)
d <- Crazy()

## Model formula.
f <- list(y ~ x, ~ 1)

## Estimate srt forest.
b <- srt(f, data = d, family = Gaussian, K = 2, lambda = 0.0001, ntrees = 40)

## Predict on parameter scale.
p <- predict(b, type = "parameter")
print(head(p))

## Now for single parameters on the predictor scale
## and for each tree.
pmu <- predict(b, model = "mu", FUN = identity)
psigma <- predict(b, model = "sigma", FUN = identity)
print(head(psigma))

## Plot.
plot(y ~ x, data = d, ylim = range(c(pmu, d$y)))
i <- order(d$x)
matplot(d$x[i], pmu[i, ], type = "l", lty = 1,
  col = rgb(0, 0, 1, alpha = 0.3), lwd = 2, add = TRUE)
lines(rowMeans(pmu)[i] ~ d$x[i], col = 2, lwd = 2)
}
}

\keyword{regression}
\keyword{models}

