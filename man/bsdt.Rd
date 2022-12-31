\name{bsdt}
\alias{bsdt}
\alias{variables}
\alias{predict.bsdt}
\alias{plot.bsdt_variables}

\title{
  Boosting Soft Distributional Trees
}

\description{
  The function uses a type of likelihood-based boosting algorithm with batch optimization.
  In each iteration, a small soft tree is fitted and added to the predictor, with a control
  parameter controlling the strength of an update, similar to classical gradient boosting.
  Each soft tree uses only one variable for the split. Because of the batch optimization,
  the algorithm is suitable for use with very large data sets.
}

\usage{
## Model fitting function.
bsdt(..., k = 4, maxit = 100, batch_ids = NULL,
  nu = 0.1, lambda = 0.0001, omin = 50, faster = TRUE,
  plot = TRUE, verbose = TRUE, flush = TRUE, eps_logLik = Inf)

## Predict method.
\method{predict}{bsdt}(object, newdata, model = NULL,
  type = c("link", "parameter"), ...)

## Selection frequencies of selected variables,
variables(x)

## Plotting method.
\method{plot}{bsdt_variables}(x, legend = TRUE, ...)
}

\arguments{
  \item{\dots}{arguments passed to the model frame function, i.e., \code{formula},
    \code{data} and \code{family}. For the plotting method, further arguments passed for plotting.}
  \item{k}{integer, the size/depth of the soft trees.}
  \item{maxit}{if argument \code{batch_ids} is not supplied, the number of iterations
    the algorithm should run. In this case \code{maxit} batches will be drawn using
    65\% of the data each.}
  \item{batch_ids}{a list of indices that should be used for each batch, see the examples.}
  \item{nu}{the step length control parameter.}
  \item{lambda}{a shrinkage parameter that can be applied for the inner weights of the soft tree.}
  \item{omin}{the minimum number of observations used controlling one soft split.}
  \item{faster}{logical, if set to \code{faster = TRUE}, instead a complete search for the next
    split variable based on the log-likelihood, a faster serach using the current working response
    is used.}
  \item{plot}{logical, should additional information be plotted during estimation.}
  \item{verbose}{logical, should additional information be printed?}
  \item{flush}{logical, if interactive, then additional information will be printed in one line.}
  \item{eps_logLik}{numeric, amount of which the log-likelihood may change during updating. The
    argument is useful when on some batches numerical instabilities occur.}
  \item{object}{an object of class \code{"srt"}, \code{"srf"} or \code{"srtboost"}.}
  \item{newdata}{a data frame or list containing the values of the model
    covariates at which predictions are required.}
  \item{model}{character or integer, specifies the model for which predictions should be computed.}
  \item{type}{character, if \code{type = "link"} the predictor of the corresponding \code{model}
    is returned. If \code{type = "parameter"} predictions on the distributional parameter scale
    are returned.}
  \item{x}{an object fitted by \code{bsdt()}. For the plotting method for selected variables an
    object returned by function \code{variables()}.}
  \item{legend}{logical, should a legend be plotted?}
}

\value{
  A list of class \code{"bsdt"}
}

\references{
  Umlauf N, Klein N (2022). Distributional Adaptive Soft Regression Trees.
  \doi{doi:10.48550/ARXIV.2210.10389}
}

\seealso{
  \code{\link{srt}}, \code{\link{predict.srt}}
}

\examples{
\dontrun{## Generate some data.
set.seed(123)

n <- 1000
d <- data.frame("x" = runif(n, -pi, 2 * pi))
d$z <- as.factor(d$x > 0)
d$y <- 10 + sin(d$x) + c(0, 2)[d$z] + rnorm(n, sd = exp(-2 + cos(d$x)))

## Model formula.
f <- list(y ~ x + z, ~ x + z)

## List of batch indices.
batch_ids <- lapply(1:100, function(i) sample(nrow(d), replace = TRUE))

## Estimate model.
b <- bsdt(f, data = d, family = NO,
  batch_ids = batch_ids, k = 4, nu = 0.1,
  faster = TRUE)

## Predict parameters.
p <- predict(b, type = "parameter")

## Plot data and predicted quantiles.
plot(y ~ x, data = d)

qp <- NULL
for(j in c(0.025, 0.5, 0.975))
  qp <- cbind(qp, family(b)$q(j, p))

i <- order(d$x)
matplot(d$x[i], qp[i, ], type = "l",
  lty = c(2, 1, 2), col = 4, add = TRUE, lwd = 2)
}
}

\keyword{models}
\keyword{regression}

