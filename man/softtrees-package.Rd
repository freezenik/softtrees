\name{softtrees-package}
\alias{softtrees-package}
\alias{softtrees}
\docType{package}

\title{
  \packageTitle{softtrees}
}

\description{
  \packageDescription{softtrees}
}

\details{
  Estimation of soft distributional regression trees is performed by function \code{\link{srt}},
  the boosting type algorithm is implemented by function \code{\link{srtboost}}.

  In addition, extractor functions like \code{\link[stats]{family}}, \code{\link[stats]{residuals}},
  \code{\link[stats]{fitted}}, \code{\link[stats]{logLik}}, \code{\link[graphics]{plot}},
  etc., are provided.

  \packageIndices{softtrees}
}

\author{
  Nikolaus Umlauf \email{Nikolaus.Umlauf@uibk.ac.at}
}

\references{
  Umlauf N, Klein N (2022). Distributional Adaptive Soft Regression Trees.
  \doi{doi:10.48550/ARXIV.2210.10389}
}

\keyword{ package }
\seealso{
  \code{\link{srt}}, \code{\link{srtboost}}, \code{\link{predict.srt}}
}

\examples{
## see ?srt and ?srtboost.
}
