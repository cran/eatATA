% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrixExlusionTuples.R
\name{matrixExclusionTuples}
\alias{matrixExclusionTuples}
\title{Create item exclusion tuples from matrix.}
\usage{
matrixExclusionTuples(exclMatrix)
}
\arguments{
\item{exclMatrix}{A \code{data.frame} or \code{matrix} with information on item exclusiveness.}
}
\value{
A \code{data.frame} with two columns.
}
\description{
If item exclusions are stored as a matrix, \code{matrixExclusionTuples} transforms this format into item pairs ('tuples').
Information on exclusions has to be coded as \code{1} (items are exclusive) and \code{0} (items are not exclusive).
}
\details{
Exclusion tuples can be used by \code{\link{itemExclusionConstraint}} to set up exclusion constraints.
}
\examples{
# Example data.frame
exclDF <- data.frame(c(0, 1, 0, 0),
                     c(1, 0, 0, 1),
                     c(0, 0, 0, 0),
                     c(0, 1, 0, 0))
rownames(exclDF) <- colnames(exclDF) <- paste0("item_", 1:4)

# Create tuples
matrixExclusionTuples(exclDF)


}
