% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/depletePoolConstraint.R
\name{depletePoolConstraint}
\alias{depletePoolConstraint}
\title{Use complete item pool.}
\usage{
depletePoolConstraint(nForms, nItems = NULL, itemIDs = NULL)
}
\arguments{
\item{nForms}{Number of forms to be created.}

\item{nItems}{Number of items in the item pool [optional to create \code{itemIDs} automatically].}

\item{itemIDs}{a character vector of item IDs in correct ordering, or NULL.}
}
\value{
A sparse matrix.
}
\description{
Creates constraints that assure that every item in the item pool is used
(at least) once. Essentially a wrapper around \code{itemUsageConstraint}.
}
\examples{
depletePoolConstraint(2, itemIDs = 1:10)

}
