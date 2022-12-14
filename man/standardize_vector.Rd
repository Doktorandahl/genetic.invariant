% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{standardize_vector}
\alias{standardize_vector}
\alias{normalize_vector}
\title{Standardize and normalize vector}
\usage{
standardize_vector(x)

normalize_vector(x)
}
\arguments{
\item{x}{Numeric vector to be standardized or normalized}
}
\value{
Standardized vector
}
\description{
Standardization and normalization of numeric vectors
}
\section{Functions}{
\itemize{
\item \code{standardize_vector()}: Standardizes vector by subtracting the mean and dividing by the standard deviation

\item \code{normalize_vector()}: Normalizes vector by subtracting the minimum value and dividing by the maximum value

}}
\examples{
x <- c(0,3,2,1,9,5,4,3,2,1)
standardize_vector(x)
}
