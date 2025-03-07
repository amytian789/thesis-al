% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/active_learning.R
\name{active_learning}
\alias{active_learning}
\title{Main active learning method}
\usage{
active_learning(X, y, method = "lda", ...)
}
\arguments{
\item{X}{the full data matrix, n x d, including all unlabeled data}

\item{y}{a factor vector with 2 levels and NAs for unlabeled data}

\item{method}{the method name}

\item{...}{additional parameters for the active learning method}
}
\value{
an index corresponding to the row of X to learn the label of next
}
\description{
The missing labels in y are denoted by NA.
This method takes x as a matrix of all the data
}

