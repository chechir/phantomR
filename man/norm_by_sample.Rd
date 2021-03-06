% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalisation.R
\name{norm_by_sample}
\alias{norm_by_sample}
\title{Normalise data using different methods}
\usage{
norm_by_sample(df, mf_cols, type = "MedianNorm")
}
\arguments{
\item{df}{Input data frame}

\item{mf_cols}{Input name of molecular features columns}

\item{type}{Type of normalisation to perform}
}
\description{
Normalise data assuming that samples are in rows, and molecular features in columns
}
\author{
BDA Team \email{matias.thayer@owlstone..co.uk}
BDA Team
License: GNU GPL (>= 2)
}
