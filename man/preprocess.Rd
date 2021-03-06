% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocess.R
\name{preprocess}
\alias{preprocess}
\title{Preprocess metabolomics data}
\usage{
preprocess(
  df,
  mf_ids,
  sparsity_threshold = 0.2,
  imputation_func = min,
  imputation_ratio = 0.5
)
}
\arguments{
\item{df}{Input data frame}

\item{imputation_func}{function to impute values. e.g. median, min, etc}

\item{imputation_ratio}{ratio to apply to values returned by the imputation function.}

\item{mf_cols}{Input name of molecular features columns}

\item{sparsity_thershold}{Only keep MFs where the sparsity is higher that the given threshold}
}
\description{
Preprocess: perform sparsity removal or features under the threshold and impute according to a given function
}
\author{
BDA Team \email{matias.thayer@owlstone..co.uk}
BDA Team
License: GNU GPL (>= 2)
}
