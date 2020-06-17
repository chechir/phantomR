#'Preprocess metabolomics data
#'@description Preprocess: perform sparsity removal or features under the threshold and impute according to a given function
#'@param df Input data frame
#'@param mf_cols Input name of molecular features columns
#'@param sparsity_thershold Only keep MFs where the sparsity is higher that the given threshold
#'@param imputation_func function to impute values. e.g. median, min, etc
#'@param imputation_ratio ratio to apply to values returned by the imputation function.
#'@author BDA Team \email{matias.thayer@owlstone..co.uk}
#'BDA Team
#'License: GNU GPL (>= 2)
#'@export
#'
preprocess <- function(df, mf_ids, sparsity_threshold=0.2, imputation_func=min, imputation_ratio=0.5){
  feats = as.data.frame(cbind(df))  # create copy and make sure it's a data.frame

  # Sparsity removal
  feats[feats == 0] <- NA  # assume zero is missing, like in GS2
  feat_sparsity = colMeans(is.na(feats[, mf_ids]))
  mf_ids_sparse_true_false = feat_sparsity <= sparsity_threshold
  mf_ids_complete = mf_ids[mf_ids_sparse_true_false]
  removed_mf_ids = setdiff(mf_ids, mf_ids_complete)
  feats = feats[,!(names(feats) %in% removed_mf_ids)]  # remove sparse columns

  print(str_c("Total number of features before sparsity removal: ", length(mf_ids)))
  print(str_c("Total number of features after sparsity removal: ", length(mf_ids_complete)))
  print(str_c("Features removed due to sparsity: ", removed_mf_ids))
  print(str_c("Total Samples: ", dim(feats)[1]))

  # Imputation
  impute_values = apply(feats[, mf_ids_complete], 2, imputation_func, na.rm=TRUE) * imputation_ratio
  for (col in mf_ids_complete){
    feats[, col] = na.fill(feats[, col], impute_values[col])
  }
  feats
}

