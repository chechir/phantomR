#'Scale data by centering to the mean
#'@description Scale data by column (assuming that samples are in rows, and molecular features in columns)
#'@param df Input data frame
#'@param mf_cols Input name of molecular features columns
#'@param type Input name of molecular features columns
#'@author BDA Team \email{matias.thayer@owlstone..co.uk}
#'BDA Team
#'License: GNU GPL (>= 2)
#'@export
#'
scale_by_feat <- function(df, mf_cols, type='MeanCenter'){
  result = cbind(as.data.frame(df))
  for (mf in mf_cols){
    if(type=='MeanCenter'){
      result[, mf] = result[, mf] - mean(as.matrix(df[, mf]))
    } else {
      stop(str_interp('Scaling method ${type} not implemented'))
    }
  }
  result
}
