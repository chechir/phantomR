#'Normalise data using different methods
#'@description Normalise data assuming that samples are in rows, and molecular features in columns
#'@param df Input data frame
#'@param mf_cols Input name of molecular features columns
#'@author Stefano and Matias \email{matias.thayer@owlstone..co.uk}
#'BDA Team
#'License: GNU GPL (>= 2)
#'@export

norm_by_sample <- function(df, mf_cols, type='MedianNorm'){
  # df = process_data
  # mf_cols = mf_ids_complete
  result = cbind(as.data.frame(df))
  type='median_norm'
  # median_val = median(as.matrix(dt[, col]))
  for (id_sample in result[, "id"]){
    ixs = which(result[, "id"] == id_sample)
    row = as.numeric(result[ixs, mf_cols])

    if(type=='median_norm'){
      result[ixs, mf_cols] = row / median(row, na.rm = TRUE)
    if(type=='std_norm'){
      result[ixs, mf_cols] = row / sd(row, na.rm = TRUE)
    } else {
      stop(str_interp('Scaling method ${type} not implemented'))
    }
  }
  result
  }
}
