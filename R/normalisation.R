#'Normalise data using different methods
#'@description Normalise data assuming that samples are in rows, and molecular features in columns
#'@param df Input data frame
#'@param mf_cols Input name of molecular features columns
#'@author BDA Team \email{matias.thayer@owlstone..co.uk}
#'BDA Team
#'License: GNU GPL (>= 2)
#'@export
#'
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


#'Obtain Owlstone scalling factors
#'@description Normalise data assuming that samples are in rows, and molecular features in columns
#'@param df Input data frame (rows as samples and columns as features)
#'@param time_col column that contains the time aggregation (e.g. sequence or batch)
#'@param feature_cols vector with the name of the features to use
#'@author BDA Team \email{matias.thayer@owlstone..co.uk}
#'BDA Team
#'License: GNU GPL (>= 2)
#'@export
#'
get_factor_at_time_to_divide_by <- function(df, time_col, feature_cols){
  # DF could be a QCs or features df
  # df = data.frame(feats)
  # time_col = "seq_from_filename"
  # feature_cols = mf_ids
  ratio_to_global_median = cbind(df)
  for (mf in feature_cols){
    ratio_to_global_median[, mf] = ratio_to_global_median[, mf] / median(as.matrix(ratio_to_global_median[, mf]))
  }
  medians_by_time = ratio_to_global_median %>%
    group_by_at(time_col) %>%
    summarise_at(feature_cols, median)

  medians_of_feature_medians = apply(medians_by_time[, feature_cols], 1, median)
  result = data.frame(
    time_col = medians_by_time[, time_col],
    scalling_factor = medians_of_feature_medians
  )
  result
}


#'Scale data frame using scaling factor (divide by scalling factors)
#'@description Normalise data assuming that samples are in rows, and molecular features in columns
#'@param df_to_scale Input data frame (rows as samples and columns as features)
#'@param time_col column that contains the time aggregation (e.g. sequence or batch)
#'@param feature_cols vector with the name of the features to use
#'@param factor_at_time_to_divide_by data frame with the scaling factor. It must be consistent with the time_col parameter
#'@author BDA Team \email{matias.thayer@owlstone..co.uk}
#'BDA Team
#'License: GNU GPL (>= 2)
#'@export
#'
norm_owlstone <- function(df_to_scale, time_col, feature_cols, factor_at_time_to_divide_by){
  result_df = merge(df_to_scale, factor_at_time_to_divide_by, by=time_col)
  for(mf in feature_cols){
    result_df[, mf] = result_df[, mf] / result_df[, 'scalling_factor']
  }
  result_df['scalling_factor'] <- NULL
  result_df
}
