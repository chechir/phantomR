context("Testing phantomR - Basic Functionality")

library("dplyr")

test_that("get_factor_at_time_to_divide_by",{
  df = data.frame(
    "seq_id" = c(1, 1, 1, 2, 2, 2),
    "MF1"= c(2, 4, 6, 10, 20, 60),
    "MF2"= c(2, 4, 6, 10, 20, 60)
  )
  feature_cols = c("MF1", "MF2")
  time_col = "seq_id"
  result = get_factor_at_time_to_divide_by(df, time_col, feature_cols)

  expect_equal(result$scalling_factor, c(0.5, 2.5))
})


test_that("scale_owlstone",{
  df = data.frame(
    "seq_id" = c(1, 1, 1, 2, 2, 2),
    "MF1"= c(2, 4, 6, 10, 20, 60),
    "MF2"= c(2, 4, 6, 10, 20, 60)
  )
  feature_cols = c("MF1", "MF2")
  time_col = "seq_id"
  scalling_factors = get_factor_at_time_to_divide_by(df, time_col, feature_cols)
  result = norm_owlstone(df, time_col, feature_cols, scalling_factors)
  expect_equal(result$MF1, c(4, 8, 12, 4, 8, 24))
})
