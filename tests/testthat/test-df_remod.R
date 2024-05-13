test_that("df_remod function works correctly", {
  # Example datasets
  dataframe1 <- data.frame(col1 = (1:4) * 10, col2 = c("A", "B", "A", "A"), stringsAsFactors = TRUE)
  dataframe2 <- data.frame(col1 = (1:4) * 10, col2 = 5:8, stringsAsFactors = TRUE)
  dataframe3 <- data.frame(col1 = (3:8) * 10, col2 = 5:10, stringsAsFactors = TRUE)
  rownames(dataframe3) <- paste0("row", 1:6)

  # Test cases
  # Simple examples
  # handles simple examples
    expect_no_error(df_remod(dataframe1))
    expect_no_error(df_remod(dataframe2))
    expect_no_error(df_remod(dataframe3))

  # Test argument quanti.col.name
  # handles argument quanti.col.name
    expect_no_error(df_remod(dataframe1, quanti.col.name = "quanti"))
    expect_no_error(df_remod(dataframe2, quanti.col.name = "quanti"))
    expect_no_error(df_remod(dataframe3, quanti.col.name = "quanti"))
    expect_no_error(df_remod(dataframe2, quanti.col.name = "quantity"))

  # Test argument quali.col.name
  # handles argument quali.col.name
    expect_no_error(df_remod(dataframe1, quali.col.name = "quali"))
    expect_no_error(df_remod(dataframe2, quali.col.name = "quali"))
    expect_no_error(df_remod(dataframe3, quali.col.name = "quali"))
    expect_no_error(df_remod(dataframe3, quali.col.name = "quality"))

  # Test all arguments
  # handles all arguments
    expect_no_error(df_remod(dataframe1, quanti.col.name = "quanti", quali.col.name = "quali", safer_check = TRUE))
    expect_no_error(df_remod(dataframe2, quanti.col.name = "quanti", quali.col.name = "quali", safer_check = TRUE))
    expect_no_error(df_remod(dataframe3, quanti.col.name = "quantity", quali.col.name = "quality", safer_check = TRUE))
    expect_no_error(df_remod(dataframe2, quanti.col.name = "quantity", quali.col.name = "quality", safer_check = TRUE))

})
