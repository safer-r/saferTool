test_that("df_remod function works correctly", {
  # Example datasets
  dataframe1 <- data.frame(col1 = (1:4) * 10, col2 = c("A", "B", "A", "A"), stringsAsFactors = TRUE)
  dataframe2 <- data.frame(col1 = (1:4) * 10, col2 = 5:8, stringsAsFactors = TRUE)

  # Test cases
  # Simple examples
  # handles simple examples
    expect_no_error(df_remod(dataframe1))
    expect_no_error(df_remod(dataframe2))

  # Test quanti.col.name argument
  # handles quanti.col.name argument
    expect_no_error(df_remod(dataframe1, quanti.col.name = "quanti"))
    expect_no_error(df_remod(dataframe2, quanti.col.name = "quanti"))
    expect_no_error(df_remod(dataframe2, quanti.col.name = "quantity"))

  # Test quali.col.name argument
  # handles quali.col.name argument
    expect_no_error(df_remod(dataframe1, quali.col.name = "quali"))
    expect_no_error(df_remod(dataframe2, quali.col.name = "quali"))
    expect_no_error(df_remod(dataframe2, quali.col.name = "quality"))

  # Test all arguments
  # handles all arguments
    expect_no_error(df_remod(dataframe1, quanti.col.name = "quanti", quali.col.name = "quali"))
    expect_no_error(df_remod(dataframe2, quanti.col.name = "quanti", quali.col.name = "quali"))
    expect_no_error(df_remod(dataframe2, quanti.col.name = "quantity", quali.col.name = "quality"))

})
