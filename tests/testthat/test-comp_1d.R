test_that("comp_1d function works correctly", {
  # Example datasets
  vec1 <- 1:5
  vec2 <- 3:6
  fac1 <- factor(LETTERS[1:5])
  fac2 <- factor(LETTERS[4:7])
  fac3 <- factor(LETTERS[10:11])
  tab1 <- as.table(1:5)
  tab2 <- as.table(3:6)
  names(vec1) <- LETTERS[1:5]
  names(vec2) <- LETTERS[1:4]

  # All the arguments
  expect_no_error(comp_1d(vec1, vec2, safer_check = TRUE))
  expect_no_error(comp_1d(fac1, fac2, safer_check = TRUE))
  expect_no_error(comp_1d(fac1, fac3, safer_check = TRUE))
  expect_no_error(comp_1d(vec1, fac1, safer_check = TRUE))
  expect_no_error(comp_1d(tab1, tab1, safer_check = TRUE))
  expect_no_error(comp_1d(tab1, tab2, safer_check = TRUE))
  expect_no_error(comp_1d(tab1, vec1, safer_check = TRUE))
  expect_no_error(comp_1d(tab1, fac1, safer_check = TRUE))

})