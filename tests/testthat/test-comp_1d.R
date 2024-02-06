test_that("comp_1d function works correctly", {
  # Datasets
  vec1 <- 1:5
  vec2 <- 3:6
  fac1 <- factor(LETTERS[1:5])
  fac2 <- factor(LETTERS[4:7])
  fac3 <- factor(LETTERS[10:11])
  tab1 <- as.table(1:5)

  # All the arguments
  test_that("All arguments are handled", {
    names(vec1) <- LETTERS[1:5]
    expect_no_error(comp_1d(vec1, vec2))
    
    names(vec1) <- LETTERS[1:5]
    names(vec2) <- LETTERS[1:4]
    
    expect_no_error(comp_1d(vec1, vec2))
    expect_no_error(comp_1d(fac1, fac2))
    expect_no_error(comp_1d(fac1, fac3))
    expect_no_error(comp_1d(vec1, fac1))
    expect_no_error(comp_1d(tab1, tab1))
    expect_no_error(comp_1d(tab1, vec1))
  })
})