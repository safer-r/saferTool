test_that("head2 function works correctly", {
  # Example datasets
  mat1 <- diag(1:20)
  dimnames(mat1) <- list(letters[1:20], LETTERS[1:20]) # diagonal matrix 20 * 20 with row names and column names

  # Test cases
  # Simple examples
    expect_no_error(head2(data1 = mat1))
    expect_no_error(head2(letters, 8))

  # Test argument n
    expect_no_error(head2(data1 = mat1, n = 5))

  # Test argument side
    expect_no_error(head2(data1 = mat1, side = "r"))

  # Test all arguments
    expect_no_error(head2(
      data1 = mat1, 
      n = 6, 
      side = "l", 
      safer_check = TRUE))
})
