test_that("tail2 function works correctly", {
  # Example dataset
  mat1 <- diag(1:20)
  dimnames(mat1) <- list(letters[1:20], LETTERS[1:20])

  # Test cases
  test_that("tail2 returns correct results", {
    # Test without arguments
    expect_equal(tail2(data1 = mat1), mat1)

    # Test with n argument
    expect_equal(tail2(data1 = mat1, n = 5), tail(mat1, n = 5))

    # Test with side argument
    expect_equal(tail2(data1 = mat1, side = "r"), mat1[, (ncol(mat1) - 4):ncol(mat1)])

    # Test with all arguments
    expect_equal(tail2(data1 = mat1, n = 6, side = "l"), mat1[1:6, ])
  })
})