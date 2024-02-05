test_that("min2 function works correctly", {
  # Example datasets
  vec1 <- -1:3
  vec2 <- c(1:3, NA)
  vec3 <- c(1:3, Inf)
  vec4 <- 1:3 / 3
  vec5 <- c(1, 2, NA, -Inf)
  log1 <- c(TRUE, FALSE, NA)
  log2 <- c(TRUE, FALSE, FALSE)
  mat1 <- matrix(vec1)
  mat2 <- matrix(vec5)
  mat3 <- matrix(log1)
  mat4 <- matrix(log2)
  fac1 <- factor(vec5)
  tab1 <- table(fac1)

  # Test cases
  test_that("handles vector of integers", {
    expect_equal(min2(x = vec1), -1)
  })

  test_that("handles vector of integers with NA", {
    expect_equal(min2(x = vec2), 1)
  })

  test_that("handles vector of integers with Inf", {
    expect_equal(min2(x = vec3), -Inf)
  })

  test_that("handles vector of proportions", {
    expect_equal(min2(x = vec4), 1/3)
  })

  test_that("handles vector of integers but stored as double, with NA and Inf", {
    expect_equal(min2(x = vec5), -Inf)
  })

  test_that("handles logical vector with NA", {
    expect_no_error(min2(x = log1))
  })

  test_that("handles logical vector", {
    expect_no_error(min2(x = log2))
  })

  test_that("handles 1D matrix of integers", {
    expect_equal(min2(x = mat1), -1)
  })

  test_that("handles 1D matrix of integers, NA, and Inf", {
    expect_equal(min2(x = mat2), -Inf)
  })

  test_that("handles 1D matrix of logical values", {
    expect_no_error(min2(x = mat3))
  })

  test_that("handles 1D matrix of logical values and NA", {
    expect_no_error(min2(x = mat4))
  })

  test_that("handles 1D table", {
    expect_no_error(min2(x = tab1))
  })

  # Test arguments
  test_that("handles na.rm argument", {
    expect_equal(min2(x = vec1, na.rm = TRUE), -1)
    expect_equal(min2(x = vec2, na.rm = TRUE), 1)
    expect_no_error(min2(x = log2, na.rm = TRUE))
    expect_no_error(min2(x = log1, na.rm = TRUE))
    expect_no_error(min2(x = mat3, na.rm = TRUE))
  })

  test_that("handles finite argument", {
    expect_equal(min2(x = vec1, finite = TRUE), -1)
    expect_no_error(min2(x = vec3, finite = TRUE))
  })

  test_that("handles all arguments", {
    expect_no_error(min2(x = vec5, na.rm = TRUE, finite = FALSE))
    expect_no_error(min2(x = mat4, na.rm = TRUE, finite = FALSE))
  })
})
