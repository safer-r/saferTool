test_that("sum2 function works correctly", {
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
  test_that("sums data correctly", {
    expect_equal(sum2(x = vec1), sum(vec1))
    expect_equal(sum2(x = vec2), sum(vec2, na.rm = TRUE))
    expect_equal(sum2(x = vec3), sum(vec3, na.rm = TRUE, finite = TRUE))
    expect_equal(sum2(x = vec4), sum(vec4))
    expect_equal(sum2(x = vec5), sum(vec5, na.rm = TRUE, finite = FALSE))
    expect_equal(sum2(x = log1), sum(as.numeric(log1), na.rm = TRUE, finite = TRUE))
    expect_equal(sum2(x = log2), sum(as.numeric(log2), na.rm = TRUE, finite = TRUE))
    expect_equal(sum2(x = mat1), sum(vec1))
    expect_equal(sum2(x = mat2), sum(vec5, na.rm = TRUE, finite = FALSE))
    expect_equal(sum2(x = mat3), sum(as.numeric(log1), na.rm = TRUE, finite = TRUE))
    expect_equal(sum2(x = mat4), sum(as.numeric(log2), na.rm = TRUE, finite = TRUE))
    expect_equal(sum2(x = tab1), sum(as.numeric(vec5), na.rm = TRUE, finite = FALSE))
  })

  # Test argument na.rm
  test_that("handles na.rm argument correctly", {
    expect_equal(sum2(x = vec1, na.rm = TRUE), sum(vec1))
    expect_equal(sum2(x = vec2, na.rm = TRUE), sum(vec2, na.rm = TRUE))
    expect_equal(sum2(x = log2, na.rm = TRUE), sum(as.numeric(log2), na.rm = TRUE, finite = TRUE))
    expect_equal(sum2(x = log1, na.rm = TRUE), sum(as.numeric(log1), na.rm = TRUE, finite = TRUE))
    expect_equal(sum2(x = mat3, na.rm = TRUE), sum(as.numeric(log1), na.rm = TRUE, finite = TRUE))
  })

  # Test argument finite
  test_that("handles finite argument correctly", {
    expect_equal(sum2(x = vec1, finite = TRUE), sum(vec1, na.rm = TRUE, finite = TRUE))
    expect_equal(sum2(x = vec3, finite = TRUE), sum(vec3, na.rm = TRUE, finite = TRUE))
  })

  # Test all arguments
  test_that("handles all arguments correctly", {
    expect_equal(sum2(x = vec5, na.rm = TRUE, finite = FALSE), sum(vec5, na.rm = TRUE, finite = FALSE))
    expect_equal(sum2(x = mat4, na.rm = TRUE, finite = FALSE), sum(as.numeric(log2), na.rm = TRUE, finite = TRUE))
  })
})
