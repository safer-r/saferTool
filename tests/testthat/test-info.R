test_that("info function works correctly", {
  # Example datasets
  vec1 <- -1:3
  vec2 <- 1:3 / 3
  vec3 <- c(1, 2, NA, -Inf)
  vec4 <- "pearson"
  vec5 <- c("a", "b", "a", NA)
  mat1 <- matrix(vec1)
  dimnames(mat1) <- list(NULL, "M1")
  mat2 <- matrix(c(1:5, NA), ncol = 2, dimnames = list(c("ROW1", "ROW2", "ROW3"), c("M1", "M2")))
  df1 <- as.data.frame(mat2)
  l1 <- list(L1 = 1:3, L2 = letters[1:3])
  fac1 <- factor(rep(letters[4:6], c(4:6)))
  tab1 <- table(fac1)
  tab2 <- table(fac1, fac1)
  exp1 <- expression("a")
  name1 <- substitute(exp1)
  fun1 <- mean
  fun2 <- sum
  fun3 <- get("<-")
  env1 <- new.env()
  s4 <- show
  call1 <- call("call1")

  # Test cases
  # Simple examples
  test_that("handles simple examples", {
    expect_no_error(info(data = vec1))
    expect_no_error(info(data = vec2))
    expect_no_error(info(data = vec3))
    expect_no_error(info(data = vec4))
    expect_no_error(info(data = vec5))
    expect_no_error(info(data = mat1))
    expect_no_error(info(data = mat2))
    expect_no_error(info(data = df1))
    expect_no_error(info(data = l1))
    expect_no_error(info(data = fac1))
    expect_no_error(info(data = tab1))
    expect_no_error(info(data = tab2))
    expect_no_error(info(data = exp1))
    expect_no_error(info(data = name1))
    expect_no_error(info(data = fun1))
    expect_no_error(info(data = fun2))
    expect_no_error(info(data = fun3))
    expect_no_error(info(data = env1))
    expect_no_error(info(data = s4))
    expect_no_error(info(data = call1))
  })

  # Test all arguments
  test_that("handles all arguments", {
    expect_no_error(info(data = vec1, n = 1, warn.print = FALSE, safer_check = TRUE))
  })
})
