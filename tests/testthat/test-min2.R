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
    # Test argument x
      expect_equal(min2(x = vec1), -1)
      expect_equal(min2(x = vec2), as.integer(NA))
      expect_equal(min2(x = vec3), 1)
      expect_equal(min2(x = vec4), 1/3)
      expect_equal(min2(x = vec5), as.double(NA))
    
      expect_no_error(min2(x = log1))
      expect_no_error(min2(x = log2))
    
      expect_equal(min2(x = mat1), -1)
      expect_equal(min2(x = mat2), as.double(NA))

      expect_no_error(min2(x = mat3))
      expect_no_error(min2(x = mat4))
      expect_no_error(min2(x = tab1))
    
    # Test argument na.rm
      expect_equal(min2(x = vec1, na.rm = TRUE), -1)
      expect_equal(min2(x = vec2, na.rm = TRUE), 1)
      expect_no_error(min2(x = log2, na.rm = TRUE))
      expect_no_error(min2(x = log1, na.rm = TRUE))
      expect_no_error(min2(x = mat3, na.rm = TRUE))
    
    # Test argument finite
      expect_equal(min2(x = vec1, finite = TRUE), -1)
      expect_no_error(min2(x = vec3, finite = TRUE))
    
    # Test all arguments
      expect_no_error(min2(x = vec5, na.rm = TRUE, finite = FALSE, safer_check = TRUE))
      expect_no_error(min2(x = mat4, na.rm = TRUE, finite = FALSE, safer_check = TRUE))
})
