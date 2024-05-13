test_that("range2 function works correctly", {
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
      expect_equal(range2(x = vec1), c(-1, 3))
      expect_equal(range2(x = vec2), as.integer(c(NA, NA)))
      expect_equal(range2(x = vec3), c(1, Inf))
      expect_equal(range2(x = vec4), c(0.33333333, 1))
      expect_equal(range2(x = vec5), as.double(c(NA, NA)))
      expect_equal(range2(x = log1), as.integer(c(NA, NA)))
      expect_equal(range2(x = log2), c(0,1))
      expect_equal(range2(x = mat1), c(-1, 3))
      expect_equal(range2(x = mat2), as.double(c(NA, NA)))
      expect_equal(range2(x = mat3), as.integer(c(NA, NA)))
      expect_equal(range2(x = mat4), c(0, 1))
      expect_equal(range2(x = tab1), c(1, 1))
    
    
    # Test argument na.rm
      expect_equal(range2(x = vec1, na.rm = TRUE), c(-1, 3))
      expect_equal(range2(x = vec2, na.rm = TRUE), c(1, 3))
      expect_equal(range2(x = log2, na.rm = TRUE), c(0, 1))
      expect_equal(range2(x = log1, na.rm = TRUE), c(0, 1))
      expect_equal(range2(x = mat3, na.rm = TRUE), c(0, 1))
    
    
    # Test argument finite
      expect_equal(range2(x = vec1, finite = TRUE), c(-1, 3))
      expect_equal(range2(x = vec3, finite = TRUE), c(1, 3))
    
    
    # Test all arguments
      expect_equal(range2(x = vec5, na.rm = TRUE, finite = FALSE, safer_check = TRUE), c(-Inf, 2))
      expect_equal(range2(x = mat4, na.rm = TRUE, finite = FALSE, safer_check = TRUE), c(0,1))
    
})
