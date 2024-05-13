test_that("comp_2d function works correctly", {
    # Example datasets
    mat1 <- matrix(1:1e6, ncol = 5, dimnames = list(NULL, LETTERS[1:5]))
    mat2 <- matrix(as.integer((1:1e6) + 1e6/5), ncol = 5, dimnames = list(NULL, LETTERS[1:5]))
    mat3 <- matrix((1:1e6) + 1e6/5, ncol = 5, dimnames = list(NULL, LETTERS[1:5]))
    mat4 <- matrix(1:10, byrow = TRUE, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5]))
    mat5 <- matrix(c(1:5, 101:105, 6:10), byrow = TRUE, ncol = 5, dimnames = list(c("a", "z", "b"), c(LETTERS[1:2], "k", LETTERS[5:4])))
    mat6 <- matrix(c(1:5, 101:105, 6:10), byrow = TRUE, ncol = 5, dimnames = list(c("x", "z", "y"), c(LETTERS[1:2], "k", LETTERS[5:4])))
    dataframe1 <- as.data.frame(matrix(1:10, byrow = TRUE, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])))
    dataframe2 <- as.data.frame(matrix(c(1:5, 101:105, 6:10), byrow = TRUE, ncol = 5, dimnames = list(c("a", "z", "b"), c(LETTERS[1:2], "k", LETTERS[5:4]))))
    dataframe3 <- as.data.frame(matrix(c(1:5, 101:105, 6:10), byrow = TRUE, ncol = 5, dimnames = list(c("x", "z", "y"), c(LETTERS[1:2], "k", LETTERS[5:4]))))
    
    # Test large matrices with the same content
    # handles large matrices with the same content
      expect_no_error(comp_2d(mat1, mat2, safer_check = TRUE))
    
    
    # Test large matrices with different content
    # handles large matrices with different content
      expect_no_error(comp_2d(mat1, mat3, safer_check = TRUE))
    
    
    # Test matrices with the same row content and same row names
    # handles matrices with the same row content and same row names
      expect_no_error(comp_2d(mat4, mat5, safer_check = TRUE))
    
    
    # Test matrices with the same row content but different row names
    # handles matrices with the same row content but different row names
      expect_no_error(comp_2d(mat4, mat6, safer_check = TRUE))
    
    # Test data frames with the same row content and same row names, not same mode between columns
    # handles data frames with the same row content and same row names, not same mode between columns
    dataframe1[, 5] <- as.character(dataframe1[, 5])
    dataframe2[, 5] <- as.character(dataframe2[, 5])
      expect_no_error(comp_2d(dataframe1, dataframe2, safer_check = TRUE))
    
    # Test data frames with the same row content but different row names
    # handles data frames with the same row content but different row names
    dataframe1[, 5] <- as.character(dataframe1[, 5])
    dataframe3[, 5] <- as.character(dataframe3[, 5])
      expect_no_error(comp_2d(dataframe1, dataframe3, safer_check = TRUE))
    
})
