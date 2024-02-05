test_that("tail2 function works correctly", {
    # Example dataset
    mat1 <- diag(1:20)
    dimnames(mat1) <- list(letters[1:20], LETTERS[1:20])
    
    # Test cases
    # Test without arguments
    expect_no_error(tail2(data1 = mat1))
    
    # Test with n argument
    expect_no_error(tail2(data1 = mat1, n = 5))
    
    # Test with side argument
    expect_no_error(tail2(data1 = mat1, side = "r"))
    
    # Test with all arguments
    expect_no_error(tail2(data1 = mat1, n = 6, side = "l"))
})