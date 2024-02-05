test_that("name_change function works correctly", {
    # Example datasets
    char1 <- c("A", "B", "C", "D", "E", "F")
    char2 <- c("A", "C", "E")
    char3 <- c("A", "B", "C", "C_modif1", "D")
    char4 <- c("A", "A_modif1", "C")
    
    # Test cases
    
    expect_no_error(name_change(char1, char2))
    
    # Test all arguments
    
    expect_no_error(name_change(char3, char4))
    
})