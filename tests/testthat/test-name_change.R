test_that("name_change function works correctly", {
    # Example datasets
    char1 <- c("A", "B", "C", "D", "E", "F")
    char2 <- c("A", "C", "E")
    char3 <- c("A", "B", "C", "C_modif1", "D")
    char4 <- c("A", "A_modif1", "C")
    
    # Test cases
    
    expect_equal(name_change(char1, char2), c("A_modif1", "B", "C_modif2", "D", "E_modif3", "F"))
    
    # Test all arguments
    
    expect_equal(name_change(char3, char4), c("A_modif1", "B", "C_modif2", "C_modif1_modif3", "D_modif4"))
    
})