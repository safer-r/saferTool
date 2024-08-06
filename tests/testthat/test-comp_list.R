test_that("comp_list function works correctly", {
  # Example datasets
  list1 <- list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) # 2D list
  list2 <- list(1:5, LETTERS[1:2]) # 2D list
  list3 <- list(a = 1:5, b = LETTERS[1:2]) # 2D list
  list4 <- list(LETTERS[5:9], matrix(1:6), 1:5) # 2D list

  # Test cases
  # Test lists with the same content
    expect_no_error(comp_list(list1, list1, safer_check = TRUE))
  
  # Test lists with different content
    expect_no_error(comp_list(list1, list3, safer_check = TRUE))

  # Test lists with different lengths
    expect_no_error(comp_list(list2, list4, safer_check = TRUE))

  # Test lists with same lengths
    expect_no_error(comp_list(list1, list4, safer_check = TRUE))
  
})