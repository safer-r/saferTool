testthat::test_that("comp_list()", {

    ## data argument values
    num1 <- 1.5
    int1 <- 3L
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor("a")
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    vec_num1 <- c(NA, 10, 100.001, 333.0001254, 12312.1235)
    list1 <- list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) # named list
    list2 <- list(1:5, LETTERS[1:2]) # unnamed list
    list3 <- list(a = 1:5, b = LETTERS[1:2]) # named list
    list4 <- list(LETTERS[5:9], matrix(1:6), 1:5) # unnamed list
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(comp_list(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(comp_list(data1 = list1, data2 = list4, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferTool:::comp_list()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()") # errors on the missing data1 default, but the function_name block is executed first
    #### end function name

    ########  arg with no default values
    testthat::expect_error(comp_list())
    # the missing() based block cannot fire when both args are provided: nothing more to test here
    ########  end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(comp_list(data1 = NULL, data2 = list4, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(comp_list(data1 = list1, data2 = list4, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(comp_list(data1 = list(), data2 = list4, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = logical(), lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(comp_list(data1 = list1, data2 = list4, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(comp_list(data1 = NA, data2 = list4, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(comp_list(data1 = list1, data2 = list4, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(comp_list(data1 = list1, data2 = list4, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_no_error(comp_list(data1 = list1, data2 = list4, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = list4, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(comp_list(data1 = list1, data2 = list4, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(comp_list(data1 = list1, data2 = list4, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(comp_list(data1 = list1, data2 = list4, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # data1
    testthat::expect_error(comp_list(data1 = vec_num1, data2 = list4, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = mat1, data2 = list4, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = factor1, data2 = list4, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = expr1, data2 = list4, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = fun1, data2 = list4, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(comp_list(data1 = list(1), data2 = list4, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end data1
    # data2
    testthat::expect_error(comp_list(data1 = list1, data2 = vec_num1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_list(data1 = list1, data2 = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(comp_list(data1 = list1, data2 = list(1), safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end data2
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## code that protects set.seed() in the global environment
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ######## end warning initiation

    ######## graphic device checking
    ######## end graphic device checking

    ######## other checkings
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # identical lists (fast path)
    result <- comp_list(data1 = list1, data2 = list1, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result$same_length, expected = TRUE)
    testthat::expect_equal(object = result$length, expected = 3)
    testthat::expect_equal(object = result$same_names, expected = TRUE)
    testthat::expect_equal(object = result$name, expected = c("a", "b", "d"))
    testthat::expect_equal(object = result$any_id_names, expected = TRUE)
    testthat::expect_equal(object = result$same_names_pos1, expected = 1:3)
    testthat::expect_equal(object = result$same_names_pos2, expected = 1:3)
    testthat::expect_equal(object = result$same_names_match1, expected = 1:3)
    testthat::expect_equal(object = result$same_names_match2, expected = 1:3)
    testthat::expect_equal(object = result$common_names, expected = c("a", "b", "d"))
    testthat::expect_equal(object = result$any_id_compartments, expected = TRUE)
    testthat::expect_equal(object = result$same_compartments_pos1, expected = 1:3)
    testthat::expect_equal(object = result$same_compartments_pos2, expected = 1:3)
    testthat::expect_equal(object = result$same_compartments_match1, expected = 1:3)
    testthat::expect_equal(object = result$same_compartments_match2, expected = 1:3)
    testthat::expect_equal(object = result$common_compartments, expected = list1)
    testthat::expect_equal(object = result$same_order, expected = TRUE)
    testthat::expect_equal(object = result$identical_object, expected = TRUE)
    testthat::expect_equal(object = result$identical_content, expected = TRUE)
    # identical single-compartment lists with no names: same.names stays NULL
    result2 <- comp_list(data1 = list(1:3), data2 = list(1:3), safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result2$same_length, expected = TRUE)
    testthat::expect_equal(object = result2$length, expected = 1)
    testthat::expect_equal(object = result2$same_names, expected = NULL)
    testthat::expect_equal(object = result2$any_id_names, expected = FALSE) # no names at all, so no names in common
    testthat::expect_equal(object = result2$same_compartments_pos1, expected = 1)
    testthat::expect_equal(object = result2$same_compartments_pos2, expected = 1)
    testthat::expect_equal(object = result2$common_compartments, expected = list(1:3))
    testthat::expect_equal(object = result2$same_order, expected = TRUE)
    testthat::expect_equal(object = result2$identical_object, expected = TRUE)
    testthat::expect_equal(object = result2$identical_content, expected = TRUE)
    # different names and different compartments, same length
    result3 <- comp_list(data1 = list1, data2 = list4, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result3$same_length, expected = TRUE)
    testthat::expect_equal(object = result3$length, expected = 3)
    testthat::expect_equal(object = result3$same_names, expected = FALSE)
    testthat::expect_equal(object = result3$name, expected = NULL)
    testthat::expect_equal(object = result3$any_id_names, expected = FALSE)
    testthat::expect_equal(object = result3$same_names_pos1, expected = NULL)
    testthat::expect_equal(object = result3$same_names_pos2, expected = NULL)
    testthat::expect_equal(object = result3$same_names_match1, expected = NULL)
    testthat::expect_equal(object = result3$same_names_match2, expected = NULL)
    testthat::expect_equal(object = result3$common_names, expected = NULL)
    testthat::expect_equal(object = result3$any_id_compartments, expected = TRUE)
    testthat::expect_equal(object = result3$same_compartments_pos1, expected = c(1, 3))
    testthat::expect_equal(object = result3$same_compartments_pos2, expected = c(2, 3))
    testthat::expect_equal(object = result3$same_compartments_match1, expected = c(3, NA, 2))
    testthat::expect_equal(object = result3$same_compartments_match2, expected = c(NA, 3, 1))
    testthat::expect_equal(object = result3$common_compartments, expected = list(1:5, matrix(1:6)))
    testthat::expect_equal(object = result3$same_order, expected = NULL)
    testthat::expect_equal(object = result3$identical_object, expected = FALSE)
    testthat::expect_equal(object = result3$identical_content, expected = FALSE)
    # same names, different length (subset compartments)
    result4 <- comp_list(data1 = list1, data2 = list3, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result4$same_length, expected = FALSE)
    testthat::expect_equal(object = result4$length, expected = NULL)
    testthat::expect_equal(object = result4$same_names, expected = FALSE)
    testthat::expect_equal(object = result4$any_id_names, expected = TRUE)
    testthat::expect_equal(object = result4$same_names_pos1, expected = c(1, 2))
    testthat::expect_equal(object = result4$same_names_pos2, expected = c(1, 2))
    testthat::expect_equal(object = result4$same_names_match1, expected = c(1, 2, NA))
    testthat::expect_equal(object = result4$same_names_match2, expected = c(1, 2))
    testthat::expect_equal(object = result4$common_names, expected = c("a", "b"))
    testthat::expect_equal(object = result4$any_id_compartments, expected = TRUE)
    testthat::expect_equal(object = result4$same_compartments_pos1, expected = c(1, 2))
    testthat::expect_equal(object = result4$same_compartments_pos2, expected = c(1, 2))
    testthat::expect_equal(object = result4$same_compartments_match1, expected = c(1, 2, NA))
    testthat::expect_equal(object = result4$same_compartments_match2, expected = c(1, 2))
    testthat::expect_equal(object = result4$common_compartments, expected = list(1:5, LETTERS[1:2]))
    testthat::expect_equal(object = result4$same_order, expected = NULL)
    testthat::expect_equal(object = result4$identical_object, expected = FALSE)
    testthat::expect_equal(object = result4$identical_content, expected = FALSE)
    # both lists without names, different length
    result5 <- comp_list(data1 = list2, data2 = list4, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result5$same_length, expected = FALSE)
    testthat::expect_equal(object = result5$length, expected = NULL)
    testthat::expect_equal(object = result5$same_names, expected = NULL) # not FALSE because none of the two lists has names
    testthat::expect_equal(object = result5$any_id_names, expected = FALSE)
    testthat::expect_equal(object = result5$any_id_compartments, expected = TRUE)
    testthat::expect_equal(object = result5$same_compartments_pos1, expected = 1)
    testthat::expect_equal(object = result5$same_compartments_pos2, expected = 3)
    testthat::expect_equal(object = result5$same_compartments_match1, expected = c(3, NA))
    testthat::expect_equal(object = result5$same_compartments_match2, expected = c(NA, NA, 1))
    testthat::expect_equal(object = result5$common_compartments, expected = list(1:5))
    testthat::expect_equal(object = result5$same_order, expected = NULL)
    testthat::expect_equal(object = result5$identical_object, expected = FALSE)
    testthat::expect_equal(object = result5$identical_content, expected = FALSE)
    # same compartments in a different order: same.order == FALSE (not NULL because match1/match2 have no NA)
    result6 <- comp_list(data1 = list(1:3, 4:6), data2 = list(4:6, 1:3), safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result6$same_length, expected = TRUE)
    testthat::expect_equal(object = result6$length, expected = 2)
    testthat::expect_equal(object = result6$same_names, expected = NULL)
    testthat::expect_equal(object = result6$any_id_names, expected = FALSE)
    testthat::expect_equal(object = result6$any_id_compartments, expected = TRUE)
    testthat::expect_equal(object = result6$same_compartments_pos1, expected = c(1, 2))
    testthat::expect_equal(object = result6$same_compartments_pos2, expected = c(1, 2))
    testthat::expect_equal(object = result6$same_compartments_match1, expected = c(2, 1))
    testthat::expect_equal(object = result6$same_compartments_match2, expected = c(2, 1))
    testthat::expect_equal(object = result6$common_compartments, expected = list(1:3, 4:6))
    testthat::expect_equal(object = result6$same_order, expected = FALSE)
    testthat::expect_equal(object = result6$identical_object, expected = FALSE)
    testthat::expect_equal(object = result6$identical_content, expected = FALSE)
    # same compartment content, different names: identical.content == TRUE but identical.object == FALSE
    result7 <- comp_list(data1 = list(a = 1:3), data2 = list(b = 1:3), safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result7$same_length, expected = TRUE)
    testthat::expect_equal(object = result7$length, expected = 1)
    testthat::expect_equal(object = result7$same_names, expected = FALSE)
    testthat::expect_equal(object = result7$any_id_names, expected = FALSE)
    testthat::expect_equal(object = result7$any_id_compartments, expected = TRUE)
    testthat::expect_equal(object = result7$same_compartments_pos1, expected = 1)
    testthat::expect_equal(object = result7$same_compartments_pos2, expected = 1)
    testthat::expect_equal(object = result7$same_compartments_match1, expected = 1)
    testthat::expect_equal(object = result7$same_compartments_match2, expected = 1)
    testthat::expect_equal(object = result7$common_compartments, expected = list(1:3))
    testthat::expect_equal(object = result7$same_order, expected = TRUE)
    testthat::expect_equal(object = result7$identical_object, expected = FALSE)
    testthat::expect_equal(object = result7$identical_content, expected = TRUE)
    # swapped compartments with different names
    result8 <- comp_list(data1 = list(a = 1:3, b = "x"), data2 = list(c = "x", d = 1:3), safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result8$same_length, expected = TRUE)
    testthat::expect_equal(object = result8$length, expected = 2)
    testthat::expect_equal(object = result8$same_names, expected = FALSE)
    testthat::expect_equal(object = result8$any_id_names, expected = FALSE)
    testthat::expect_equal(object = result8$any_id_compartments, expected = TRUE)
    testthat::expect_equal(object = result8$same_compartments_pos1, expected = c(1, 2))
    testthat::expect_equal(object = result8$same_compartments_pos2, expected = c(1, 2))
    testthat::expect_equal(object = result8$same_compartments_match1, expected = c(2, 1))
    testthat::expect_equal(object = result8$same_compartments_match2, expected = c(2, 1))
    testthat::expect_equal(object = result8$common_compartments, expected = list(1:3, "x"))
    testthat::expect_equal(object = result8$same_order, expected = FALSE)
    testthat::expect_equal(object = result8$identical_object, expected = FALSE)
    testthat::expect_equal(object = result8$identical_content, expected = FALSE)
    # no compartment in common: same.order stays NULL
    result9 <- comp_list(data1 = list(1:3), data2 = list("x"), safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result9$same_length, expected = TRUE)
    testthat::expect_equal(object = result9$length, expected = 1)
    testthat::expect_equal(object = result9$same_names, expected = NULL)
    testthat::expect_equal(object = result9$any_id_names, expected = FALSE)
    testthat::expect_equal(object = result9$any_id_compartments, expected = FALSE)
    testthat::expect_equal(object = result9$same_compartments_pos1, expected = NULL)
    testthat::expect_equal(object = result9$same_compartments_pos2, expected = NULL)
    testthat::expect_equal(object = result9$same_compartments_match1, expected = NULL)
    testthat::expect_equal(object = result9$same_compartments_match2, expected = NULL)
    testthat::expect_equal(object = result9$common_compartments, expected = NULL)
    testthat::expect_equal(object = result9$same_order, expected = NULL)
    testthat::expect_equal(object = result9$identical_object, expected = FALSE)
    testthat::expect_equal(object = result9$identical_content, expected = FALSE)
    # same names, same length, different content (covers the same.names <- TRUE branch of the non-identical path)
    result10 <- comp_list(data1 = list(a = 1:3), data2 = list(a = 4:6), safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result10$same_length, expected = TRUE)
    testthat::expect_equal(object = result10$length, expected = 1)
    testthat::expect_equal(object = result10$same_names, expected = TRUE)
    testthat::expect_equal(object = result10$name, expected = "a")
    testthat::expect_equal(object = result10$any_id_names, expected = TRUE)
    testthat::expect_equal(object = result10$same_names_pos1, expected = 1)
    testthat::expect_equal(object = result10$same_names_pos2, expected = 1)
    testthat::expect_equal(object = result10$same_names_match1, expected = 1)
    testthat::expect_equal(object = result10$same_names_match2, expected = 1)
    testthat::expect_equal(object = result10$common_names, expected = "a")
    testthat::expect_equal(object = result10$any_id_compartments, expected = FALSE)
    testthat::expect_equal(object = result10$common_compartments, expected = NULL)
    testthat::expect_equal(object = result10$same_order, expected = NULL)
    testthat::expect_equal(object = result10$identical_object, expected = FALSE)
    testthat::expect_equal(object = result10$identical_content, expected = FALSE)
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
