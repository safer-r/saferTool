testthat::test_that("name_change()", {

    ## data argument values
    num1 <- 1.5
    int1 <- 3L
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor("a")
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    vec1 <- c("A", "B", "C", "A", "D")
    vec2 <- c("A", "C")
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(name_change(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferTool:::name_change()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()") # errors on the missing data1/data2 defaults, but the function_name block is executed first
    #### end function name

    ########  arg with no default values
    testthat::expect_error(name_change())
    data1 <- vec1
    testthat::expect_error(name_change()) # data2 still missing in the call
    base::rm(data1)
    ########  end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(name_change(data1 = NULL, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = NULL, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = NULL, duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = NULL, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = NULL, error_text = ""))
    testthat::expect_no_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(name_change(data1 = character(), data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = integer(), data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = double(), data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = logical(), data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = complex(), data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = list(), data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = character(), added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = integer(), added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = double(), added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = logical(), added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = complex(), added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = list(), added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = character(), duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = integer(), duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = double(), duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = logical(), duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = complex(), duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = list(), duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = logical(), lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = data.frame(), lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = list(), lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = character(), error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = integer(), error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = double(), error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = complex(), error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = data.frame(), error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = list(), error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = character(), safer_check = TRUE, error_text = ""))
    testthat::expect_no_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = character())) # but error_text is converted to ""
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(name_change(data1 = NA, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = NA, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = NA, duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = NA, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = NA, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NA, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = 1, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = mat1, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = factor1, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = expr1, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = fun1, error_text = ""))
    testthat::expect_no_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_no_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = FALSE, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_no_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = 1, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = c(TRUE, FALSE), safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = mat1, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = factor1, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = expr1, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = fun1, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = "PATH_NOT_GOOD", safer_check = TRUE, error_text = ""))
    testthat::expect_no_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), safer_check = TRUE, error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # data1
    testthat::expect_error(name_change(data1 = num1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = int1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = mat1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = factor1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = expr1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = fun1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    # end data1
    # data2
    testthat::expect_error(name_change(data1 = vec1, data2 = num1, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = mat1, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = factor1, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = expr1, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = fun1, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    # end data2
    # added_string
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = 1, duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = c("_modif", "_modif2"), duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = "")) # length must be 1
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = TRUE, duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = expr1, duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = fun1, duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_no_error(name_change(data1 = vec1, data2 = vec2, added_string = "X", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = "")) # any single character string works
    # end added_string
    # duplicate
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = 1, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = "TRUE", lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = c(TRUE, FALSE), lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = mat1, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = expr1, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = fun1, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_no_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_no_error(name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = FALSE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    # end duplicate
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(name_change(data1 = c("A", ""), data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = c("A", ""), added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
    testthat::expect_error(name_change(data1 = vec1, data2 = vec2, added_string = "", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = ""))
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
    testthat::expect_error(name_change(data1 = vec1, data2 = c("A", "A"), added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = "")) # duplicated elements in data2
    testthat::expect_error(name_change(data1 = c("A_modif", "B"), data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = "")) # data1 elements ending with added_string
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # elements present in data2: modified with added_string
    result <- name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = "")
    testthat::expect_equal(object = result$data, expected = c("A_modif", "B", "C_modif", "A_modif", "D"))
    testthat::expect_equal(object = result$posi, expected = c(1, 3, 4))
    testthat::expect_equal(object = result$ini, expected = c("A", "C", "A"))
    testthat::expect_equal(object = result$post, expected = c("A_modif", "C_modif", "A_modif"))
    # duplicate = FALSE: incremented suffixes to avoid duplicated modified elements
    result2 <- name_change(data1 = vec1, data2 = vec2, added_string = "_modif", duplicate = FALSE, lib_path = NULL, safer_check = TRUE, error_text = "")
    testthat::expect_equal(object = result2$data, expected = c("A_modif1", "B", "C_modif", "A_modif2", "D"))
    testthat::expect_equal(object = result2$post, expected = c("A_modif1", "C_modif", "A_modif2"))
    # no element of data1 in data2: no modification
    result3 <- name_change(data1 = c("A", "B"), data2 = c("X", "Y"), added_string = "_modif", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = "")
    testthat::expect_equal(object = result3$data, expected = c("A", "B"))
    testthat::expect_null(object = result3$posi)
    testthat::expect_null(object = result3$ini)
    testthat::expect_null(object = result3$post)
    # different added_string
    result4 <- name_change(data1 = vec1, data2 = vec2, added_string = "_2", duplicate = TRUE, lib_path = NULL, safer_check = TRUE, error_text = "")
    testthat::expect_equal(object = result4$data, expected = c("A_2", "B", "C_2", "A_2", "D"))
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
