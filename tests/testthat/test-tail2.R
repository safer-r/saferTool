testthat::test_that("tail2()", {

    ## data argument values
    num1 <- 1.5
    int1 <- 3L
    mat1 <- base::matrix(-1:3)
    mat2d <- base::matrix(data = 1:20, ncol = 4, dimnames = base::list(letters[1:5], LETTERS[1:4]))
    mat1d <- base::matrix(1:3)
    df1 <- base::data.frame(a = 1:4, b = 11:14)
    tab1 <- base::table(fac1 = base::factor(rep(letters[1:3], each = 4)))
    factor1 <- base::as.factor("a")
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    vec1 <- letters
    vec_num1 <- c(1, 2, NA)
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(tail2(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferTool:::tail2()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()") # errors on the missing data1 default, but the function_name block is executed first
    #### end function name

    ########  arg with no default values
    testthat::expect_error(tail2())
    # the missing() based block cannot fire when data1 is provided: nothing more to test here
    ########  end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(tail2(data1 = NULL, n = 3, side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = NULL, side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(tail2(data1 = character(), n = 3, side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = integer(), n = 3, side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = double(), n = 3, side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = logical(), n = 3, side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = complex(), n = 3, side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = data.frame(), n = 3, side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = list(), n = 3, side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = integer(), side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = double(), side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = logical(), side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = list(), lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(tail2(data1 = NA, n = 3, side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = NA, side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_no_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    # lib_path is checked even when safer_check is FALSE (backbone v21.2: lib_path used outside if(safer_check == TRUE){}, so the lib_path check is always executed)
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = FALSE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # n
    testthat::expect_error(tail2(data1 = mat2d, n = "a", side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = c(1, 2), side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = TRUE, side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = mat1, side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = factor1, side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = expr1, side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = fun1, side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 2.5, side = "l", safer_check = TRUE, lib_path = NULL, error_text = "")) # not integer (typeof integer not respected)
    testthat::expect_no_error(tail2(data1 = mat2d, n = 2L, side = "l", safer_check = TRUE, lib_path = NULL, error_text = "")) # integer allowed
    testthat::expect_no_error(tail2(data1 = mat2d, n = 2, side = "l", safer_check = TRUE, lib_path = NULL, error_text = "")) # double_as_integer_allowed = TRUE
    # end n
    # side
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = "z", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = c("l", "r"), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(tail2(data1 = mat2d, n = 3, side = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(tail2(data1 = mat2d, n = 3, side = "l", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(tail2(data1 = mat2d, n = 3, side = "r", safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end side
    # data1 not checked by arg_check() because it can be any object
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
    testthat::expect_error(tail2(data1 = mat2d, n = -1, side = "l", safer_check = TRUE, lib_path = NULL, error_text = "")) # non negative integer check (neg_values = FALSE)
    testthat::expect_error(tail2(data1 = mat2d, n = 0, side = "l", safer_check = TRUE, lib_path = NULL, error_text = "")) # n < 1 not allowed
    testthat::expect_no_error(tail2(data1 = mat2d, n = 1, side = "l", safer_check = TRUE, lib_path = NULL, error_text = "")) # 1 allowed
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # 2D matrix, left side: last rows
    result <- tail2(data1 = mat2d, n = 2, side = "l", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result, expected = mat2d[4:5, 1:2])
    # 2D matrix, right side: last rows and last columns
    result2 <- tail2(data1 = mat2d, n = 2, side = "r", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result2, expected = mat2d[4:5, 3:4])
    # n larger than the number of rows: all rows returned
    result3 <- tail2(data1 = mat2d, n = 10, side = "l", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result3, expected = mat2d[1:5, 1:4])
    # vector input: dispatched to utils::tail()
    result4 <- tail2(data1 = vec1, n = 3, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result4, expected = letters[24:26])
    # numeric vector with NA
    result5 <- tail2(data1 = vec_num1, n = 2, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result5, expected = c(2, NA))
    # data.frame input, right side: when n >= the number of columns, all columns are kept
    result6 <- tail2(data1 = df1, n = 2, side = "r", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result6, expected = df1[3:4, 1:2])
    # 1D-like single column matrix: 2D branch (dim has 2 elements), keeps the matrix structure thanks to drop = FALSE
    result7 <- tail2(data1 = mat1d, n = 2, side = "l", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result7, expected = base::matrix(2:3, ncol = 1))
    # table input: dispatched to utils::tail() like utils::tail() does on tables
    result8 <- tail2(data1 = tab1, n = 2, side = "r", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result8, expected = utils::tail(x = tab1, n = 2))
    # data.frame, left side
    result9 <- tail2(data1 = df1, n = 2, side = "l", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result9, expected = df1[3:4, 1:2])
    # factor input: dispatched to utils::tail()
    result10 <- tail2(data1 = factor1, n = 1, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result10, expected = utils::tail(x = factor1, n = 1))
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
