testthat::test_that("max2()", {

    ## data argument values
    num1 <- 1.5
    int1 <- 3L
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor("a")
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    vec1 <- -1:3
    vec2 <- c(1:3, NA)
    vec3 <- c(1:3, Inf)
    vec4 <- 1:3 / 3
    vec5 <- c(1, 2, NA, -Inf)
    log1 <- c(TRUE, FALSE, NA)
    log2 <- c(TRUE, FALSE, FALSE)
    mat2 <- base::matrix(vec5)
    mat3 <- base::matrix(log1)
    mat4 <- base::matrix(log2)
    fac1 <- base::factor(vec5)
    tab1 <- base::table(fac1)
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(max2(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(max2(x = vec5, na_rm = TRUE, finite = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferTool:::max2()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()") # errors on the missing x default, but the function_name block is executed first
    #### end function name

    ########  arg with no default values
    testthat::expect_error(max2())
    # the missing() based block cannot fire when x is provided: nothing more to test here
    ########  end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(max2(x = NULL, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = NULL, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(max2(x = character(), na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = integer(), na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = double(), na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = logical(), na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = complex(), na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = data.frame(), na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = list(), na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = integer(), finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = double(), finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = logical(), finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = complex(), finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = data.frame(), finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = list(), finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = list(), lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(max2(x = NA, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = NA, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_no_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    # lib_path is checked even when safer_check is FALSE (backbone v21.2: lib_path used outside if(safer_check == TRUE){}, so the lib_path check is always executed)
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = FALSE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # x (two arg_check calls combined into a single manual error block)
    testthat::expect_error(max2(x = factor1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = "a", na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = expr1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = fun1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = list(1), na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = data.frame(a = 1:2), na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # mode numeric
    testthat::expect_no_error(max2(x = mat1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # mode numeric
    testthat::expect_no_error(max2(x = tab1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # mode numeric
    testthat::expect_no_error(max2(x = log2, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # mode logical
    testthat::expect_no_error(max2(x = mat3, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # mode logical
    # end x
    # na_rm
    testthat::expect_error(max2(x = vec1, na_rm = "a", finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = c(TRUE, FALSE), finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = 1, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = mat1, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = factor1, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = expr1, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = fun1, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(max2(x = vec1, na_rm = FALSE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end na_rm
    # finite
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = "a", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(max2(x = vec1, na_rm = TRUE, finite = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(max2(x = vec1, na_rm = TRUE, finite = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(max2(x = vec1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end finite
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
    # basic vector
    testthat::expect_equal(object = max2(x = vec1, na_rm = FALSE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = 3)
    # NA kept by default
    testthat::expect_true(object = base::is.na(max2(x = vec2, na_rm = FALSE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")))
    # na_rm = TRUE removes NA
    testthat::expect_equal(object = max2(x = vec2, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = 3)
    # Inf kept by default
    testthat::expect_equal(object = max2(x = vec3, na_rm = FALSE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = Inf)
    # finite = TRUE removes Inf (and -Inf)
    testthat::expect_equal(object = max2(x = vec3, na_rm = FALSE, finite = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = 3)
    # NA and -Inf together: finite = TRUE removes -Inf but NA remains
    testthat::expect_true(object = base::is.na(max2(x = vec5, na_rm = FALSE, finite = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")))
    # NA and -Inf together with both flags
    testthat::expect_equal(object = max2(x = vec5, na_rm = TRUE, finite = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = 2)
    # logical vectors: NA kept by default, removed with na_rm = TRUE (TRUE -> 1, FALSE -> 0 in max/min family)
    testthat::expect_true(object = base::is.na(max2(x = log1, na_rm = FALSE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")))
    testthat::expect_equal(object = max2(x = log1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = 1)
    testthat::expect_equal(object = max2(x = log2, na_rm = FALSE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = 1)
    # matrix input
    testthat::expect_equal(object = max2(x = mat2, na_rm = TRUE, finite = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = 2)
    testthat::expect_equal(object = max2(x = mat3, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = 1)
    testthat::expect_equal(object = max2(x = mat4, na_rm = FALSE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = 1)
    # table input: max works on tables
    testthat::expect_equal(object = max2(x = tab1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = 1)
    testthat::expect_equal(object = base::typeof(x = max2(x = tab1, na_rm = TRUE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")), expected = "integer")
    # proportions (double)
    testthat::expect_equal(object = max2(x = vec4, na_rm = FALSE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = 1)
    # 1D matrix of integers
    testthat::expect_equal(object = max2(x = mat1, na_rm = FALSE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = 3)
    testthat::expect_equal(object = base::typeof(x = max2(x = vec1, na_rm = FALSE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")), expected = "integer")
    # factor input: not caught by the arg_check x branch (mode "numeric" via mode(numeric_factor)), so the error comes from base::max()
    testthat::expect_error(object = max2(x = fac1, na_rm = FALSE, finite = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
