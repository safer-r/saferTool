testthat::test_that("info()", {

    ## data argument values
    num1 <- 1.5
    int1 <- 3L
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor("a")
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    vec1 <- -1:3 # vector of integers
    vec3 <- c(1, 2, NA, -Inf) # vector of double with NA and -Inf
    vec_int <- 1:3 # vector of integers
    vec4 <- "pearson" # vector of characters
    cpx1 <- base::as.complex(1) # complex
    mat2 <- base::matrix(data = c(1:5, NA), ncol = 2, dimnames = base::list(c("ROW1", "ROW2", "ROW3"), c("M1", "M2"))) # 2D matrix with NA
    df1 <- base::as.data.frame(mat2) # data.frame
    l1 <- list(L1 = 1:3, L2 = letters[1:3]) # list
    l2 <- list(a = base::factor(c("x", "y")), b = base::factor(base::ordered(c(1, 2)))) # list with factors
    fac1 <- base::factor(rep(letters[4:6], times = c(4, 5, 6))) # factor
    tab1 <- base::table(fac1) # 1D table
    tab2 <- base::table(fac1, fac1) # 2D table
    env1 <- base::new.env() # environment (empty non NULL object, must be accepted by info())
    sym1 <- base::as.name("expr1") # symbol
    call1 <- base::call(name = "call1") # call
    df2 <- base::data.frame(a = base::factor(c("a", "b")), o = base::factor(base::ordered(c(1, 2), labels = c("low", "high"))), stringsAsFactors = FALSE)
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(info(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferTool:::info()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()") # errors on the missing data default, but the function_name block is executed first
    #### end function name

    ########  arg with no default values
    testthat::expect_error(info())
    # the missing() based block cannot fire when data is provided: nothing more to test here
    ########  end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(info(data = NULL, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # data is not in the empty non NULL list: info() is designed for any object, including empty ones (e.g., environment)
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = list(), lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(info(data = NA, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_no_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    # lib_path is checked even when safer_check is FALSE (backbone v21.2: lib_path used outside if(safer_check == TRUE){}, so the lib_path check is always executed)
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = FALSE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # n: checked only when not NULL (the else branch keeps the checked_arg_names bookkeeping alive without problem)
    testthat::expect_error(info(data = vec1, n = "a", warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = c(1, 2), warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = TRUE, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = mat1, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = factor1, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = expr1, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = fun1, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = 2.5, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # not integer (typeof integer not respected)
    testthat::expect_no_error(info(data = vec1, n = 2L, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # integer allowed
    testthat::expect_no_error(info(data = vec1, n = 2, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # double_as_integer_allowed = TRUE
    testthat::expect_no_error(info(data = vec1, n = NULL, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # NULL allowed (no arg_check)
    # end n
    # warn_print
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = "a", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(info(data = vec1, n = NULL, warn_print = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(info(data = vec1, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(info(data = vec1, n = NULL, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end warn_print
    # data not checked by arg_check() because it can be any object
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
    testthat::expect_error(info(data = vec1, n = -1, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # non negative integer check (neg_values = FALSE)
    testthat::expect_error(info(data = vec1, n = 0, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # n < 1 not allowed
    testthat::expect_no_error(info(data = vec1, n = 1, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")) # 1 allowed (the truncation warning is tested below)
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # NAME: deparse of the data argument
    result <- info(data = vec3, n = NULL, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result[["NAME"]], expected = "vec3")
    # numeric vector with NA and -Inf: INF.NB, RANGE, SUM, MEAN compartments
    testthat::expect_equal(object = base::names(x = result), expected = c("NAME", "CLASS", "TYPE", "LENGTH", "INF.NB", "RANGE", "SUM", "MEAN", "NA.NB", "HEAD", "TAIL", "SUMMARY", "STRUCTURE")) # no WARNING compartment: warn is NULL
    testthat::expect_equal(object = result[["CLASS"]], expected = "numeric")
    testthat::expect_equal(object = result[["TYPE"]], expected = "double")
    testthat::expect_equal(object = result[["LENGTH"]], expected = 4)
    testthat::expect_equal(object = result[["INF.NB"]], expected = 1)
    testthat::expect_equal(object = result[["RANGE"]], expected = c(1, 2))
    testthat::expect_equal(object = result[["SUM"]], expected = 3)
    testthat::expect_equal(object = result[["MEAN"]], expected = 1.5)
    testthat::expect_equal(object = result[["NA.NB"]], expected = 1)
    testthat::expect_equal(object = result[["HEAD"]], expected = vec3)
    testthat::expect_equal(object = result[["TAIL"]], expected = vec3)
    testthat::expect_equal(object = result[["WARNING"]], expected = NULL)
    testthat::expect_false(object = "WARNING" %in% base::names(x = result))
    # integer vector: typeof integer, INF.NB present (typeof integer)
    result2 <- info(data = vec_int, n = NULL, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result2[["TYPE"]], expected = "integer")
    testthat::expect_equal(object = result2[["INF.NB"]], expected = 0)
    testthat::expect_equal(object = result2[["RANGE"]], expected = c(1, 3))
    testthat::expect_equal(object = result2[["SUM"]], expected = 6)
    testthat::expect_equal(object = result2[["MEAN"]], expected = 2)
    testthat::expect_equal(object = result2[["NA.NB"]], expected = 0)
    # character vector: no INF.NB/RANGE/SUM/MEAN, but NA.NB present (typeof character)
    result3 <- info(data = vec4, n = NULL, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result3[["TYPE"]], expected = "character")
    testthat::expect_true(object = base::is.null(x = result3[["INF.NB"]]))
    testthat::expect_true(object = base::is.null(x = result3[["RANGE"]]))
    testthat::expect_true(object = base::is.null(x = result3[["SUM"]]))
    testthat::expect_true(object = base::is.null(x = result3[["MEAN"]]))
    testthat::expect_equal(object = result3[["NA.NB"]], expected = 0)
    # complex vector: NA.NB present (typeof complex)
    result4 <- info(data = cpx1, n = NULL, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result4[["NA.NB"]], expected = 0)
    testthat::expect_true(object = base::is.null(x = result4[["INF.NB"]]))
    # 2D matrix with dimnames: DIMENSION, ROW_NAMES, COL_NAMES
    result5 <- info(data = mat2, n = NULL, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result5[["DIMENSION"]], expected = c(NROW = 3L, NCOL = 2L))
    testthat::expect_equal(object = result5[["ROW_NAMES"]], expected = c("ROW1", "ROW2", "ROW3"))
    testthat::expect_equal(object = result5[["COL_NAMES"]], expected = c("M1", "M2")) # typo COLUM_NAMES of the initial dev version fixed
    testthat::expect_equal(object = result5[["NA.NB"]], expected = 1)
    testthat::expect_equal(object = result5[["INF.NB"]], expected = 0)
    # data.frame: COLUMN_TYPE present, factors handled (including ordered factors)
    result6 <- info(data = df1, n = NULL, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result6[["COLUMN_TYPE"]], expected = base::sapply(X = df1, FUN = "typeof", simplify = TRUE, USE.NAMES = TRUE))
    result7 <- info(data = df2, n = NULL, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result7[["COLUMN_TYPE"]][["a"]], expected = "factor")
    testthat::expect_equal(object = result7[["COLUMN_TYPE"]][["o"]], expected = "ordered factor")
    # data.frame with non ordered factors only (unlist branch of the factor handling)
    df3 <- base::data.frame(a = base::factor(c("x", "y")), b = base::factor(c("k", "l")), stringsAsFactors = FALSE)
    result6b <- info(data = df3, n = NULL, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result6b[["COLUMN_TYPE"]][["a"]], expected = "factor")
    testthat::expect_equal(object = result6b[["COLUMN_TYPE"]][["b"]], expected = "factor")
    # list: COMPARTMENT_NAMES and COMPARTMENT_TYPE
    result8 <- info(data = l1, n = NULL, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result8[["COMPARTMENT_NAMES"]], expected = c("L1", "L2"))
    testthat::expect_equal(object = result8[["COMPARTMENT_TYPE"]], expected = c(L1 = "integer", L2 = "character"))
    testthat::expect_true(object = base::is.null(x = result8[["COLUMN_TYPE"]]))
    # list with factors (including ordered)
    result9 <- info(data = l2, n = NULL, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result9[["COMPARTMENT_TYPE"]][["a"]], expected = "factor")
    testthat::expect_equal(object = result9[["COMPARTMENT_TYPE"]][["b"]], expected = "ordered factor")
    # list with non ordered factors only (unlist branch of the factor handling)
    l3 <- list(a = base::factor(c("x", "y")), b = base::factor(c("k", "l")))
    result9b <- info(data = l3, n = NULL, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result9b[["COMPARTMENT_TYPE"]][["a"]], expected = "factor")
    testthat::expect_equal(object = result9b[["COMPARTMENT_TYPE"]][["b"]], expected = "factor")
    # factor input: no INF.NB (class factor not in the typeof integer/double branch), NA.NB present
    result10 <- info(data = fac1, n = NULL, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.null(x = result10[["INF.NB"]]))
    testthat::expect_equal(object = result10[["NA.NB"]], expected = 0)
    testthat::expect_equal(object = result10[["CLASS"]], expected = "factor")
    # 1D table: DIMENSION present but no ROW_NAMES/COL_NAMES (dim of a 1D table has a single compartment)
    result11 <- info(data = tab1, n = NULL, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result11[["DIMENSION"]], expected = 3L)
    testthat::expect_true(object = base::is.null(x = result11[["ROW_NAMES"]]))
    testthat::expect_true(object = base::is.null(x = result11[["COL_NAMES"]]))
    testthat::expect_equal(object = result11[["INF.NB"]], expected = 0) # typeof table is "integer"
    # 2D table: ROW_NAMES and COL_NAMES present
    result12 <- info(data = tab2, n = NULL, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result12[["ROW_NAMES"]], expected = c("d", "e", "f"))
    testthat::expect_equal(object = result12[["COL_NAMES"]], expected = c("d", "e", "f"))
    # function input: no NA.NB (is.na() on a function returns FALSE with a warning, so the guarded get_message() path is taken)
    result13 <- info(data = fun1, n = NULL, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.null(x = result13[["NA.NB"]]))
    testthat::expect_equal(object = result13[["CLASS"]], expected = "function")
    # environment input (empty non NULL object): works, no NA.NB
    result14 <- info(data = env1, n = NULL, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.null(x = result14[["NA.NB"]]))
    testthat::expect_equal(object = result14[["CLASS"]], expected = "environment")
    testthat::expect_equal(object = result14[["LENGTH"]], expected = 0)
    # symbol input: no NA.NB
    result15 <- info(data = sym1, n = NULL, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.null(x = result15[["NA.NB"]]))
    # call input: no NA.NB
    result16 <- info(data = call1, n = NULL, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.null(x = result16[["NA.NB"]]))
    # truncation with n: compartments truncated except STRUCTURE, warning message stored when warn_print = FALSE
    result17 <- info(data = vec3, n = 1, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result17[["HEAD"]], expected = 1)
    testthat::expect_equal(object = result17[["RANGE"]], expected = 1) # utils::head(x = c(1, 2), n = 1)
    testthat::expect_equal(object = result17[["STRUCTURE"]], expected = result[["STRUCTURE"]]) # STRUCTURE not truncated
    testthat::expect_equal(object = result17[["WARNING"]], expected = "SOME COMPARTMENTS CAN BE TRUNCATED (n ARGUMENT IS 1)")
    # truncation with n and warn_print = TRUE: warning emitted on exit (not stored in the output)
    warn_txt <- base::tryCatch(expr = info(data = vec3, n = 1, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""), warning = function(w){base::conditionMessage(w)})
    testthat::expect_equal(object = warn_txt, expected = "FROM info():\n\nSOME COMPARTMENTS CAN BE TRUNCATED (n ARGUMENT IS 1)")
    # n larger than the compartment length: no change
    result19 <- info(data = vec1, n = 100, warn_print = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result19[["HEAD"]], expected = vec1)
    # warning restoration: warning.length restored after info() (on.exit)
    ini_warning_length <- base::options()$warning.length
    result20 <- info(data = vec1, n = NULL, warn_print = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = base::options()$warning.length, expected = ini_warning_length)
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
