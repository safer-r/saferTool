testthat::test_that("df_remod()", {

    ## data argument values
    num1 <- 1.5
    int1 <- 3L
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor("a")
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    df1 <- base::data.frame(col1 = (1:4)*10, col2 = 5:8, stringsAsFactors = TRUE) ; base::rownames(df1) <- base::paste0("row", 1:4)
    df2 <- base::data.frame(a = c("x", "x", "y"), b = c(1, 2, 3), stringsAsFactors = TRUE)
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(df_remod(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferTool:::df_remod()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()") # errors on the missing df default, but the function_name block is executed first
    #### end function name

    ########  arg with no default values
    testthat::expect_error(df_remod())
    df <- df1
    testthat::expect_error(df_remod()) # df in the frame is not enough: the block checks the call arguments
    base::rm(df)
    ########  end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(df_remod(df = NULL, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = NULL, quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(df_remod(df = character(), quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = integer(), quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = double(), quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = logical(), quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = complex(), quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = list(), quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = character(), quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = integer(), quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = double(), quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = logical(), quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = complex(), quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = list(), quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = list(), lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(df_remod(df = NA, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = NA, quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_no_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # df
    testthat::expect_error(df_remod(df = fun1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = expr1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = factor1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = mat1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end df
    # quanti_col_name
    testthat::expect_error(df_remod(df = df1, quanti_col_name = 1, quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = c("a", "b"), quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = "")) # length must be 1
    testthat::expect_error(df_remod(df = df1, quanti_col_name = TRUE, quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = expr1, quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = fun1, quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end quanti_col_name
    # quali_col_name
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = c("a", "b"), safer_check = TRUE, lib_path = NULL, error_text = "")) # length must be 1
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end quali_col_name
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "", safer_check = TRUE, lib_path = NULL, error_text = ""))
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
    testthat::expect_error(df_remod(df = base::data.frame(a = c("x"), b = c("y"), stringsAsFactors = TRUE), quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = "")) # 2 character cols
    testthat::expect_error(df_remod(df = base::data.frame(a = c("x"), b = c(1), stringsAsFactors = TRUE), quanti_col_name = "q2", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = "")) # character col with custom names
    testthat::expect_error(df_remod(df = base::data.frame(a = c("x"), b = c(1), stringsAsFactors = TRUE), quanti_col_name = "quanti", quali_col_name = "k2", safer_check = TRUE, lib_path = NULL, error_text = "")) # character col with custom names
    testthat::expect_error(df_remod(df = base::data.frame(a = 1:2, b = 3:4, c = c("x", "y"), stringsAsFactors = TRUE), quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = "")) # 3 cols with one character
    testthat::expect_error(df_remod(df = base::data.frame(a = c("x", "y"), stringsAsFactors = TRUE), quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = "")) # 1 character col
    dfr_withname <- base::data.frame(a = 1:2, b = 3:4, stringsAsFactors = TRUE)
    testthat::expect_no_error(df_remod(df = dfr_withname, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = "")) # numeric df WITH column names: OK
    dfr_noname <- base::as.data.frame(base::matrix(1:4, ncol = 2), stringsAsFactors = TRUE) # V1/V2 auto names first
    base::names(dfr_noname) <- NULL
    testthat::expect_error(df_remod(df = dfr_noname, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = "")) # numeric df without column names
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # transfo 1: all numeric with rownames
    result <- df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = base::names(x = result), expected = c("quanti", "quali", "ini_rowname"))
    testthat::expect_equal(object = result$quanti, expected = base::c(10, 20, 30, 40, 5, 6, 7, 8))
    testthat::expect_equal(object = base::as.character(x = result$quali), expected = base::rep(c("col1", "col2"), each = 4))
    testthat::expect_equal(object = base::as.character(x = result$ini_rowname), expected = base::rep(base::paste0("row", 1:4), times = 2))
    # transfo 1 without rownames: ini_rowname column still added (row indices used as names)
    df3 <- base::data.frame(a = 1:3, b = 4:6, stringsAsFactors = TRUE)
    result2 <- df_remod(df = df3, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = base::names(x = result2), expected = c("quanti", "quali", "ini_rowname"))
    testthat::expect_equal(object = result2$quanti, expected = c(1, 2, 3, 4, 5, 6))
    testthat::expect_equal(object = base::as.character(x = result2$quali), expected = base::rep(c("a", "b"), each = 3))
    testthat::expect_equal(object = base::as.character(x = result2$ini_rowname), expected = base::as.character(x = base::rep(1:3, times = 2)))
    # transfo 1 with auto names (as.data.frame(matrix()) gives V1/V2)
    df4 <- base::as.data.frame(base::matrix(1:4, ncol = 2), stringsAsFactors = TRUE)
    result3 <- df_remod(df = df4, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = base::as.character(x = result3$quali), expected = base::rep(c("V1", "V2"), each = 2))
    # transfo 1 with custom column names
    result4 <- df_remod(df = df3, quanti_col_name = "q2", quali_col_name = "k2", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = base::names(x = result4), expected = c("q2", "k2", "ini_rowname"))
    # transfo 2: 1 numeric + 1 character col (character first: reordered to quanti/quali before split); shorter groups padded with NA
    result5 <- df_remod(df = df2, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = base::names(x = result5), expected = c("x", "y"))
    testthat::expect_equal(object = base::as.character(x = result5$x), expected = c("1", "2"))
    testthat::expect_equal(object = base::as.character(x = result5$y), expected = c("3", NA_character_))
    # transfo 2: factor column converted to character first (x values 1 and 2 in groups x and y)
    df5 <- base::data.frame(f = base::factor(c("x", "y")), n = 1:2, stringsAsFactors = TRUE)
    result6 <- df_remod(df = df5, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = base::names(x = result6), expected = c("x", "y"))
    testthat::expect_equal(object = base::as.character(x = result6$x), expected = "1")
    testthat::expect_equal(object = base::as.character(x = result6$y), expected = "2")
    # transfo 2: numeric first, character second (no reordering)
    df6 <- base::data.frame(a = c(1, 2, 3), b = c("x", "x", "y"), stringsAsFactors = TRUE)
    result7 <- df_remod(df = df6, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = base::names(x = result7), expected = c("x", "y"))
    testthat::expect_equal(object = base::as.character(x = result7$x), expected = c("1", "2"))
    testthat::expect_equal(object = base::as.character(x = result7$y), expected = c("3", NA_character_))
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
