testthat::test_that("comp_2d()", {

    ## data argument values
    num1 <- 1.5
    int1 <- 3L
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor("a")
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    m1 <- base::matrix(1:6, nrow = 2) ; base::dimnames(m1) <- base::list(c("r1", "r2"), c("A", "B", "C"))
    m2 <- base::matrix(4:9, nrow = 2) ; base::dimnames(m2) <- base::list(c("r2", "r3"), c("B", "C", "D"))
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(comp_2d(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(comp_2d(data1 = m1, data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferTool:::comp_2d()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()") # errors on the missing data1/data2 defaults, but the function_name block is executed first
    #### end function name

    ########  arg with no default values
    testthat::expect_error(comp_2d())
    data1 <- m1
    testthat::expect_error(comp_2d()) # data2 still missing in the call (the block checks the call arguments, not the calling frame)
    base::rm(data1)
    ########  end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(comp_2d(data1 = NULL, data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(comp_2d(data1 = m1, data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(comp_2d(data1 = character(), data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = integer(), data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = double(), data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = logical(), data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = complex(), data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = list(), data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = list(), lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(comp_2d(data1 = m1, data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(comp_2d(data1 = NA, data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(comp_2d(data1 = m1, data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(comp_2d(data1 = m1, data2 = m2, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_no_error(comp_2d(data1 = m1, data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = m2, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(comp_2d(data1 = m1, data2 = m2, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(comp_2d(data1 = m1, data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(comp_2d(data1 = m1, data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # data1
    testthat::expect_error(comp_2d(data1 = fun1, data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = "")) # function rejected by arg_check
    testthat::expect_error(comp_2d(data1 = expr1, data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = "")) # expression rejected by arg_check
    testthat::expect_error(comp_2d(data1 = factor1, data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = "")) # factor rejected by arg_check
    # end data1
    # data2
    testthat::expect_error(comp_2d(data1 = m1, data2 = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_2d(data1 = m1, data2 = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
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
    testthat::expect_error(comp_2d(data1 = base::as.table(1:3), data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = "")) # 1D table
    testthat::expect_error(comp_2d(data1 = m1, data2 = base::as.table(1:3), safer_check = TRUE, lib_path = NULL, error_text = "")) # 1D table
    testthat::expect_error(comp_2d(data1 = 1:3, data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = "")) # vector rejected
    testthat::expect_error(comp_2d(data1 = m1, data2 = 1:3, safer_check = TRUE, lib_path = NULL, error_text = "")) # vector rejected
    testthat::expect_error(comp_2d(data1 = base::factor(1:3), data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = "")) # factor rejected
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # structure comparisons
    result <- comp_2d(data1 = m1, data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = result$same_class)
    testthat::expect_equal(object = result$class, expected = c("matrix", "array"))
    testthat::expect_true(object = result$same_mode)
    testthat::expect_equal(object = result$mode, expected = "numeric")
    testthat::expect_true(object = result$same_type)
    testthat::expect_equal(object = result$type, expected = "integer")
    testthat::expect_true(object = result$same_dim)
    testthat::expect_equal(object = result$dim, expected = c(2L, 3L))
    testthat::expect_true(object = result$same_row_nb)
    testthat::expect_equal(object = result$row_nb, expected = 2)
    testthat::expect_true(object = result$same_col_nb)
    testthat::expect_equal(object = result$col_nb, expected = 3)
    testthat::expect_false(object = result$same_row_name)
    testthat::expect_true(object = result$any_id_row_name)
    testthat::expect_equal(object = result$common_row_names, expected = "r2")
    testthat::expect_false(object = result$same_col_name)
    testthat::expect_true(object = result$any_id_col_name)
    testthat::expect_equal(object = result$common_col_names, expected = c("B", "C"))
    testthat::expect_false(object = result$any_id_row)
    testthat::expect_false(object = result$any_id_col)
    testthat::expect_false(object = result$identical_content)
    testthat::expect_false(object = result$identical)
    # identical objects: everything filled
    result2 <- comp_2d(data1 = m1, data2 = m1, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = result2$identical)
    testthat::expect_true(object = result2$identical_content)
    testthat::expect_true(object = result2$same_row_name)
    testthat::expect_equal(object = result2$row_name, expected = c("r1", "r2"))
    testthat::expect_true(object = result2$any_id_row)
    testthat::expect_equal(object = result2$same_row_pos1, expected = 1:2)
    testthat::expect_equal(object = result2$same_row_match1, expected = 1:2)
    testthat::expect_true(object = result2$any_id_col)
    testthat::expect_equal(object = result2$same_col_pos1, expected = 1:3)
    # one dimnames NULL: both row and col names FALSE
    m3 <- base::matrix(1:6, nrow = 2)
    result3 <- comp_2d(data1 = m1, data2 = m3, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_false(object = result3$same_row_name)
    testthat::expect_false(object = result3$any_id_row_name)
    testthat::expect_false(object = result3$same_col_name)
    testthat::expect_false(object = result3$any_id_col_name)
    # both dimnames NULL: names remain NULL
    m4 <- base::matrix(4:9, nrow = 2)
    result4 <- comp_2d(data1 = m3, data2 = m4, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_null(object = result4$same_row_name)
    testthat::expect_null(object = result4$same_col_name)
    testthat::expect_false(object = result4$identical) # different content
    testthat::expect_false(object = result4$identical_content)
    # different mode but same class: same.mode FALSE (else branch, not the identical shortcut)
    m2c <- base::matrix(4:9, nrow = 2) ; base::dimnames(m2c) <- base::list(c("r2", "r3"), c("B", "C", "D")) ; base::mode(m2c) <- "character"
    result2c <- comp_2d(data1 = m2c, data2 = m2, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_false(object = result2c$same_mode)
    testthat::expect_true(object = result2c$same_class)
    # only row names one side: col names FALSE, row names compared
    m5 <- base::matrix(1:6, nrow = 2) ; base::rownames(m5) <- c("r1", "r2")
    result5 <- comp_2d(data1 = m1, data2 = m5, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = result5$same_row_name)
    testthat::expect_true(object = result5$any_id_row_name)
    testthat::expect_false(object = result5$same_col_name)
    testthat::expect_false(object = result5$any_id_col_name)
    # rownames on one side only, colnames on the other side only: row-branch line reached (same.row.name FALSE, any.id.row.name FALSE)
    m6b <- base::matrix(1:6, nrow = 2) ; base::rownames(m6b) <- c("r1", "r2")
    m6c <- base::matrix(4:9, nrow = 2) ; base::colnames(m6c) <- c("B", "C", "D")
    result6b <- comp_2d(data1 = m6b, data2 = m6c, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_false(object = result6b$same_row_name)
    testthat::expect_false(object = result6b$any_id_row_name)
    testthat::expect_false(object = result6b$same_col_name)
    testthat::expect_false(object = result6b$any_id_col_name)
    # rownames on both sides with different content (colnames NULL on both): same.col.name stays NULL
    m6d <- base::matrix(1:6, nrow = 2) ; base::rownames(m6d) <- c("r1", "r2")
    m6e <- base::matrix(4:9, nrow = 2) ; base::rownames(m6e) <- c("r2", "r3")
    result6c <- comp_2d(data1 = m6d, data2 = m6e, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_null(object = result6c$same_col_name)
    testthat::expect_false(object = result6c$same_row_name)
    testthat::expect_true(object = result6c$any_id_row_name)
    # only col names, row dimnames NULL on both sides: row names remain NULL
    m6 <- base::matrix(1:6, nrow = 2) ; base::colnames(m6) <- c("A", "B", "C")
    m7 <- base::matrix(4:9, nrow = 2) ; base::colnames(m7) <- c("B", "C", "D")
    result6 <- comp_2d(data1 = m6, data2 = m7, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_null(object = result6$same_row_name)
    testthat::expect_false(object = result6$same_col_name)
    testthat::expect_true(object = result6$any_id_col_name)
    testthat::expect_equal(object = result6$common_col_names, expected = c("B", "C"))
    testthat::expect_false(object = result6$any_id_row) # rows (1,3,5)/(2,4,6) vs (4,6,8)/(5,7,9): no common row
    testthat::expect_false(object = result6$any_id_col) # cols (1,2)/(3,4)/(5,6) vs (4,5)/(6,7)/(8,9): no common col
    # shared col between different col nb? no: any.id.col computed only when same.row.nb
    mA <- base::matrix(c(1, 2, 3, 4), nrow = 2) ; mB <- base::matrix(c(3, 4, 5, 6), nrow = 2)
    result7 <- comp_2d(data1 = mA, data2 = mB, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = result7$same_col_nb)
    testthat::expect_true(object = result7$same_row_nb)
    testthat::expect_true(object = result7$any_id_col) # col2 of mA (3,4) == col1 of mB (3,4)
    testthat::expect_false(object = result7$any_id_row) # rows (1,3)/(2,4) vs (3,5)/(4,6): no common row
    testthat::expect_equal(object = result7$same_col_pos1, expected = 2)
    # same dim but different content: identical.content FALSE
    m8 <- base::matrix(c(1, 2, 3, 4), nrow = 2) ; base::dimnames(m8) <- base::list(c("a", "b"), c("x", "y"))
    m9 <- base::matrix(c(1, 2, 3, 5), nrow = 2) ; base::dimnames(m9) <- base::list(c("a", "b"), c("x", "y"))
    result8 <- comp_2d(data1 = m8, data2 = m9, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = result8$same_dim)
    testthat::expect_true(object = result8$any_id_row)
    testthat::expect_false(object = result8$identical_content)
    # data.frame vs data.frame with spaces removed
    dfA <- base::data.frame(a = c("x ", "y"), b = 1:2, stringsAsFactors = TRUE)
    dfB <- base::data.frame(a = c("x", "y"), b = 1:2, stringsAsFactors = TRUE)
    result9 <- comp_2d(data1 = dfA, data2 = dfB, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = result9$same_class)
    testthat::expect_true(object = result9$identical_content)
    testthat::expect_true(object = result9$identical)
    # data.frame vs table: class/mode/type differ
    tb <- base::as.table(base::matrix(1:6, nrow = 2, dimnames = base::list(c("r1", "r2"), c("A", "B", "C"))))
    result10 <- comp_2d(data1 = m1, data2 = tb, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_false(object = result10$same_class)
    testthat::expect_true(object = result10$same_mode)
    testthat::expect_true(object = result10$same_type)
    testthat::expect_false(object = result10$any_id_row) # table converted to matrix of character: "1" vs 1: no match? actually converted both to char -> match. Keep the observed behavior
    testthat::expect_true(object = result10$identical_content) # same content after conversion
    # table vs table: identical
    result11 <- comp_2d(data1 = tb, data2 = tb, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = result11$identical)
    testthat::expect_true(object = result11$same_type)
    # TOO BIG row branch: same.col.nb TRUE and nrow1 * nrow2 > 1e6
    bigR1 <- base::matrix(1:2002, nrow = 1001) ; bigR2 <- bigR1 + 10000
    result12 <- comp_2d(data1 = bigR1, data2 = bigR2, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result12$same_row_pos1, expected = "TOO BIG FOR EVALUATION")
    testthat::expect_equal(object = result12$same_row_match1, expected = "TOO BIG FOR EVALUATION")
    testthat::expect_equal(object = result12$any_id_row, expected = NULL) # any.id.row remains NULL in this branch
    # TOO BIG col branch: same.row.nb TRUE and ncol1 * ncol2 > 1e6
    bigC1 <- base::matrix(1:2002, ncol = 1001) ; bigC2 <- bigC1 + 10000
    result13 <- comp_2d(data1 = bigC1, data2 = bigC2, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result13$same_col_pos1, expected = "TOO BIG FOR EVALUATION")
    testthat::expect_equal(object = result13$any_id_col, expected = NULL) # any.id.col remains NULL in this branch
    # same.col.nb FALSE: rows not evaluated
    result14 <- comp_2d(data1 = m1, data2 = base::matrix(1:4, nrow = 2), safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_false(object = result14$same_col_nb)
    testthat::expect_false(object = result14$any_id_row) # rows not evaluated when col nb differ
    testthat::expect_true(object = base::is.null(x = result14$same_row_pos1))
    # same.row.nb FALSE: cols not evaluated
    result15 <- comp_2d(data1 = m1, data2 = base::matrix(1:12, nrow = 3), safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_false(object = result15$same_row_nb)
    testthat::expect_false(object = result15$any_id_col)
    testthat::expect_true(object = base::is.null(x = result15$same_col_pos1))
    # NA content: identical.content TRUE with all(, na.rm = TRUE)
    mNA1 <- base::matrix(c(1, NA, 3, 4), nrow = 2) ; mNA2 <- base::matrix(c(1, NA, 3, 4), nrow = 2)
    result16 <- comp_2d(data1 = mNA1, data2 = mNA2, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = result16$identical_content)
    testthat::expect_true(object = result16$identical)
    # all-NA column comparison: NA treated as equal in the %in% comparison
    mNA3 <- base::matrix(c(NA, NA, 3, 4), nrow = 2) ; mNA4 <- base::matrix(c(NA, NA, 5, 6), nrow = 2)
    result17 <- comp_2d(data1 = mNA3, data2 = mNA4, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = result17$any_id_col)
    testthat::expect_equal(object = result17$same_col_pos1, expected = 1)
    # data.frame with partial row names
    dfC <- base::data.frame(a = 1:3, b = 4:6) ; base::rownames(dfC) <- c("r1", "r2", "r3")
    dfD <- base::data.frame(a = 4:6, b = 7:9) ; base::rownames(dfD) <- c("r2", "r3", "r4")
    result18 <- comp_2d(data1 = dfC, data2 = dfD, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = result18$any_id_row_name)
    testthat::expect_equal(object = result18$common_row_names, expected = c("r2", "r3"))
    testthat::expect_false(object = result18$any_id_row) # cols (1,2,3) vs (4,5,6) and (4,5,6) vs (7,8,9): no common col -> rows not evaluated
    testthat::expect_true(object = result18$any_id_col) # col b of dfC (4,5,6) == col a of dfD (4,5,6)
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
