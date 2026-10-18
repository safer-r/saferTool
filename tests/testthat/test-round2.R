testthat::test_that("round2()", {

    ## data argument values
    num1 <- 1.5
    int1 <- 3L
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor("a")
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    vec_num1 <- c(NA, 10, 100.001, 333.0001254, 12312.1235)
    vec_char1 <- c(NA, "10", "100.001", "333.0001254", "12312.1235")
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(round2(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferTool:::round2()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()") # errors on the missing data default, but the function_name block is executed first
    #### end function name

    ########  arg with no default values
    testthat::expect_error(round2())
    data <- vec_num1
    testthat::expect_error(round2())
    base::rm(data)
    ########  end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(round2(data = NULL, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = NULL, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(round2(data = character(), dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = integer(), dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = double(), dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = logical(), dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = complex(), dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = data.frame(), dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = list(), dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = integer(), after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = double(), after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = logical(), after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = complex(), after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = data.frame(), after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = list(), after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = list(), lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(round2(data = NA, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = NA, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_no_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # data
    testthat::expect_error(round2(data = factor1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = expr1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = fun1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = list(1), dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = c(TRUE, FALSE), dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = Inf, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # Inf not allowed (inf_values = FALSE for numeric)
    testthat::expect_error(round2(data = c(1, Inf), dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # Inf not allowed (inf_values = FALSE for numeric)
    testthat::expect_no_error(round2(data = c(1, NA), dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # NA allowed (na_contain = TRUE)
    testthat::expect_no_error(round2(data = "a", dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # character allowed
    # end data
    # dec_nb
    testthat::expect_error(round2(data = vec_num1, dec_nb = "a", after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = c(TRUE, FALSE), after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = mat1, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = factor1, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = expr1, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = fun1, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2.5, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # not integer (typeof integer not respected)
    testthat::expect_error(round2(data = vec_num1, dec_nb = -1, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # negative not allowed (neg_values = FALSE)
    testthat::expect_no_error(round2(data = vec_num1, dec_nb = 2L, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # integer allowed
    testthat::expect_no_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # double_as_integer_allowed = TRUE
    testthat::expect_no_error(round2(data = vec_num1, dec_nb = 0, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # 0 allowed
    # end dec_nb
    # after_lead_zero
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = 1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = "TRUE", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = c(TRUE, FALSE), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = factor1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(round2(data = vec_num1, dec_nb = 2, after_lead_zero = FALSE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end after_lead_zero
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    testthat::expect_error(round2(data = c("1.5", ""), dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
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
    testthat::expect_error(round2(data = vec_num1, dec_nb = -5, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")) # non negative integer check (neg_values = FALSE)
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # numeric input, default after_lead_zero = TRUE: decimals kept after leading zeros
    result <- round2(data = vec_num1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result, expected = c(NA, 10, 100.001, 333.00013, 12312.12))
    # dec_nb = 3
    result2 <- round2(data = vec_num1, dec_nb = 3, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result2, expected = c(NA, 10, 100.001, 333.000125, 12312.123))
    # character input: returned as character (numbers without "." are not processed)
    result3 <- round2(data = vec_char1, dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result3, expected = c(NA, "10", "100.001", "333.00013", "12312.12"))
    testthat::expect_equal(object = base::mode(x = result3), expected = "character")
    # after_lead_zero = FALSE: no leading-zero protection (100.001 -> 100.00)
    result4 <- round2(data = vec_num1, dec_nb = 2, after_lead_zero = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result4, expected = c(NA, 10, 100, 333, 12312.12))
    # after_lead_zero = TRUE with leading zeros: 0.001 keeps 3 decimals with dec_nb = 2 (count of leading zeros after the dot is added to dec_nb, bounded by the number of decimals)
    result5 <- round2(data = c(0.001, 0.0101, 0.1), dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result5, expected = c(0.001, 0.01, 0.1))
    # after_lead_zero = FALSE with leading zeros: truncated to dec_nb decimals
    result6 <- round2(data = c(0.001, 0.0101, 0.1), dec_nb = 2, after_lead_zero = FALSE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result6, expected = c(0, 0.01, 0.1))
    # dec_nb = 0: no decimals
    result7 <- round2(data = c(1.005, 2.115), dec_nb = 0, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result7, expected = c(1, 2))
    # dec_nb larger than the number of decimals: no change
    result8 <- round2(data = 0.001, dec_nb = 5, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result8, expected = 0.001)
    # integer input (no decimal part detected by grepl)
    result9 <- round2(data = c(1, 2, 3), dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result9, expected = c(1, 2, 3))
    # negative numbers
    result10 <- round2(data = c(-1.555, 0), dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result10, expected = c(-1.55, 0))
    # matrix input: values rounded (as.character() flattens the matrix, so a vector is returned)
    result11 <- round2(data = base::matrix(c(1.111, 2.222)), dec_nb = 1, after_lead_zero = TRUE, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_equal(object = result11, expected = c(1.1, 2.2))
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
