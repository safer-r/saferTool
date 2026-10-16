testthat::test_that("sample2()", {

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
    df1 <- base::data.frame(a = 1:4, b = c("a", "b", "c", "d"), stringsAsFactors = TRUE)
    list1 <- base::list(a = 1, b = 2, c = 3)
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(sample2(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = 1, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferTool:::sample2()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()") # errors on the missing x default, but the function_name block is executed first
    #### end function name

    ########  arg with no default values
    testthat::expect_error(sample2())
    # the missing() based block cannot fire when x is provided: nothing more to test here
    ########  end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(sample2(x = NULL, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = NULL, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(sample2(x = character(), size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = integer(), size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = double(), size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = logical(), size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = complex(), size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = data.frame(), size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = list(), size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = integer(), replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = double(), replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = logical(), replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = complex(), replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = data.frame(), replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = list(), replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = integer(), prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = double(), prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = data.frame(), prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = list(), prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = integer(), set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = logical(), set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = data.frame(), set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = list(), set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = list(), lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(sample2(x = NA, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = NA, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = NA, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NA, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # x not checked by arg_check() because it can be any object that has a length and that can be subsetted using the [ operator, like in base::sample()
    # size
    testthat::expect_error(sample2(x = vec1, size = "a", replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = c(2, 3), replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = mat1, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = factor1, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = expr1, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = fun1, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = list(2), replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = data.frame(a = 2), replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = -2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2.5, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = TRUE, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # integer
    testthat::expect_no_error(sample2(x = vec1, size = 2.0, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # double with 0 modulo
    testthat::expect_no_error(sample2(x = vec1, size = NULL, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end size
    # replace
    testthat::expect_error(sample2(x = vec1, size = 2, replace = "a", prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = c(TRUE, FALSE), prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = 1, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = mat1, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = factor1, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = expr1, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = fun1, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = TRUE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end replace
    # prob
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = "a", set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = TRUE, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = mat1, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = factor1, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = expr1, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = fun1, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = list(0.5), set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = data.frame(a = 0.5), set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = c(0.5, -0.5, 0.5, 0.5, 0.5), set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = c(0.5, Inf, 0.5, 0.5, 0.5), set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = c(0.5, -Inf, 0.5, 0.5, 0.5), set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = FALSE, prob = c(1:3 / 3, 0.5, 0.5), set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # prob with the same length as x (vec4 has only 3 values, so extended here; the prob length checking of the other checkings section is tested below)
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = FALSE, prob = c(0.5, 0.5, 0.5, 0.5, 0.5), set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = FALSE, prob = c(0.5, 1.0, 0.5, 0.5, 0.5), set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # integer like doubles allowed (mode numeric)
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end prob
    # set_seed
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = "a", safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = c(1, 2), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = mat1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = as.factor(1.5), safer_check = TRUE, lib_path = NULL, error_text = "")) # factor has class "factor" but typeof "integer" and length 1: the arg_check with typeof = "integer" and double_as_integer_allowed = TRUE does not report a problem because a factor of a numeric value is integer like. This is an accepted limitation of the typeof-based checking of set_seed (like in base::set.seed(), which accepts anything coercible to an integer). The class of factors is not checked to keep the checking consistent with the double_as_integer_allowed semantics
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = expr1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = list(1), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = data.frame(a = 1), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = TRUE, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = 1.5, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = 1, safer_check = TRUE, lib_path = NULL, error_text = "")) # integer
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = 1.0, safer_check = TRUE, lib_path = NULL, error_text = "")) # double with 0 modulo
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = -1, safer_check = TRUE, lib_path = NULL, error_text = "")) # negative seeds are valid in base::set.seed()
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end set_seed
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## code that protects set.seed() in the global environment
    # restore of .Random.seed when it exists before the call
    base::set.seed(seed = 100)
    seed_before <- base::get(x = ".Random.seed", envir = base::globalenv())
    result1 <- sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = 1, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_identical(object = base::get(x = ".Random.seed", envir = base::globalenv()), expected = seed_before) # .Random.seed restored after function execution
    testthat::expect_identical(object = sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = 1, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = result1) # reproducibility with the same set_seed value
    # no restore (and no restore error) when set_seed = NULL: the random generator state is directly used and modified, like in base::sample()
    base::set.seed(seed = 100)
    seed_before2 <- base::get(x = ".Random.seed", envir = base::globalenv())
    result2 <- sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_false(object = base::identical(x = base::get(x = ".Random.seed", envir = base::globalenv()), y = seed_before2)) # .Random.seed modified, like in base::sample()
    testthat::expect_true(object = base::is.vector(x = result2)) # the sampling itself worked
    # .Random.seed creation when it does not exist before the call: the initial state (absence of .Random.seed) is restored after function execution
    base::rm(list = ".Random.seed", envir = base::globalenv())
    testthat::expect_false(object = base::exists(x = ".Random.seed", envir = base::globalenv(), inherits = FALSE))
    result3 <- sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = 1, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = base::is.vector(x = result3)) # the sampling itself worked
    testthat::expect_false(object = base::exists(x = ".Random.seed", envir = base::globalenv(), inherits = FALSE)) # .Random.seed absent again after function execution
    # seed value out of the base::set.seed() integer range
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = Inf, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = 2^40, safer_check = TRUE, lib_path = NULL, error_text = "")) # 2^40 has a 0 modulo (so passes the arg_check with double_as_integer_allowed = TRUE) but is out of the base::set.seed() integer range
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ######## end warning initiation

    ######## graphic device checking
    ######## end graphic device checking

    ######## other checkings
    # data preparation: size = NULL means that all the elements of x are sampled exactly once
    testthat::expect_equal(object = base::sort(x = sample2(x = vec1, size = NULL, replace = FALSE, prob = NULL, set_seed = 1, safer_check = TRUE, lib_path = NULL, error_text = "")), expected = vec1)
    # prob length checking
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = c(0.5, 0.5), set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(sample2(x = vec1, size = 2, replace = FALSE, prob = c(0.5, 0.5, 0.5, 0.5, 0.5), set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # error path of base::sample.int(): all-zero probabilities
    testthat::expect_error(sample2(x = vec1, size = 2, replace = FALSE, prob = c(0, 0, 0, 0, 0), set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    # basic vector
    testthat::expect_true(object = base::all(sample2(x = vec1, size = 2, replace = FALSE, prob = NULL, set_seed = 1, safer_check = TRUE, lib_path = NULL, error_text = "") %in% vec1))
    # the correction of the confusing behavior of base::sample() when x is made of a single positive numeric value
    base::set.seed(seed = 1)
    testthat::expect_equal(object = sample2(x = 4, size = 3, replace = TRUE, prob = NULL, set_seed = 1, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = c(4, 4, 4)) # unlike base::sample(x = 4, size = 3, replace = TRUE) which returns 3 draws between 1 and 4
    testthat::expect_error(sample2(x = 4, size = 3, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # the single value 4 cannot be sampled 3 times without replacement, unlike base::sample(x = 4, size = 3, replace = FALSE) which returns 3 distinct values between 1 and 4 without error
    testthat::expect_equal(object = sample2(x = 4, size = 1, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = 4) # single value sampled once without replacement
    testthat::expect_equal(object = sample2(x = 4, size = 0, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = base::double(length = 0)) # empty sampling of a single value (x = 4 is of type double, so the output is numeric(0), like in base::sample(x = 4, size = 0, replace = FALSE))
    testthat::expect_equal(object = sample2(x = 4, size = NULL, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = 4) # size = NULL on a single value: the single value is sampled exactly once, unlike base::sample(x = 4) which draws 4 values between 1 and 4
    testthat::expect_equal(object = sample2(x = 4.5, size = 3, replace = TRUE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = c(4.5, 4.5, 4.5)) # single decimal value: value itself, unlike base::sample(x = 4.5, size = 3, replace = TRUE) which draws 3 values between 1 and 4
    testthat::expect_error(sample2(x = 4.5, size = 3, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = "")) # like for the single value 4: the single value 4.5 cannot be sampled 3 times without replacement
    testthat::expect_equal(object = sample2(x = 4.5, size = 1, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = 4.5) # a single value can be sampled once without replacement
    # equivalence with base::sample() for all the other x values
    testthat::expect_equal(object = sample2(x = vec1, size = 3, replace = FALSE, prob = NULL, set_seed = 42, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = {base::set.seed(seed = 42) ; base::sample(x = vec1, size = 3, replace = FALSE)})
    testthat::expect_equal(object = sample2(x = c("a", "b", "c"), size = 4, replace = TRUE, prob = NULL, set_seed = 42, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = {base::set.seed(seed = 42) ; base::sample(x = c("a", "b", "c"), size = 4, replace = TRUE)})
    testthat::expect_equal(object = sample2(x = mat2, size = 2, replace = FALSE, prob = NULL, set_seed = 42, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = {base::set.seed(seed = 42) ; base::sample(x = mat2, size = 2, replace = FALSE)})
    testthat::expect_equal(object = sample2(x = log2, size = 2, replace = FALSE, prob = NULL, set_seed = 42, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = {base::set.seed(seed = 42) ; base::sample(x = log2, size = 2, replace = FALSE)})
    testthat::expect_equal(object = sample2(x = fac1, size = 4, replace = TRUE, prob = NULL, set_seed = 42, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = {base::set.seed(seed = 42) ; base::sample(x = fac1, size = 4, replace = TRUE)})
    testthat::expect_equal(object = sample2(x = tab1, size = 2, replace = FALSE, prob = NULL, set_seed = 42, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = {base::set.seed(seed = 42) ; base::sample(x = tab1, size = 2, replace = FALSE)})
    testthat::expect_equal(object = sample2(x = df1, size = 2, replace = FALSE, prob = NULL, set_seed = 42, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = {base::set.seed(seed = 42) ; base::sample(x = df1, size = 2, replace = FALSE)})
    testthat::expect_equal(object = sample2(x = list1, size = 2, replace = FALSE, prob = NULL, set_seed = 42, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = {base::set.seed(seed = 42) ; base::sample(x = list1, size = 2, replace = FALSE)})
    testthat::expect_equal(object = sample2(x = TRUE, size = 3, replace = TRUE, prob = NULL, set_seed = 42, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = {base::set.seed(seed = 42) ; base::sample(x = TRUE, size = 3, replace = TRUE)})
    testthat::expect_equal(object = sample2(x = num1, size = 3, replace = TRUE, prob = NULL, set_seed = 42, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = c(1.5, 1.5, 1.5)) # single decimal value below 1: value itself, unlike base::sample(x = 1.5, size = 3, replace = TRUE) which draws 3 values between 1 and 1 (sample.int(1, ...))
    # x with NA and Inf values (accepted, like in base::sample())
    testthat::expect_true(object = base::all(sample2(x = vec2, size = 3, replace = FALSE, prob = NULL, set_seed = 42, safer_check = TRUE, lib_path = NULL, error_text = "") %in% vec2)) # NA values are sampled like any other element
    testthat::expect_equal(object = base::sort(x = sample2(x = vec2, size = NULL, replace = FALSE, prob = NULL, set_seed = 42, safer_check = TRUE, lib_path = NULL, error_text = ""), na.last = TRUE), expected = vec2) # na.last = TRUE to keep the NA at the end, like in vec2
    testthat::expect_true(object = base::all(sample2(x = vec3, size = 3, replace = FALSE, prob = NULL, set_seed = 42, safer_check = TRUE, lib_path = NULL, error_text = "") %in% vec3)) # Inf values are sampled like any other element
    # size larger than the length of x with replace = TRUE: allowed, like in base::sample()
    testthat::expect_equal(object = base::length(x = sample2(x = vec1, size = 10, replace = TRUE, prob = NULL, set_seed = 42, safer_check = TRUE, lib_path = NULL, error_text = "")), expected = 10)
    # size larger than the length of x with replace = FALSE: error path of base::sample.int()
    testthat::expect_error(sample2(x = vec1, size = 10, replace = FALSE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # infinite size
    testthat::expect_error(sample2(x = vec1, size = Inf, replace = TRUE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # prob with zero for some elements: zero probability elements are never drawn
    testthat::expect_equal(object = base::unique(x = sample2(x = vec1, size = 10, replace = TRUE, prob = c(0, 1, 0, 0, 0), set_seed = 42, safer_check = TRUE, lib_path = NULL, error_text = "")), expected = 0) # only the second element of vec1 (-1:3, i.e., 0) can be drawn
    # x with a single positive integer but size = 1 with replacement: same value returned (corrected behavior)
    testthat::expect_equal(object = sample2(x = 4, size = 1, replace = TRUE, prob = NULL, set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""), expected = 4)
    # error path of base::sample.int(): negative probability with a correct length (the arg_check prob block is bypassed by the length check, so the error comes from base::sample.int())
    testthat::expect_error(sample2(x = vec4, size = 2, replace = FALSE, prob = c(-0.5, 0.5, 1.0), set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # error path of base::sample.int(): NA in probabilities
    testthat::expect_error(sample2(x = vec4, size = 2, replace = FALSE, prob = c(NA, 0.5, 1.0), set_seed = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
