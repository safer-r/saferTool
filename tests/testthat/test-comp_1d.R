testthat::test_that("comp_1d()", {

    ## data argument values
    num1 <- 1.5
    int1 <- 3L
    mat1 <- base::matrix(-1:3)
    factor1 <- base::as.factor("a")
    expr1 <- expression(1)
    fun1 <- function(x){x = 1}
    vec1 <- 1:5
    vec2 <- 3:6
    fac1 <- base::factor(LETTERS[1:5])
    fac2 <- base::factor(LETTERS[4:7])
    fac3 <- base::factor(LETTERS[10:11])
    tab1 <- base::as.table(1:5)
    tab2 <- base::as.table(3:6)
    nvec1 <- vec1 ; base::names(nvec1) <- base::LETTERS[1:5]
    nvec2 <- vec2 ; base::names(nvec2) <- base::LETTERS[1:4]
    ## end data argument values

    ## initialization of tests
    testthat::expect_error(comp_1d(caca = 1)) # to test for the absence of ...
    testthat::expect_no_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = "")) # to test that this example works
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    #### function name
    testthat::expect_error(saferTool:::comp_1d()) # to test if(function_name[1] == "::()" | function_name[1] == ":::()") # errors on the missing data1/data2 defaults, but the function_name block is executed first
    #### end function name

    ########  arg with no default values
    testthat::expect_error(comp_1d())
    data1 <- vec1
    testthat::expect_error(comp_1d()) # data2 still missing in the call (the block checks the call arguments, not the calling frame)
    base::rm(data1)
    ########  end arg with no default values

    ######## management of NULL arguments
    testthat::expect_error(comp_1d(data1 = NULL, data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = NULL, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = NULL, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = NULL))
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    testthat::expect_error(comp_1d(data1 = character(), data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = integer(), data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = double(), data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = logical(), data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = complex(), data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = data.frame(), data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = list(), data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = character(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = integer(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = double(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = logical(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = complex(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = data.frame(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = list(), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = character(), lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = integer(), lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = double(), lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = complex(), lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = data.frame(), lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = list(), lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = TRUE, lib_path = character(), error_text = ""))
    testthat::expect_no_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = character())) # but error_text is converted to ""
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    testthat::expect_error(comp_1d(data1 = NA, data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = NA, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = NA, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = TRUE, lib_path = NA, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = NA))
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = 1, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = c(TRUE, FALSE), lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = mat1, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = factor1, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = expr1, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = fun1, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_no_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ######## end safer_check argument checking

    ######## check of lib_path
    testthat::expect_no_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = TRUE, lib_path = 1, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = TRUE, lib_path = c(TRUE, FALSE), error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = TRUE, lib_path = mat1, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = TRUE, lib_path = factor1, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = TRUE, lib_path = expr1, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = TRUE, lib_path = fun1, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = TRUE, lib_path = "PATH_NOT_GOOD", error_text = ""))
    testthat::expect_no_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = TRUE, lib_path = base:::.libPaths(new = , include.site = TRUE), error_text = ""))
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    testthat::expect_no_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end check of the required functions from the required packages

    ######## critical operator checking
    testthat::expect_no_error(comp_1d(data1 = vec1, data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = "")) # see above for the result comparison
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    # data1
    testthat::expect_no_error(comp_1d(data1 = num1, data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = "")) # single numeric value is a valid 1D vector
    testthat::expect_error(comp_1d(data1 = list(1, 2), data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = mat1, data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = "")) # 2D matrix rejected later in other checkings
    testthat::expect_error(comp_1d(data1 = base::matrix(1:4, ncol = 2), data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = fun1, data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = ""))
    # end data1
    # data2
    testthat::expect_error(comp_1d(data1 = vec1, data2 = list(1, 2), safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = fun1, safer_check = TRUE, lib_path = NULL, error_text = ""))
    testthat::expect_error(comp_1d(data1 = vec1, data2 = base::matrix(1:4, ncol = 2), safer_check = TRUE, lib_path = NULL, error_text = ""))
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
    testthat::expect_error(comp_1d(data1 = base::matrix(1:4, ncol = 2), data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = "")) # 2D table/matrix/array (error_text_start message)
    testthat::expect_error(comp_1d(data1 = vec1, data2 = base::matrix(1:4, ncol = 2), safer_check = TRUE, lib_path = NULL, error_text = "")) # 2D table/matrix/array
    ######## end other checkings
    #### end second round of checking and data preparation

    #### main code
    # identical objects: all fields filled
    result <- comp_1d(data1 = vec1, data2 = vec1, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = result$same_class)
    testthat::expect_equal(object = result$class, expected = "integer")
    testthat::expect_true(object = result$same_length)
    testthat::expect_equal(object = result$length, expected = 5)
    testthat::expect_true(object = result$any_id_elements)
    testthat::expect_equal(object = result$common_elements, expected = vec1)
    testthat::expect_true(object = result$same_order)
    testthat::expect_true(object = result$identical_object)
    testthat::expect_true(object = result$identical_content)
    # identical but different lengths: no identical
    result2 <- comp_1d(data1 = vec1, data2 = 1:3, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_false(object = result2$identical_object)
    testthat::expect_false(object = result2$identical_content)
    testthat::expect_true(object = result2$any_id_elements)
    # different classes
    result3 <- comp_1d(data1 = vec1, data2 = base::as.character(vec1), safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_false(object = result3$same_class)
    testthat::expect_null(object = result3$class)
    # identical factors: levels filled
    result4 <- comp_1d(data1 = fac1, data2 = fac1, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = result4$same_levels)
    testthat::expect_equal(object = result4$levels, expected = base::levels(x = fac1))
    testthat::expect_true(object = result4$any_id_levels)
    testthat::expect_true(object = result4$identical_object)
    # factors with identical levels but different content (non-identical objects: same.levels branch reached)
    fac4a <- base::factor(x = c("a", "b"), levels = c("a", "b"))
    fac4b <- base::factor(x = c("a", "a"), levels = c("a", "b"))
    result4b <- comp_1d(data1 = fac4a, data2 = fac4b, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = result4b$same_levels)
    testthat::expect_equal(object = result4b$levels, expected = c("a", "b"))
    testthat::expect_false(object = result4b$identical_object)
    # factors with different levels but common ones
    result5 <- comp_1d(data1 = fac1, data2 = fac2, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_false(object = result5$same_levels)
    testthat::expect_true(object = result5$any_id_levels)
    testthat::expect_equal(object = base::sort(x = result5$common_levels), expected = c("D", "E"))
    testthat::expect_true(object = base::is.null(x = result5$common_names)) # factors: names not compared
    # factors without any common level
    result6 <- comp_1d(data1 = fac1, data2 = fac3, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_false(object = result6$any_id_levels)
    testthat::expect_true(object = base::is.null(x = result6$common_levels))
    testthat::expect_false(object = result6$any_id_elements)
    # named vectors: same names
    result7 <- comp_1d(data1 = nvec1, data2 = nvec1, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = result7$same_names)
    testthat::expect_equal(object = result7$name, expected = base::LETTERS[1:5])
    testthat::expect_true(object = result7$any_id_names)
    testthat::expect_equal(object = result7$common_names, expected = base::LETTERS[1:5])
    # named vectors with identical names but different content (non-identical objects: same.names branch reached)
    nvec3 <- nvec1 ; base::names(nvec3) <- base::LETTERS[1:5]
    nvec4 <- c(9, 8, 7, 6, 5) ; base::names(nvec4) <- base::LETTERS[1:5]
    result7b <- comp_1d(data1 = nvec3, data2 = nvec4, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = result7b$same_names)
    testthat::expect_equal(object = result7b$name, expected = base::LETTERS[1:5])
    testthat::expect_false(object = result7b$identical_object)
    # named vectors: partial common names, no identical order
    result8 <- comp_1d(data1 = nvec1, data2 = nvec2, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_false(object = result8$same_names)
    testthat::expect_true(object = result8$any_id_names)
    testthat::expect_equal(object = base::sort(x = result8$common_names), expected = c("A", "B", "C", "D"))
    testthat::expect_true(object = base::is.null(x = result8$same_order)) # same.order is NULL because identical.content is FALSE and sort(data1) != sort(data2)
    # named vs unnamed
    result9 <- comp_1d(data1 = nvec1, data2 = vec2, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_false(object = result9$same_names)
    testthat::expect_false(object = result9$any_id_names) # one side unnamed: no id names possible
    # 1D tables
    result10 <- comp_1d(data1 = tab1, data2 = tab2, safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = result10$same_class)
    testthat::expect_false(object = result10$same_length)
    testthat::expect_true(object = result10$any_id_elements)
    testthat::expect_true(object = base::is.null(x = result10$same_order)) # sort(tab1) != sort(tab2) and not identical
    # identical order but different content: sort-identical branch
    result11 <- comp_1d(data1 = c(1, 2, 3), data2 = c(3, 2, 1), safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_false(object = result11$same_order)
    testthat::expect_equal(object = result11$order1, expected = c(1, 2, 3))
    testthat::expect_equal(object = result11$order2, expected = c(3, 2, 1))
    testthat::expect_false(object = result11$identical_content)
    # no common elements
    result12 <- comp_1d(data1 = c(1, 2), data2 = c(3, 4), safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_false(object = result12$any_id_elements)
    testthat::expect_true(object = base::is.null(x = result12$common_elements))
    testthat::expect_true(object = result12$same_class)
    testthat::expect_true(object = base::is.null(x = result12$same_order)) # same.length TRUE but sort() different: same.order NULL? no: identical(data1, data2) FALSE and sort identical? (1,2) vs (3,4): different -> NULL
    # NA in data: na_contain = TRUE
    result13 <- comp_1d(data1 = c(1, NA), data2 = c(NA, 2), safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_true(object = result13$any_id_elements)
    testthat::expect_equal(object = result13$same_elements_pos1, expected = 2)
    testthat::expect_equal(object = result13$same_elements_match1, expected = c(NA, 1)) # match(c(1, NA), c(NA, 2)): 1 not in data2, NA matches the NA of data2
    # factor compared to character vector of same content: factor converted to character
    result14 <- comp_1d(data1 = fac1, data2 = base::as.character(LETTERS[1:5]), safer_check = TRUE, lib_path = NULL, error_text = "")
    testthat::expect_false(object = result14$same_class)
    testthat::expect_true(object = result14$identical_content) # factor converted to character for content comparison
    #### end main code

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
