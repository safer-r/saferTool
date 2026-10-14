# This file tests the "mandatory arg of safer-r functions" checking block present in every saferTool function
# This block fires only if the function is written without one of the mandatory formals (lib_path, safer_check, error_text for public functions)
# It cannot be triggered by user values: the test uses a temporary function built from the same body but with the mandatory formal removed from formals()
# No # nocov markers are used, and no R source files are modified: the block is actually executed by the tests

testthat::test_that("mandatory arg block of safer-r functions (public functions)", {

    ## data argument values
    # full calls with all formals except the mandatory one to drop (lib_path for all these functions)
    full_calls <- base::list(
        comp_1d = base::quote(comp_1d(data1 = 1:3, data2 = 1:3, safer_check = FALSE, error_text = "")),
        comp_2d = base::quote(comp_2d(data1 = base::matrix(1:4, ncol = 2), data2 = base::matrix(1:4, ncol = 2), safer_check = FALSE, error_text = "")),
        df_remod = base::quote(df_remod(df = base::data.frame(a = 1:2, stringsAsFactors = TRUE), quanti_col_name = "quanti", quali_col_name = "quali", safer_check = FALSE, error_text = "")),
        name_change = base::quote(name_change(data1 = c("A", "B"), data2 = c("A"), added_string = "_modif", duplicate = TRUE, safer_check = FALSE, error_text = "")),
        round2 = base::quote(round2(data = 1.5, dec_nb = 2, after_lead_zero = TRUE, safer_check = FALSE, error_text = ""))
    )
    ## end data argument values

    ## initialization of tests
    # none
    ## end initialization of tests

    ## tests (ordered by arg appearance and conditions in the code)

    ######## mandatory arg of safer-r functions
    for(fn in base::names(x = full_calls)){
        f <- base::get(x = fn, envir = base::asNamespace("saferTool"))
        fml <- base::formals(fun = f)
        fml_dropped <- fml[base::setdiff(x = base::names(x = fml), y = "lib_path")] # drop lib_path: the block must fire
        tempo_env <- base::new.env(parent = base::environment(fun = f))
        tempo_env[[fn]] <- base::as.function(base::c(fml_dropped, base::list(base::body(f))), envir = tempo_env)
        cl <- full_calls[[fn]]
        res <- base::tryCatch(
            expr = base::eval(expr = cl, envir = tempo_env),
            error = function(e){base::conditionMessage(e)}
        )
        testthat::expect_true(object = base::grepl(x = res, pattern = "FOLLOWING ARGUMENT IS MANDATORY IN SAFER-R FUNCTIONS:", fixed = TRUE), label = base::paste0(fn, ": mandatory-args block message"))
        testthat::expect_true(object = base::grepl(x = res, pattern = "lib_path", fixed = TRUE), label = base::paste0(fn, ": lib_path reported as missing mandatory arg"))
        testthat::expect_false(object = base::grepl(x = res, pattern = "object '", fixed = TRUE), label = base::paste0(fn, ": no error before the mandatory-args block"))
    }
    ######## end mandatory arg of safer-r functions

    ## end tests (ordered by arg appearance and conditions in the code)

    ## other tests
    # restore check: with all formals present, the block must NOT fire
    testthat::expect_no_error(object = saferTool::round2(data = 1.5, dec_nb = 2, after_lead_zero = TRUE, safer_check = FALSE, lib_path = NULL, error_text = ""))
    ## end other tests
    base::rm(list = base::ls()) # to avoid a warning that block CRAN tests
})
