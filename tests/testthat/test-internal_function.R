test_that("test .internal_function.R", {

    # .pack_and_function_check()
    fun_wrong1 <- "geom_point"  # incorrect input
    fun_wrong2 <- "ggplot2::non_existent_function"  # incorrect function name
    fun_good <- "ggplot2::geom_point" # correct input
    path_wrong <- "path/to/library"  # incorrect input
    path_good <- NULL  # incorrect input

    expect_no_error(.pack_and_function_check(
        fun = fun_good, 
        lib.path = path_good,
        external.function.name = "FUN1",
        external.package.name = "P1"
    ))
    expect_error(.pack_and_function_check(
        fun = fun_wrong1, 
        lib.path = path_good,
        external.function.name = "FUN1",
        external.package.name = "P1"
    ))
    expect_error(.pack_and_function_check(
        fun = fun_wrong2, 
        lib.path = path_good,
        external.function.name = "FUN1",
        external.package.name = "P1"
    ))
    expect_error(.pack_and_function_check(
        fun = fun_good, 
        lib.path = path_wrong,
        external.function.name = "FUN1",
        external.package.name = "P1"
    ))

    # .base_op_check()
    expect_no_error(.base_op_check(
        external.function.name = "FUN1",
        external.package.name = "P1"
    ))

    # .safer_backone_check()
    x_good1 <- 1:3
    x_good2 <- TRUE
    x_wrong <- "a"
    na.rm_good <- TRUE
    na.rm_wrong <- "a"
    finite_good <- TRUE
    finite_wrong <- "b"
    safer_check_good <- TRUE 
    safer_check_wrong <- "d" 

    expect_no_error(.safer_backone_check(
        x = x_good1,
        na.rm = na.rm_good,
        finite = finite_good,
        safer_check = safer_check_good, 
        arg.user.setting =  list(
            x = x_good1,
            na.rm = na.rm_good,
            finite = finite_good,
            safer_check = safer_check_good
        ),
        external.function.name = "FUN1",
        external.package.name = "P1"
    ))

    expect_no_error(.safer_backone_check(
        x = x_good2,
        na.rm = na.rm_good,
        finite = finite_good,
        safer_check = safer_check_good, 
        arg.user.setting =  list(
            x = x_good2,
            na.rm = na.rm_good,
            finite = finite_good,
            safer_check = safer_check_good
        ),
        external.function.name = "FUN1",
        external.package.name = "P1"
    ))

    expect_error(.safer_backone_check(
        x = x_wrong,
        na.rm = na.rm_good,
        finite = finite_good,
        safer_check = safer_check_good, 
        arg.user.setting =  list(
            x = x_wrong,
            na.rm = na.rm_good,
            finite = finite_good,
            safer_check = safer_check_good
        ),
        external.function.name = "FUN1",
        external.package.name = "P1"
    ))

    expect_error(.safer_backone_check(
        x = x_good1,
        na.rm = na.rm_wrong,
        finite = finite_good,
        safer_check = safer_check_good, 
        arg.user.setting =  list(
            x = x_good1,
            na.rm = na.rm_wrong,
            finite = finite_good,
            safer_check = safer_check_good
        ),
        external.function.name = "FUN1",
        external.package.name = "P1"
    ))

    expect_error(.safer_backone_check(
        x = x_good1,
        na.rm = na.rm_good,
        finite = finite_wrong,
        safer_check = safer_check_good, 
        arg.user.setting =  list(
            x = x_good1,
            na.rm = na.rm_good,
            finite = finite_wrong,
            safer_check = safer_check_good
        ),
        external.function.name = "FUN1",
        external.package.name = "P1"
    ))

    expect_error(.safer_backone_check(
        x = x_good1,
        na.rm = na.rm_good,
        finite = finite_good,
        safer_check = safer_check_wrong, 
        arg.user.setting =  list(
            x = x_good1,
            na.rm = na.rm_good,
            finite = finite_good,
            safer_check = safer_check_wrong
        ),
        external.function.name = "FUN1",
        external.package.name = "P1"
    ))

})