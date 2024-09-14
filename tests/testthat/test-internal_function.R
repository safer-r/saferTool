test_that(".pack_and_function_check handles valid input correctly", {

  # Mock inputs
  fun <- "geom_point"  # incorrect input
  
  # Call the function and expect an error
    expect_error(.pack_and_function_check(fun, "path/to/library", "fun1"))

  # Mock inputs
  fun <- "ggplot2::geom_point"
  lib.path <- "path/to/nonexistent/library"  # incorrect library path
  
  # Call the function and expect an error
    expect_error(.pack_and_function_check(fun, lib.path, "fun1"))

  # Mock inputs
  fun <- "ggplot2::non_existent_function"  # incorrect function name
  
  # Call the function and expect an error
    expect_error(.pack_and_function_check(fun, "path/to/library", "fun1"))


  # Mock inputs
  x <- "not_numeric_or_logical"
  na.rm <- TRUE
  
  # Call the function and expect an error
    expect_error(.safer_backone_check(x, na.rm, FALSE, "test_function"))

  # Mock inputs
  na.rm <- TRUE
  
  # Call the function and expect an error
    expect_error(.safer_backone_check(NULL, na.rm, FALSE, "test_function"))
})