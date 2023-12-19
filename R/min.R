#' @title min
#' @description
#' Find and return the minimum value of a set of data.
#' @param x Numeric or logical vector or matrix or numeric table where find the initial values to use.
#' @param na.rm Single logical value. Should missing values (NA and NaN) be removed ?
#' @param finite Single logical value. Should infinite values (Inf and -Inf) be removed ? Warning: this argument does not remove NA and NaN. Please use the na.rm argument.
#' @returns The minimum value of the given arguments. 
#' @details
#' REQUIRED PACKAGES
#' 
#' none
#' 
#' 
#' REQUIRED FUNCTIONS FROM THE cute PACKAGE
#' 
#' none
#' @examples
#' vec <- c(1:3) ; min(x = vec)
#' 
#' vec <- c(1, 3, 5, TRUE, FALSE) ; min(x = vec)
#' 
#' 
#' # This example returns an error because of the character in the vector
#' # vec <- c(1,3,5,TRUE,"apple") ; min(x = vec)
#' @export
min <- function(
        x,
        na.rm = FALSE,
        finite = FALSE
){
    # DEBUGGING
    # vec <- c(1,3,5,TRUE, -Inf,NA) ; x = vec ; na.rm = TRUE ; finite = FALSE # for function debugging
    # package name
    package.name <- "cuteTool"
    # end package name
    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package()", "function()") if "package::function()" is used.
    if(function.name[1] == "::()"){
        function.name <- function.name[3]
    }
    # end function name
    .arguments_check(
        x = x,
        na.rm = na.rm,
        finite = finite,
        external.function.name = function.name
    )
    # main code
    if(finite == TRUE){
        x <- x[ ! x %in% c(Inf, -Inf)]
    }
    output <- base::min(x, na.rm = na.rm)
    # end main code
    # output
    # warning output
    # end warning output
    return(output)
    # end output
}