#' @title range2
#' @description
#' Find and return the minimum value and the maximum value of a set of data.
#' @param x Numeric or logical vector or matrix or numeric table where find the initial values to use.
#' @param na.rm Single logical value. Should missing values (NA and NaN) be removed ?
#' @param finite Single logical value. Should infinite values (Inf and -Inf) be removed ? Warning: this argument does not remove NA and NaN. Please use the na.rm argument.
#' @returns The minimum value and the maximum value of the given arguments. 
#' @examples
#' vec <- c(1:3) ; range2(x = vec)
#' 
#' vec <- c(1,3,5,TRUE) ; range2(x = vec)
#' 
#' 
#' # This example returns an error because of the character in the vector
#' # vec <- c(1,3,5,TRUE,"apple") ; range2(x = vec)
#' @export
range2 <- function(
        x,
        na.rm = FALSE,
        finite = FALSE
        
){
    # DEBUGGING
    # vec <- c(1,3,5,TRUE, Inf) ; range2(x = vec, finite = TRUE) # for function debugging
    # package name
    package.name <- "saferTool"
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
    output <- base::range(x, na.rm = na.rm, finite = finite)
    # end main code
    # output
    # warning output
    # end warning output
    return(output)
    # end output
}