#' @title sum2
#' @description
#' Calculate the sum of numeric and logical value.
#' @param x Numeric or logical vector or matrix or numeric table where find the initial values to calculate.
#' @param na.rm Single logical value. Should missing values (NA and NaN) be removed ?
#' @param finite Single logical value. Should infinite values (Inf and -Inf) be removed ? Warning: this argument does not remove NA and NaN. Please use the na.rm argument.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns The sum of the given arguments. Returns NA if the input contains NA and the argument na.rm = TRUE, else returns a value.
#' @examples
#' vec <- c(1:3) ; sum2(x = vec)
#' 
#' vec <- c(1,3,5,TRUE) ; sum2(x = vec)
#' 
#' 
#' 
#' # This example returns an error because of the character in the vector
#' # vec <- c(1,3,5,TRUE,"apple") ; sum2(x = vec)
#' @export
sum2 <- function(
        x,
        na.rm = FALSE,
        finite = FALSE,
        safer_check = TRUE
){
    # DEBUGGING
    # vec <- c(1,3,5,TRUE) ; sum2(x = vec) ; safer_check = TRUE # for function debugging
    # package name
    package.name <- "saferTool"
    # end package name
    # function name
    function.name <- base::paste0(base::as.list(base::match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package()", "function()") if "package::function()" is used.
    if(function.name[1] == "::()"){
        function.name <- function.name[3]
    }
    # end function name
    # critical operator checking
    if(safer_check == TRUE){
        .base_op_check(
            external.function.name = function.name,
            external.package.name = package.name
    )
    }
    # end critical operator checking

    
    .arguments_check(
        x = x,
        na.rm = na.rm,
        finite = finite,
        external.function.name = function.name
    )
    # main code
    if(finite == TRUE){
        x <- x[ ! x %in% base::c(Inf, -Inf)]
    }
    output <- base::sum(x, na.rm = na.rm)
    # end main code

    # output
    # warning output
    # end warning output
    base::return(output)
    # end output
}