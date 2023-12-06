#' @title sum
#' @description
#' Calculate the sum of numeric and logical value.
#' @param x Numeric or logical vector or matrix or numeric table where find the initial values to calculate.
#' @param na.rm Single logical value. Should missing values (NA and NaN) be removed ?
#' @param finite Single logical value. Should infinite values (Inf and -Inf) be removed ? Warning: this argument does not remove NA and NaN. Please use the na.rm argument.
#' @returns The sum of the given arguments. Returns NA if the input contains NA and the argument na.rm = TRUE, else returns a value.
#' @details
#' REQUIRED PACKAGES
#' 
#' none
#' 
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' none
#' @examples
#' vec <- c(1:3) ; sum(x = vec)
#' 
#' vec <- c(1,3,5,TRUE) ; sum(x = vec)
#' 
#' 
#' 
#' # This example returns an error because of the character in the vector
#' # vec <- c(1,3,5,TRUE,"apple") ; sum(x = vec)
#' @export
sum <- function(
        x,
        na.rm = FALSE,
        finite = FALSE
){
    # DEBUGGING
    # vec <- c(1,3,5,TRUE) ; sum(x = vec) # for function debugging
     .arguments_check(
        x = x,
        na.rm = na.rm,
        finite = finite
    )
    
    # main code
    if(finite == TRUE){
    x <- x[ ! x %in% c(Inf, -Inf)]
    }
    output <- base::sum(x, na.rm = na.rm)
    # end main code
    # output
    # warning output
    # end warning output
    return(output)
    # end output
}