#' @title max
#' @description
#' Find and return the maximum value of a set of data.
#' @param x Numeric or logical vector or matrix or numeric table where find the initial values to use.
#' @param na.rm Single logical value. Should missing values (NA and NaN) be removed ?
#' @param finite Single logical value. Should infinite values (Inf and -Inf) be removed ? Warning: this argument does not remove NA and NaN. Please use the na.rm argument.
#' @returns The maximum value of the given arguments. 
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
#' vec <- c(1:3) ; max(x = vec)
#' 
#' vec <- c(1,3,5,TRUE) ; max(x = vec)
#' 
#' 
#' # This example returns an error because of the character in the vector
#' # vec <- c(1,3,5,TRUE,"apple") ; max(x = vec)
#' @export
max <- function(
        x,
        na.rm = FALSE,
        finite = FALSE
        
){
    .arguments_check(
        x = x,
        na.rm = na.rm,
        finite = finite
    )
    # main code
    if(finite == TRUE){
        x <- x[ ! x %in% c(Inf, -Inf)]
    }
    output <- base::max(x, na.rm = na.rm)
    # end main code
    # output
    # warning output
    # end warning output
    return(output)
    # end output
}