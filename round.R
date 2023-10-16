######## round() #### rounding number if decimal present

#' @title round
#' @description
#' Round a vector of values, if decimal, with the desired number of decimal digits after the decimal leading zeros.
#' @param data A vector of numbers (numeric or character mode).
#' @param dec.nb Number of required decimal digits.
#' @param after.lead.zero Logical. If FALSE, rounding is performed for all the decimal numbers, whatever the leading zeros (e.g., 0.123 -> 0.12 and 0.00128 -> 0.00). If TRUE, dec.nb are taken after the leading zeros (e.g., 0.123 -> 0.12 and 0.00128 -> 0.0013).
#' @returns The modified vector.
#' @details 
#' REQUIRED PACKAGES
#' 
#' none
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' fun_check()
#' 
#' WARNINGS
#' 
#' Work well with numbers as character strings, but not always with numerical numbers because of the floating point.
#' 
#' Numeric values are really truncated from a part of their decimal digits, whatever options(digits) settings.
#' 
#' See ?.Machine or https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r, with the interexting formula: abs(x - round(x)) > .Machine$double.eps^0.5.
#' @examples
#' ini.options <- options()$digits ; 
#' options(digits = 8) ; 
#' cat(fun_round(data = c(NA, 10, 100.001, 333.0001254, 12312.1235), dec.nb = 2, after.lead.zero = FALSE), "\n\n") ; 
#' options(digits = ini.options)
#' 
#' 
#' ini.options <- options()$digits ; 
#' options(digits = 8) ; 
#' cat(fun_round(data = c(NA, 10, 100.001, 333.0001254, 12312.1235), dec.nb = 2, after.lead.zero = TRUE), "\n\n") ; 
#' options(digits = ini.options)
#' 
#' 
#' ini.options <- options()$digits ; 
#' options(digits = 8) ; 
#' cat(fun_round(data = c(NA, "10", "100.001", "333.0001254", "12312.1235"), dec.nb = 2, after.lead.zero = FALSE), "\n\n") ; 
#' options(digits = ini.options)
#' 
#' 
#' ini.options <- options()$digits ; 
#' options(digits = 8) ; 
#' cat(fun_round(data = c(NA, "10", "100.001", "333.0001254", "12312.1235"), dec.nb = 2, after.lead.zero = TRUE), "\n\n") ; 
#' options(digits = ini.options)
#' @export
fun_round <- function(data, dec.nb = 2, after.lead.zero = TRUE){

    # DEBUGGING
    # data = data = c(10, 100.001, 333.0001254, 12312.1235) ; dec.nb = 2 ; after.lead.zero = FALSE # # for function debugging
    # data = data = c("10", "100.001", "333.0001254", "12312.1235") ; dec.nb = 2 ; after.lead.zero = TRUE # # for function debugging
    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    # end function name
    # required function checking
    if(length(utils::find("fun_check", mode = "function")) == 0L){
        tempo.cat <- paste0("ERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end required function checking
    # argument checking
    # argument checking without fun_check()
    if( ! (all(typeof(data) == "character") | all(typeof(data) == "double") | all(typeof(data) == "integer"))){
        tempo.cat <- paste0("ERROR IN ", function.name, ": data ARGUMENT MUST BE A VECTOR OF NUMBERS (IN NUMERIC OR CHARACTER MODE)")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end argument checking without fun_check()
    # argument checking with fun_check()
    arg.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- fun_check(data = data, class = "vector", na.contain = TRUE, fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = dec.nb, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = after.lead.zero, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(arg.check)){
        if(any(arg.check) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with fun_check()
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
    # end argument checking
    # main code
    tempo <- grepl(x = data, pattern = "\\.") # detection of decimal numbers
    ini.mode <- base::mode(data)
    data <- as.character(data) # to really truncate decimal digits
    for(i in 1:length(data)){ # scan all the numbers of the vector
        if(tempo[i] == TRUE){ # means decimal number
            if(after.lead.zero == TRUE){
                zero.pos <- unlist(gregexpr(text=data[i], pattern = 0)) # recover all the position of the zeros in the number. -1 if no zeros (do not record the leading and trailing zeros)
            }else{
                zero.pos <- -1 # -1 as if no zero
            }
            dot.pos <- unlist(gregexpr(text=data[i], pattern = "\\.")) # recover all the position of the zeros in the number
            digit.pos <- unlist(gregexpr(text=data[i], pattern = "[[:digit:]]")) # recover all the position of the digits in the number
            dec.pos <- digit.pos[digit.pos > dot.pos]
            count <- 0
            while((dot.pos + count + 1) %in% zero.pos & (dot.pos + count + 1) <= max(dec.pos) & (count + dec.nb) < length(dec.pos)){ # count the number of leading zeros in the decimal part
                count <- count + 1
            }
            data[i] <- formatC(as.numeric(data[i]), digits = (count + dec.nb), format = "f")
        }
    }
    if(ini.mode != "character"){
        data <- as.numeric(data)
    }
    return(data)
}
