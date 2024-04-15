#' @title round2
#' @description
#' Round a vector of values, if decimal, with the desired number of decimal digits after the decimal leading zeros.
#' @param data A vector of numbers (numeric or character mode).
#' @param dec.nb Single numeric value. Number of required decimal digits.
#' @param after.lead.zero Single logical value. If FALSE, rounding is performed for all the decimal numbers, whatever the leading zeros (e.g., 0.123 -> 0.12 and 0.00128 -> 0.00). If TRUE, dec.nb are taken after the leading zeros (e.g., 0.123 -> 0.12 and 0.00128 -> 0.0013).
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns The modified vector.
#' @details 
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
#' cat(round2(data = c(NA, 10, 100.001, 333.0001254, 12312.1235), dec.nb = 2, after.lead.zero = FALSE), "\n\n") ; 
#' options(digits = ini.options)
#' @importFrom saferDev arg_check
#' @export
round2 <- function(
        data, 
        dec.nb = 2, 
        after.lead.zero = TRUE,
        safer_check = TRUE
){
    
    # DEBUGGING
    # data = data = c(10, 100.001, 333.0001254, 12312.1235) ; dec.nb = 2 ; after.lead.zero = FALSE ; safer_check = TRUE # for function debugging
    # data = data = c("10", "100.001", "333.0001254", "12312.1235") ; dec.nb = 2 ; after.lead.zero = TRUE ; safer_check = TRUE # # for function debugging
    # package name
    package.name <- "saferTool"
    # end package name
    # function name
   ini <- base::match.call(expand.dots = FALSE) # initial parameters (specific of arg_test())
    function.name <- base::paste0(base::as.list(base::match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package()", "function()") if "package::function()" is used.
    if(function.name[1] == "::()"){
        function.name <- function.name[3]
    }
    arg.names <- base::names(base::formals(fun = base::sys.function(base::sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- base::as.list(base::match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # critical operator checking
    if(safer_check == TRUE){
        .base_op_check(
            external.function.name = function.name,
            external.package.name = package.name
    )
    }
    # end critical operator checking


    # package checking
    # check of lib.path
    # end check of lib.path

    # check of the required function from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check(
        fun = base::c(
            "saferDev::arg_check"
        ),
        lib.path = NULL,
        external.function.name = function.name,
        external.package.name = package.name
    )
    }
    # end check of the required function from the required packages
    # end package checking

    # argument primary checking
    # arg with no default values
    mandat.args <- base::c(
        "data"
    )
    tempo <- base::eval(base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat.args, collapse = "),base::missing("), "))")))
    if(base::any(tempo)){ # normally no NA for missing() output
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args, collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    
    
    # argument checking with arg_check()
    arg.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(arg.check <- base::c(arg.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    tempo <- saferDev::arg_check(data = data, class = "vector", na.contain = TRUE, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = dec.nb, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, neg.values = FALSE, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = after.lead.zero, class = "logical", length = 1, fun.name = function.name) ; base::eval(ee)
    if( ! base::is.null(arg.check)){
        if(base::any(arg.check, na.rm = TRUE) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
    # end check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) %in% base::c("list", "NULL"), na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = get, env = base::sys.nframe(), inherit = FALSE), FUN = is.na), FUN = any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    
    # management of NULL arguments
    tempo.arg <-base::c(
        "data", 
        "dec.nb", 
        "after.lead.zero",
        "safer_check"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = is.null)
    if(base::any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    
    # warning initiation
    # end warning initiation
    
    # other checkings
    # argument checking without arg_check()
    if( ! (base::all(base::typeof(data) == "character") | base::all(base::typeof(data) == "double") | base::all(base::typeof(data) == "integer"))){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: data ARGUMENT MUST BE A VECTOR OF NUMBERS (IN NUMERIC OR CHARACTER MODE)")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end argument checking without arg_check()
    # end other checkings
    # end second round of checking and data preparation

    # main code
    tempo <- base::grepl(x = data, pattern = "\\.") # detection of decimal numbers
    ini.mode <- base::mode(data)
    data <- base::as.character(data) # to really truncate decimal digits
    for(i in 1:base::length(data)){ # scan all the numbers of the vector
        if(tempo[i] == TRUE){ # means decimal number
            if(after.lead.zero == TRUE){
                zero.pos <- base::unlist(base::gregexpr(text=data[i], pattern = 0)) # recover all the position of the zeros in the number. -1 if no zeros (do not record the leading and trailing zeros)
            }else{
                zero.pos <- -1 # -1 as if no zero
            }
            dot.pos <- base::unlist(base::gregexpr(text=data[i], pattern = "\\.")) # recover all the position of the zeros in the number
            digit.pos <- base::unlist(base::gregexpr(text=data[i], pattern = "[[:digit:]]")) # recover all the position of the digits in the number
            dec.pos <- digit.pos[digit.pos > dot.pos]
            count <- 0
            while((dot.pos + count + 1) %in% zero.pos & (dot.pos + count + 1) <= base::max(dec.pos) & (count + dec.nb) < base::length(dec.pos)){ # count the number of leading zeros in the decimal part
                count <- count + 1
            }
            data[i] <- base::formatC(base::as.numeric(data[i]), digits = (count + dec.nb), format = "f")
        }
    }
    if(ini.mode != "character"){
        data <- base::as.numeric(data)
    }
    # output
    # warning output
    # end warning output
    base::return(data)
    # end output
    # end main code
}
