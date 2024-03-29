#' @title .pack_and_function_check
#' @description
#' Check if 1) required functions are present in required packages and 2) required packages are installed locally.
#' Simplified version of saferDev::is_function_here(), used as internal function for the other functions of the package.
#' @param fun Character vector of the names of the required functions, preceded by the name of the package they belong to and a double colon. Example: c("ggplot2::geom_point", "grid::gpar").
#' @param lib.path Character vector specifying the absolute pathways of the directories containing the listed packages in the fun argument, if not in the default directories. If NULL, the function checks only in the .libPaths() default R library folders.
#' @param external.function.name Name of the function using the .pack_and_function_check() function.
#' @returns An error message if at least one of the checked packages is missing in lib.path, or if at least one of the checked functions is missing in the required package, nothing otherwise.
#' @details
#' WARNING
#' 
#' arguments of the .pack_and_function_check() function are not checked, so use carefully inside other functions
#' 
#' @examples
#' \dontrun{
#' # Example that shouldn't be run because this is an internal function
#' .pack_and_function_check(fun = c("ggplot2::geom_point", "grid::gpar"))
#' 
#' # This example returns an error
#' .pack_and_function_check(fun = "ggplot2::notgood") 
#' }
#' @keywords internal
#' @rdname internal_function


.pack_and_function_check <- function(
        fun, 
        lib.path,
        external.function.name
){
    # DEBUGGING
    # fun = "ggplot2::geom_point" ; lib.path = "C:/Program Files/R/R-4.3.1/library" ; external.function.name = "fun1"
    # check of lib.path
    # already done in the main function
    if(base::is.null(lib.path)){
        lib.path <- .libPaths() # .libPaths(new = lib.path) # or .libPaths(new = c(.libPaths(), lib.path))
    }
    # end check of lib.path
    # main code
    tempo.log <- base::grepl(x = fun, pattern = "^.+::.+$")
    if( ! base::all(tempo.log)){
        tempo.cat <- base::paste0("ERROR IN THE CODE OF THE ", external.function.name, " OF THE saferTool PACKAGE.\nTHE STRING IN fun ARGUMENT MUST CONTAIN \"::\":\n", base::paste(fun[ ! tempo.log], collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    pkg.fun.name.list <- base::strsplit(fun, "::") # package in 1 and function in 2
    pkg.name <- base::sapply(X = pkg.fun.name.list, FUN = function(x){x[1]})
    pkg.log <- pkg.name %in% base::rownames(utils::installed.packages(lib.loc = lib.path))
    if( ! base::all(pkg.log)){
        tempo <- pkg.name[ ! pkg.log]
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            external.function.name, 
            " OF THE saferTool PACKAGE. REQUIRED PACKAGE", 
            base::ifelse(base::length(tempo) == 1L, base::paste0(":\n", tempo), base::paste0("S:\n", base::paste(tempo, collapse = "\n"))), 
            "\nMUST BE INSTALLED IN", 
            base::ifelse(base::length(lib.path) == 1L, "", " ONE OF THESE FOLDERS"), 
            ":\n", 
            base::paste(lib.path, collapse = "\n")
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    fun.log <- base::sapply(X = pkg.fun.name.list, FUN = function(x){base::exists(x[2], envir = base::asNamespace(x[1]))})
    if( ! base::all(fun.log)){
        tempo <- fun[ ! fun.log]
        tempo.cat <- base::paste0(
            "ERROR IN ", 
            external.function.name, 
            " OF THE saferTool PACKAGE. REQUIRED FUNCTION",
            base::ifelse(length(tempo) == 1L, " IS ", "S ARE "), 
            "MISSING IN THE INSTALLED PACKAGE", 
            base::ifelse(base::length(tempo) == 1L, base::paste0(":\n", tempo), base::paste0("S:\n", base::paste(tempo, collapse = "\n")))
        )
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
}


#' @title .arguments_check
#' @description
#' Check if 1) the types of input are correct and 2) an input is missed.
#' @param x Numeric or logical vector or matrix or numeric table where find the initial values to calculate.
#' @param na.rm Single logical value. Should missing values (NA and NaN) be removed ?
#' @param finite Single logical value. Should infinite values (Inf and -Inf) be removed ? Warning: this argument does not remove NA and NaN. Please use the na.rm argument.
#' @param external.function.name Name of the function using the .arguments_check() function.
#' @returns An error message if the type of input is not correct, or input missed, nothing otherwise.
#' @details
#' WARNING
#' 
#' arguments of the .arguments_check() function are not checked, so use carefully inside other functions
#' @examples
#' \dontrun{
#' # Example that shouldn't be run because this is an internal function
#' .arguments_check(x = 1:3, na.rm = TRUE)
#' 
#' .arguments_check(x = c(Inf, NA), na.rm = TRUE, finite = TRUE)
#' }
#' @keywords internal
#' @importFrom saferDev arg_check
#' @rdname internal_function_backbone
.arguments_check <- function(
        x,
        na.rm,
        finite,
        external.function.name
){
    # package name
    package.name <- "saferTool"
    # end package name
    # function name
    ini <- match.call(expand.dots = FALSE) # initial parameters (specific of arg_test())
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package()", "function()") if "package::function()" is used.
    if(function.name[1] == "::()"){
        function.name <- function.name[3]
    }
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    # package checking
    # check of lib.path
    # end check of lib.path
    # check of the required function from the required packages
    .pack_and_function_check(
        fun = c(
            "saferDev::arg_check"
        ),
        lib.path = NULL, # no lib.path for the functions using .arguments_check()
        external.function.name = external.function.name
    )
    # end check of the required function from the required packages
    # end package checking
    
    # argument primary checking
    # arg with no default values
    mandat.args <- c(
        "x"
    )
    tempo <- eval(parse(text = paste0("missing(", paste0(mandat.args, collapse = ") | missing("), ")")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", external.function.name, " OF THE ", package.name, " PACKAGE: \nFOLLOWING ARGUMENT", ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(argum.check = c(argum.check, tempo$problem) , text.check = c(text.check, tempo$text) , checked.arg.names = c(checked.arg.names, tempo$object.name))
    tempo <- saferDev::arg_check(data = na.rm, class = "vector", typeof = "logical", length = 1, fun.name = external.function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = finite, class = "vector", typeof = "logical", length = 1, fun.name = external.function.name) ; eval(ee)
    tempo1 <- saferDev::arg_check(data = x, class = "vector", mode = "numeric", na.contain = TRUE, fun.name = external.function.name)
    tempo2 <- saferDev::arg_check(data = x, class = "vector", mode = "logical", na.contain = TRUE, fun.name = external.function.name)
    tempo3 <- saferDev::arg_check(data = x, class = "vector", mode = "complex", na.contain = TRUE, fun.name = external.function.name)
    tempo4 <- saferDev::arg_check(data = x, class = "matrix", mode = "numeric", na.contain = TRUE, fun.name = external.function.name)
    tempo5 <- saferDev::arg_check(data = x, class = "matrix", mode = "logical", na.contain = TRUE, fun.name = external.function.name)
    tempo6 <- saferDev::arg_check(data = x, class = "matrix", mode = "complex", na.contain = TRUE, fun.name = external.function.name)
    tempo7 <- saferDev::arg_check(data = x, class = "table", mode = "numeric", na.contain = TRUE, fun.name = external.function.name)
    
    if(tempo1$problem == TRUE & tempo2$problem == TRUE & tempo3$problem == TRUE & tempo4$problem == TRUE & tempo5$problem == TRUE& tempo6$problem == TRUE & tempo7$problem == TRUE){
        tempo.cat <- paste0("ERROR IN ", external.function.name, " OF THE", package.name, " PACKAGE: x ARGUMENT MUST BE A VECTOR, MATRIX OR TABLE OF NUMERIC OR LOGICAL VALUES")
        text.check <- c(text.check, tempo.cat)
        argum.check <- c(argum.check, TRUE)
    }
    if( ! is.null(argum.check)){
        if(any(argum.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
    # end check with r_debugging_tools
    # end argument primary checking
    # second round of checking and data preparation
    # management of NA arguments
    if( ! (all(class(arg.user.setting) == "list", na.rm = TRUE) & length(arg.user.setting) == 0)){
        tempo.arg <- names(arg.user.setting) # values provided by the user
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = length) == 1L # no argument provided by the user can be just NA
        if(any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- paste0("ERROR IN ", external.function.name, " OF THE", package.name, " PACKAGE:\n", ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:\n", paste0(tempo.arg[tempo.log], collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <- c(
        "x",
        "na.rm",
        "finite"
    )
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.null)
    if(any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- paste0("ERROR IN ", external.function.name, " OF THE", package.name, " PACKAGE:\n", ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    # warning initiation
    
    # end warning initiation
    # other checkings
    # end other checkings
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # end second round of checking and data preparation
}