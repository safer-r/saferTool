#' @title sum
#' @description
#' Calculate the sum of numeric and logical value.
#' @param x Numeric or logical vector or matrix or numeric table where find the initial values to calculate.
#' @param na.rm Single logical value. Should missing values be removed ?
#' @returns The sum of the values in the vector. Returns NA if the input contains NA and the argument na.rm = TRUE, else returns a value.
#' @details
#' REQUIRED PACKAGES
#' 
#' cuteDev
#' 
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' arg_check()
#' @examples
#' vec <- c(1:3) ; sum(x = vec)
#' 
#' vec <- c(1,3,5,TRUE) ; sum(x = vec)
#' 
#' 
#' 
#' # This example returns an error because of the character in the vector
#' vec <- c(1,3,5,TRUE,"apple") ; sum(x = vec)
#' @importFrom cuteDev arg_check
#' @export
sum <- function(
        x,
        na.rm = FALSE
){
    # DEBUGGING
    # vec <- c(1,3,5,TRUE) ; sum(x = vec) # for function debugging
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
    .pack_and_function_check <- function(
        req.package = c(
            "cuteDev::arg_check"
        ),
        lib.path = NULL,
        external.function.name = function.name
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
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", "HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(argum.check = c(argum.check, tempo$problem) , text.check = c(text.check, tempo$text) , checked.arg.names = c(checked.arg.names, tempo$object.name))
    tempo <- cuteDev::arg_check(data = na.rm, class = "vector", typeof = "logical", length = 1, fun.name = function.name) ; eval(ee)
    
    tempo1 <- cuteDev::arg_check(data = x, class = "vector", mode = "numeric", na.contain = TRUE, fun.name = function.name)
    tempo2 <- cuteDev::arg_check(data = x, class = "vector", mode = "logical", na.contain = TRUE, fun.name = function.name)
    tempo3 <- cuteDev::arg_check(data = x, class = "matrix", mode = "numeric", na.contain = TRUE, fun.name = function.name)
    tempo4 <- cuteDev::arg_check(data = x, class = "matrix", mode = "logical", na.contain = TRUE, fun.name = function.name)
    tempo5 <- cuteDev::arg_check(data = x, class = "table", mode = "numeric", na.contain = TRUE, fun.name = function.name)
    if(tempo1$problem == TRUE & tempo2$problem == TRUE & tempo3$problem == TRUE & tempo4$problem == TRUE & tempo5$problem == TRUE){
        tempo.cat <- paste0("ERROR IN ", function.name, ": x ARGUMENT MUST BE A NUMERIC OR LOGICAL VECTOR, MATRIX OR TABLE")
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
            tempo.cat <- paste0("ERROR IN ", function.name, "\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <- c(
        "x",
        "na.rm"
    )
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.null)
    if(any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- paste0("ERROR IN ", function.name, ":\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
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
    
    # main code
    result <- 0
    length.arg <- length(x)
    if(tempo1 == TRUE | tempo2 == TRUE | tempo3 == TRUE | tempo4 == TRUE){
        for(i in 1:length.arg){
            if(is.null(x[i])){
                x[i] <- 0
            }
            if((!is.na(x[i])) | (na.rm == FALSE && is.na(x[i]))){
                result <- result + x[i]
            }
        }
    }else{
        name.arg <- as.numeric(names(x))
        elements.arg <- as.numeric(name.arg)
        for(i in 1:6){
            if (!(is.na(name.arg[i]) && na.rm == TRUE))
                result <- result + name.arg[i] * elements.arg[i]
        }
    }
    # end main code
    # output
    # warning output
    # end warning output
    return(result)
    # end output
}