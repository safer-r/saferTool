#' @title tail
#' @description
#' As tail() but display the left or right head of big 2D objects.
#' @param data1 Any object but more dedicated for matrix, data frame or table.
#' @param n As in tail() but for for matrix, data frame or table, number of dimension to print (10 means 10 rows and columns).
#' @param side Either "l" or "r" for the left or right side of the 2D object (only for matrix, data frame or table).
#' @returns The tail.
#' @details 
#' REQUIRED PACKAGES
#' 
#' cuteDev
#' 
#' 
#' REQUIRED FUNCTIONS FROM THE cute PACKAGE
#' 
#' arg_check()
#'
#'
#' BEWARE
#' 
#' Other arguments of tail() not used.
#' @examples
#' obs1 = matrix(1:30, ncol = 5, dimnames = list(letters[1:6], LETTERS[1:5])) ; 
#' obs1 ; 
#' tail(obs1, 3, "r")
#' @importFrom cuteDev arg_check
#' @export
tail <- function(
        data1, 
        n = 6, 
        side = "l"
){
    # DEBUGGING
    # data1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) # for function debugging
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
        "data1"
    )
    tempo <- eval(parse(text = paste0("c(missing(", paste0(mandat.args, collapse = "),missing("), "))")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    
    # argument checking with arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(argum.check <- c(argum.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- cuteDev::arg_check(data = n, class = "vector", typeof = "integer", double.as.integer.allowed = TRUE, length = 1, fun.name = function.name) ; eval(ee)
    tempo <- cuteDev::arg_check(data = side, options = c("l", "r"), length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(argum.check)){
        if(any(argum.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
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
    tempo.arg <-c(
        "data1", 
        "n", 
        "side"
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
    # output
    # warning output
    # end warning output
    if( ! (any(class(data1) %in% c("data.frame", "table")) | all(class(data1) %in% c("matrix", "array")))){ # before R4.0.0, it was  ! any(class(data1) %in% c("matrix", "data.frame", "table"))
        return(tail(data1, n))
    }else{
        obs.dim <- dim(data1)
        row <- ifelse(obs.dim[1] < n, 1, obs.dim[1] - n + 1):obs.dim[1]
        if(side == "l"){
            col <- 1:ifelse(obs.dim[2] < n, obs.dim[2], n)
        }
        if(side == "r"){
            col <- ifelse(obs.dim[2] < n, 1, obs.dim[2] - n + 1):obs.dim[2]
        }
        return(data1[row, col])
    }
    # end output
    # end main code
}
