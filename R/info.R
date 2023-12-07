#' @title info
#' @description
#' Provide a broad description of an object.
#' @param data Object to analyse.
#' @param n Single positive integer value indicating the n first number of elements to display per compartment of the output list (i.e., head(..., n)). Write NULL to return all the elements. Does not apply for the $STRUCTURE compartment output.
#' @param warn.print Single logical value. Print potential warnings at the end of the execution? If FALSE the warning messages are added in the output list as an additional compartment (or NULL if no message).
#' @returns
#' A list containing information, depending on the class and type of data. The backbone is generally:
#' - $NAME: name of the object.
#' - $CLASS: class of the object (class() value).
#' - $TYPE: type of the object (typeof() value).
#' - $LENGTH: length of the object (length() value).
#' - $NA.NB: number of NA and NaN (only for type "logical", "integer", "double", "complex", "character" or "list").
#' - $HEAD: head of the object (head() value).
#' - $TAIL: tail of the object (tail() value).
#' - $DIMENSION: dimension (only for object with dimensions).
#' - $SUMMARY: object summary (summary() value).
#' - $STRUCTURE: object structure (str() value).
#' - $WARNING: warning messages (only if the warn.print argument is FALSE).
#' 
#' If data is made of numerics, provide also:
#' - $INF.NB: number of Inf and -Inf.
#' - $RANGE: range after removing Inf and NA.
#' - $SUM: sum after removing Inf and NA.
#' - $MEAN: mean after removing Inf and NA.
#' 
#' If data is a 2D object, provide also:
#' 
#' - $ROW_NAMES: row names.
#' - $COL_NAMES: column names.
#' 
#' If data is a data frame, provide also:
#' 
#' - $COLUMN_TYPE: type of each column (typeof() value).
#' 
#' If data is a list, provide also:
#' 
#' - $COMPARTMENT_NAMES: names of the comprtments.
#' - $COMPARTMENT_TYPE: type of each compartment (typeof() value).
#' @details 
#' REQUIRED PACKAGES
#' 
#' cuteDev
#' 
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' arg_check()
#' 
#' get_message()
#'
#'
#' WARNINGS
#' 
#' None
#' @examples
#' info(data = 1:3)
#' @importFrom cuteDev arg_check
#' @importFrom cuteDev get_message
#' @export
info <- function(
        data, 
        n = NULL, 
        warn.print = TRUE
){
    # DEBUGGING
    # mat1 <- matrix(1:3) ; data = mat1 ; n = NULL ; warn.print = TRUE # for function debugging
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
            "cuteDev::arg_check", 
            "cuteDev::get_message()"
        ),
        lib.path = NULL,
        external.function.name = function.name
    )
    # end check of the required function from the required packages
    # end package checking

    # argument primary checking
    # arg with no default values
    mandat.args <- c(
        "data"
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
    if( ! is.null(n)){
        tempo <- cuteDev::arg_check(data = n, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, fun.name = function.name) ; eval(ee)
    }else{
        # no arg_check test here, it is just for checked.arg.names
        tempo <- cuteDev::arg_check(data = n, class = "vector")
        checked.arg.names <- c(checked.arg.names, tempo$object.name)
    }
    tempo <- cuteDev::arg_check(data = warn.print, class = "logical", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(argum.check)){
        if(any(argum.check) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
    # end argument primary checking
    # second round of checking and data preparation
    # management of NA arguments
    if( ! (all(class(arg.user.setting) == "list") & length(arg.user.setting) == 0)){
        tempo.arg <- names(arg.user.setting) # values provided by the user
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = length) == 1L # no argument provided by the user can be just NA
        if(any(tempo.log) == TRUE){
            tempo.cat <- paste0("ERROR IN ", function.name, "\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <-c(
        "data", 
        # "n", # because can be NULL
        "warn.print"
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
    ini.warning.length <- options()$warning.length
    options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0 
    # end warning initiation
    # other checkings
    if( ! is.null(n)){
        if(n < 1){
            tempo.cat <- paste0("ERROR IN ", function.name, ": n ARGUMENT MUST BE A POSITIVE AND NON NULL INTEGER")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", ifelse(is.null(warn), "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }else if(is.finite(n)){
            # warn.count <- warn.count + 1
            tempo.warn <- paste0("SOME COMPARTMENTS CAN BE TRUNCATED (n ARGUMENT IS ", n, ")")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, paste0(warn, "\n\n", tempo.warn)))
        }
    }
    # end other checkings
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # end second round of checking and data preparation

    # main code
    # new environment
    env.name <- paste0("env", as.numeric(Sys.time()))
    if(exists(env.name, where = -1)){ # verify if still ok when info() is inside a function
        tempo.cat <- paste0("ERROR IN ", function.name, ": ENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else{
        assign(env.name, new.env())
        assign("data", data, envir = get(env.name, envir = sys.nframe(), inherits = FALSE)) # data assigned in a new envir for test
    }
    # end new environment
    data.name <- deparse(substitute(data))
    output <- list("NAME" = data.name)
    tempo.try.error <- cuteDev::get_message(data = "class(data)", kind = "error", header = FALSE, env = get(env.name, envir = sys.nframe(), inherits = FALSE))
    if(is.null(tempo.try.error)){
        tempo <- list("CLASS" = class(data))
        output <- c(output, tempo)
    }
    tempo.try.error <- cuteDev::get_message(data = "typeof(data)", kind = "error", header = FALSE, env = get(env.name, envir = sys.nframe(), inherits = FALSE))
    if(is.null(tempo.try.error)){
        tempo <- list("TYPE" = typeof(data))
        output <- c(output, tempo)
    }
    tempo.try.error <- cuteDev::get_message(data = "length(data)", kind = "error", header = FALSE, env = get(env.name, envir = sys.nframe(), inherits = FALSE))
    if(is.null(tempo.try.error)){
        tempo <- list("LENGTH" = length(data))
        output <- c(output, tempo)
    }
    if(all(typeof(data) %in% c("integer", "numeric", "double")) & ! any(class(data) %in% "factor")){ # all() without na.rm -> ok because typeof(NA) is "logical" # any() without na.rm -> ok because class(NA) is "logical"
        tempo <- list("INF.NB" = sum(is.infinite(data)))
        output <- c(output, tempo)
        tempo <- list("RANGE" = range(data[ ! is.infinite(data)], na.rm = TRUE))
        output <- c(output, tempo)
        tempo <- list("SUM" = sum(data[ ! is.infinite(data)], na.rm = TRUE))
        output <- c(output, tempo)
        tempo <- list("MEAN" = mean(data[ ! is.infinite(data)], na.rm = TRUE))
        output <- c(output, tempo)
    }
    if(all(typeof(data) %in% c("logical", "integer", "double", "complex", "character", "list"))){ # all() without na.rm -> ok because typeof(NA) is "logical"
        tempo.try.error <- cuteDev::get_message(data = "is.na(data)", kind = "error", header = FALSE, env = get(env.name, envir = sys.nframe(), inherits = FALSE))
        if(is.null(tempo.try.error)){
            tempo <- list("NA.NB" = sum(is.na(data)))
            output <- c(output, tempo)
        }
    }
    tempo.try.error <- cuteDev::get_message(data = "head(data)", kind = "error", header = FALSE, env = get(env.name, envir = sys.nframe(), inherits = FALSE))
    if(is.null(tempo.try.error)){
        tempo <- list("HEAD" = head(data))
        output <- c(output, tempo)
        tempo <- list("TAIL" = tail(data)) # no reason that tail() does not work if head() works
        output <- c(output, tempo)
    }
    tempo.try.error <- cuteDev::get_message(data = "dim(data)", kind = "error", header = FALSE, env = get(env.name, envir = sys.nframe(), inherits = FALSE))
    if(is.null(tempo.try.error)){
        if(length(dim(data)) > 0){
            tempo <- list("DIMENSION" = dim(data))
            if(length(tempo[[1]]) == 2L){
                names(tempo[[1]]) <- c("NROW", "NCOL")
            }
            output <- c(output, tempo)
        }
    }
    if(all(class(data) == "data.frame") | all(class(data) %in% c("matrix", "array")) | all(class(data) == "table")){ # all() without na.rm -> ok because typeof(NA) is "logical"
        if(length(dim(data)) > 1){ # to avoid 1D table
            tempo <- list("ROW_NAMES" = dimnames(data)[[1]])
            output <- c(output, tempo)
            tempo <- list("COLUM_NAMES" = dimnames(data)[[2]])
            output <- c(output, tempo)
        }
    }
    tempo.try.error <- cuteDev::get_message(data = "summary(data)", kind = "error", header = FALSE, env = get(env.name, envir = sys.nframe(), inherits = FALSE))
    if(is.null(tempo.try.error)){
        tempo <- list("SUMMARY" = summary(data))
        output <- c(output, tempo)
    }
    tempo.try.error <- cuteDev::get_message(data = "noquote(matrix(base::capture.output(base::str(data))))", kind = "error", header = FALSE, env = get(env.name, envir = sys.nframe(), inherits = FALSE))
    if(is.null(tempo.try.error)){
        tempo <- base::capture.output(base::str(data))
        tempo <- list("STRUCTURE" = noquote(matrix(tempo, dimnames = list(rep("", length(tempo)), "")))) # str() print automatically, ls.str() not but does not give the order of the data.frame
        output <- c(output, tempo)
    }
    if(all(class(data) == "data.frame")){ # all() without na.rm -> ok because class(NA) is "logical"
        tempo <- list("COLUMN_TYPE" = sapply(data, FUN = "typeof"))
        if(any(sapply(data, FUN = "class") %in% "factor")){ # if an ordered factor is present, then sapply(data, FUN = "class") return a list but works with any(sapply(data, FUN = "class") %in% "factor") # any() without na.rm -> ok because class(NA) is "logical"
            tempo.class <- sapply(data, FUN = "class")
            if(any(unlist(tempo.class) %in% "ordered")){ # any() without na.rm -> ok because class(NA) is "logical"
                tempo2 <- sapply(tempo.class, paste, collapse = " ") # paste the "ordered" factor" in "ordered factor"
            }else{
                tempo2 <- unlist(tempo.class)
            }
            tempo[["COLUMN_TYPE"]][grepl(x = tempo2, pattern = "factor")] <- tempo2[grepl(x = tempo2, pattern = "factor")]
        }
        output <- c(output, tempo)
    }
    if(all(class(data) == "list")){ # all() without na.rm -> ok because class(NA) is "logical"
        tempo <- list("COMPARTMENT_NAMES" = names(data))
        output <- c(output, tempo)
        tempo <- list("COMPARTMENT_TYPE" = sapply(data, FUN = "typeof"))
        if(any(unlist(sapply(data, FUN = "class")) %in% "factor")){ # if an ordered factor is present, then sapply(data, FUN = "class") return a list but works with any(sapply(data, FUN = "class") %in% "factor")  # any() without na.rm -> ok because class(NA) is "logical"
            tempo.class <- sapply(data, FUN = "class")
            if(any(unlist(tempo.class) %in% "ordered")){ # any() without na.rm -> ok because class(NA) is "logical"
                tempo2 <- sapply(tempo.class, paste, collapse = " ") # paste the "ordered" factor" in "ordered factor"
            }else{
                tempo2 <- unlist(tempo.class)
            }
            tempo[["COMPARTMENT_TYPE"]][grepl(x = tempo2, pattern = "factor")] <- tempo2[grepl(x = tempo2, pattern = "factor")]
        }
        output <- c(output, tempo)
    }
    if( ! is.null(n)){
        output[names(output) != "STRUCTURE"] <- lapply(X = output[names(output) != "STRUCTURE"], FUN = head, n = n, simplify = FALSE)
    }
    # output
    # warning output
    if(warn.print == TRUE & ! is.null(warn)){
        on.exit(warning(paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE))
      }
      on.exit(expr = options(warning.length = ini.warning.length), add = TRUE)
    # end warning output
    if(warn.print == FALSE){
        output <- c(output, WARNING = warn)
    }else if(warn.print == TRUE & ! is.null(warn)){
        on.exit(warning(paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE))
    }
    on.exit(expr = options(warning.length = ini.warning.length), add = TRUE)
    return(output)
    # end output
    # end main code
}