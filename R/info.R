#' @title info
#' @description
#' Provide a broad description of an object.
#' @param data Object to analyse.
#' @param n Single positive integer value indicating the n first number of elements to display per compartment of the output list (i.e., head(..., n)). Write NULL to return all the elements. Does not apply for the $STRUCTURE compartment output.
#' @param warn.print Single logical value. Print potential warnings at the end of the execution? If FALSE the warning messages are added in the output list as an additional compartment (or NULL if no message).
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns
#' A list containing information, depending on the class and type of data. The backbone is generally:
#' 
#' - $NAME: name of the object.
#' 
#' - $CLASS: class of the object (class() value).
#' 
#' - $TYPE: type of the object (typeof() value).
#' 
#' - $LENGTH: length of the object (length() value).
#' 
#' - $NA.NB: number of NA and NaN (only for type "logical", "integer", "double", "complex", "character" or "list").
#' 
#' - $HEAD: head of the object (head() value).
#' 
#' - $TAIL: tail of the object (tail() value).
#' 
#' - $DIMENSION: dimension (only for object with dimensions).
#' 
#' - $SUMMARY: object summary (summary() value).
#' 
#' - $STRUCTURE: object structure (str() value).
#' 
#' - $WARNING: warning messages (only if the warn.print argument is FALSE).
#' 
#' If data is made of numerics, provide also:
#' 
#' - $INF.NB: number of Inf and -Inf.
#' 
#' - $RANGE: range after removing Inf and NA.
#' 
#' - $SUM: sum after removing Inf and NA.
#' 
#' - $MEAN: mean after removing Inf and NA.
#' 
#' If data is a 2D object, provide also:
#' 
#' - $ROW_NAMES: row names.
#' 
#' - $COL_NAMES: column names.
#' 
#' If data is a data frame, provide also:
#' 
#' - $COLUMN_TYPE: type of each column (typeof() value).
#' 
#' If data is a list, provide also:
#' 
#' - $COMPARTMENT_NAMES: names of the comprtments.
#' 
#' - $COMPARTMENT_TYPE: type of each compartment (typeof() value).
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' info(data = 1:3)
#' @importFrom saferDev arg_check
#' @importFrom saferDev get_message
#' @export
info <- function(
        data, 
        n = NULL, 
        warn.print = TRUE,
        safer_check = TRUE
){
    # DEBUGGING
    # mat1 <- matrix(1:3) ; data = mat1 ; n = NULL ; warn.print = TRUE ; safer_check = TRUE# for function debugging
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
    
    # two function from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check(
        fun = base::c(
            "saferDev::arg_check",
            "saferDev::get_message"
        ),
        lib.path = NULL,
        external.function.name = function.name
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
    if(base::any(tempo)){ # normally no NA for base::missing() output
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args, collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum.check <- base::c(argum.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    if( ! base::is.null(n)){
        tempo <- saferDev::arg_check(data = n, class = "vector", typeof = "integer", length = 1, double.as.integer.allowed = TRUE, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    }else{
        # no arg_check test here, it is just for checked.arg.names
        tempo <- saferDev::arg_check(data = n, class = "vector", safer_check = FALSE)
        checked.arg.names <- base::c(checked.arg.names, tempo$object.name)
    }
    tempo <- saferDev::arg_check(data = warn.print, class = "logical", length = 1, fun.name = function.name, safer_check = FALSE) ; base::eval(ee)
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
    # end argument primary checking


    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) == "list") & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.na), FUN = base::any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    # management of NULL arguments
    tempo.arg <-base::c(
        "data", 
        # "n", # because can be NULL
        "warn.print",
        "safer_check"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.null)
    if(base::any(tempo.log) == TRUE){# normally no NA with base::is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    # warning initiation
    ini.warning.length <- base::options()$warning.length
    base::options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0 
    # end warning initiation
    # other checkings
    if( ! base::is.null(n)){
        if(n < 1){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nn ARGUMENT MUST BE A POSITIVE AND NON NULL INTEGER")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n", base::ifelse(base::is.null(warn), "", base::paste0("IN ADDITION\nWARNING", base::ifelse(warn.count > 1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }else if(base::is.finite(n)){
            # warn.count <- warn.count + 1
            tempo.warn <- base::paste0("SOME COMPARTMENTS CAN BE TRUNCATED (n ARGUMENT IS ", n, ")")
            warn <- base::paste0(base::ifelse(base::is.null(warn), tempo.warn, base::paste0(warn, "\n\n", tempo.warn)))
        }
    }
    # end other checkings
    # end second round of checking and data preparation
    
    # main code
    # new environment
    env.name <- base::paste0("env", base::as.numeric(base::Sys.time()))
    if(base::exists(env.name, where = -1)){ # verify if still ok when info() is inside a function
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }else{
        base::assign(env.name, base::new.env())
        base::assign("data", data, envir = base::get(env.name, envir = base::sys.nframe(), inherits = FALSE)) # data assigned in a new envir for test
    }
    # end new environment
    data.name <- base::deparse(base::substitute(data))
    output <- base::list("NAME" = data.name)
    tempo.try.error <- saferDev::get_message(data = "base::class(data)", kind = "error", header = FALSE, env = base::get(env.name, envir =base::sys.nframe(), inherits = FALSE), safer_check = FALSE)
    if(base::is.null(tempo.try.error)){
        tempo <- base::list("CLASS" = base::class(data))
        output <- base::c(output, tempo)
    }
    tempo.try.error <- saferDev::get_message(data = "base::typeof(data)", kind = "error", header = FALSE, env = base::get(env.name, envir = base::sys.nframe(), inherits = FALSE), safer_check = FALSE)
    if(base::is.null(tempo.try.error)){
        tempo <- base::list("TYPE" = base::typeof(data))
        output <- base::c(output, tempo)
    }
    tempo.try.error <- saferDev::get_message(data = "base::length(data)", kind = "error", header = FALSE, env = base::get(env.name, envir = base::sys.nframe(), inherits = FALSE), safer_check = FALSE)
    if(base::is.null(tempo.try.error)){
        tempo <- base::list("LENGTH" = base::length(data))
        output <- base::c(output, tempo)
    }
    if(base::all(base::typeof(data) %in% base::c("integer", "numeric", "double")) & ! base::any(base::class(data) %in% "factor")){ # base::all() without na.rm -> ok because base::typeof(NA) is "logical" # base::any() without na.rm -> ok because base::class(NA) is "logical"
        tempo <- base::list("INF.NB" = base::sum(base::is.infinite(data)))
        output <- base::c(output, tempo)
        tempo <- base::list("RANGE" = base::range(data[ ! base::is.infinite(data)], na.rm = TRUE))
        output <- base::c(output, tempo)
        tempo <- base::list("SUM" = base::sum(data[ ! base::is.infinite(data)], na.rm = TRUE))
        output <- base::c(output, tempo)
        tempo <- base::list("MEAN" = base::mean(data[ ! base::is.infinite(data)], na.rm = TRUE))
        output <- base::c(output, tempo)
    }
    if(base::all(base::typeof(data) %in% base::c("logical", "integer", "double", "complex", "character", "list"))){ # base::all() without na.rm -> ok because base::typeof(NA) is "logical"
        tempo.try.error <- saferDev::get_message(data = "base::is.na(data)", kind = "error", header = FALSE, env = base::get(env.name, envir = base::sys.nframe(), inherits = FALSE), safer_check = FALSE)
        if(base::is.null(tempo.try.error)){
            tempo <- base::list("NA.NB" = base::sum(base::is.na(data)))
            output <- base::c(output, tempo)
        }
    }
    tempo.try.error <- saferDev::get_message(data = "utils::head(data)", kind = "error", header = FALSE, env = base::get(env.name, envir = base::sys.nframe(), inherits = FALSE), safer_check = FALSE)
    if(base::is.null(tempo.try.error)){
        tempo <- base::list("HEAD" = utils::head(data))
        output <- base::c(output, tempo)
        tempo <- base::list("TAIL" = utils::tail(data)) # no reason that utils::tail() does not work if utils::head() works
        output <- base::c(output, tempo)
    }
    tempo.try.error <- saferDev::get_message(data = "base::dim(data)", kind = "error", header = FALSE, env = base::get(env.name, envir = base::sys.nframe(), inherits = FALSE), safer_check = FALSE)
    if(base::is.null(tempo.try.error)){
        if(base::length(base::dim(data)) > 0){
            tempo <- base::list("DIMENSION" = base::dim(data))
            if(base::length(tempo[[1]]) == 2L){
                base::names(tempo[[1]]) <- base::c("NROW", "NCOL")
            }
            output <- base::c(output, tempo)
        }
    }
    if(base::all(base::class(data) == "data.frame") | base::all(base::class(data) %in% base::c("matrix", "array")) | base::all(base::class(data) == "table")){ # base::all() without na.rm -> ok because base::typeof(NA) is "logical"
        if(base::length(base::dim(data)) > 1){ # to avoid 1D table
            tempo <- base::list("ROW_NAMES" = base::dimnames(data)[[1]])
            output <- base::c(output, tempo)
            tempo <- base::list("COLUM_NAMES" = base::dimnames(data)[[2]])
            output <- base::c(output, tempo)
        }
    }
    tempo.try.error <- saferDev::get_message(data = "base::summary(data)", kind = "error", header = FALSE, env = base::get(env.name, envir = base::sys.nframe(), inherits = FALSE), safer_check = FALSE)
    if(base::is.null(tempo.try.error)){
        tempo <- base::list("SUMMARY" = base::summary(data))
        output <- base::c(output, tempo)
    }
    tempo.try.error <- saferDev::get_message(data = "base::noquote(base::matrix(utils::capture.output(utils::str(data))))", kind = "error", header = FALSE, env = base::get(env.name, envir = base::sys.nframe(), inherits = FALSE), safer_check = FALSE)
    if(base::is.null(tempo.try.error)){
        tempo <- utils::capture.output(utils::str(data))
        tempo <- base::list("STRUCTURE" = base::noquote(base::matrix(tempo, dimnames = base::list(base::rep("", base::length(tempo)), "")))) # utils::str() print automatically, utils::ls.str() not but does not give the order of the data.frame
        output <- base::c(output, tempo)
    }
    if(base::all(base::class(data) == "data.frame")){ # base::all() without na.rm -> ok because base::class(NA) is "logical"
        tempo <- base::list("COLUMN_TYPE" = base::sapply(data, FUN = "typeof"))
        if(base::any(base::sapply(data, FUN = "class") %in% "factor")){ # if an ordered factor is present, then base::sapply(data, FUN = "class") return a list but works with base::any(base::sapply(data, FUN = "class") %in% "factor") # base::any() without na.rm -> ok because base::class(NA) is "logical"
            tempo.class <- base::sapply(data, FUN = "class")
            if(base::any(base::unlist(tempo.class) %in% "ordered")){ # base::any() without na.rm -> ok because base::class(NA) is "logical"
                tempo2 <- base::sapply(tempo.class, paste, collapse = " ") # paste the "ordered" factor" in "ordered factor"
            }else{
                tempo2 <- base::unlist(tempo.class)
            }
            tempo[["COLUMN_TYPE"]][base::grepl(x = tempo2, pattern = "factor")] <- tempo2[base::grepl(x = tempo2, pattern = "factor")]
        }
        output <- base::c(output, tempo)
    }
    if(base::all(base::class(data) == "list")){ # base::all() without na.rm -> ok because base::class(NA) is "logical"
        tempo <- base::list("COMPARTMENT_NAMES" = base::names(data))
        output <- base::c(output, tempo)
        tempo <- base::list("COMPARTMENT_TYPE" = base::sapply(data, FUN = "typeof"))
        if(base::any(base::unlist(base::sapply(data, FUN = "class")) %in% "factor")){ # if an ordered factor is present, then base::sapply(data, FUN = "class") return a list but works with base::any(base::sapply(data, FUN = "class") %in% "factor")  # base::any() without na.rm -> ok because base::class(NA) is "logical"
            tempo.class <- base::sapply(data, FUN = "class")
            if(base::any(base::unlist(tempo.class) %in% "ordered")){ # base::any() without na.rm -> ok because base::class(NA) is "logical"
                tempo2 <- base::sapply(tempo.class, paste, collapse = " ") # paste the "ordered" factor" in "ordered factor"
            }else{
                tempo2 <- base::unlist(tempo.class)
            }
            tempo[["COMPARTMENT_TYPE"]][base::grepl(x = tempo2, pattern = "factor")] <- tempo2[base::grepl(x = tempo2, pattern = "factor")]
        }
        output <- base::c(output, tempo)
    }
    if( ! base::is.null(n)){
        output[base::names(output) != "STRUCTURE"] <- base::lapply(X = output[base::names(output) != "STRUCTURE"], FUN = utils::head, n = n, simplify = FALSE)
    }
    # output
    # warning output
    if(warn.print == TRUE & ! base::is.null(warn)){
        base::on.exit(base::warning(base::paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE))
    }
    base::on.exit(expr = base::options(warning.length = ini.warning.length), add = TRUE)
    # end warning output
    if(warn.print == FALSE){
        output <- base::c(output, WARNING = warn)
    }else if(warn.print == TRUE & ! base::is.null(warn)){
        base::on.exit(base::warning(base::paste0("FROM ", function.name, ":\n\n", warn), call. = FALSE))
    }
    base::on.exit(expr = base::options(warning.length = ini.warning.length), add = TRUE)
    base::return(output)
    # end output
    # end main code
}