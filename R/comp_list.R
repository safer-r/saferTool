#' @title comp_list
#' @description
#' Compare two lists. Check and report in a list if the 2 datasets have:
#' 
#' - same length
#' 
#' - common names
#' 
#' - common compartments
#' @param data1 List.
#' @param data2 List.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns 
#' A list containing:
#' 
#' - $same.length: logical. Are number of elements identical?
#' 
#' - $length: number of elements in the 2 datasets (NULL otherwise).
#' 
#' - $same.names: logical. Are element names identical ?
#' 
#' - $name: name of elements of the 2 datasets if identical (NULL otherwise).
#' 
#' - $any.id.name: logical. Is there any element names identical ?
#' 
#' - $same.names.pos1: positions, in data1, of the element names identical in data2.
#' 
#' - $same.names.pos2: positions, in data2, of the compartment names identical in data1.
#' 
#' - $any.id.compartment: logical. is there any identical compartments ?
#' 
#' - $same.compartment.pos1: positions, in data1, of the compartments identical in data2.
#' 
#' - $same.compartment.pos2: positions, in data2, of the compartments identical in data1.
#' 
#' - $identical.object: logical. Are objects identical (kind of object, compartment names and content)?
#' 
#' - $identical.content: logical. Are content objects identical (identical compartments excluding compartment names)?
#' @examples
#' obs1 = list(1:5, LETTERS[1:2]) ; 
#' obs2 = list(a = 1:5, b = LETTERS[1:2]) ; 
#' comp_list(obs1, obs2)
#' @export
comp_list <- function(
        data1, 
        data2,
        safer_check = TRUE
){
    
    # DEBUGGING
    # data1 = list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) ; data2 = list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) ; safer_check = TRUE # for function debugging
    # data1 = list(a = 1:5, b = LETTERS[1:2]) ; data2 = list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) ; safer_check = TRUE# for function debugging
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
            external.package.name = package.name)
    }
    # end critical operator checking

    
    # package checking
    # check of lib.path
    # end check of lib.path

    # check of the required function from the required packages
    # end check of the required function from the required packages
    # end package checking
    
    # argument primary checking
    # arg with no default values
    mandat.args <- base::c(
        "data1", 
        "data2"
    )
    tempo <- base::eval(base::parse(text = base::paste0("base::c(base::missing(", base::paste0(mandat.args, collapse = "),base::missing("), "))")))
    if(base::any(tempo)){ # normally no NA for missing() output
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args, collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with arg_check()
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) # activate this line and use the function to check arguments status
    # end check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
    # reserved word checking to avoid bugs
    # end reserved word checking to avoid bugs
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) %in% base::c("list", "NULL"), na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.na), FUN = base::any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    
    # management of NULL arguments
    tempo.arg <-base::c(
        "data1", 
        "data2",
        "safer_check"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.null)
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
    if( ! base::any(base::class(data1) %in% "list")){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: THE data1 ARGUMENT MUST BE A LIST")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! base::any(base::class(data2) %in% "list")){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: THE data2 ARGUMENT MUST BE A LIST")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end other checkings
    # end second round of checking and data preparation
    
    # main code
    same.length <- NULL
    length <- NULL
    same.names <- NULL
    name <- NULL
    any.id.name <- NULL
    same.names.pos1 <- NULL
    same.names.pos2 <- NULL
    any.id.compartment <- NULL
    same.compartment.pos1 <- NULL
    same.compartment.pos2 <- NULL
    identical.object <- NULL
    identical.content <- NULL
    if(base::identical(data1, data2)){
        same.length <- TRUE
        length <- base::length(data1)
        if( ! base::is.null(base::names(data1))){
            same.names <- TRUE
            name <- base::names(data1)
            any.id.name <- TRUE
            same.names.pos1 <- 1:base::length(data1)
            same.names.pos2 <- 1:base::length(data2)
        }
        any.id.compartment <- TRUE
        same.compartment.pos1 <- 1:base::length(data1)
        same.compartment.pos2 <- 1:base::length(data2)
        identical.object <- TRUE
        identical.content <- TRUE
    }else{
        identical.object <- FALSE
        if( ! base::identical(base::length(data1), base::length(data2))){
            same.length<- FALSE
        }else{
            same.length<- TRUE
            length <- base::length(data1)
        }
        if( ! (base::is.null(base::names(data1)) & base::is.null(base::names(data2)))){
            if( ! base::identical(base::names(data1), base::names(data2))){
                same.names <- FALSE
            }else{
                same.names <- TRUE
                name <- base::names(data1)
            }
            any.id.name <- FALSE
            if(base::any(base::names(data1) %in% base::names(data2))){
                any.id.name <- TRUE
                same.names.pos1 <- base::which(base::names(data1) %in% base::names(data2))
            }
            if(base::any(base::names(data2) %in% base::names(data1))){
                any.id.name <- TRUE
                same.names.pos2 <- base::which(base::names(data2) %in% base::names(data1))
            }
        }
        base::names(data1) <- NULL
        base::names(data2) <- NULL
        any.id.compartment <- FALSE
        if(base::any(data1 %in% data2)){
            any.id.compartment <- TRUE
            same.compartment.pos1 <- base::which(data1 %in% data2)
        }
        if(base::any(data2 %in% data1)){
            any.id.compartment <- TRUE
            same.compartment.pos2 <- base::which(data2 %in% data1)
        }
        if(same.length == TRUE & ! base::all(base::is.null(same.compartment.pos1), base::is.null(same.compartment.pos2), na.rm = TRUE)){
            if(base::identical(same.compartment.pos1, same.compartment.pos2)){
                identical.content <- TRUE
            }else{
                identical.content <- FALSE
            }
        }else{
            identical.content <- FALSE
        }
    }
    output <- base::list(same.length = same.length, length = length, same.names = same.names, name = name, any.id.name = any.id.name, same.names.pos1 = same.names.pos1, same.names.pos2 = same.names.pos2, any.id.compartment = any.id.compartment, same.compartment.pos1 = same.compartment.pos1, same.compartment.pos2 = same.compartment.pos2, identical.object = identical.object, identical.content = identical.content)
    # output
    # warning output
    # end warning output
    base::return(output)
    # end output
    # end main code
}
