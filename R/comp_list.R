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
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns 
#' A list containing:
#' 
#' - $same.length: logical. Are number of compartments identical?
#' 
#' - $length: number of compartments in the 2 datasets (NULL otherwise).
#' 
#' - $same.names: logical. Are compartment names identical? NULL if data1 and data2 have no names.
#' 
#' - $name: name of compartments of the 2 datasets if identical (NULL otherwise).
#' 
#' - $any.id.names: logical. Is there any compartment names identical ?
#' 
#' - $same.names.pos1: positions, in data1, of the compartment names identical in data2. NULL if no identical names.
#' 
#' - $same.names.pos2: positions, in data2, of the compartment names identical in data1. NULL if no identical names.
#' 
#' - $same.names.match1: positions, in data2, of the names that match the names in data1, as given by match(data1, data2) (NULL otherwise).
#' 
#' - $same.names.match2: positions, in data1, of the names that match the names in data2, as given by match(data1, data2) (NULL otherwise).
#' 
#' - $common.names: common compartment names between data1 and data2 (can be a subset of $name or not). NULL if no common compartment names.
#' 
#' - $any.id.compartments: logical. is there any identical compartments ?
#' 
#' - $same.compartments.pos1: positions, in data1, of the compartments identical in data2. NULL if no identical compartments.
#' 
#' - $same.compartments.pos2: positions, in data2, of the compartments identical in data1. NULL if no identical compartments.
#' 
#' - $same.compartments.match1: positions, in data2, of the compartments that match the compartments in data1, as given by match(data1, data2) (NULL otherwise).
#' 
#' - $same.compartments.match2: positions, in data1, of the compartments that match the compartments in data2, as given by match(data1, data2) (NULL otherwise).
#' 
#' - $common.compartments: common compartments between data1 and data2. NULL if no common compartments.
#' 
#' - $same.order: logical. Are all compartments in the same order? TRUE if compartments of data1 and data2 are identical. FALSE if compartments of data1 and data2 are made of the same compartments but not in the same order. NULL otherwise (different length for instance).
#' 
#' - $identical.object: logical. Are lists identical (compartment names, content, including content order)?
#'
#' - $identical.content: logical. Are content list identical (identical compartments, including order, excluding kind of object and compartment names)?
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' obs1 = list(1:5, LETTERS[1:2]) ; 
#' obs2 = list(a = 1:5, b = LETTERS[1:2]) ; 
#' obs3 = list(LETTERS[1:2], 1:5) ; 
#' comp_list(obs1, obs2) ;
#' comp_list(obs1, obs3)
#' @export
comp_list <- function(
        data1, 
        data2,
        safer_check = TRUE
){
    
    # DEBUGGING
    # data1 = list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) ; data2 = list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) ; safer_check = TRUE # for function debugging
    # data1 = list(a = 1:5, b = LETTERS[1:2]) ; data2 = list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) ; safer_check = TRUE # for function debugging
    # data1 = list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) ; data2 = list(LETTERS[5:9], matrix(1:6), 1:5) ; safer_check = TRUE # for function debugging
    # data1 = list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) ; data2 = list(a = 1:5, b = LETTERS[1:2], e = matrix(1:6)) ; safer_check = TRUE # for function debugging
    # data1 = list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) ; data2 = list(a = 1:5, b = LETTERS[1:2], e = matrix(1:6)) ; safer_check = TRUE # for function debugging
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
    if(base::any(tempo)){ # normally no NA for base::missing() output
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args[tempo], collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
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
        if(base::any(tempo.log) == TRUE){ # normally no NA because base::is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
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
    if(base::any(tempo.log) == TRUE){# normally no NA with base::is.null()
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), base::paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    
    # warning initiation
    # end warning initiation
    
    # other checkings
    if( ! base::any(base::class(data1) %in% "list")){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE data1 ARGUMENT MUST BE A LIST")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! base::any(base::class(data2) %in% "list")){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE data2 ARGUMENT MUST BE A LIST")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end other checkings
    # end second round of checking and data preparation
    
    # main code
    same.length <- FALSE
    length2 <- NULL
    same.names <- NULL # not FALSE to deal with absence of name
    name <- NULL
    any.id.names <- FALSE
    same.names.pos1 <- NULL
    same.names.pos2 <- NULL
    same.names.match1 <- NULL
    same.names.match2 <- NULL
    common.names <- NULL
    any.id.compartments <- FALSE
    same.compartments.pos1 <- NULL
    same.compartments.pos2 <- NULL
    same.compartments.match1 <- NULL
    same.compartments.match2 <- NULL
    common.compartments <- NULL
    same.order <- NULL
    identical.object <- FALSE
    identical.content <- FALSE
    if(base::identical(data1, data2)){
        same.length <- TRUE
        length2 <- base::length(data1)
        if( ! base::is.null(base::names(data1))){
            same.names <- TRUE
            name <- base::names(data1)
            any.id.names <- TRUE
            same.names.pos1 <- 1:base::length(data1)
            same.names.pos2 <- 1:base::length(data2)
            same.names.match1 <- 1:base::length(data1)
            same.names.match2 <- 1:base::length(data2)
            common.names <- base::names(data1)
        }
        any.id.compartments <- TRUE
        same.compartments.pos1 <- 1:base::length(data1)
        same.compartments.pos2 <- 1:base::length(data2)
        same.compartments.match1 <- 1:base::length(data1)
        same.compartments.match2 <- 1:base::length(data2)
        common.compartments <- data1
        same.order <- TRUE
        identical.object <- TRUE
        identical.content <- TRUE
    }else{
        if(base::identical(base::length(data1), base::length(data2))){
            same.length <- TRUE
            length2 <- base::length(data1)
        }
        if( ! (base::is.null(base::names(data1)) & base::is.null(base::names(data2)))){
            if( ! base::identical(base::names(data1), base::names(data2))){
                same.names <- FALSE
            }else{
                same.names <- TRUE
                name <- base::names(data1)
            }
            if(base::any(base::names(data1) %in% base::names(data2))){
                any.id.names <- TRUE
                same.names.pos1 <- base::which(base::names(data1) %in% base::names(data2))
                same.names.match1 <- base::match(base::names(data1), base::names(data2))
            }
            if(base::any(base::names(data2) %in% base::names(data1))){
                any.id.names <- TRUE
                same.names.pos2 <- base::which(base::names(data2) %in% base::names(data1))
                same.names.match2 <- base::match(base::names(data2), base::names(data1))
            }
            if(any.id.names == TRUE){
                common.names <- base::unique(base::c(base::names(data1)[same.names.pos1], base::names(data2)[same.names.pos2]))
            }
        }
        base::names(data1) <- NULL # names solved -> to do not be disturbed by names
        base::names(data2) <- NULL # names solved -> to do not be disturbed by names
        if(base::any(data1 %in% data2)){
            any.id.compartments <- TRUE
            same.compartments.pos1 <- base::which(data1 %in% data2)
            same.compartments.match1 <- base::match(data1, data2)
        }
        if(base::any(data2 %in% data1)){
            any.id.compartments <- TRUE
            same.compartments.pos2 <- base::which(data2 %in% data1)
            same.compartments.match2 <- base::match(data2, data1)
        }
        if(any.id.compartments == TRUE){
            common.compartments <- base::unique(base::c(data1[same.compartments.pos1], data2[same.compartments.pos2]))
        }
        if(base::identical(data1, data2)){
            identical.content <- TRUE
            same.order <- TRUE
        }else if(same.length == TRUE & all( ! is.na(same.compartments.match1)) & all( ! is.na(same.compartments.match2))){
                same.order <- FALSE
        }
    }
    # output
    # warning output
    # end warning output
    output <- base::list(same.length = same.length, length = length2, same.names = same.names, name = name, any.id.names = any.id.names, same.names.pos1 = same.names.pos1, same.names.pos2 = same.names.pos2, same.names.match1 = same.names.match1, same.names.match2 = same.names.match2, common.names = common.names, any.id.compartments = any.id.compartments, same.compartments.pos1 = same.compartments.pos1, same.compartments.pos2 = same.compartments.pos2, same.compartments.match1 = same.compartments.match1, same.compartments.match2 = same.compartments.match2, common.compartments = common.compartments, same.order = same.order, identical.object = identical.object, identical.content = identical.content)
    base::return(output)
    # end output
    # end main code
}
