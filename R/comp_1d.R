#' @title comp_1d
#' @description
#' Compare two 1D datasets (vector or factor or 1D table, or 1D matrix or 1D array) of the same class or not. Check and report in a list if the 2 datasets have:
#' 
#' - same class
#' 
#' - common elements
#' 
#' - common element names (except factors)
#' 
#' - common levels (factors only)
#' @param data1 Vector or factor or 1D table, or 1D matrix or 1D array.
#' @param data2 Vector or factor or 1D table, or 1D matrix or 1D array.
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
#' @returns 
#' A list containing:
#' 
#' - $same.class: logical. Are class identical?
#' 
#' - $class: class of the 2 datasets (NULL otherwise).
#' 
#' - $same.length: logical. Are number of elements identical?
#' 
#' - $length: number of elements in the 2 datasets (NULL otherwise).
#' 
#' - $same.levels: logical. Are levels identical? NULL if data1 and data2 are not factors.
#' 
#' - $levels: levels of the 2 datasets if identical (NULL otherwise or NULL if data1 and data2 are not factors).
#' 
#' - $any.id.levels: logical. Is there any identical levels? (NULL if data1 and data2 are not factors).
#' 
#' - $same.levels.pos1: positions, in data1, of the levels identical in data2 (NULL otherwise or NULL if data1 and data2 are not factors).
#' 
#' - $same.levels.pos2: positions, in data2, of the levels identical in data1 (NULL otherwise or NULL if data1 and data2 are not factors).
#' 
#' - $same.levels.match1: positions, in data2, of the levels that match the levels in data1, as given by match(data1, data2) (NULL otherwise or NULL if data1 and data2 are not factors).
#' 
#' - $same.levels.match2: positions, in data1, of the levels that match the levels in data2, as given by match(data1, data2) (NULL otherwise or NULL if data1 and data2 are not factors).
#' 
#' - $common.levels: common levels between data1 and data2 (can be a subset of $levels or not). NULL if no common levels or if data1 and data2 are not factors.
#' 
#' - $same.names: logical. Are element names identical? NULL if data1 and data2 have no names.
#' 
#' - $name: name of elements of the 2 datasets if identical (NULL otherwise).
#' 
#' - $any.id.name: logical. Is there any element names identical ?
#' 
#' - $same.names.pos1: positions, in data1, of the element names identical in data2. NULL if no identical names.
#' 
#' - $same.names.pos2: positions, in data2, of the elements names identical in data1. NULL if no identical names.
#' 
#' - $same.names.match1: positions, in data2, of the names that match the names in data1, as given by match(data1, data2) (NULL otherwise).
#' 
#' - $same.names.match2: positions, in data1, of the names that match the names in data2, as given by match(data1, data2) (NULL otherwise).
#' 
#' - $common.names: common element names between data1 and data2 (can be a subset of $name or not). NULL if no common element names.
#' 
#' - $any.id.element: logical. is there any identical elements ?
#' 
#' - $same.elements.pos1: positions, in data1, of the elements identical in data2. NULL if no identical elements.
#' 
#' - $same.elements.pos2: positions, in data2, of the elements identical in data1. NULL if no identical elements.
#' 
#' - $same.elements.match1: positions, in data2, of the elements that match the elements in data1, as given by match(data1, data2) (NULL otherwise).
#' 
#' - $same.elements.match2: positions, in data1, of the elements that match the elements in data2, as given by match(data1, data2) (NULL otherwise).
#' 
#' - $common.elements: common elements between data1 and data2. NULL if no common elements.
#' 
#' - $same.order: logical. Are all elements in the same order? TRUE or FALSE if elements of data1 and data2 are identical but not necessary in the same order. NULL otherwise (different length for instance).
#' 
#' - $order1: order of all elements of data1. NULL if $same.order is FALSE.
#' 
#' - $order2: order of all elements of data2. NULL if $same.order is FALSE.
#' 
#' - $identical.object: logical. Are objects identical (kind of object, element names, content, including content order)?
#'
#' - $identical.content: logical. Are content objects identical (identical elements, including order, excluding kind of object and element names)?
#' @examples
#' obs1 = 1:5 ; obs2 = 1:5 ; names(obs1) <- LETTERS[1:5] ; names(obs2) <- LETTERS[1:5] ; comp_1d(obs1, obs2)
#' obs1 = factor(LETTERS[1:5]) ; obs2 = factor(LETTERS[1:5]) ; comp_1d(obs1, obs2)
#' obs1 = factor(c(LETTERS[1:4], "E")) ; obs2 = factor(c(LETTERS[1:4], "F")) ; comp_1d(obs1, obs2)
#' obs1 = 1:5 ; obs2 = 1.1:6.1 ; comp_1d(obs1, obs2)
#' @export
comp_1d <- function(
        data1, 
        data2,
        safer_check = TRUE
){
    # DEBUGGING
    # data1 = 1:5 ; data2 = 1:5 ; names(data1) <- LETTERS[1:5] ; names(data2) <- LETTERS[1:5] ; safer_check = TRUE # for function debugging
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
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args, collapse = "\n"))
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
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) %in% base::c("list", "NULL"), na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.na), FUN = base::any)) & base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = base::length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
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
    if( ! base::any(base::class(data1) %in% base::c("logical", "integer", "numeric", "character", "factor", "table"))){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: THE data1 ARGUMENT MUST BE A NON NULL VECTOR, FACTOR OR 1D TABLE")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else if(base::all(base::class(data1) %in% "table")){
        if(base::length(base::dim(data1)) > 1){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: THE data1 ARGUMENT MUST BE A 1D TABLE")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    if( ! base::any(base::class(data2) %in% base::c("logical", "integer", "numeric", "character", "factor", "table"))){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: THE data2 ARGUMENT MUST BE A NON NULL VECTOR, FACTOR OR 1D TABLE")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else if(base::all(base::class(data2) %in% "table")){
        if(base::length(base::dim(data2)) > 1){
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: THE data2 ARGUMENT MUST BE A 1D TABLE")
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end other checkings
    
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # end second round of checking and data preparation

    # main code
    same.class <- FALSE
    class <- NULL
    same.length <- FALSE
    length <- NULL
    same.levels <- NULL # not FALSE to deal with no factors
    levels <- NULL
    any.id.levels <- FALSE
    same.levels.pos1 <- NULL
    same.levels.pos2 <- NULL
    same.levels.match1 <- NULL
    same.levels.match2 <- NULL
    common.levels <- NULL
    same.names <- NULL # not FALSE to deal with absence of name
    name <- NULL
    any.id.name <- FALSE
    same.names.pos1 <- NULL
    same.names.pos2 <- NULL
    same.names.match1 <- NULL
    same.names.match2 <- NULL
    common.names <- NULL
    any.id.element <- FALSE
    same.elements.pos1 <- NULL
    same.elements.pos2 <- NULL
    same.elements.match1 <- NULL
    same.elements.match2 <- NULL
    common.elements <- NULL
    same.order <- NULL
    order1 <- NULL
    order2 <- NULL
    identical.object <- FALSE
    identical.content <- FALSE
    if(base::identical(data1, data2)){
        same.class <- TRUE
        class <- base::class(data1)
        same.length <- TRUE
        length <- base::length(data1)
        if(base::any(base::class(data1) %in% "factor")){
            same.levels <- TRUE
            levels <- base::levels(data1)
            any.id.levels <- TRUE
            same.levels.pos1 <- 1:base::length(base::levels(data1))
            same.levels.pos2 <- 1:base::length(base::levels(data2))
            same.levels.match1 <- 1:base::length(base::levels(data1))
            same.levels.match2 <- 1:base::length(base::levels(data2))
            common.levels <- base::levels(data1)
        }
        if( ! base::is.null(base::names(data1))){
            same.names <- TRUE
            name <- base::names(data1)
            any.id.name <- TRUE
            same.names.pos1 <- 1:base::length(data1)
            same.names.pos2 <- 1:base::length(data2)
            same.names.match1 <- 1:base::length(data1)
            same.names.match2 <- 1:base::length(data2)
            common.names <- base::names(data1)
        }
        any.id.element <- TRUE
        same.elements.pos1 <- 1:base::length(data1)
        same.elements.pos2 <- 1:base::length(data2)
        same.elements.match1 <- 1:base::length(data1)
        same.elements.match2 <- 1:base::length(data2)
        common.elements <- data1
        same.order <- TRUE
        order1 <- base::order(data1)
        order2 <- base::order(data2)
        identical.object <- TRUE
        identical.content <- TRUE
    }else{
        if(base::identical(base::class(data1), base::class(data2))){
            same.class <- TRUE
            class <- base::class(data1)
        }
        if(base::identical(base::length(data1), base::length(data2))){
            same.length<- TRUE
            length <- base::length(data1)
        }
        if(base::any(base::class(data1) %in% "factor") & base::any(base::class(data2) %in% "factor")){
            if(base::identical(base::levels(data1), base::levels(data2))){
                same.levels <- TRUE
                levels <- base::levels(data1)
            }else{
                same.levels <- FALSE
            }
            if(base::any(base::levels(data1) %in% base::levels(data2))){
                any.id.levels <- TRUE
                same.levels.pos1 <- base::which(base::levels(data1) %in% base::levels(data2))
                same.levels.match1 <- base::match(base::levels(data1), base::levels(data2))
            }
            if(base::any(base::levels(data2) %in% base::levels(data1))){
                any.id.levels <- TRUE
                same.levels.pos2 <- base::which(base::levels(data2) %in% base::levels(data1))
                same.levels.match2 <- base::match(base::levels(data2), base::levels(data1))
            }
            if(any.id.levels == TRUE){
                common.levels <- base::unique(base::c(base::levels(data1)[same.levels.pos1], base::levels(data2)[same.levels.pos2]))
            }
        }
        if(base::any(base::class(data1) %in% "factor")){ # to compare content
            data1 <- base::as.character(data1)
        }
        if(base::any(base::class(data2) %in% "factor")){ # to compare content
            data2 <- base::as.character(data2)
        }
        if( ! (base::is.null(base::names(data1)) & base::is.null(base::names(data2)))){
            if(base::identical(base::names(data1), base::names(data2))){
                same.names <- TRUE
                name <- base::names(data1)
            }else{
                same.names <- FALSE
            }
            if(base::any(base::names(data1) %in% base::names(data2))){
                any.id.name <- TRUE
                same.names.pos1 <- base::which(base::names(data1) %in% base::names(data2))
                same.names.match1 <- base::match(base::names(data1), base::names(data2))
            }
            if(base::any(base::names(data2) %in% base::names(data1))){
                any.id.name <- TRUE
                same.names.pos2 <- base::which(base::names(data2) %in% base::names(data1))
                same.names.match2 <- base::match(base::names(data2), base::names(data1))
            }
            if(any.id.name == TRUE){
                common.names <- base::unique(base::c(base::names(data1)[same.names.pos1], base::names(data2)[same.names.pos2]))
            }
        }
        base::names(data1) <- NULL # names solved -> to do not be disturbed by names
        base::names(data2) <- NULL # names solved -> to do not be disturbed by names
        if(base::any(data1 %in% data2)){
            any.id.element <- TRUE
            same.elements.pos1 <- base::which(data1 %in% data2)
            same.elements.match1 <- base::match(data1, data2)
        }
        if(base::any(data2 %in% data1)){
            any.id.element <- TRUE
            same.elements.pos2 <- base::which(data2 %in% data1)
            same.elements.match2 <- base::match(data2, data1)
        }
        if(any.id.element == TRUE){
            common.elements <- base::unique(base::c(data1[same.elements.pos1], data2[same.elements.pos2]))
        }
        if(base::identical(data1, data2)){
            identical.content <- TRUE
            same.order <- TRUE
        }else if(base::identical(base::sort(data1), base::sort(data2))){
            same.order <- FALSE
            order1 <- base::order(data1)
            order2 <- base::order(data2)
        }
    }
    # output
    # warning output
    # end warning output
    output <- base::list(same.class = same.class, class = class, same.length = same.length, length = length, same.levels = same.levels, levels = levels, any.id.levels = any.id.levels, same.levels.pos1 = same.levels.pos1, same.levels.pos2 = same.levels.pos2, same.levels.match1 = same.levels.match1, same.levels.match2 = same.levels.match2, common.levels = common.levels, same.names = same.names, name = name, any.id.name = any.id.name, same.names.pos1 = same.names.pos1, same.names.pos2 = same.names.pos2, same.names.match1 = same.names.match1, same.names.match2 = same.names.match2, common.names = common.names, any.id.element = any.id.element, same.elements.pos1 = same.elements.pos1, same.elements.pos2 = same.elements.pos2, same.elements.match1 = same.elements.match1, same.elements.match2 = same.elements.match2, common.elements = common.elements, same.order = same.order, order1 = order1, order2 = order2, identical.object = identical.object, identical.content = identical.content)
    base::return(output)
    # end output
    # end main code
}

