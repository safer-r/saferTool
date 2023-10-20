######## fun_comp_1d() #### comparison of two 1D datasets (vectors, factors, 1D tables)

# todo list check OK
# Check r_debugging_tools-v1.4.R
# Check fun_test() 20201107 (see cute_checks.docx) 
# example sheet 
# check all and any OK
# -> clear to go Apollo
# -> transferred into the cute package

#' @title comp_1d
#' @description
#' Compare two 1D datasets (vector or factor or 1D table, or 1D matrix or 1D array) of the same class or not. Check and report in a list if the 2 datasets have:
#' - same class
#' - common elements
#' - common element names (except factors)
#' - common levels (factors only)
#' @param data1 Vector or factor or 1D table, or 1D matrix or 1D array.
#' @param data2 Vector or factor or 1D table, or 1D matrix or 1D array.
#' @returns 
# A list containing:
# - $same.class: logical. Are class identical?
# - $class: class of the 2 datasets (NULL otherwise).
# - $same.length: logical. Are number of elements identical?
# - $length: number of elements in the 2 datasets (NULL otherwise).
# - $same.levels: logical. Are levels identical? NULL if data1 and data2 are not factors.
# - $levels: levels of the 2 datasets if identical (NULL otherwise or NULL if data1 and data2 are not factors).
# - $any.id.levels: logical. Is there any identical levels? (NULL if data1 and data2 are not factors).
# - $same.levels.pos1: positions, in data1, of the levels identical in data2 (NULL otherwise or NULL if data1 and data2 are not factors).
# - $same.levels.pos2: positions, in data2, of the levels identical in data1 (NULL otherwise or NULL if data1 and data2 are not factors).
# - $same.levels.match1: positions, in data2, of the levels that match the levels in data1, as given by match(data1, data2) (NULL otherwise or NULL if data1 and data2 are not factors).
# - $same.levels.match2: positions, in data1, of the levels that match the levels in data2, as given by match(data1, data2) (NULL otherwise or NULL if data1 and data2 are not factors).
# - $common.levels: common levels between data1 and data2 (can be a subset of $levels or not). NULL if no common levels or if data1 and data2 are not factors.
# - $same.names: logical. Are element names identical? NULL if data1 and data2 have no names.
# - $name: name of elements of the 2 datasets if identical (NULL otherwise).
# - $any.id.name: logical. Is there any element names identical ?
# - $same.names.pos1: positions, in data1, of the element names identical in data2. NULL if no identical names.
# - $same.names.pos2: positions, in data2, of the elements names identical in data1. NULL if no identical names.
# - $same.names.match1: positions, in data2, of the names that match the names in data1, as given by match(data1, data2) (NULL otherwise).
# - $same.names.match2: positions, in data1, of the names that match the names in data2, as given by match(data1, data2) (NULL otherwise).
# - $common.names: common element names between data1 and data2 (can be a subset of $name or not). NULL if no common element names.
# - $any.id.element: logical. is there any identical elements ?
# - $same.elements.pos1: positions, in data1, of the elements identical in data2. NULL if no identical elements.
# - $same.elements.pos2: positions, in data2, of the elements identical in data1. NULL if no identical elements.
# - $same.elements.match1: positions, in data2, of the elements that match the elements in data1, as given by match(data1, data2) (NULL otherwise).
# - $same.elements.match2: positions, in data1, of the elements that match the elements in data2, as given by match(data1, data2) (NULL otherwise).
# - $common.elements: common elements between data1 and data2. NULL if no common elements.
# - $same.order: logical. Are all elements in the same order? TRUE or FALSE if elements of data1 and data2 are identical but not necessary in the same order. NULL otherwise (different length for instance).
# - $order1: order of all elements of data1. NULL if $same.order is FALSE.
# - $order2: order of all elements of data2. NULL if $same.order is FALSE.
# - $identical.object: logical. Are objects identical (kind of object, element names, content, including content order)?
# - $identical.content: logical. Are content objects identical (identical elements, including order, excluding kind of object and element names)?
#' @details 
#' REQUIRED PACKAGES
#' 
#' none
#' 
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' none
#' @examples
#' obs1 = 1:5 ; obs2 = 1:5 ; names(obs1) <- LETTERS[1:5] ; names(obs2) <- LETTERS[1:5] ; fun_comp_1d(obs1, obs2)
#' obs1 = 1:5 ; obs2 = 1:5 ; names(obs1) <- LETTERS[1:5] ; fun_comp_1d(obs1, obs2)
#' obs1 = 1:5 ; obs2 = 3:6 ; names(obs1) <- LETTERS[1:5] ; names(obs2) <- LETTERS[1:4] ; fun_comp_1d(obs1, obs2)
#' obs1 = factor(LETTERS[1:5]) ; obs2 = factor(LETTERS[1:5]) ; fun_comp_1d(obs1, obs2)
#' obs1 = factor(LETTERS[1:5]) ; obs2 = factor(LETTERS[10:11]) ; fun_comp_1d(obs1, obs2)
#' obs1 = factor(LETTERS[1:5]) ; obs2 = factor(LETTERS[4:7]) ; fun_comp_1d(obs1, obs2)
#' obs1 = factor(c(LETTERS[1:4], "E")) ; obs2 = factor(c(LETTERS[1:4], "F")) ; fun_comp_1d(obs1, obs2)
#' obs1 = 1:5 ; obs2 = factor(LETTERS[1:5]) ; fun_comp_1d(obs1, obs2)
#' obs1 = 1:5 ; obs2 = 1.1:6.1 ; fun_comp_1d(obs1, obs2)
#' obs1 = as.table(1:5); obs2 = as.table(1:5) ; fun_comp_1d(obs1, obs2)
#' obs1 = as.table(1:5); obs2 = 1:5 ; fun_comp_1d(obs1, obs2)
#' @export
fun_comp_1d <- function(
        data1, 
        data2
){
    # DEBUGGING
    # data1 = 1:5 ; data2 = 1:5 ; names(data1) <- LETTERS[1:5] ; names(data2) <- LETTERS[1:5] # for function debugging
    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    
    # required function checking
    # end required function checking
    
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    
    # arg with no default values
    mandat.args <- c(
        "data1", 
        "data2"
    )
    tempo <- eval(parse(text = paste0("missing(", paste0(mandat.args, collapse = ") | missing("), ")")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", "HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    
    # argument primary checking
    if( ! any(class(data1) %in% c("logical", "integer", "numeric", "character", "factor", "table"))){
        tempo.cat <- paste0("ERROR IN ", function.name, ": THE data1 ARGUMENT MUST BE A NON NULL VECTOR, FACTOR OR 1D TABLE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else if(all(class(data1) %in% "table")){
        if(length(dim(data1)) > 1){
            tempo.cat <- paste0("ERROR IN ", function.name, ": THE data1 ARGUMENT MUST BE A 1D TABLE")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    if( ! any(class(data2) %in% c("logical", "integer", "numeric", "character", "factor", "table"))){
        tempo.cat <- paste0("ERROR IN ", function.name, ": THE data2 ARGUMENT MUST BE A NON NULL VECTOR, FACTOR OR 1D TABLE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }else if(all(class(data2) %in% "table")){
        if(length(dim(data2)) > 1){
            tempo.cat <- paste0("ERROR IN ", function.name, ": THE data2 ARGUMENT MUST BE A 1D TABLE")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) # activate this line and use the function to check arguments status
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
        "data2"
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
    
    # reserved word checking
    # end reserved word checking
    # end second round of checking and data preparation
    
    # package checking
    # end package checking
    
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
    if(identical(data1, data2)){
        same.class <- TRUE
        class <- class(data1)
        same.length <- TRUE
        length <- length(data1)
        if(any(class(data1) %in% "factor")){
            same.levels <- TRUE
            levels <- levels(data1)
            any.id.levels <- TRUE
            same.levels.pos1 <- 1:length(levels(data1))
            same.levels.pos2 <- 1:length(levels(data2))
            same.levels.match1 <- 1:length(levels(data1))
            same.levels.match2 <- 1:length(levels(data2))
            common.levels <- levels(data1)
        }
        if( ! is.null(names(data1))){
            same.names <- TRUE
            name <- names(data1)
            any.id.name <- TRUE
            same.names.pos1 <- 1:length(data1)
            same.names.pos2 <- 1:length(data2)
            same.names.match1 <- 1:length(data1)
            same.names.match2 <- 1:length(data2)
            common.names <- names(data1)
        }
        any.id.element <- TRUE
        same.elements.pos1 <- 1:length(data1)
        same.elements.pos2 <- 1:length(data2)
        same.elements.match1 <- 1:length(data1)
        same.elements.match2 <- 1:length(data2)
        common.elements <- data1
        same.order <- TRUE
        order1 <- order(data1)
        order2 <- order(data2)
        identical.object <- TRUE
        identical.content <- TRUE
    }else{
        if(identical(class(data1), class(data2))){
            same.class <- TRUE
            class <- class(data1)
        }
        if(identical(length(data1), length(data2))){
            same.length<- TRUE
            length <- length(data1)
        }
        if(any(class(data1) %in% "factor") & any(class(data2) %in% "factor")){
            if(identical(levels(data1), levels(data2))){
                same.levels <- TRUE
                levels <- levels(data1)
            }else{
                same.levels <- FALSE
            }
            if(any(levels(data1) %in% levels(data2))){
                any.id.levels <- TRUE
                same.levels.pos1 <- which(levels(data1) %in% levels(data2))
                same.levels.match1 <- match(levels(data1), levels(data2))
            }
            if(any(levels(data2) %in% levels(data1))){
                any.id.levels <- TRUE
                same.levels.pos2 <- which(levels(data2) %in% levels(data1))
                same.levels.match2 <- match(levels(data2), levels(data1))
            }
            if(any.id.levels == TRUE){
                common.levels <- unique(c(levels(data1)[same.levels.pos1], levels(data2)[same.levels.pos2]))
            }
        }
        if(any(class(data1) %in% "factor")){ # to compare content
            data1 <- as.character(data1)
        }
        if(any(class(data2) %in% "factor")){ # to compare content
            data2 <- as.character(data2)
        }
        if( ! (is.null(names(data1)) & is.null(names(data2)))){
            if(identical(names(data1), names(data2))){
                same.names <- TRUE
                name <- names(data1)
            }else{
                same.names <- FALSE
            }
            if(any(names(data1) %in% names(data2))){
                any.id.name <- TRUE
                same.names.pos1 <- which(names(data1) %in% names(data2))
                same.names.match1 <- match(names(data1), names(data2))
            }
            if(any(names(data2) %in% names(data1))){
                any.id.name <- TRUE
                same.names.pos2 <- which(names(data2) %in% names(data1))
                same.names.match2 <- match(names(data2), names(data1))
            }
            if(any.id.name == TRUE){
                common.names <- unique(c(names(data1)[same.names.pos1], names(data2)[same.names.pos2]))
            }
        }
        names(data1) <- NULL # names solved -> to do not be disturbed by names
        names(data2) <- NULL # names solved -> to do not be disturbed by names
        if(any(data1 %in% data2)){
            any.id.element <- TRUE
            same.elements.pos1 <- which(data1 %in% data2)
            same.elements.match1 <- match(data1, data2)
        }
        if(any(data2 %in% data1)){
            any.id.element <- TRUE
            same.elements.pos2 <- which(data2 %in% data1)
            same.elements.match2 <- match(data2, data1)
        }
        if(any.id.element == TRUE){
            common.elements <- unique(c(data1[same.elements.pos1], data2[same.elements.pos2]))
        }
        if(identical(data1, data2)){
            identical.content <- TRUE
            same.order <- TRUE
        }else if(identical(sort(data1), sort(data2))){
            same.order <- FALSE
            order1 <- order(data1)
            order2 <- order(data2)
        }
    }
    # output
    output <- list(same.class = same.class, class = class, same.length = same.length, length = length, same.levels = same.levels, levels = levels, any.id.levels = any.id.levels, same.levels.pos1 = same.levels.pos1, same.levels.pos2 = same.levels.pos2, same.levels.match1 = same.levels.match1, same.levels.match2 = same.levels.match2, common.levels = common.levels, same.names = same.names, name = name, any.id.name = any.id.name, same.names.pos1 = same.names.pos1, same.names.pos2 = same.names.pos2, same.names.match1 = same.names.match1, same.names.match2 = same.names.match2, common.names = common.names, any.id.element = any.id.element, same.elements.pos1 = same.elements.pos1, same.elements.pos2 = same.elements.pos2, same.elements.match1 = same.elements.match1, same.elements.match2 = same.elements.match2, common.elements = common.elements, same.order = same.order, order1 = order1, order2 = order2, identical.object = identical.object, identical.content = identical.content)
    return(output)
    # end output
    # end main code
}

