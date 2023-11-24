#' @title comp_list
#' @description
#' Compare two lists. Check and report in a list if the 2 datasets have:
#' - same length
#' - common names
#' - common compartments
#' @param data1 List.
#' @param data2 List.
#' @returns 
#' A list containing:
#' - $same.length: logical. Are number of elements identical?
#' - $length: number of elements in the 2 datasets (NULL otherwise).
#' - $same.names: logical. Are element names identical ?
#' - $name: name of elements of the 2 datasets if identical (NULL otherwise).
#' - $any.id.name: logical. Is there any element names identical ?
#' - $same.names.pos1: positions, in data1, of the element names identical in data2.
#' - $same.names.pos2: positions, in data2, of the compartment names identical in data1.
#' - $any.id.compartment: logical. is there any identical compartments ?
#' - $same.compartment.pos1: positions, in data1, of the compartments identical in data2.
#' - $same.compartment.pos2: positions, in data2, of the compartments identical in data1.
#' - $identical.object: logical. Are objects identical (kind of object, compartment names and content)?
#' - $identical.content: logical. Are content objects identical (identical compartments excluding compartment names)?
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
#' obs1 = list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) ; 
#' obs2 = list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) ; 
#' fun_comp_list(obs1, obs2)
#' 
#' 
#' obs1 = list(1:5, LETTERS[1:2]) ; 
#' obs2 = list(a = 1:5, b = LETTERS[1:2]) ; 
#' fun_comp_list(obs1, obs2)
#' 
#' 
#' obs1 = list(b = 1:5, c = LETTERS[1:2]) ; 
#' obs2 = list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) ; 
#' fun_comp_list(obs1, obs2)
#' 
#' 
#' obs1 = list(b = 1:5, c = LETTERS[1:2]) ; 
#' obs2 = list(LETTERS[5:9], matrix(1:6), 1:5) ; 
#' fun_comp_list(obs1, obs2)
#' @export
fun_comp_list <- function(
        data1, 
        data2
){
    
    # DEBUGGING
    # data1 = list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) ; data2 = list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) # for function debugging
    # data1 = list(a = 1:5, b = LETTERS[1:2]) ; data2 = list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) # for function debugging
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
    tempo <- eval(parse(text = paste0("c(missing(", paste0(mandat.args, collapse = "),missing("), "))")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    
    # argument primary checking
    if( ! any(class(data1) %in% "list")){
        tempo.cat <- paste0("ERROR IN ", function.name, ": THE data1 ARGUMENT MUST BE A LIST")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! any(class(data2) %in% "list")){
        tempo.cat <- paste0("ERROR IN ", function.name, ": THE data2 ARGUMENT MUST BE A LIST")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
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
    if(identical(data1, data2)){
        same.length <- TRUE
        length <- length(data1)
        if( ! is.null(names(data1))){
            same.names <- TRUE
            name <- names(data1)
            any.id.name <- TRUE
            same.names.pos1 <- 1:length(data1)
            same.names.pos2 <- 1:length(data2)
        }
        any.id.compartment <- TRUE
        same.compartment.pos1 <- 1:length(data1)
        same.compartment.pos2 <- 1:length(data2)
        identical.object <- TRUE
        identical.content <- TRUE
    }else{
        identical.object <- FALSE
        if( ! identical(length(data1), length(data2))){
            same.length<- FALSE
        }else{
            same.length<- TRUE
            length <- length(data1)
        }
        if( ! (is.null(names(data1)) & is.null(names(data2)))){
            if( ! identical(names(data1), names(data2))){
                same.names <- FALSE
            }else{
                same.names <- TRUE
                name <- names(data1)
            }
            any.id.name <- FALSE
            if(any(names(data1) %in% names(data2))){
                any.id.name <- TRUE
                same.names.pos1 <- which(names(data1) %in% names(data2))
            }
            if(any(names(data2) %in% names(data1))){
                any.id.name <- TRUE
                same.names.pos2 <- which(names(data2) %in% names(data1))
            }
        }
        names(data1) <- NULL
        names(data2) <- NULL
        any.id.compartment <- FALSE
        if(any(data1 %in% data2)){
            any.id.compartment <- TRUE
            same.compartment.pos1 <- which(data1 %in% data2)
        }
        if(any(data2 %in% data1)){
            any.id.compartment <- TRUE
            same.compartment.pos2 <- which(data2 %in% data1)
        }
        if(same.length == TRUE & ! all(is.null(same.compartment.pos1), is.null(same.compartment.pos2), na.rm = TRUE)){
            if(identical(same.compartment.pos1, same.compartment.pos2)){
                identical.content <- TRUE
            }else{
                identical.content <- FALSE
            }
        }else{
            identical.content <- FALSE
        }
    }
    output <- list(same.length = same.length, length = length, same.names = same.names, name = name, any.id.name = any.id.name, same.names.pos1 = same.names.pos1, same.names.pos2 = same.names.pos2, any.id.compartment = any.id.compartment, same.compartment.pos1 = same.compartment.pos1, same.compartment.pos2 = same.compartment.pos2, identical.object = identical.object, identical.content = identical.content)
    # output
    return(output)
    # end output
    # end main code
}
