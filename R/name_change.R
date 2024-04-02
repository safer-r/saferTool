#' @title name_change
#' @description
#' This function allow to check if a vector of character strings, like column names of a data frame, has elements present in another vector (vector of reserved words or column names of another data frame before merging).
#' @param data1 Vector of character strings to check and modify.
#' @param data2 Reference vector of character strings (duplicated elemetns not authorized).
#' @param added.string Single character string added at the end of the modified string in elements of data1 if present in data2.
#' @param duplicate Single logical value. Return the elements of data1 still with duplicated elements? Defaut to TRUE, i.e., duplicated elements remain duplicated.
#' @returns
#' A list containing :
#' 
#' - $data: the modified data1 (in the same order as in the initial data1).
#' 
#' - $posi: the modified positions in data1. NULL if no modification.
#' 
#' - $ini: the initial elements before modification. NULL if no modification.
#' 
#' - $post: the modified elements in the same order as in ini. NULL if no modification.
#' @details
#'  - If elements are duplicated in data1 and match an element of data2, then the value of added.string is added at the end of the matched element of data1.
#'  - If the same occurs with the argument duplicate = FALSE, then value of added.string is added at the end of the matched element of data1, together with an incremented number so that elements in data1 are not anymore duplicated.
#'  - An error message is returned if any of the data1 elements end with the string in added.string.
#' @examples
#' obs1 <- c("A", "B", "C", "A", "D") ; 
#' obs2 <- c("A", "C") ; 
#' name_change(obs1, obs2)
#' @importFrom saferDev arg_check
#' @export
name_change <- function(
        data1, 
        data2, 
        added.string = "_modif",
        duplicate = TRUE
){
    # DEBUGGING
    # data1 = c("A", "A", "B", "C", "A", "D") ; data2 <- c("A", "C") ; added.string = "_modif" ; duplicate = TRUE # for function debugging
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
    # package checking
    # check of lib.path
    # end check of lib.path
    
    # check of the required function from the required packages
    .pack_and_function_check(
        fun = base::c(
            "saferDev::arg_check"
        ),
        lib.path = NULL,
        external.function.name = function.name
    )
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
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- base::expression(argum.check <- base::c(argum.check, tempo$problem) , text.check <- base::c(text.check, tempo$text) , checked.arg.names <- base::c(checked.arg.names, tempo$object.name))
    tempo <- saferDev::arg_check(data = data1, class = "vector", mode = "character", fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = data2, class = "vector", mode = "character", fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = added.string, class = "vector", mode = "character", length = 1, fun.name = function.name) ; base::eval(ee)
    tempo <- saferDev::arg_check(data = duplicate, class = "vector", mode = "logical", length = 1, fun.name = function.name) ; base::eval(ee)
    if( ! base::is.null(argum.check)){
        if(base::any(argum.check, na.rm = TRUE) == TRUE){
            base::stop(base::paste0("\n\n================\n\n", base::paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
    # end check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
    # management of NA arguments
    if( ! (base::all(base::class(arg.user.setting) == "list", na.rm = TRUE) & base::length(arg.user.setting) == 0)){
        tempo.arg <- base::names(arg.user.setting) # values provided by the user
        tempo.log <- base::suppressWarnings(base::sapply(base::lapply(base::lapply(tempo.arg, FUN = base::get, env = base::sys.nframe(), inherit = FALSE), FUN = is.na), FUN = any)) & base::lapply(base::lapply(tempo.arg, FUN = get, env = base::sys.nframe(), inherit = FALSE), FUN = base::length) == 1L # no argument provided by the user can be just NA
        if(base::any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \n", base::ifelse(base::sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", base::paste0(tempo.arg[tempo.log], collapse = "\n"))
            base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    
    # management of NULL arguments
    tempo.arg <- base::c(
        "data1", 
        "data2", 
        "added.string",
        "duplicate"
    )
    tempo.log <- base::sapply(base::lapply(tempo.arg, FUN = get, env = base::sys.nframe(), inherit = FALSE), FUN = base::is.null)
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
    if(base::any(base::duplicated(data2))){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\nTHE data2 ARGUMENT CANNOT HAVE DUPLICATED ELEMENTS:\n", base::paste0(base::unique(data2[base::duplicated(data2)]), collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    tempo <- base::grepl(x = data1, pattern = base::paste0("^.*", added.string, "$"))
    if(base::any(tempo)){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\nELEMENTS OF THE data1 ARGUMENT END WITH THE STRING OF THE added.string (", added.string, ") ARGUMENT:\n", base::paste0(data1[tempo], collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end other checkings
    # end second round of checking and data preparation
    
    
    # package checking
    # end package checking
    
    # main code
    data <- data1
    posi <- NULL
    ini <- NULL
    post <- NULL
    if(base::any(data1 %in% data2)){
        ini <- data1[data1 %in% data2]
        match.names.unique <- base::unique(ini)
        post <- base::vector(mode = "character", base::length(ini))
        for(i2 in 1:base::length(match.names.unique)){
            count <- NULL
            tempo <- base::sum(ini %in% match.names.unique[i2], na.rm = TRUE) # duplicated elements in data1 that match 1 element in data2 
            if(tempo > 1 & duplicate == FALSE){ 
                count <- 1:tempo
            }
            res <- base::paste0(match.names.unique[i2], added.string, count)
            data[data1 %in% match.names.unique[i2]] <- res
            post[ini %in% match.names.unique[i2]] <- res
        }
        posi <- base::which( ! data1 %in% data)
    }

    # output
    # warning output
    # end warning output
    output <- base::list(data = data, posi = posi, ini = ini, post = post)
    base::return(output)
    # end output
    # end main code
}

