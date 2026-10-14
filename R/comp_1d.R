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
#' @param safer_check Single logical value. Perform some "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) correct \code{lib_path} argument value 2) required functions and related packages effectively present in local R libraries and 3) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{unique(c(new, .Library.site, .Library))}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default) or if the \code{safer_check} argument is \code{FALSE}: only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}. Of note, in \code{arg_check()}, \code{error_text} is also used at the end of the string returned when no problem is detected.
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
#' - $any.id.names: logical. Is there any element names identical ?
#' 
#' - $same.names.pos1: positions, in data1, of the element names identical in data2. NULL if no identical names.
#' 
#' - $same.names.pos2: positions, in data2, of the element names identical in data1. NULL if no identical names.
#' 
#' - $same.names.match1: positions, in data2, of the names that match the names in data1, as given by match(data1, data2) (NULL otherwise).
#' 
#' - $same.names.match2: positions, in data1, of the names that match the names in data2, as given by match(data1, data2) (NULL otherwise).
#' 
#' - $common.names: common element names between data1 and data2 (can be a subset of $name or not). NULL if no common element names.
#' 
#' - $any.id.elements: logical. is there any identical elements ?
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
#' - $order1: order of all elements of data1. NULL if $same.order is NULL.
#' 
#' - $order2: order of all elements of data2. NULL if $same.order is NULL.
#' 
#' - $identical.object: logical. Are objects identical (kind of object, element names, content, including content order)?
#'
#' - $identical.content: logical. Are content objects identical (identical elements, including order, excluding kind of object and element names)?
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' obs1 = 1:5 ; obs2 = 1:5 ; names(obs1) <- LETTERS[1:5] ; names(obs2) <- LETTERS[1:5] ; comp_1d(obs1, obs2)
#' obs1 = factor(LETTERS[1:5]) ; obs2 = factor(LETTERS[1:5]) ; comp_1d(obs1, obs2)
#' obs1 = factor(c(LETTERS[1:4], "E")) ; obs2 = factor(c(LETTERS[1:4], "F")) ; comp_1d(obs1, obs2)
#' obs1 = 1:5 ; obs2 = 1.1:6.1 ; comp_1d(obs1, obs2)
#' @export
comp_1d <- function(
    data1, 
    data2,
    safer_check = TRUE, 
    lib_path = NULL, 
    error_text = ""
){
    # DEBUGGING
    # data1 = 1:5 ; data2 = 1:5 ; names(data1) <- LETTERS[1:5] ; names(data2) <- LETTERS[1:5] ; safer_check = TRUE # for function debugging


    #### package name
    package_name <- "saferTool" # write NULL if the function developed is not in a package
    #### end package name

    #### internal error report link
    internal_error_report_link <- base::paste0("https://github.com/safer-r/", package_name, "/issues/new", collapse = NULL, recycle0 = FALSE) # link where to post an issue indicated in an internal error message. Write NULL if no link to propose, or no internal error message
    #### end internal error report link

    #### function name
    tempo_settings <- base::as.list(x = base::match.call(definition = base::sys.function(which = base::sys.parent(n = 0)), call = base::sys.call(which = base::sys.parent(n = 0)), expand.dots = FALSE, envir = base::parent.frame(n = 2L))) # warning: I have written n = 0 to avoid error when a safer function is inside another functions. In addition, arguments values retrieved are not evaluated base::match.call, but this is solved with get() below
    function_name <- base::paste0(tempo_settings[[1]], "()", collapse = NULL, recycle0 = FALSE) 
    # function name with "()" paste, which split into a vector of three: c("::()", "package ()", "function ()") if "package::function()" is used.
    if(function_name[1] == "::()" | function_name[1] == ":::()"){
        function_name <- function_name[3]
    }
    #### end function name

    #### arguments settings
    arg_user_setting <- tempo_settings[-1] # list of the argument settings (excluding default values not provided by the user). Always a list, even if 1 argument. So ok for lapply() usage (management of NA section)
    arg_user_setting_names <- base::names(x = arg_user_setting)
    # evaluation of values if they are expression, call, etc.
    if(base::length(x = arg_user_setting) != 0){
        arg_user_setting_eval <- base::lapply(
            X = arg_user_setting_names, 
            FUN = function(x){
                base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = TRUE) # n = 2 because of lapply(), inherit = TRUE to be sure to correctly evaluate
            }
        )
        base::names(x = arg_user_setting_eval) <- arg_user_setting_names
    }else{
        arg_user_setting_eval <- NULL
    }
    # end evaluation of values if they are expression, call, etc.
    arg_names <- base::names(x = base::formals(fun = base::sys.function(which = base::sys.parent(n = 0)), envir = base::parent.frame(n = 1))) # names of all the arguments
    #### end arguments settings

    #### error_text initiation

    ######## basic error text start
    error_text <- base::paste0(base::unlist(x = error_text, recursive = TRUE, use.names = TRUE), collapse = "", recycle0 = FALSE) # convert everything to string. if error_text is a string, changes nothing. If NULL or empty (even list) -> "" so no need to check for management of NULL or empty value
    package_function_name <- base::paste0(
        base::ifelse(test = base::is.null(x = package_name), yes = "", no = base::paste0(package_name, base::ifelse(test = base::grepl(x = function_name, pattern = "^\\.", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), yes = ":::", no = "::"), collapse = NULL, recycle0 = FALSE)), 
        function_name,
        collapse = NULL, 
        recycle0 = FALSE
    )
    error_text_start <- base::paste0(
        "ERROR IN ", # must not be changed, because this "ERROR IN " string is used for text replacement
        package_function_name, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    ######## end basic error text start

    ######## internal error text
    intern_error_text_start <- base::paste0(
        package_function_name, 
        base::ifelse(test = error_text == "", yes = ".", no = error_text), 
        "\n\n", 
        collapse = NULL, 
        recycle0 = FALSE
    )
    intern_error_text_end <- base::ifelse(test = base::is.null(x = internal_error_report_link), yes = "", no = base::paste0("\n\nPLEASE, REPORT THIS ERROR HERE: ", internal_error_report_link, ".", collapse = NULL, recycle0 = FALSE))
    ######## end internal error text

    ######## error text when embedding
    # use this in the error_text of safer functions if present in your main code 
    embed_error_text  <- base::sub(pattern = "^ERROR IN ", replacement = " INSIDE ", x = error_text_start, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
    embed_error_text  <- base::sub(pattern = "\n*$", replacement = "", x = embed_error_text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) # remove all the trailing \n, because added later
    ######## end error text when embedding

    #### end error_text initiation

    #### argument primary checking

    ######## arg ... forbidden
    # nocov start
    # codecov inactivated because it is an internal control of code writing, impossible to cover with argument values.
    if("..." %in% arg_names) {
        # This check is here in case the developer has not correctly written the argument of its function
        tempo_cat <- base::paste0(
            error_text_start, 
            "ARGUMENT ... IS NOT ALLOWED IN SAFER-R FUNCTIONS.\n\nPLEASE, REWRITE YOUR FUNCTION CORRECTLY.", 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    # nocov end
    ######## end arg ... forbidden

    ######## mandatory arg of safer-r functions
    mandat_args <- base::c("lib_path", "safer_check", "error_text")
    tempo_log <- ! mandat_args %in% arg_names
    if(base::any(x = tempo_log, na.rm = TRUE)) {
        # This check is here in case the developer has not correctly written the argument of its function
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S ARE", no = " IS"), 
            " MANDATORY IN SAFER-R FUNCTIONS:\n", 
            base::paste0(mandat_args[tempo_log], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end mandatory arg of safer-r functions

    ######## arg with no default values
    # optional section: remove the code if none of your arguments has no default value
    no_def_args <- base::c(
        "data1", 
        "data2"
    )
    tempo <- base::eval(expr = base::parse(text = base::paste0("base::c(base::missing(", base::paste0(no_def_args, collapse = "),base::missing(", recycle0 = FALSE), "))", collapse = NULL, recycle0 = FALSE), file = "", n = NULL, prompt = "?", keep.source = base::getOption(x = "keep.source", default = NULL), srcfile = NULL, encoding = "unknown"), envir = base::environment(fun = NULL), enclos = base::environment(fun = NULL))
    if(base::any(tempo, na.rm = TRUE)){
        tempo_cat <- base::paste0(
            error_text_start, 
            "FOLLOWING ARGUMENT", 
            base::ifelse(test = base::sum(tempo, na.rm = TRUE) > 1, yes = "S HAVE", no = " HAS"), 
            " NO DEFAULT VALUE AND REQUIRE ONE:\n", 
            base::paste0(no_def_args[tempo], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end arg with no default values

    ######## management of NULL arguments
    # before NA checking because is.na(NULL) return logical(0) and all(logical(0)) is TRUE (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "data1", 
        "data2",
        "safer_check" 
        # "lib_path", # inactivated because can be NULL
        # "error_text" # inactivated because NULL converted to "" above
    )
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because sapply(lapply())
    if(base::any(tempo_log, na.rm = TRUE)){ # normally no NA with base::is.null()
        tempo_cat <- base::paste0(
            error_text_start, 
            base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
            " CANNOT BE NULL:\n", 
            base::paste0(tempo_arg[tempo_log], collapse = "\n", recycle0 = FALSE), 
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end management of NULL arguments

    ######## management of empty non NULL arguments
    # # before NA checking because is.na(logical()) is logical(0) (but secured with & base::length(x = x) > 0)
    tempo_arg <-base::c(
        "data1", 
        "data2", 
        "safer_check", 
        "lib_path"
        # "error_text" # inactivated because empty value converted to "" above
    )
    tempo_arg_user_setting_eval <- arg_user_setting_eval[base::names(x = arg_user_setting_eval) %in% tempo_arg]
    if(base::length(x = tempo_arg_user_setting_eval) != 0){
        tempo_log <- base::suppressWarnings(
            expr = base::sapply(
                X = tempo_arg_user_setting_eval, 
                FUN = function(x){
                    base::length(x = x) == 0 & ! base::is.null(x = x)
                }, 
                simplify = TRUE, 
                USE.NAMES = TRUE
            ), 
            classes = "warning"
        ) # no argument provided by the user can be empty non NULL object. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even if 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE AN EMPTY NON NULL OBJECT:\n", 
                base::paste0(base::names(x = tempo_arg_user_setting_eval)[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end management of empty non NULL arguments

    ######## management of NA arguments
    # Mandataory section : argument of safer-r functions cannot have NA as only value, to prevent all(, na.rm = TRUE) or any(, na.rm = TRUE) to return a logical value
    if(base::length(x = arg_user_setting_eval) != 0){
        tempo_log <- base::suppressWarnings(
            expr = base::sapply(
                X = base::lapply(
                    X = arg_user_setting_eval, 
                    FUN = function(x){
                        base::is.na(x = x) # if x is empty, return empty, but ok with below
                    }
                ), 
                FUN = function(x){
                    base::all(x = x, na.rm = TRUE) & base::length(x = x) > 0 # if x is empty, return FALSE, so OK
                }, 
                simplify = TRUE, 
                USE.NAMES = TRUE
            ), 
            classes = "warning"
        ) # no argument provided by the user can be just made of NA. is.na(NULL) returns logical(0), the reason why base::length(x = x) > 0 is used # warning: all(x = x, na.rm = TRUE) but normally no NA because base::is.na() used here. Warning: would not work if arg_user_setting_eval is a vector (because treat each element as a compartment), but ok because it is always a list, even if 0 or 1 argument in the developed function
        if(base::any(tempo_log, na.rm = TRUE)){
            tempo_cat <- base::paste0(
                error_text_start, 
                base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "THESE ARGUMENTS", no = "THIS ARGUMENT"), 
                " CANNOT BE MADE OF NA ONLY:\n", 
                base::paste0(base::names(x = arg_user_setting_eval)[tempo_log], collapse = "\n", recycle0 = FALSE), 
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end management of NA arguments

    #### end argument primary checking

    #### environment checking

    ######## safer_check argument checking
    if( ! (base::all(base::typeof(x = safer_check) == "logical", na.rm = TRUE) & base::length(x = safer_check) == 1)){ # no need to test NA because NA only already managed above and base::length(x = safer_check) == 1)
        if(base::all(base::mode(x = safer_check) == "function", na.rm = TRUE)){
            safer_check <- base::deparse1(expr = safer_check, collapse = "", width.cutoff = 500L)
        }
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE safer_check ARGUMENT VALUE MUST BE A SINGLE LOGICAL VALUE (TRUE OR FALSE ONLY).\nHERE IT IS:\n", 
            base::ifelse(test = base::length(x = safer_check) == 0 | base::all(base::suppressWarnings(expr = safer_check == base::quote(expr = ), classes = "warning"), na.rm = TRUE) | base::all(safer_check == "", na.rm = TRUE), yes = "<NULL, \"\", EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(safer_check, collapse = "\n", recycle0 = FALSE)),
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end safer_check argument checking

    ######## check of lib_path
    # must be before any :: or ::: non basic package calling
    if(safer_check == TRUE){ # this line must be inactivated if you want to use lib_path in the main code (other than in safer functions present in the main code) 
        if( ! base::is.null(x = lib_path)){ #  is.null(NA) returns FALSE so OK.
            if( ! base::all(base::typeof(x = lib_path) == "character", na.rm = TRUE)){ # na.rm = TRUE but no NA returned with typeof (typeof(NA) == "character" returns FALSE)
                if(base::all(base::mode(x = lib_path) == "function", na.rm = TRUE)){
                    lib_path <- base::deparse1(expr = lib_path, collapse = "", width.cutoff = 500L)
                }
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "THE DIRECTORY PATH INDICATED IN THE lib_path ARGUMENT MUST BE A VECTOR OF CHARACTERS.\nHERE IT IS:\n", 
                    base::ifelse(test = base::length(x = lib_path) == 0 | base::all(base::suppressWarnings(expr = lib_path == base::quote(expr = ), classes = "warning"), na.rm = TRUE), yes = "<NULL, EMPTY OBJECT OR EMPTY NAME>", no = base::paste0(lib_path, collapse = "\n", recycle0 = FALSE)),
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else if( ! base::all(base::dir.exists(paths = lib_path), na.rm = TRUE)){ # separation to avoid the problem of tempo$problem == FALSE and lib_path == NA. dir.exists(paths = NA) returns an error, so ok. dir.exists(paths = "") returns FALSE so ok
                tempo_log <- ! base::dir.exists(paths = lib_path)
                tempo_cat_b <- lib_path[tempo_log] # here lib_path is character string
                tempo_cat_b[tempo_cat_b == ""] <- "\"\""
                tempo_cat <- base::paste0(
                    error_text_start, 
                    "THE DIRECTORY PATH",
                    base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "S", no = ""), 
                    " INDICATED IN THE lib_path ARGUMENT DO", 
                    base::ifelse(test = base::sum(tempo_log, na.rm = TRUE) > 1, yes = "", no = "ES"), 
                    " NOT EXIST:\n", 
                    base::paste0(tempo_cat_b, collapse = "\n", recycle0 = FALSE), 
                    collapse = NULL, 
                    recycle0 = FALSE
                )
                base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
            }else{
                ini_lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
                base::on.exit(expr = base::.libPaths(new = ini_lib_path, include.site = TRUE), add = TRUE, after = TRUE) # return to the previous libPaths()
                base::.libPaths(new = base::sub(x = base::c(ini_lib_path, lib_path), pattern = "/$|\\\\$", replacement = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE), include.site = TRUE) # base::.libPaths(new = ) add path to default path. BEWARE: base::.libPaths() does not support / at the end of a submitted path. The reason of the check and replacement of the last / or \\ in path
                lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument
            }
        }else{
            lib_path <- base::.libPaths(new = , include.site = TRUE) # normal to have empty new argument # base::.libPaths(new = lib_path) # or base::.libPaths(new = base::c(base:::.libPaths(), lib_path))
        }
    }  # this line must be inactivated if you want to use lib_path in the main code (other than in safer functions present in the main code) 
    ######## end check of lib_path

    ######## check of the required functions from the required packages
    if(safer_check == TRUE){
        .pack_and_function_check <- utils::getFromNamespace(x = ".pack_and_function_check", ns = "saferDev", pos = , envir = )
        .pack_and_function_check(
            fun = base::c(
                # functions required in this code
                "saferDev::arg_check", # write each function preceeded by their package name
                # end functions required in this code
                # internal functions required in this code
                "saferDev:::.base_op_check"
                # end internal functions required in this code
            ),
            lib_path = lib_path, # write NULL if your function does not have any lib_path argument
            error_text = embed_error_text
        )
    }
    ######## end check of the required functions from the required packages

    ######## escaping CRAN submission NOTE for internal functions

    .base_op_check <- utils::getFromNamespace(x = ".base_op_check", ns = "saferDev", pos = , envir = )
    # add here in the internal functions that are used in your main code (copy-paste the line above and replace .base_op_check by the name of the internal function
    # not mandatory if your function is not designed for submission to the CRAN

    ######## end escaping CRAN submission NOTE for internal functions

    ######## critical operator checking
    if(safer_check == TRUE){
        .base_op_check(
            error_text = embed_error_text
        )
    }
    ######## end critical operator checking

    #### end environment checking

    #### argument secondary checking

    ######## argument checking with arg_check()
    argum_check <- NULL
    text_check <- NULL
    checked_arg_names <- NULL # for function debbuging: used by r_debugging_tools
    arg_check_error_text <- base::paste0("ERROR ", embed_error_text, "\n\n", collapse = NULL, recycle0 = FALSE) # must be used instead of error_text = embed_error_text when several arg_check are performed on the same argument (tempo1, tempo2, see below)
    ee <- base::expression(argum_check <- base::c(argum_check, tempo$problem) , text_check <- base::c(text_check, tempo$text) , checked_arg_names <- base::c(checked_arg_names, tempo$object.name))
    # copy - paste ththe line below as much, one for each of the arguments of your own function
    tempo1 <- saferDev::arg_check(data = data1, class = NULL, typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text) # covers numeric vectors (also factors and 1D tables/matrices/arrays of numbers, which all have mode "numeric")
    tempo1b <- saferDev::arg_check(data = data1, class = NULL, typeof = NULL, mode = "character", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text) # covers character vectors
    tempo1c <- saferDev::arg_check(data = data1, class = NULL, typeof = NULL, mode = "logical", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text) # covers logical vectors
    tempo2 <- saferDev::arg_check(data = data1, class = "factor", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text)
    tempo3 <- saferDev::arg_check(data = data1, class = "matrix", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text) # before R 4.0.0
    tempo4 <- saferDev::arg_check(data = data1, class = "table", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text)
    tempo5 <- saferDev::arg_check(data = data1, class = "array", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text) # before R 4.0.0
    if(tempo1$problem == TRUE & tempo1b$problem == TRUE & tempo1c$problem == TRUE & tempo2$problem == TRUE & tempo3$problem == TRUE & tempo4$problem == TRUE & tempo5$problem == TRUE){
        tempo_cat <- base::paste0(
            arg_check_error_text, 
            "data1 ARGUMENT MUST BE A NON NULL VECTOR, FACTOR OR 1D TABLE/MATRIX/ARRAY.",
            collapse = NULL, 
            recycle0 = FALSE
        )
        text_check <- base::c(text_check, tempo_cat)
        argum_check <- base::c(argum_check, TRUE)
        checked_arg_names <- base::c(checked_arg_names, tempo1$object.name)
    }
    tempo1 <- saferDev::arg_check(data = data2, class = NULL, typeof = NULL, mode = "numeric", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text) # covers numeric vectors (also factors and 1D tables/matrices/arrays of numbers, which all have mode "numeric")
    tempo1b <- saferDev::arg_check(data = data2, class = NULL, typeof = NULL, mode = "character", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text) # covers character vectors
    tempo1c <- saferDev::arg_check(data = data2, class = NULL, typeof = NULL, mode = "logical", length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text) # covers logical vectors
    tempo2 <- saferDev::arg_check(data = data2, class = "factor", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text)
    tempo3 <- saferDev::arg_check(data = data2, class = "matrix", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text) # before R 4.0.0
    tempo4 <- saferDev::arg_check(data = data2, class = "table", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text)
    tempo5 <- saferDev::arg_check(data = data2, class = "array", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text) # before R 4.0.0
    if(tempo1$problem == TRUE & tempo1b$problem == TRUE & tempo1c$problem == TRUE & tempo2$problem == TRUE & tempo3$problem == TRUE & tempo4$problem == TRUE & tempo5$problem == TRUE){
        tempo_cat <- base::paste0(
            arg_check_error_text, 
            "data2 ARGUMENT MUST BE A NON NULL VECTOR, FACTOR OR 1D TABLE/MATRIX/ARRAY.",
            collapse = NULL, 
            recycle0 = FALSE
        )
        text_check <- base::c(text_check, tempo_cat)
        argum_check <- base::c(argum_check, TRUE)
        checked_arg_names <- base::c(checked_arg_names, tempo1$object.name)
    }
    # lib_path already checked above
    # safer_check already checked above
    # error_text converted to single string above
    if( ! base::is.null(x = argum_check)){
        if(base::any(argum_check, na.rm = TRUE)){
            base::stop(base::paste0("\n\n================\n\n", base::paste0(text_check[argum_check], collapse = "\n\n", recycle0 = FALSE), "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    # check with r_debugging_tools
    # source("https://gitlab.pasteur.fr/gmillot/debugging_tools_for_r_dev/-/raw/v1.8/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using saferDev::arg_check()
    # end check with r_debugging_tools
    ######## end argument checking with arg_check()

    ######## management of "" in arguments of mode character
    ######## end management of "" in arguments of mode character

    #### end argument secondary checking

    #### second round of checking and data preparation

    ######## code that protects set.seed() in the global environment
    ######## end code that protects set.seed() in the global environment

    ######## warning initiation
    ######## end warning initiation

    ######## graphic device checking
    ######## end graphic device checking

    ######## other checkings
    if(base::any(base::c("table", "matrix", "array") %in% base::class(data1))){
        if(base::length(base::dim(data1)) > 1){
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE data1 ARGUMENT MUST BE A 1D 1D TABLE/MATRIX/ARRAY.",
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    if(base::any(base::c("table", "matrix", "array") %in% base::class(data2))){
        if(base::length(base::dim(data2)) > 1){
            tempo_cat <- base::paste0(
                error_text_start, 
                "THE data2 ARGUMENT MUST BE A 1D 1D TABLE/MATRIX/ARRAY.",
                collapse = NULL, 
                recycle0 = FALSE
            )
            base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        }
    }
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
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
    any.id.names <- FALSE
    same.names.pos1 <- NULL
    same.names.pos2 <- NULL
    same.names.match1 <- NULL
    same.names.match2 <- NULL
    common.names <- NULL
    any.id.elements <- FALSE
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
            any.id.names <- TRUE
            same.names.pos1 <- 1:base::length(data1)
            same.names.pos2 <- 1:base::length(data2)
            same.names.match1 <- 1:base::length(data1)
            same.names.match2 <- 1:base::length(data2)
            common.names <- base::names(data1)
        }
        any.id.elements <- TRUE
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
            any.id.elements <- TRUE
            same.elements.pos1 <- base::which(data1 %in% data2)
            same.elements.match1 <- base::match(data1, data2)
        }
        if(base::any(data2 %in% data1)){
            any.id.elements <- TRUE
            same.elements.pos2 <- base::which(data2 %in% data1)
            same.elements.match2 <- base::match(data2, data1)
        }
        if(any.id.elements == TRUE){
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
    output <- base::list(same.class = same.class, class = class, same.length = same.length, length = length, same.levels = same.levels, levels = levels, any.id.levels = any.id.levels, same.levels.pos1 = same.levels.pos1, same.levels.pos2 = same.levels.pos2, same.levels.match1 = same.levels.match1, same.levels.match2 = same.levels.match2, common.levels = common.levels, same.names = same.names, name = name, any.id.names = any.id.names, same.names.pos1 = same.names.pos1, same.names.pos2 = same.names.pos2, same.names.match1 = same.names.match1, same.names.match2 = same.names.match2, common.names = common.names, any.id.elements = any.id.elements, same.elements.pos1 = same.elements.pos1, same.elements.pos2 = same.elements.pos2, same.elements.match1 = same.elements.match1, same.elements.match2 = same.elements.match2, common.elements = common.elements, same.order = same.order, order1 = order1, order2 = order2, identical.object = identical.object, identical.content = identical.content)
    #### end main code

    #### warning output
    #### end warning output

    #### output
    base::return(output)
    #### end output
}

