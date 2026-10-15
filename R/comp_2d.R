#' @title comp_2d
#' @description
#' Compare two 2D datasets of the same class or not. Check and report in a list if the 2 datasets have:
#' 
#' - same class
#' 
#' - same type
#' 
#' - common row names
#' 
#' - common column names
#' 
#' - same row number
#' 
#' - same column number
#' 
#' - potential identical rows between the 2 datasets
#' 
#' - potential identical columns between the 2 datasets
#' @param data1 Matrix, data frame or table.
#' @param data2 Matrix, data frame or table.
#' @param safer_check Single logical value. Perform the "safer" checks? If \code{TRUE}, checkings are performed before main code running (see the \href{https://github.com/safer-r}{safer-r project}): 1) correct \code{lib_path} argument value 2) required functions and related packages effectively present in local R libraries and 3) R classical operators (like \code{"<-"}) not overwritten by another package because of the R scope. Warning: must be set to \code{FALSE} if this function is used inside another "safer" function to avoid pointless multiple checkings.
#' @param lib_path Vector of characters specifying the absolute pathways of the directories containing the required packages for the function, if not in the default directories. Useful when R packages are not installed in the default directories because of lack of admin rights. More precisely, \code{lib_path} is passed through the \code{new} argument of \code{.libPaths()} so that the new library paths are \code{c(lib_path, .libPaths())}. Warning: \code{.libPaths()} is restored to the initial paths, after function execution. Ignored if \code{NULL} (default) or if the \code{safer_check} argument is \code{FALSE}: only the pathways specified by the current \code{.libPaths()} are used for package calling.
#' @param error_text Single character string used to add information in error messages returned by the function, notably if the function is inside other functions, which is practical for debugging. Example: \code{error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."}. If \code{NULL}, converted into \code{""}.
#' @returns 
#' A list containing:
#' 
#' - $same.class: logical. Are classes identical ?
#' 
#' - $class: identical class of the 2 datasets (NULL otherwise).
#' 
#' - $same.mode: logical. Are modes identical ?
#' 
#' - $mode: identical mode of the 2 datasets (NULL otherwise).
#' 
#' - $same.type: logical. Are types identical ?
#' 
#' - $type: identical type of the 2 datasets (NULL otherwise).
#' 
#' - $same.dim: logical. Are dimension identical ?
#' 
#' - $dim: dimension of the 2 datasets (NULL otherwise).
#' 
#' - $same.row.nb: logical. Are number of rows identical ?
#' 
#' - $row.nb: nb of rows of the 2 datasets if identical (NULL otherwise).
#' 
#' - $same.col.nb: logical. Are number of columns identical ?
#' 
#' - $col.nb: nb of columns of the 2 datasets if identical (NULL otherwise).
#' 
#' - $same.row.name: logical. Are row names identical ? NULL if no row names in the two 2D datasets.
#' 
#' - $row.name: name of rows of the 2 datasets if identical (NULL otherwise).
#' 
#' - $any.id.row.name: logical. Is there any row names identical ? NULL if no row names in the two 2D datasets.
#' 
#' - $same.row.names.pos1: positions, in data1, of the row names identical in data2.
#' 
#' - $same.row.names.pos2: positions, in data2, of the row names identical in data1.
#' 
#' - $same.row.names.match1: positions, in data2, of the row names that match the row names in data1, as given by match(data1, data2) (NULL otherwise).
#' 
#' - $same.row.names.match2: positions, in data1, of the row names that match the row names in data2, as given by match(data1, data2) (NULL otherwise).
#' 
#' - $common.row.names: common row names between data1 and data2 (can be a subset of $name or not). NULL if no common row names.
#' 
#' - $same.col.name: logical. Are column names identical ? NULL if no col names in the two 2D datasets.
#' 
#' - $col.name: name of columns of the 2 datasets if identical (NULL otherwise).
#' 
#' - $any.id.col.name: logical. Is there any column names identical ? NULL if no col names in the two 2D datasets.
#' 
#' - $same.col.names.pos1: positions, in data1, of the column names identical in data2.
#' 
#' - $same.col.names.pos2: positions, in data2, of the column names identical in data1.
#' 
#' - $same.col.names.match1: positions, in data2, of the column names that match the column names in data1, as given by match(data1, data2) (NULL otherwise).
#' 
#' - $same.col.names.match2: positions, in data1, of the column names that match the column names in data2, as given by match(data1, data2) (NULL otherwise).
#' 
#' - $common.col.names: common column names between data1 and data2 (can be a subset of $name or not). NULL if no common column names.
#' 
#' - $any.id.row: logical. is there identical rows (not considering row names)? NULL if nrow(data1) * nrow(data2) > 1e6. Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character.
#' 
#' - $same.row.pos1: positions, in data1, of the rows identical in data2 (not considering row names). Return "TOO BIG FOR EVALUATION" if nrow(data1) * nrow(data2) > 1e6. Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character.
#' 
#' - $same.row.pos2: positions, in data2, of the rows identical in data1 (not considering row names). Return "TOO BIG FOR EVALUATION" if nrow(data1) * nrow(data2) > 1e6. Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character.
#' 
#' - $same.row.match1: positions, in data2, of the rows that match the rows in data1, as given by match(data1, data2) (NULL otherwise). Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character.
#' 
#' - $same.row.match2: positions, in data1, of the rows that match the rows in data2, as given by match(data1, data2) (NULL otherwise). Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character.
#' 
#' - $any.id.col: logical. is there identical columns (not considering column names)? NULL if ncol(data1) * ncol(data2) > 1e6. Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character.
#' 
#' - $same.col.pos1: position in data1 of the cols identical in data2 (not considering column names). Return "TOO BIG FOR EVALUATION" if ncol(data1) * ncol(data2) > 1e6. Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character.
#' 
#' - $same.col.pos2: position in data2 of the cols identical in data1 (not considering column names). Return "TOO BIG FOR EVALUATION" if ncol(data1) * ncol(data2) > 1e6. Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character.
#' 
#' - $same.col.match1: positions, in data2, of the columns that match the columns in data1, as given by match(data1, data2) (NULL otherwise). Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character.
#' 
#' - $same.row.match2: positions, in data1, of the columns that match the columns in data2, as given by match(data1, data2) (NULL otherwise). Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character.
#' 
#' - $identical.content: logical. Are contents identical ? Row and column names are not considered. Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character).
#' 
#' - $identical: logical. Idem as $identical.content but including row & column names.
#' @details 
#' WARNINGS
#' 
#' The results in:
#' 
#' $any.id.row,
#' 
#' $same.row.pos1,
#' 
#' $same.row.pos2,
#' 
#' $same.row.match1,
#' 
#' $same.row.match2,
#' 
#' $any.id.col, 
#' 
#' $same.col.pos1,
#' 
#' $same.col.pos2,
#' 
#' $same.col.match1,
#' 
#' $same.col.match2,
#' 
#' $identical.content,
#' 
#' $identical
#' 
#' Does not take into account the mode and type (integer, double, character, etc.) of the matrix, data frame and table content. Indeed, comparisons are performed after conversion of the content into characters. This allows the 2 by 2 comparisons of data frame rows. However, the same mode and same type information is provided with the $same.mode and $same.type result, which is convenient when dealing with matrices and tables. But the different modes and types between column of a data frame is never considered. Thus, be careful when concluding that columns of two different data frames are the same, because the values can be identical but not the mode or type (integer in the first data frame column, and double in the second data frame column, for instance).
#' 
#' 
#' "TOO BIG FOR EVALUATION" returned in $same.row.pos1, $same.row.pos2, $same.row.match1 and $same.row.match2 when nrow(data1) * nrow(data2) > 1e6 and $any.id.row is returned NULL.
#' 
#' 
#' "TOO BIG FOR EVALUATION" returned in $same.col.pos1, $ame.col.pos2, $same.col.match1 and $same.col.match2 when ncol(data1) * ncol(data2) > 1e6 and $any.id.col is returned NULL.
#' @author Gael Millot <gael.millot@pasteur.fr>
#' @author Yushi Han <yushi.han2000@gmail.com>
#' @author Haiding Wang <wanghaiding442@gmail.com>
#' @examples
#' obs1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; 
#' obs2 = as.data.frame(matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])), stringsAsFactors = TRUE) ;
#' obs1 ; 
#' obs2 ; 
#' comp_2d(obs1, obs2)
#' 
#' 
#' # Matrices: same row content but not same row names
#' 
#' obs1 = t(matrix(1:10, byrow = TRUE, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5]))) ; 
#' obs2 = t(matrix(c(1:5, 101:105, 6:10), byrow = TRUE, ncol = 5, dimnames = list(c("a", "z", "b"), c(LETTERS[1:2], "k", LETTERS[5:4])))) ; 
#' obs1 ; 
#' obs2 ; 
#' comp_2d(obs1, obs2)
#' @export
comp_2d <- function(
    data1, 
    data2,
    safer_check = TRUE, 
    lib_path = NULL, 
    error_text = ""
){
    # DEBUGGING
    # data1 = matrix(1:10, ncol = 5) ; data2 = matrix(1:10, ncol = 5) ; safer_check = TRUE ; lib_path = NULL ; error_text = "" # for function debugging
    # data1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; data2 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; safer_check = TRUE ; lib_path = NULL ; error_text = "" # for function debugging
    # data1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; data2 = matrix(1:10, ncol = 5) ;safer_check = TRUE ; lib_path = NULL ; error_text = "" # for function debugging
    # data1 = matrix(1:15, byrow = TRUE, ncol = 5, dimnames = list(letters[1:3], LETTERS[1:5])) ; data2 = matrix(1:10, byrow = TRUE, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ;safer_check = TRUE ; lib_path = NULL ; error_text = "" # for function debugging
    # data1 = matrix(1:15, ncol = 5, dimnames = list(letters[1:3], LETTERS[1:5])) ; data2 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; safer_check = TRUE ; lib_path = NULL ; error_text = ""# for function debugging
    # data1 = matrix(1:15, ncol = 5, dimnames = list(paste0("A", letters[1:3]), LETTERS[1:5])) ; data2 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; safer_check = TRUE ; lib_path = NULL ; error_text = "" # for function debugging
    # data1 = matrix(1:15, ncol = 5, dimnames = list(letters[1:3], LETTERS[1:5])) ; data2 = matrix(1:12, ncol = 4, dimnames = list(letters[1:3], LETTERS[1:4])) ; safer_check = TRUE ; lib_path = NULL ; error_text = "" ; safer_check = TRUE ; lib_path = NULL ; error_text = "" # for function debugging
    # data1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; data2 = matrix(101:110, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; safer_check = TRUE ; lib_path = NULL ; error_text = "" # for function debugging
    # data1 = data.frame(a = 1:3, b= letters[1:3], row.names = LETTERS[1:3], stringsAsFactors = TRUE) ; data2 = data.frame(A = 1:3, B= letters[1:3], stringsAsFactors = TRUE) ; safer_check = TRUE ; lib_path = NULL ; error_text = "" # for function debugging
    # data1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; data2 = as.data.frame(matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])), stringsAsFactors = TRUE) ; safer_check = TRUE ; lib_path = NULL ; error_text = "" # for function debugging
    # data1 = matrix(1:10, byrow = TRUE, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; data2 = matrix(c(1:5, 101:105, 6:10), byrow = TRUE, ncol = 5, dimnames = list(c("a", "z", "b"), c(LETTERS[1:2], "k", LETTERS[5:4]))) ; safer_check = TRUE ; lib_path = NULL ; error_text = "" # for function debugging
    # data1 = table(Exp1 = c("A", "A", "A", "B", "B", "B"), Exp2 = c("A1", "B1", "A1", "C1", "C1", "B1")) ; data2 = data.frame(A = 1:3, B= letters[1:3], stringsAsFactors = TRUE) ; safer_check = TRUE ; lib_path = NULL ; error_text = "" # for function debugging
    # data1 = matrix(1:1e6, ncol = 5, dimnames = list(NULL, LETTERS[1:5])) ; data2 = matrix((1:1e6)+1e6/5, ncol = 5, dimnames = list(NULL, LETTERS[1:5])) ; safer_check = TRUE ; lib_path = NULL ; error_text = ""
    # data1 = data.frame(1) ; data2 = data.frame(1:8) ; safer_check = TRUE ; lib_path = NULL ; error_text = ""





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
    tempo_log <- base::sapply(X = base::lapply(X = tempo_arg, FUN = function(x){base::get(x = x, pos = -1L, envir = base::parent.frame(n = 2), mode = "any", inherits = FALSE)}), FUN = function(x){base::is.null(x = x)}, simplify = TRUE, USE.NAMES = TRUE) # parent.frame(n = 2) because get() is evaluated inside the FUN closure of the inner lapply: parent.frame(n = 1) is lapply's frame, parent.frame(n = 2) is BACKBONE's frame.
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
    # Mandatory section : argument of safer-r functions cannot have NA as only value, to prevent all(, na.rm = TRUE) or any(, na.rm = TRUE) to return a logical value
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
        saferDev:::.pack_and_function_check(
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

    # add here in the internal functions that are used in your main code (copy-paste the line above and replace .base_op_check by the name of the internal function
    # not mandatory if your function is not designed for submission to the CRAN

    ######## end escaping CRAN submission NOTE for internal functions

    ######## critical operator checking
    if(safer_check == TRUE){
        saferDev:::.base_op_check(
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
    tempo1 <- saferDev::arg_check(data = data1, class = "data.frame", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text)
    tempo2 <- saferDev::arg_check(data = data1, class = "matrix", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text) # before R 4.0.0
    tempo3 <- saferDev::arg_check(data = data1, class = c("matrix", "array"), typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text) # since R 4.0.0
    tempo4 <- saferDev::arg_check(data = data1, class = "table", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text)
    if(tempo1$problem == TRUE & tempo2$problem == TRUE & tempo3$problem == TRUE & tempo4$problem == TRUE){
        tempo_cat <- base::paste0(
            arg_check_error_text, 
            "data1 ARGUMENT MUST BE (1) A DATA FRAME, (2) A MATRIX OR (3) A TABLE",
            collapse = NULL, 
            recycle0 = FALSE
        )
        text_check <- base::c(text_check, tempo_cat)
        argum_check <- base::c(argum_check, TRUE)
        checked_arg_names <- base::c(checked_arg_names, tempo1$object.name)
    }
    tempo1 <- saferDev::arg_check(data = data2, class = "data.frame", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text)
    tempo2 <- saferDev::arg_check(data = data2, class = "matrix", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text) # before R 4.0.0
    tempo3 <- saferDev::arg_check(data = data2, class = c("matrix", "array"), typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text) # since R 4.0.0
    tempo4 <- saferDev::arg_check(data = data2, class = "table", typeof = NULL, mode = NULL, length = NULL, prop = FALSE, double_as_integer_allowed = FALSE, options = NULL, all_options_in_data = FALSE, na_contain = TRUE, neg_values = TRUE, inf_values = TRUE, print = FALSE, data_name = NULL, data_arg = TRUE, safer_check = FALSE, lib_path = lib_path, error_text = arg_check_error_text)
    if(tempo1$problem == TRUE & tempo2$problem == TRUE & tempo3$problem == TRUE & tempo4$problem == TRUE){
        tempo_cat <- base::paste0(
            arg_check_error_text, 
            "data2 ARGUMENT MUST BE (1) A DATA FRAME, (2) A MATRIX OR (3) A TABLE",
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
    if( ! (base::any(base::class(data1) %in% base::c("data.frame", "table")) | base::all(base::class(data1) %in% base::c("matrix", "array")))){ # before R4.0.0, it was  ! base::any(base::class(data1) %in% base::c("matrix", "data.frame", "table"))
        # nocov start
        # codecov inactivated because arg_check() above only allows data.frame/matrix/array/table classes, so this branch is unreachable with argument values.
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE data1 ARGUMENT MUST BE A MATRIX, DATA FRAME OR TABLE",
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        # nocov end
    }
    if( ! (base::any(base::class(data2) %in% base::c("data.frame", "table")) | base::all(base::class(data2) %in% base::c("matrix", "array")))){ # before R4.0.0, it was  ! base::any(base::class(data2) %in% base::c("matrix", "data.frame", "table"))
        # nocov start
        # codecov inactivated because arg_check() above only allows data.frame/matrix/array/table classes, so this branch is unreachable with argument values.
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE data2 ARGUMENT MUST BE A MATRIX, DATA FRAME OR TABLE",
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
        # nocov end
    }
    if(base::all(base::class(data1) == "table") & base::length(base::dim(data1)) == 1L){ # base::class() cannot return NA
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE data1 ARGUMENT IS A 1D TABLE. USE THE comp_1d FUNCTION",
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    if(base::all(base::class(data2) == "table", na.rm = TRUE) & base::length(base::dim(data2)) == 1L){ # base::class() cannot return NA
        tempo_cat <- base::paste0(
            error_text_start, 
            "THE data2 ARGUMENT IS A 1D TABLE. USE THE comp_1d FUNCTION",
            collapse = NULL, 
            recycle0 = FALSE
        )
        base::stop(base::paste0("\n\n================\n\n", tempo_cat, "\n\n================\n\n", collapse = NULL, recycle0 = FALSE), call. = FALSE, domain = NULL)
    }
    ######## end other checkings

    #### end second round of checking and data preparation

    #### main code
    same.class <- NULL
    class <- NULL
    same.mode <- NULL
    mode <- NULL
    same.type <- NULL
    type <- NULL
    same.dim <- NULL
    dim <- NULL
    same.row.nb <- NULL
    row.nb <- NULL
    same.col.nb <- NULL
    col.nb <- NULL
    same.row.name <- NULL
    row.name <- NULL
    any.id.row.name <- NULL
    same.row.names.pos1 <- NULL
    same.row.names.pos2 <- NULL
    same.row.names.match1 <- NULL
    same.row.names.match2 <- NULL
    common.row.names <- NULL
    same.col.name <- NULL
    any.id.col.name <- NULL
    same.col.names.pos1 <- NULL
    same.col.names.pos2 <- NULL
    same.col.names.match1 <- NULL
    same.col.names.match2 <- NULL
    common.col.names <- NULL
    col.name <- NULL
    any.id.row <- NULL
    same.row.pos1 <- NULL
    same.row.pos2 <- NULL
    same.row.match1 <- NULL
    same.row.match2 <- NULL
    any.id.col <- NULL
    same.col.pos1 <- NULL
    same.col.pos2 <- NULL
    same.col.match1 <- NULL
    same.col.match2 <- NULL
    identical.object <- NULL
    identical.content <- NULL
    # structure
    if( ! base::identical(base::class(data1), base::class(data2))){
        same.class <- FALSE
    }else{
        same.class <- TRUE
        class <- base::class(data1)
    }
    if( ! base::identical(base::mode(data1), base::mode(data2))){
        same.mode<- FALSE
    }else{
        same.mode<- TRUE
        mode <- base::mode(data1)
    }
    if( ! base::identical(base::typeof(data1), base::typeof(data2))){
        same.type <- FALSE
    }else{
        same.type <- TRUE
        type<- base::typeof(data1)
    }
    if( ! base::identical(base::dim(data1), base::dim(data2))){
        same.dim <- FALSE
    }else{
        same.dim <- TRUE
        dim <- base::dim(data1)
    }
    if( ! base::identical(base::nrow(data1), base::nrow(data2))){
        same.row.nb <- FALSE
    }else{
        same.row.nb <- TRUE
        row.nb <- base::nrow(data1)
    }
    if( ! base::identical(base::ncol(data1), base::ncol(data2))){
        same.col.nb <- FALSE
    }else{
        same.col.nb <- TRUE
        col.nb <- base::ncol(data1)
    }
    # end structure
    # conversion of object into matrix and content into characters
    if(base::all(base::class(data1) %in% base::c("data.frame"))){
        tempo_dim_names <- base::dimnames(data1)
        data1 <- base::apply(data1, 2, function(x){base::gsub('\\s+', '',x)}) # convert into matrix of character whitout space in the character strings, since as.matrix use base::format() to convert into characters
        data1 <- base::as.matrix(data1)
        base::dimnames(data1) <- tempo_dim_names

    }else if(base::all(base::class(data1) %in% base::c("table"))){
        data1 <- base::matrix(data1, ncol = base::ncol(data1), dimnames = base::dimnames(data1))
        base::mode(data1) <- "character"
    }
    if(base::all(base::class(data2) %in% base::c("data.frame"))){
        tempo_dim_names <- base::dimnames(data2)
        data2 <- base::apply(data2, 2, function(x){base::gsub('\\s+', '',x)}) # convert into matrix of character whitout space in the character strings, since as.matrix use base::format() to convert into characters
        data2 <- base::as.matrix(data2)
        base::dimnames(data2) <- tempo_dim_names
    }else if(base::all(base::class(data2) %in% base::c("table"))){
        data2 <- base::matrix(data2, ncol = base::ncol(data2), dimnames = base::dimnames(data2))
        base::mode(data2) <- "character"
    }
    # end conversion of object into matrix and content into characters
    if(base::identical(data1, data2)){ # before R4.0.0, it was  ! base::any(base::class(data1) %in% base::c("matrix", "data.frame", "table"))
        same.row.name <- TRUE
        row.name <- base::dimnames(data1)[[1]]
        any.id.row.name <- TRUE
        same.row.names.pos1 <- 1:row.nb
        same.row.names.pos2 <- 1:row.nb
        same.row.names.match1 <- 1:row.nb
        same.row.names.match2 <- 1:row.nb
        common.row.names <- base::dimnames(data1)[[1]]
        same.col.name <- TRUE
        col.name <- base::dimnames(data1)[[2]]
        any.id.col.name <- TRUE
        same.col.names.pos1 <- 1:col.nb
        same.col.names.pos2 <- 1:col.nb
        same.col.names.match1 <- 1:col.nb
        same.col.names.match2 <- 1:col.nb
        common.col.names <- base::dimnames(data1)[[2]]
        any.id.row <- TRUE
        same.row.pos1 <- 1:row.nb
        same.row.pos2 <- 1:row.nb
        same.row.match1 <- 1:row.nb
        same.row.match2 <- 1:row.nb
        any.id.col <- TRUE
        same.col.pos1 <- 1:col.nb
        same.col.pos2 <- 1:col.nb
        same.col.match1 <- 1:col.nb
        same.col.match2 <- 1:col.nb
        identical.object <- TRUE
        identical.content <- TRUE
    }else{
        identical.object <- FALSE
        # row and col names
        if(base::is.null(base::dimnames(data1)) & base::is.null(base::dimnames(data2))){
            same.row.name <- NULL # but already NULL
            same.col.name <- NULL # but already NULL
            # other row names param remain NULL
        }else if((base::is.null(base::dimnames(data1)) & ! base::is.null(base::dimnames(data2))) | ( ! base::is.null(base::dimnames(data1)) & base::is.null(base::dimnames(data2)))){
            same.row.name <- FALSE
            same.col.name <- FALSE
            any.id.row.name <- FALSE
            any.id.col.name <- FALSE
            # other row names param remain NULL
        }else{
            # row names
            if(base::is.null(base::dimnames(data1)[[1]]) & base::is.null(base::dimnames(data2)[[1]])){
                same.row.name <- NULL # but already NULL
                # other row names param remain NULL
            }else if((base::is.null(base::dimnames(data1)[[1]]) & ! base::is.null(base::dimnames(data2)[[1]])) | ( ! base::is.null(base::dimnames(data1)[[1]]) & base::is.null(base::dimnames(data2)[[1]]))){
                same.row.name <- FALSE
                any.id.row.name <- FALSE
                # other row names param remain NULL
            }else if(base::identical(base::dimnames(data1)[[1]], base::dimnames(data2)[[1]])){
                same.row.name <- TRUE
                row.name <- base::dimnames(data1)[[1]]
                any.id.row.name <- TRUE
                same.row.names.pos1 <- 1:base::nrow(data1)
                same.row.names.pos2 <- 1:base::nrow(data1)
                same.row.names.match1 <- 1:base::nrow(data1)
                same.row.names.match2 <- 1:base::nrow(data1)
                common.row.names <- base::dimnames(data1)[[1]]
            }else{
                same.row.name <- FALSE
                any.id.row.name <- FALSE
                if(base::any(base::dimnames(data1)[[1]] %in% base::dimnames(data2)[[1]])){
                    any.id.row.name <- TRUE
                    same.row.names.pos1 <- base::which(base::dimnames(data1)[[1]] %in% base::dimnames(data2)[[1]])
                    same.row.names.match1 <- base::match(base::dimnames(data1)[[1]], base::dimnames(data2)[[1]])
                }
                if(base::any(base::dimnames(data2)[[1]] %in% base::dimnames(data1)[[1]])){
                    any.id.row.name <- TRUE
                    same.row.names.pos2 <- base::which(base::dimnames(data2)[[1]] %in% base::dimnames(data1)[[1]])
                    same.row.names.match2 <- base::match(base::dimnames(data2)[[1]], base::dimnames(data1)[[1]])
                }
                if(any.id.row.name == TRUE){
                    common.row.names <- base::unique(base::c(base::dimnames(data1)[[1]][same.row.names.pos1], base::dimnames(data2)[[1]][same.row.names.pos2]))
                }
            }
            # col names
            if(base::is.null(base::dimnames(data1)[[2]]) & base::is.null(base::dimnames(data2)[[2]])){
                same.col.name <- NULL # but already NULL
                # other col names param remain NULL
            }else if((base::is.null(base::dimnames(data1)[[2]]) & ! base::is.null(base::dimnames(data2)[[2]])) | ( ! base::is.null(base::dimnames(data1)[[2]]) & base::is.null(base::dimnames(data2)[[2]]))){
                same.col.name <- FALSE
                any.id.col.name <- FALSE
                # other col names param remain NULL
            }else if(base::identical(base::dimnames(data1)[[2]], base::dimnames(data2)[[2]])){
                same.col.name <- TRUE
                col.name <- base::dimnames(data1)[[2]]
                any.id.col.name <- TRUE
                same.col.names.pos1 <- 1:base::ncol(data1)
                same.col.names.pos2 <- 1:base::ncol(data1)
                same.col.names.match1 <- 1:base::ncol(data1)
                same.col.names.match2 <- 1:base::ncol(data1)
                common.col.names <- base::dimnames(data1)[[2]]
            }else{
                same.col.name <- FALSE
                any.id.col.name <- FALSE
                if(base::any(base::dimnames(data1)[[2]] %in% base::dimnames(data2)[[2]])){
                    any.id.col.name <- TRUE
                    same.col.names.pos1 <- base::which(base::dimnames(data1)[[2]] %in% base::dimnames(data2)[[2]])
                    same.col.names.match1 <- base::match(base::dimnames(data1)[[2]], base::dimnames(data2)[[2]])
                }
                if(base::any(base::dimnames(data2)[[2]] %in% base::dimnames(data1)[[2]])){
                    any.id.col.name <- TRUE
                    same.col.names.pos2 <- base::which(base::dimnames(data2)[[2]] %in% base::dimnames(data1)[[2]])
                    same.col.names.match2 <- base::match(base::dimnames(data2)[[2]], base::dimnames(data1)[[2]])
                }
                if(any.id.col.name == TRUE){
                    common.col.names <- base::unique(base::c(base::dimnames(data1)[[2]][same.col.names.pos1], base::dimnames(data2)[[2]][same.col.names.pos2]))
                }
            }
        }
        # identical row and col content
        base::row.names(data1) <- base::paste0("A", 1:base::nrow(data1))
        base::row.names(data2) <- base::paste0("A", 1:base::nrow(data2))
        base::colnames(data1) <- base::paste0("A", 1:base::ncol(data1))
        base::colnames(data2) <- base::paste0("A", 1:base::ncol(data2))
        if(same.col.nb == TRUE){ # because if not the same col nb, the row cannot be identical
            if(base::as.double(base::nrow(data1)) * base::as.double(base::nrow(data2)) <= 1e6){
                tempo1 <- base::c(base::as.data.frame(base::t(data1), stringsAsFactors = FALSE)) # conversion into list. This work fast with characters
                tempo2 <- base::c(base::as.data.frame(base::t(data2), stringsAsFactors = FALSE)) # conversion into list. This work fast with characters
                same.row.pos1 <- base::which(tempo1 %in% tempo2)
                same.row.pos2 <- base::which(tempo2 %in% tempo1)
                if((base::length(same.row.pos1) == 0L & base::length(same.row.pos2) == 0L) | base::all(base::is.na(same.row.pos1)) | base::all(base::is.na(same.row.pos2))){
                    any.id.row <- FALSE
                    same.row.pos1 <- NULL
                    same.row.pos2 <- NULL
                    # same.row.match1 <- NULL # already NULL above
                    # same.row.match2 <- NULL # already NULL above
                }else{
                    any.id.row <- TRUE
                    same.row.pos1 <- same.row.pos1[ ! base::is.na(same.row.pos1)]
                    same.row.pos2 <- same.row.pos2[ ! base::is.na(same.row.pos2)]
                    same.row.match1 <- base::match(tempo1, tempo2)
                    same.row.match2 <- base::match(tempo2, tempo1)
                }
            }else{
                same.row.pos1 <- "TOO BIG FOR EVALUATION"
                same.row.pos2 <- "TOO BIG FOR EVALUATION"
                same.row.match1 <- "TOO BIG FOR EVALUATION"
                same.row.match2 <- "TOO BIG FOR EVALUATION"
            }
        }else{
            any.id.row <- FALSE
            # same.row.pos1 and 2 remain NULL
        }
        if(same.row.nb == TRUE){ # because if not the same row nb, the col cannot be identical
            if(base::as.double(base::ncol(data1)) * base::as.double(base::ncol(data2)) <= 1e6){
                tempo1 <- base::c(base::as.data.frame(data1, stringsAsFactors = FALSE))
                tempo2 <- base::c(base::as.data.frame(data2, stringsAsFactors = FALSE))
                same.col.pos1 <- base::which(tempo1 %in% tempo2)
                same.col.pos2 <- base::which(tempo2 %in% tempo1)
                if((base::length(same.col.pos1) == 0L & base::length(same.col.pos2) == 0L) | base::all(base::is.na(same.col.pos1)) | base::all(base::is.na(same.col.pos2))){
                    any.id.col <- FALSE
                    same.col.pos1 <- NULL
                    same.col.pos2 <- NULL
                    # same.col.match1 <- NULL # already NULL above
                    # same.col.match2 <- NULL # already NULL above
                }else{
                    any.id.col <- TRUE
                    same.col.pos1 <- same.col.pos1[ ! base::is.na(same.col.pos1)]
                    same.col.pos2 <- same.col.pos2[ ! base::is.na(same.col.pos2)]
                    same.col.match1 <- base::match(tempo1, tempo2)
                    same.col.match2 <- base::match(tempo2, tempo1)
                }
            }else{
                same.col.pos1 <- "TOO BIG FOR EVALUATION"
                same.col.pos2 <- "TOO BIG FOR EVALUATION"
                same.col.match1 <- "TOO BIG FOR EVALUATION"
                same.col.match2 <- "TOO BIG FOR EVALUATION"
            }
        }else{
            any.id.col <- FALSE
            # same.col.pos1 and 2 remain NULL
        }
        if(same.dim == TRUE){
            if(base::all(data1 == data2, na.rm = TRUE)){
                identical.content <- TRUE
            }else{
                identical.content <- FALSE
            }
        }else{
            identical.content <- FALSE
        }
    }
    output <- base::list(same.class = same.class, class = class, same.mode = same.mode, mode = mode, same.type = same.type , type = type, same.dim = same.dim, dim = dim, same.row.nb = same.row.nb, row.nb = row.nb, same.col.nb = same.col.nb , col.nb = col.nb, same.row.name = same.row.name, row.name = row.name, any.id.row.name = any.id.row.name, same.row.names.pos1 = same.row.names.pos1, same.row.names.pos2 = same.row.names.pos2, same.row.names.match1 = same.row.names.match1, same.row.names.match2 = same.row.names.match2, common.row.names = common.row.names, same.col.name = same.col.name, col.name = col.name,any.id.col.name = any.id.col.name, same.col.names.pos1 = same.col.names.pos1, same.col.names.pos2 = same.col.names.pos2, same.col.names.match1 = same.col.names.match1, same.col.names.match2 = same.col.names.match2, common.col.names = common.col.names, any.id.row = any.id.row, same.row.pos1 = same.row.pos1, same.row.pos2 = same.row.pos2, same.row.match1 = same.row.match1, same.row.match2 = same.row.match2, any.id.col = any.id.col, same.col.pos1 = same.col.pos1, same.col.pos2 = same.col.pos2, same.col.match1 = same.col.match1, same.col.match2 = same.col.match2, identical.content = identical.content, identical = identical.object)
    #### end main code

    #### warning output
    # must be before return()
    #### end warning output

    #### output
    base::return(output)
    #### end output
}
