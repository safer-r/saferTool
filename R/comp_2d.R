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
#' @param safer_check Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.
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
        safer_check = TRUE
){
    # DEBUGGING
    # data1 = matrix(1:10, ncol = 5) ; data2 = matrix(1:10, ncol = 5) ; safer_check = TRUE # for function debugging
    # data1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; data2 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; safer_check = TRUE # for function debugging
    # data1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; data2 = matrix(1:10, ncol = 5) ;safer_check = TRUE # for function debugging
    # data1 = matrix(1:15, byrow = TRUE, ncol = 5, dimnames = list(letters[1:3], LETTERS[1:5])) ; data2 = matrix(1:10, byrow = TRUE, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ;safer_check = TRUE # for function debugging
    # data1 = matrix(1:15, ncol = 5, dimnames = list(letters[1:3], LETTERS[1:5])) ; data2 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; safer_check = TRUE# for function debugging
    # data1 = matrix(1:15, ncol = 5, dimnames = list(paste0("A", letters[1:3]), LETTERS[1:5])) ; data2 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; safer_check = TRUE # for function debugging
    # data1 = matrix(1:15, ncol = 5, dimnames = list(letters[1:3], LETTERS[1:5])) ; data2 = matrix(1:12, ncol = 4, dimnames = list(letters[1:3], LETTERS[1:4])) ; safer_check = TRUE ; safer_check = TRUE # for function debugging
    # data1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; data2 = matrix(101:110, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; safer_check = TRUE # for function debugging
    # data1 = data.frame(a = 1:3, b= letters[1:3], row.names = LETTERS[1:3], stringsAsFactors = TRUE) ; data2 = data.frame(A = 1:3, B= letters[1:3], stringsAsFactors = TRUE) ; safer_check = TRUE # for function debugging
    # data1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; data2 = as.data.frame(matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])), stringsAsFactors = TRUE) ; safer_check = TRUE # for function debugging
    # data1 = matrix(1:10, byrow = TRUE, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; data2 = matrix(c(1:5, 101:105, 6:10), byrow = TRUE, ncol = 5, dimnames = list(c("a", "z", "b"), c(LETTERS[1:2], "k", LETTERS[5:4]))) ; safer_check = TRUE # for function debugging
    # data1 = table(Exp1 = c("A", "A", "A", "B", "B", "B"), Exp2 = c("A1", "B1", "A1", "C1", "C1", "B1")) ; data2 = data.frame(A = 1:3, B= letters[1:3], stringsAsFactors = TRUE) ; safer_check = TRUE # for function debugging
    # data1 = matrix(1:1e6, ncol = 5, dimnames = list(NULL, LETTERS[1:5])) ; data2 = matrix((1:1e6)+1e6/5, ncol = 5, dimnames = list(NULL, LETTERS[1:5])) ; safer_check = TRUE
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
    if(base::any(tempo)){ # normally no NA for base::missing() output
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nFOLLOWING ARGUMENT", base::ifelse(base::sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", base::paste0(mandat.args[tempo], collapse = "\n"))
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end arg with no default values
    # argument checking with arg_check()
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev))     # activate this line and use the function to check arguments status
    # end check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
    # reserved words (to avoid bugs)
    # end reserved words (to avoid bugs)
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
    if( ! (base::any(base::class(data1) %in% base::c("data.frame", "table")) | base::all(base::class(data1) %in% base::c("matrix", "array")))){ # before R4.0.0, it was  ! base::any(base::class(data1) %in% base::c("matrix", "data.frame", "table"))
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE data1 ARGUMENT MUST BE A MATRIX, DATA FRAME OR TABLE")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if( ! (base::any(base::class(data2) %in% base::c("data.frame", "table")) | base::all(base::class(data2) %in% base::c("matrix", "array")))){ # before R4.0.0, it was  ! base::any(base::class(data2) %in% base::c("matrix", "data.frame", "table"))
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE data2 ARGUMENT MUST BE A MATRIX, DATA FRAME OR TABLE")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if(base::all(base::class(data1) == "table") & base::length(base::dim(data1)) == 1L){ # base::class() cannot return NA
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE data1 ARGUMENT IS A 1D TABLE. USE THE comp_1d FUNCTION")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    if(base::all(base::class(data2) == "table", na.rm = TRUE) & base::length(base::dim(data2)) == 1L){
        tempo.cat <- base::paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE\nTHE data2 ARGUMENT IS A 1D TABLE. USE THE comp_1d FUNCTION")
        base::stop(base::paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in base::stop() to be able to add several messages between ==
    }
    # end other checkings
    # end second round of checking and data preparation

    # main code
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
        data1 <- base::apply(data1, 2, function(x){base::gsub('\\s+', '',x)}) # convert into matrix of character whitout space in the character strings, since as.matrix use base::format() to convert into characters
    }else if(base::all(base::class(data1) %in% base::c("table"))){
        data1 <- base::matrix(data1, ncol = base::ncol(data1), dimnames = base::dimnames(data1))
        base::mode(data1) <- "character"
    }
    if(base::all(base::class(data2) %in% base::c("data.frame"))){
        data2 <- base::apply(data2, 2, function(x){base::gsub('\\s+', '',x)}) # convert into matrix of character whitout space in the character strings, since as.matrix use base::format() to convert into characters
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
    # output
    # warning output
    # end warning output
    output <- base::list(same.class = same.class, class = class, same.mode = same.mode, mode = mode, same.type = same.type , type = type, same.dim = same.dim, dim = dim, same.row.nb = same.row.nb, row.nb = row.nb, same.col.nb = same.col.nb , col.nb = col.nb, same.row.name = same.row.name, row.name = row.name, any.id.row.name = any.id.row.name, same.row.names.pos1 = same.row.names.pos1, same.row.names.pos2 = same.row.names.pos2, same.row.names.match1 = same.row.names.match1, same.row.names.match2 = same.row.names.match2, common.row.names = common.row.names, same.col.name = same.col.name, col.name = col.name,any.id.col.name = any.id.col.name, same.col.names.pos1 = same.col.names.pos1, same.col.names.pos2 = same.col.names.pos2, same.col.names.match1 = same.col.names.match1, same.col.names.match2 = same.col.names.match2, common.col.names = common.col.names, any.id.row = any.id.row, same.row.pos1 = same.row.pos1, same.row.pos2 = same.row.pos2, same.row.match1 = same.row.match1, same.row.match2 = same.row.match2, any.id.col = any.id.col, same.col.pos1 = same.col.pos1, same.col.pos2 = same.col.pos2, same.col.match1 = same.col.match1, same.col.match2 = same.col.match2, identical.content = identical.content, identical = identical.object)
    base::return(output)
    # end output
    # end main code
}
