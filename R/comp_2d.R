#' @title comp_2d
#' @description
#' Compare two 2D datasets of the same class or not. Check and report in a list if the 2 datasets have:
#' 
#' - same class
#' - same type
#' - common row names
#' - common column names
#' - same row number
#' - same column number
#' - potential identical rows between the 2 datasets
#' - potential identical columns between the 2 datasets
#' @param data1 Matrix, data frame or table.
#' @param data2 Matrix, data frame or table.
#' @returns 
#' A list containing:
#' - $same.class: logical. Are classes identical ?
#' - $class: identical class of the 2 datasets (NULL otherwise).
#' - $same.mode: logical. Are modes identical ?
#' - $mode: identical mode of the 2 datasets (NULL otherwise).
#' - $same.type: logical. Are types identical ?
#' - $type: identical type of the 2 datasets (NULL otherwise).
#' - $same.dim: logical. Are dimension identical ?
#' - $dim: dimension of the 2 datasets (NULL otherwise).
#' - $same.row.nb: logical. Are number of rows identical ?
#' - $row.nb: nb of rows of the 2 datasets if identical (NULL otherwise).
#' - $same.col.nb: logical. Are number of columns identical ?
#' - $col.nb: nb of columns of the 2 datasets if identical (NULL otherwise).
#' - $same.row.name: logical. Are row names identical ? NULL if no row names in the two 2D datasets.
#' - $row.name: name of rows of the 2 datasets if identical (NULL otherwise).
#' - $any.id.row.name: logical. Is there any row names identical ? NULL if no row names in the two 2D datasets.
#' - $same.row.names.pos1: positions, in data1, of the row names identical in data2.
#' - $same.row.names.pos2: positions, in data2, of the row names identical in data1.
#' - $same.row.names.match1: positions, in data2, of the row names that match the row names in data1, as given by match(data1, data2) (NULL otherwise).
#' - $same.row.names.match2: positions, in data1, of the row names that match the row names in data2, as given by match(data1, data2) (NULL otherwise).
#' - $common.row.names: common row names between data1 and data2 (can be a subset of $name or not). NULL if no common row names.
#' - $same.col.name: logical. Are column names identical ? NULL if no col names in the two 2D datasets.
#' - $col.name: name of columns of the 2 datasets if identical (NULL otherwise).
#' - $any.id.col.name: logical. Is there any column names identical ? NULL if no col names in the two 2D datasets.
#' - $same.col.names.pos1: positions, in data1, of the column names identical in data2.
#' - $same.col.names.pos2: positions, in data2, of the column names identical in data1.
#' - $same.col.names.match1: positions, in data2, of the column names that match the column names in data1, as given by match(data1, data2) (NULL otherwise).
#' - $same.col.names.match2: positions, in data1, of the column names that match the column names in data2, as given by match(data1, data2) (NULL otherwise).
#' - $common.col.names: common column names between data1 and data2 (can be a subset of $name or not). NULL if no common column names.
#' - $any.id.row: logical. is there identical rows (not considering row names)? NULL if nrow(data1) * nrow(data2) > 1e6. Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character.
#' - $same.row.pos1: positions, in data1, of the rows identical in data2 (not considering row names). Return "TOO BIG FOR EVALUATION" if nrow(data1) * nrow(data2) > 1e6. Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character.
#' - $same.row.pos2: positions, in data2, of the rows identical in data1 (not considering row names). Return "TOO BIG FOR EVALUATION" if nrow(data1) * nrow(data2) > 1e6. Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character.
#' - $same.row.match1: positions, in data2, of the rows that match the rows in data1, as given by match(data1, data2) (NULL otherwise). Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character.
#' - $same.row.match2: positions, in data1, of the rows that match the rows in data2, as given by match(data1, data2) (NULL otherwise). Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character.
#' - $any.id.col: logical. is there identical columns (not considering column names)? NULL if ncol(data1) * ncol(data2) > 1e6. Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character.
#' - $same.col.pos1: position in data1 of the cols identical in data2 (not considering column names). Return "TOO BIG FOR EVALUATION" if ncol(data1) * ncol(data2) > 1e6. Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character.
#' - $same.col.pos2: position in data2 of the cols identical in data1 (not considering column names). Return "TOO BIG FOR EVALUATION" if ncol(data1) * ncol(data2) > 1e6. Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character.
#' - $same.col.match1: positions, in data2, of the columns that match the columns in data1, as given by match(data1, data2) (NULL otherwise). Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character.
#' - $same.row.match2: positions, in data1, of the columns that match the columns in data2, as given by match(data1, data2) (NULL otherwise). Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character.
#' - $identical.content: logical. Are contents identical ? Row and column names are not considered. Warning: class, mode and type are not considered (comparison of content is performed after conversion of the elements into character).
#' - $identical: logical. Idem as $identical.content but including row & column names.
#' @details 
#' REQUIRED PACKAGES
#' 
#' none
#' 
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' none
#'
#'
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
#' @examples
#' obs1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; 
#' obs2 = as.data.frame(matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])), stringsAsFactors = TRUE) ; 
#' obs1 ; 
#' obs2 ; 
#' comp_2d(obs1, obs2)
#' 
#' 
#' # large matrices
#' 
#' obs1 = matrix(1:1e6, ncol = 5, dimnames = list(NULL, LETTERS[1:5])) ; 
#' obs2 = matrix(as.integer((1:1e6)+1e6/5), ncol = 5, dimnames = list(NULL, LETTERS[1:5])) ; 
#' head(obs1) ; 
#' head(obs2) ; 
#' comp_2d(obs1, obs2)
#' 
#' obs1 = matrix(1:1e6, ncol = 5, dimnames = list(NULL, LETTERS[1:5])) ; 
#' obs2 = matrix((1:1e6)+1e6/5, ncol = 5, dimnames = list(NULL, LETTERS[1:5])) ; 
#' head(obs1) ; 
#' head(obs2) ; 
#' comp_2d(obs1, obs2)
#' 
#' 
#' # Matrices: same row content and same row names
#' 
#' obs1 = matrix(1:10, byrow = TRUE, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; 
#' obs2 = matrix(c(1:5, 101:105, 6:10), byrow = TRUE, ncol = 5, dimnames = list(c("a", "z", "b"), c(LETTERS[1:2], "k", LETTERS[5:4]))) ; 
#' obs1 ; 
#' obs2 ; 
#' comp_2d(obs1, obs2)
#' 
#' 
#' # Matrices: same row content but not same row names
#' 
#' obs1 = matrix(1:10, byrow = TRUE, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; 
#' obs2 = matrix(c(1:5, 101:105, 6:10), byrow = TRUE, ncol = 5, dimnames = list(c("x", "z", "y"), c(LETTERS[1:2], "k", LETTERS[5:4]))) ; 
#' obs1 ; 
#' obs2 ; 
#' comp_2d(obs1, obs2)
#' 
#' obs1 = t(matrix(1:10, byrow = TRUE, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5]))) ; 
#' obs2 = t(matrix(c(1:5, 101:105, 6:10), byrow = TRUE, ncol = 5, dimnames = list(c("a", "z", "b"), c(LETTERS[1:2], "k", LETTERS[5:4])))) ; 
#' obs1 ; 
#' obs2 ; 
#' comp_2d(obs1, obs2)
#' 
#' 
#' # Data frames: same row content and same row names, not same mode between columns
#' 
#' obs1 = as.data.frame(matrix(1:10, byrow = TRUE, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5]))) ; 
#' obs2 = as.data.frame(matrix(c(1:5, 101:105, 6:10), byrow = TRUE, ncol = 5, dimnames = list(c("a", "z", "b"), c(LETTERS[1:2], "k", LETTERS[5:4])))) ; 
#' obs1[, 5] <- as.character(obs1[, 5]) ; 
#' obs2[, 5] <- as.character(obs2[, 5]) ; 
#' obs1 ; 
#' obs2 ; 
#' str(obs1) ; 
#' str(obs2) ; 
#' comp_2d(obs1, obs2)
#' 
#' 
#' # Data frames: same row content but not same row names
#' 
#' obs1 = as.data.frame(matrix(1:10, byrow = TRUE, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5]))) ; 
#' obs2 = as.data.frame(matrix(c(1:5, 101:105, 6:10), byrow = TRUE, ncol = 5, dimnames = list(c("x", "z", "y"), c(LETTERS[1:2], "k", LETTERS[5:4])))) ; 
#' obs1[, 5] <- as.character(obs1[, 5]) ; 
#' obs2[, 5] <- as.character(obs2[, 5]) ; 
#' obs1 ; 
#' obs2 ; 
#' str(obs1) ; 
#' str(obs2) ; 
#' comp_2d(obs1, obs2)
#' @export
comp_2d <- function(
        data1, 
        data2
){
    # DEBUGGING
    # data1 = matrix(1:10, ncol = 5) ; data2 = matrix(1:10, ncol = 5) # for function debugging
    # data1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; data2 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) # for function debugging
    # data1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; data2 = matrix(1:10, ncol = 5) # for function debugging
    # data1 = matrix(1:15, byrow = TRUE, ncol = 5, dimnames = list(letters[1:3], LETTERS[1:5])) ; data2 = matrix(1:10, byrow = TRUE, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) # for function debugging
    # data1 = matrix(1:15, ncol = 5, dimnames = list(letters[1:3], LETTERS[1:5])) ; data2 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) # for function debugging
    # data1 = matrix(1:15, ncol = 5, dimnames = list(paste0("A", letters[1:3]), LETTERS[1:5])) ; data2 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) # for function debugging
    # data1 = matrix(1:15, ncol = 5, dimnames = list(letters[1:3], LETTERS[1:5])) ; data2 = matrix(1:12, ncol = 4, dimnames = list(letters[1:3], LETTERS[1:4])) # for function debugging
    # data1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; data2 = matrix(101:110, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) # for function debugging
    # data1 = data.frame(a = 1:3, b= letters[1:3], row.names = LETTERS[1:3], stringsAsFactors = TRUE) ; data2 = data.frame(A = 1:3, B= letters[1:3], stringsAsFactors = TRUE) # for function debugging
    # data1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; data2 = as.data.frame(matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])), stringsAsFactors = TRUE) # for function debugging
    # data1 = matrix(1:10, byrow = TRUE, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; data2 = matrix(c(1:5, 101:105, 6:10), byrow = TRUE, ncol = 5, dimnames = list(c("a", "z", "b"), c(LETTERS[1:2], "k", LETTERS[5:4]))) # for function debugging
    # data1 = table(Exp1 = c("A", "A", "A", "B", "B", "B"), Exp2 = c("A1", "B1", "A1", "C1", "C1", "B1")) ; data2 = data.frame(A = 1:3, B= letters[1:3], stringsAsFactors = TRUE) # for function debugging
    # data1 = matrix(1:1e6, ncol = 5, dimnames = list(NULL, LETTERS[1:5])) ; data2 = matrix((1:1e6)+1e6/5, ncol = 5, dimnames = list(NULL, LETTERS[1:5]))
    # function name
    ini <- match.call(expand.dots = FALSE) # initial parameters (specific of arg_test())
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()") # function name with "()" paste, which split into a vector of three: c("::()", "package()", "function()") if "package::function()" is used.
    if(function.name[1] == "::()"){
        function.name <- function.name[3]
    }
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2)))) # names of all the arguments
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1] # list of the argument settings (excluding default values not provided by the user)
    # end function name
    
    # package checking
    # check of lib.path
    # end check of lib.path

    # check of the required function from the required packages
    # end check of the required function from the required packages
    # end package checking
    
    # argument primary checking
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
    # argument checking with arg_check()
    # end argument checking with arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev))     # activate this line and use the function to check arguments status
    # end check with r_debugging_tools
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
    if( ! (any(class(data1) %in% c("data.frame", "table")) | all(class(data1) %in% c("matrix", "array")))){ # before R4.0.0, it was  ! any(class(data1) %in% c("matrix", "data.frame", "table"))
        tempo.cat <- paste0("ERROR IN ", function.name, ": THE data1 ARGUMENT MUST BE A MATRIX, DATA FRAME OR TABLE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if( ! (any(class(data2) %in% c("data.frame", "table")) | all(class(data2) %in% c("matrix", "array")))){ # before R4.0.0, it was  ! any(class(data2) %in% c("matrix", "data.frame", "table"))
        tempo.cat <- paste0("ERROR IN ", function.name, ": THE data2 ARGUMENT MUST BE A MATRIX, DATA FRAME OR TABLE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(all(class(data1) == "table") & length(dim(data1)) == 1L){ # class() cannot return NA
        tempo.cat <- paste0("ERROR IN ", function.name, ": THE data1 ARGUMENT IS A 1D TABLE. USE THE comp_1d FUNCTION")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(all(class(data2) == "table", na.rm = TRUE) & length(dim(data2)) == 1L){
        tempo.cat <- paste0("ERROR IN ", function.name, ": THE data2 ARGUMENT IS A 1D TABLE. USE THE comp_1d FUNCTION")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end other checkings
    
    # reserved word checking to avoid bugs
    # end reserved word checking to avoid bugs
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
    if( ! identical(class(data1), class(data2))){
        same.class <- FALSE
    }else{
        same.class <- TRUE
        class <- class(data1)
    }
    if( ! identical(mode(data1), mode(data2))){
        same.mode<- FALSE
    }else{
        same.mode<- TRUE
        mode <- mode(data1)
    }
    if( ! identical(typeof(data1), typeof(data2))){
        same.type <- FALSE
    }else{
        same.type <- TRUE
        type<- typeof(data1)
    }
    if( ! identical(dim(data1), dim(data2))){
        same.dim <- FALSE
    }else{
        same.dim <- TRUE
        dim <- dim(data1)
    }
    if( ! identical(nrow(data1), nrow(data2))){
        same.row.nb <- FALSE
    }else{
        same.row.nb <- TRUE
        row.nb <- nrow(data1)
    }
    if( ! identical(ncol(data1), ncol(data2))){
        same.col.nb <- FALSE
    }else{
        same.col.nb <- TRUE
        col.nb <- ncol(data1)
    }
    # end structure
    # conversion of object into matrix and content into characters
    if(all(class(data1) %in% c("data.frame"))){
        data1 <- apply(data1, 2, function(x){gsub('\\s+', '',x)}) # convert into matrix of character whitout space in the character strings, since as.matrix use format() to convert into characters
    }else if(all(class(data1) %in% c("table"))){
        data1 <- matrix(data1, ncol = ncol(data1), dimnames = dimnames(data1))
        mode(data1) <- "character"
    }
    if(all(class(data2) %in% c("data.frame"))){
        data2 <- apply(data2, 2, function(x){gsub('\\s+', '',x)}) # convert into matrix of character whitout space in the character strings, since as.matrix use format() to convert into characters
    }else if(all(class(data2) %in% c("table"))){
        data2 <- matrix(data2, ncol = ncol(data2), dimnames = dimnames(data2))
        mode(data2) <- "character"
    }
    # end conversion of object into matrix and content into characters
    if(identical(data1, data2)){ # before R4.0.0, it was  ! any(class(data1) %in% c("matrix", "data.frame", "table"))
        same.row.name <- TRUE
        row.name <- dimnames(data1)[[1]]
        any.id.row.name <- TRUE
        same.row.names.pos1 <- 1:row.nb
        same.row.names.pos2 <- 1:row.nb
        same.row.names.match1 <- 1:row.nb
        same.row.names.match2 <- 1:row.nb
        common.row.names <- dimnames(data1)[[1]]
        same.col.name <- TRUE
        col.name <- dimnames(data1)[[2]]
        any.id.col.name <- TRUE
        same.col.names.pos1 <- 1:col.nb
        same.col.names.pos2 <- 1:col.nb
        same.col.names.match1 <- 1:col.nb
        same.col.names.match2 <- 1:col.nb
        common.col.names <- dimnames(data1)[[2]]
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
        if(is.null(dimnames(data1)) & is.null(dimnames(data2))){
            same.row.name <- NULL # but already NULL
            same.col.name <- NULL # but already NULL
            # other row names param remain NULL
        }else if((is.null(dimnames(data1)) & ! is.null(dimnames(data2))) | ( ! is.null(dimnames(data1)) & is.null(dimnames(data2)))){
            same.row.name <- FALSE
            same.col.name <- FALSE
            any.id.row.name <- FALSE
            any.id.col.name <- FALSE
            # other row names param remain NULL
        }else{
            # row names
            if(is.null(dimnames(data1)[[1]]) & is.null(dimnames(data2)[[1]])){
                same.row.name <- NULL # but already NULL
                # other row names param remain NULL
            }else if((is.null(dimnames(data1)[[1]]) & ! is.null(dimnames(data2)[[1]])) | ( ! is.null(dimnames(data1)[[1]]) & is.null(dimnames(data2)[[1]]))){
                same.row.name <- FALSE
                any.id.row.name <- FALSE
                # other row names param remain NULL
            }else if(identical(dimnames(data1)[[1]], dimnames(data2)[[1]])){
                same.row.name <- TRUE
                row.name <- dimnames(data1)[[1]]
                any.id.row.name <- TRUE
                same.row.names.pos1 <- 1:nrow(data1)
                same.row.names.pos2 <- 1:nrow(data1)
                same.row.names.match1 <- 1:nrow(data1)
                same.row.names.match2 <- 1:nrow(data1)
                common.row.names <- dimnames(data1)[[1]]
            }else{
                same.row.name <- FALSE
                any.id.row.name <- FALSE
                if(any(dimnames(data1)[[1]] %in% dimnames(data2)[[1]])){
                    any.id.row.name <- TRUE
                    same.row.names.pos1 <- which(dimnames(data1)[[1]] %in% dimnames(data2)[[1]])
                    same.row.names.match1 <- match(dimnames(data1)[[1]], dimnames(data2)[[1]])
                }
                if(any(dimnames(data2)[[1]] %in% dimnames(data1)[[1]])){
                    any.id.row.name <- TRUE
                    same.row.names.pos2 <- which(dimnames(data2)[[1]] %in% dimnames(data1)[[1]])
                    same.row.names.match2 <- match(dimnames(data2)[[1]], dimnames(data1)[[1]])
                }
                if(any.id.row.name == TRUE){
                    common.row.names <- unique(c(dimnames(data1)[[1]][same.row.names.pos1], dimnames(data2)[[1]][same.row.names.pos2]))
                }
            }
            # col names
            if(is.null(dimnames(data1)[[2]]) & is.null(dimnames(data2)[[2]])){
                same.col.name <- NULL # but already NULL
                # other col names param remain NULL
            }else if((is.null(dimnames(data1)[[2]]) & ! is.null(dimnames(data2)[[2]])) | ( ! is.null(dimnames(data1)[[2]]) & is.null(dimnames(data2)[[2]]))){
                same.col.name <- FALSE
                any.id.col.name <- FALSE
                # other col names param remain NULL
            }else if(identical(dimnames(data1)[[2]], dimnames(data2)[[2]])){
                same.col.name <- TRUE
                col.name <- dimnames(data1)[[2]]
                any.id.col.name <- TRUE
                same.col.names.pos1 <- 1:ncol(data1)
                same.col.names.pos2 <- 1:ncol(data1)
                same.col.names.match1 <- 1:ncol(data1)
                same.col.names.match2 <- 1:ncol(data1)
                common.col.names <- dimnames(data1)[[2]]
            }else{
                same.col.name <- FALSE
                any.id.col.name <- FALSE
                if(any(dimnames(data1)[[2]] %in% dimnames(data2)[[2]])){
                    any.id.col.name <- TRUE
                    same.col.names.pos1 <- which(dimnames(data1)[[2]] %in% dimnames(data2)[[2]])
                    same.col.names.match1 <- match(dimnames(data1)[[2]], dimnames(data2)[[2]])
                }
                if(any(dimnames(data2)[[2]] %in% dimnames(data1)[[2]])){
                    any.id.col.name <- TRUE
                    same.col.names.pos2 <- which(dimnames(data2)[[2]] %in% dimnames(data1)[[2]])
                    same.col.names.match2 <- match(dimnames(data2)[[2]], dimnames(data1)[[2]])
                }
                if(any.id.col.name == TRUE){
                    common.col.names <- unique(c(dimnames(data1)[[2]][same.col.names.pos1], dimnames(data2)[[2]][same.col.names.pos2]))
                }
            }
        }
        # identical row and col content
        row.names(data1) <- paste0("A", 1:nrow(data1))
        row.names(data2) <- paste0("A", 1:nrow(data2))
        colnames(data1) <- paste0("A", 1:ncol(data1))
        colnames(data2) <- paste0("A", 1:ncol(data2))
        if(same.col.nb == TRUE){ # because if not the same col nb, the row cannot be identical
            if(as.double(nrow(data1)) * as.double(nrow(data2)) <= 1e6){
                tempo1 <- c(as.data.frame(t(data1), stringsAsFactors = FALSE)) # conversion into list. This work fast with characters
                tempo2 <- c(as.data.frame(t(data2), stringsAsFactors = FALSE)) # conversion into list. This work fast with characters
                same.row.pos1 <- which(tempo1 %in% tempo2)
                same.row.pos2 <- which(tempo2 %in% tempo1)
                if((length(same.row.pos1) == 0L & length(same.row.pos2) == 0L) | all(is.na(same.row.pos1)) | all(is.na(same.row.pos2))){
                    any.id.row <- FALSE
                    same.row.pos1 <- NULL
                    same.row.pos2 <- NULL
                    # same.row.match1 <- NULL # already NULL above
                    # same.row.match2 <- NULL # already NULL above
                }else{
                    any.id.row <- TRUE
                    same.row.pos1 <- same.row.pos1[ ! is.na(same.row.pos1)]
                    same.row.pos2 <- same.row.pos2[ ! is.na(same.row.pos2)]
                    same.row.match1 <- match(tempo1, tempo2)
                    same.row.match2 <- match(tempo2, tempo1)
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
            if(as.double(ncol(data1)) * as.double(ncol(data2)) <= 1e6){
                tempo1 <- c(as.data.frame(data1, stringsAsFactors = FALSE))
                tempo2 <- c(as.data.frame(data2, stringsAsFactors = FALSE))
                same.col.pos1 <- which(tempo1 %in% tempo2)
                same.col.pos2 <- which(tempo2 %in% tempo1)
                if((length(same.col.pos1) == 0L & length(same.col.pos2) == 0L) | all(is.na(same.col.pos1)) | all(is.na(same.col.pos2))){
                    any.id.col <- FALSE
                    same.col.pos1 <- NULL
                    same.col.pos2 <- NULL
                    # same.col.match1 <- NULL # already NULL above
                    # same.col.match2 <- NULL # already NULL above
                }else{
                    any.id.col <- TRUE
                    same.col.pos1 <- same.col.pos1[ ! is.na(same.col.pos1)]
                    same.col.pos2 <- same.col.pos2[ ! is.na(same.col.pos2)]
                    same.col.match1 <- match(tempo1, tempo2)
                    same.col.match2 <- match(tempo2, tempo1)
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
            if(all(data1 == data2, na.rm = TRUE)){
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
    output <- list(same.class = same.class, class = class, same.mode = same.mode, mode = mode, same.type = same.type , type = type, same.dim = same.dim, dim = dim, same.row.nb = same.row.nb, row.nb = row.nb, same.col.nb = same.col.nb , col.nb = col.nb, same.row.name = same.row.name, row.name = row.name, any.id.row.name = any.id.row.name, same.row.names.pos1 = same.row.names.pos1, same.row.names.pos2 = same.row.names.pos2, same.row.names.match1 = same.row.names.match1, same.row.names.match2 = same.row.names.match2, common.row.names = common.row.names, same.col.name = same.col.name, col.name = col.name,any.id.col.name = any.id.col.name, same.col.names.pos1 = same.col.names.pos1, same.col.names.pos2 = same.col.names.pos2, same.col.names.match1 = same.col.names.match1, same.col.names.match2 = same.col.names.match2, common.col.names = common.col.names, any.id.row = any.id.row, same.row.pos1 = same.row.pos1, same.row.pos2 = same.row.pos2, same.row.match1 = same.row.match1, same.row.match2 = same.row.match2, any.id.col = any.id.col, same.col.pos1 = same.col.pos1, same.col.pos2 = same.col.pos2, same.col.match1 = same.col.match1, same.col.match2 = same.col.match2, identical.content = identical.content, identical = identical.object)
    return(output)
    # end output
    # end main code
}
