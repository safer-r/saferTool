#' @title df_remod
#' @description
#' If the data frame is made of n numeric columns, a new data frame is created, with the 1st column gathering all the numeric values, and the 2nd column being the name of the columns of the initial data frame. If row names were present in the initial data frame, then a new ini_rowname column is added with the names of the rows.
#' 
#' 
#' If the data frame is made of one numeric column and one character or factor column, a new data frame is created, with the new columns corresponding to the split numeric values (according to the character column). NA are added a the end of each column to have the same number of rows. BEWARE: in such data frame, rows are not individuals. This means that in the example below, values 10 and 20 are associated on the same row but that means nothing in term of association.
#' @param data Data frame to convert.
#' @param quanti.col.name Single character string. Optional name for the quanti column of the new data frame.
#' @param quali.col.name Single character string. Optional name for the quali column of the new data frame.
#' @returns The modified data frame.
#' @examples
#' obs <- data.frame(col1 = (1:4)*10, col2 = c("A", "B", "A", "A"), stringsAsFactors = TRUE) ; 
#' obs ; 
#' df_remod(obs)
#' 
#' 
#' obs <- data.frame(col1 = (1:4)*10, col2 = 5:8, stringsAsFactors = TRUE) ; 
#' obs ; 
#' df_remod(obs, quanti.col.name = "quanti", quali.col.name = "quali")
#' 
#' 
#' obs <- data.frame(col1 = (1:4)*10, col2 = 5:8, stringsAsFactors = TRUE) ; 
#' rownames(obs) <- paste0("row", 1:4) ; 
#' obs ;
#' df_remod(obs, quanti.col.name = "quanti", quali.col.name = "quali")
#' @importFrom saferDev arg_check
#' @export
df_remod <- function(
        data, 
        quanti.col.name = "quanti", 
        quali.col.name = "quali"
){
    # DEBUGGING
    # data = data.frame(a = 1:3, b = 4:6, stringsAsFactors = TRUE) ; quanti.col.name = "quanti" ; quali.col.name = "quali" # for function debugging
    # data = data.frame(a = 1:3, b = 4:6, c = 11:13, stringsAsFactors = TRUE) ; quanti.col.name = "quanti" ; quali.col.name = "quali" # for function debugging
    # data = data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = TRUE) ; quanti.col.name = "quanti" ; quali.col.name = "quali" # for function debugging
    # data = data.frame(a = 1:3, b = letters[1:3], stringsAsFactors = TRUE) ; quanti.col.name = "TEST" ; quali.col.name = "quali" # for function debugging
    # data = data.frame(b = letters[1:3], a = 1:3, stringsAsFactors = TRUE) ; quanti.col.name = "quanti" ; quali.col.name = "quali" # for function debugging
    # data = data.frame(b = c("e", "e", "h"), a = 1:3, stringsAsFactors = TRUE) ; quanti.col.name = "quanti" ; quali.col.name = "quali" # for function debugging
    # package name
    package.name <- "saferTool"
    # end package name
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
    .pack_and_function_check(
        fun = c(
            "saferDev::arg_check"
        ),
        lib.path = NULL,
        external.function.name = function.name
    )
    # end check of the required function from the required packages
    # end package checking
    
    # argument primary checking
    # arg with no default values
    mandat.args <- c(
        "data"
    )
    tempo <- eval(parse(text = paste0("c(missing(", paste0(mandat.args, collapse = "),missing("), "))")))
    if(any(tempo)){ # normally no NA for missing() output
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \nFOLLOWING ARGUMENT", ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end arg with no default values
    
    # argument checking with saferDev::arg_check()
    argum.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(argum.check <- c(argum.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- saferDev::arg_check(data = quanti.col.name, class = "character", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- saferDev::arg_check(data = quali.col.name, class = "character", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(argum.check)){
        if(any(argum.check, na.rm = TRUE) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[argum.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with saferDev::arg_check()
    # check with r_debugging_tools
    # source("C:/Users/yhan/Documents/Git_projects/debugging_tools_for_r_dev/r_debugging_tools.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using arg_check()
    # end check with r_debugging_tools
    # end argument primary checking
    
    # second round of checking and data preparation
    # management of NA arguments
    if( ! (all(class(arg.user.setting) == "list", na.rm = TRUE) & length(arg.user.setting) == 0)){
        tempo.arg <- names(arg.user.setting) # values provided by the user
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = length) == 1L # no argument provided by the user can be just NA
        if(any(tempo.log) == TRUE){ # normally no NA because is.na() used here
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: \n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", "THIS ARGUMENT"), " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    # end management of NA arguments
    
    # management of NULL arguments
    tempo.arg <-c(
        "data", 
        "quanti.col.name", 
        "quali.col.name"
    )
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), inherit = FALSE), FUN = is.null)
    if(any(tempo.log) == TRUE){# normally no NA with is.null()
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE:\n", ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", "THIS ARGUMENT\n"), paste0(tempo.arg[tempo.log], collapse = "\n"),"\nCANNOT BE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end management of NULL arguments
    
    # code that protects set.seed() in the global environment
    # end code that protects set.seed() in the global environment
    
    # warning initiation
    # end warning initiation
    
    # other checkings
    # argument checking without arg_check()
    if( ! any(class(data) %in% "data.frame")){
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: THE data ARGUMENT MUST BE A DATA FRAME")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end argument checking without arg_check()
    # end other checkings
    
    # reserved word checking(to avoid bugs)
    # end reserved word checking(to avoid bugs)
    # end second round of checking and data preparation
    
    # main code
    tempo.factor <- unlist(lapply(data, class))
    for(i in 1:length(tempo.factor)){ # convert factor columns as character
        if(all(tempo.factor[i] == "factor", na.rm = TRUE)){
            data[, i] <- as.character(data[, i])
        }
    }
    tempo.factor <- unlist(lapply(data, mode))
    if(length(data) == 2L){
        if( ! ((base::mode(data[, 1]) == "character" & base::mode(data[, 2]) == "numeric") | base::mode(data[, 2]) == "character" & base::mode(data[, 1]) == "numeric" | base::mode(data[, 2]) == "numeric" & base::mode(data[, 1]) == "numeric") ){
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: IF data ARGUMENT IS A DATA FRAME MADE OF 2 COLUMNS, EITHER A COLUMN MUST BE NUMERIC AND THE OTHER CHARACTER, OR THE TWO COLUMNS MUST BE NUMERIC")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if((base::mode(data[, 1]) == "character" | base::mode(data[, 2]) == "character") & (quanti.col.name != "quanti" | quali.col.name != "quali")){
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: IMPROPER quanti.col.name OR quali.col.name RESETTINGS. THESE ARGUMENTS ARE RESERVED FOR DATA FRAMES MADE OF n NUMERIC COLUMNS ONLY")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }else{
        if( ! all(tempo.factor %in% "numeric")){
            tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: IF data ARGUMENT IS A DATA FRAME MADE OF ONE COLUMN, OR MORE THAN 2 COLUMNS, THESE COLUMNS MUST BE NUMERIC")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    if(( ! any(tempo.factor %in% "character")) & is.null(names(data))){
        tempo.cat <- paste0("ERROR IN ", function.name, " OF THE ", package.name, " PACKAGE: NUMERIC DATA FRAME in the data ARGUMENT MUST HAVE COLUMN NAMES")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    if(all(tempo.factor %in% "numeric")){ # transfo 1
        quanti <- NULL
        for(i in 1:length(data)){
            quanti <-c(quanti, data[, i])
        }
        quali <- rep(names(data), each = nrow(data))
        output.data <- data.frame(quanti, quali, stringsAsFactors = TRUE, check.names = FALSE)
        names(output.data) <- c(quanti.col.name, quali.col.name)
        # add the ini_rowname column
        ini.rownames <- rownames(data)
        tempo.data <- data
        rownames(tempo.data) <- NULL
        null.rownames <- (tempo.data)
        if( ! identical(ini.rownames, null.rownames)){
            ini_rowname <- rep(ini.rownames, times = ncol(data))
            output.data <- cbind(output.data, ini_rowname, stringsAsFactors = TRUE)
        }
    }else{ # transfo 2
        if(class(data[, 1]) == "character"){
            data <- cbind(data[2], data[1], stringsAsFactors = TRUE)
        }
        nc.max <- base::max(table(data[, 2])) # effectif maximum des classes
        nb.na <- nc.max - table(data[,2]) # nombre de NA à ajouter pour réaliser la data frame
        tempo<-split(data[, 1], data[, 2])
        for(i in 1:length(tempo)){tempo[[i]] <- append(tempo[[i]], rep(NA, nb.na[i]))} # des NA doivent être ajoutés lorsque les effectifs sont différents entre les classes. C'est uniquement pour que chaque colonne ait le même nombre de lignes
        output.data<-data.frame(tempo, stringsAsFactors = TRUE, check.names = FALSE)
    }
    # output
    # warning output
    # end warning output
    return(output.data)
    # end output
    # end main code
}