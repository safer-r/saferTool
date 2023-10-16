######## df_remod() #### remodeling a data frame to have column name as a qualitative values and vice-versa

#' @title df_remod
#' @description
#' If the data frame is made of n numeric columns, a new data frame is created, with the 1st column gathering all the numeric values, and the 2nd column being the name of the columns of the initial data frame. If row names were present in the initial data frame, then a new ini_rowname column is added with the names of the rows.
#' 
#' 
#' If the data frame is made of one numeric column and one character or factor column, a new data frame is created, with the new columns corresponding to the split numeric values (according to the character column). NA are added a the end of each column to have the same number of rows. BEWARE: in such data frame, rows are not individuals. This means that in the example below, values 10 and 20 are associated on the same row but that means nothing in term of association.
#' @param data Data frame to convert.
#' @param quanti.col.name Optional name for the quanti column of the new data frame.
#' @param quali.col.name Optional name for the quali column of the new data frame.
#' @returns The modified data frame.
#' @details 
#' REQUIRED PACKAGES
#' 
#' none
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' fun_check()
#' @examples
#' obs <- data.frame(col1 = (1:4)*10, col2 = c("A", "B", "A", "A"), stringsAsFactors = TRUE) ; 
#' obs ; 
#' fun_df_remod(obs)
#' 
#' 
#' obs <- data.frame(col1 = (1:4)*10, col2 = 5:8, stringsAsFactors = TRUE) ; 
#' obs ; 
#' fun_df_remod(obs, quanti.col.name = "quanti", quali.col.name = "quali")
#' 
#' 
#' obs <- data.frame(col1 = (1:4)*10, col2 = 5:8, stringsAsFactors = TRUE) ; 
#' rownames(obs) <- paste0("row", 1:4) ; 
#' obs ;
#' fun_df_remod(obs, quanti.col.name = "quanti", quali.col.name = "quali")
#' @export
fun_df_remod <- function(
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
    # function name
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], "()")
    # end function name
    # required function checking
    if(length(utils::find("fun_check", mode = "function")) == 0L){
        tempo.cat <- paste0("ERROR IN ", function.name, ": REQUIRED fun_check() FUNCTION IS MISSING IN THE R ENVIRONMENT")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end required function checking
    # argument checking
    # argument checking without fun_check()
    if( ! any(class(data) %in% "data.frame")){
        tempo.cat <- paste0("ERROR IN ", function.name, ": THE data ARGUMENT MUST BE A DATA FRAME")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
    }
    # end argument checking without fun_check()
    # argument checking with fun_check()
    arg.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- fun_check(data = quanti.col.name, class = "character", length = 1, fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = quali.col.name, class = "character", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(arg.check)){
        if(any(arg.check) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # end argument checking with fun_check()
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
    # end argument checking
    # main code
    tempo.factor <- unlist(lapply(data, class))
    for(i in 1:length(tempo.factor)){ # convert factor columns as character
        if(all(tempo.factor[i] == "factor")){
            data[, i] <- as.character(data[, i])
        }
    }
    tempo.factor <- unlist(lapply(data, mode))
    if(length(data) == 2L){
        if( ! ((base::mode(data[, 1]) == "character" & base::mode(data[, 2]) == "numeric") | base::mode(data[, 2]) == "character" & base::mode(data[, 1]) == "numeric" | base::mode(data[, 2]) == "numeric" & base::mode(data[, 1]) == "numeric") ){
            tempo.cat <- paste0("ERROR IN ", function.name, ": IF data ARGUMENT IS A DATA FRAME MADE OF 2 COLUMNS, EITHER A COLUMN MUST BE NUMERIC AND THE OTHER CHARACTER, OR THE TWO COLUMNS MUST BE NUMERIC")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
        if((base::mode(data[, 1]) == "character" | base::mode(data[, 2]) == "character") & (quanti.col.name != "quanti" | quali.col.name != "quali")){
            tempo.cat <- paste0("ERROR IN ", function.name, ": IMPROPER quanti.col.name OR quali.col.name RESETTINGS. THESE ARGUMENTS ARE RESERVED FOR DATA FRAMES MADE OF n NUMERIC COLUMNS ONLY")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }else{
        if( ! all(tempo.factor %in% "numeric")){
            tempo.cat <- paste0("ERROR IN ", function.name, ": IF data ARGUMENT IS A DATA FRAME MADE OF ONE COLUMN, OR MORE THAN 2 COLUMNS, THESE COLUMNS MUST BE NUMERIC")
            stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), call. = FALSE) # == in stop() to be able to add several messages between ==
        }
    }
    if(( ! any(tempo.factor %in% "character")) & is.null(names(data))){
        tempo.cat <- paste0("ERROR IN ", function.name, ": NUMERIC DATA FRAME in the data ARGUMENT MUST HAVE COLUMN NAMES")
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
        nc.max <- max(table(data[, 2])) # effectif maximum des classes
        nb.na <- nc.max - table(data[,2]) # nombre de NA à ajouter pour réaliser la data frame
        tempo<-split(data[, 1], data[, 2])
        for(i in 1:length(tempo)){tempo[[i]] <- append(tempo[[i]], rep(NA, nb.na[i]))} # des NA doivent être ajoutés lorsque les effectifs sont différents entre les classes. C'est uniquement pour que chaque colonne ait le même nombre de lignes
        output.data<-data.frame(tempo, stringsAsFactors = TRUE, check.names = FALSE)
    }
    return(output.data)
}