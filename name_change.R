######## name_change() #### check a vector of character strings and modify any string if present in another vector

#' @title name_change
#' @description
#' This function allow to check if a vector of character strings, like column names of a data frame, has elements present in another vector (vector of reserved words or column names of another data frame before merging).
#' @param data1 Vector of character strings to check and modify.
#' @param data2 Reference vector of character strings.
#' @param added.string String added at the end of the modified string in data1 if present in data2.
#' @returns
#' A list containing :
#' - $data: the modified data1 (in the same order as in the initial data1)
#' - $ini: the initial elements before modification. NULL if no modification
#' - $post: the modified elements in the same order as in ini. NULL if no modification
#' @details 
#' REQUIRED PACKAGES
#' 
#' none
#' 
#' REQUIRED FUNCTIONS FROM CUTE_LITTLE_R_FUNCTION
#' 
#' fun_check()
#'
#' @examples
#' obs1 <- c("A", "B", "C", "D") ; 
#' obs2 <- c("A", "C") ; 
#' fun_name_change(obs1, obs2)
#' 
#' 
#' obs1 <- c("A", "B", "C", "C_modif1", "D") ; 
#' obs2 <- c("A", "A_modif1", "C") ; 
#' fun_name_change(obs1, obs2) 
#' # the function checks that the new names are neither in obs1 nor in obs2 (increment the number after the added string)
#' @export
fun_name_change <- function(data1, data2, added.string = "_modif"){

    # DEBUGGING
    # data1 = c("A", "B", "C", "D") ; data2 <- c("A", "C") ; added.string = "_modif" # for function debugging
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
    arg.check <- NULL #
    text.check <- NULL #
    checked.arg.names <- NULL # for function debbuging: used by r_debugging_tools
    ee <- expression(arg.check <- c(arg.check, tempo$problem) , text.check <- c(text.check, tempo$text) , checked.arg.names <- c(checked.arg.names, tempo$object.name))
    tempo <- fun_check(data = data1, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = data2, class = "vector", mode = "character", fun.name = function.name) ; eval(ee)
    tempo <- fun_check(data = added.string, class = "vector", mode = "character", length = 1, fun.name = function.name) ; eval(ee)
    if( ! is.null(arg.check)){
        if(any(arg.check) == TRUE){
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], collapse = "\n"), "\n\n================\n\n"), call. = FALSE) #
        }
    }
    # source("C:/Users/Gael/Documents/Git_versions_to_use/debugging_tools_for_r_dev-v1.7/r_debugging_tools-v1.7.R") ; eval(parse(text = str_basic_arg_check_dev)) ; eval(parse(text = str_arg_check_with_fun_check_dev)) # activate this line and use the function (with no arguments left as NULL) to check arguments status and if they have been checked using fun_check()
    # end argument checking
    # main code
    ini <- NULL
    post <- NULL
    if(any(data1 %in% data2)){
        tempo.names <- data1[data1 %in% data2]
        ini <- NULL
        post <- NULL
        for(i2 in 1:length(tempo.names)){
            count <- 0
            tempo <- tempo.names[i2]
            while(any(tempo %in% data2) | any(tempo %in% data1)){
                count <- count + 1
                tempo <- paste0(tempo.names[i2], "_modif", count)
            }
            data1[data1 %in% tempo.names[i2]] <- paste0(tempo.names[i2], "_modif", count)
            if(count != 0){
                ini <- c(ini, tempo.names[i2])
                post <- c(post, paste0(tempo.names[i2], "_modif", count))
            }
        }
        data <- data1
    }else{
        data <- data1
    }
    output <- list(data = data, ini = ini, post = post)
    return(output)
}

