\name{info}
\alias{info}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
info(data, n = NULL, warn.print = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{n}{
%%     ~~Describe \code{n} here~~
}
  \item{warn.print}{
%%     ~~Describe \code{warn.print} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, n = NULL, warn.print = TRUE) 
{
    ini <- match.call(expand.dots = FALSE)
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], 
        "()")
    if (function.name[1] == "::()") {
        function.name <- function.name[3]
    }
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2))))
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1]
    .pack_and_function_check <- function(req.package = c("cuteDev::arg_check", 
        "cuteDev::get_message()"), lib.path = NULL, external.function.name = function.name) mandat.args <- c("data")
    tempo <- eval(parse(text = paste0("c(missing(", paste0(mandat.args, 
        collapse = "),missing("), "))")))
    if (any(tempo)) {
        tempo.cat <- paste0("ERROR IN ", function.name, "\nFOLLOWING ARGUMENT", 
            ifelse(sum(tempo, na.rm = TRUE) > 1, "S HAVE", " HAS"), 
            " NO DEFAULT VALUE AND REQUIRE ONE:\n", paste0(mandat.args, 
                collapse = "\n"))
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    argum.check <- NULL
    text.check <- NULL
    checked.arg.names <- NULL
    ee <- expression(argum.check <- c(argum.check, tempo$problem), 
        text.check <- c(text.check, tempo$text), checked.arg.names <- c(checked.arg.names, 
            tempo$object.name))
    if (!is.null(n)) {
        tempo <- cuteDev::arg_check(data = n, class = "vector", 
            typeof = "integer", length = 1, double.as.integer.allowed = TRUE, 
            fun.name = function.name)
        eval(ee)
    }
    else {
        tempo <- cuteDev::arg_check(data = n, class = "vector")
        checked.arg.names <- c(checked.arg.names, tempo$object.name)
    }
    tempo <- cuteDev::arg_check(data = warn.print, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    if (!is.null(argum.check)) {
        if (any(argum.check) == TRUE) {
            stop(paste0("\n\n================\n\n", paste(text.check[argum.check], 
                collapse = "\n"), "\n\n================\n\n"), 
                call. = FALSE)
        }
    }
    if (!(all(class(arg.user.setting) == "list") & length(arg.user.setting) == 
        0)) {
        tempo.arg <- names(arg.user.setting)
        tempo.log <- suppressWarnings(sapply(lapply(lapply(tempo.arg, 
            FUN = get, env = sys.nframe(), inherit = FALSE), 
            FUN = is.na), FUN = any)) & lapply(lapply(tempo.arg, 
            FUN = get, env = sys.nframe(), inherit = FALSE), 
            FUN = length) == 1L
        if (any(tempo.log) == TRUE) {
            tempo.cat <- paste0("ERROR IN ", function.name, "\n", 
                ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS", 
                  "THIS ARGUMENT"), " CANNOT JUST BE NA:", paste0(tempo.arg[tempo.log], 
                  collapse = "\n"))
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    tempo.arg <- c("data", "warn.print")
    tempo.log <- sapply(lapply(tempo.arg, FUN = get, env = sys.nframe(), 
        inherit = FALSE), FUN = is.null)
    if (any(tempo.log) == TRUE) {
        tempo.cat <- paste0("ERROR IN ", function.name, ":\n", 
            ifelse(sum(tempo.log, na.rm = TRUE) > 1, "THESE ARGUMENTS\n", 
                "THIS ARGUMENT\n"), paste0(tempo.arg[tempo.log], 
                collapse = "\n"), "\nCANNOT BE NULL")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    ini.warning.length <- options()$warning.length
    options(warning.length = 8170)
    warn <- NULL
    warn.count <- 0
    if (!is.null(n)) {
        if (n < 1) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": n ARGUMENT MUST BE A POSITIVE AND NON NULL INTEGER")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n", ifelse(is.null(warn), 
                  "", paste0("IN ADDITION\nWARNING", ifelse(warn.count > 
                    1, "S", ""), ":\n\n", warn))), call. = FALSE)
        }
        else if (is.finite(n)) {
            tempo.warn <- paste0("SOME COMPARTMENTS CAN BE TRUNCATED (n ARGUMENT IS ", 
                n, ")")
            warn <- paste0(ifelse(is.null(warn), tempo.warn, 
                paste0(warn, "\n\n", tempo.warn)))
        }
    }
    env.name <- paste0("env", as.numeric(Sys.time()))
    if (exists(env.name, where = -1)) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": ENVIRONMENT env.name ALREADY EXISTS. PLEASE RERUN ONCE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    else {
        assign(env.name, new.env())
        assign("data", data, envir = get(env.name, envir = sys.nframe(), 
            inherits = FALSE))
    }
    data.name <- deparse(substitute(data))
    output <- list(NAME = data.name)
    tempo.try.error <- cuteDev::get_message(data = "class(data)", 
        kind = "error", header = FALSE, env = get(env.name, envir = sys.nframe(), 
            inherits = FALSE))
    if (is.null(tempo.try.error)) {
        tempo <- list(CLASS = class(data))
        output <- c(output, tempo)
    }
    tempo.try.error <- cuteDev::get_message(data = "typeof(data)", 
        kind = "error", header = FALSE, env = get(env.name, envir = sys.nframe(), 
            inherits = FALSE))
    if (is.null(tempo.try.error)) {
        tempo <- list(TYPE = typeof(data))
        output <- c(output, tempo)
    }
    tempo.try.error <- cuteDev::get_message(data = "length(data)", 
        kind = "error", header = FALSE, env = get(env.name, envir = sys.nframe(), 
            inherits = FALSE))
    if (is.null(tempo.try.error)) {
        tempo <- list(LENGTH = length(data))
        output <- c(output, tempo)
    }
    if (all(typeof(data) \%in\% c("integer", "numeric", "double")) & 
        !any(class(data) \%in\% "factor")) {
        tempo <- list(INF.NB = sum(is.infinite(data)))
        output <- c(output, tempo)
        tempo <- list(RANGE = range(data[!is.infinite(data)], 
            na.rm = TRUE))
        output <- c(output, tempo)
        tempo <- list(SUM = sum(data[!is.infinite(data)], na.rm = TRUE))
        output <- c(output, tempo)
        tempo <- list(MEAN = mean(data[!is.infinite(data)], na.rm = TRUE))
        output <- c(output, tempo)
    }
    if (all(typeof(data) \%in\% c("logical", "integer", "double", 
        "complex", "character", "list"))) {
        tempo.try.error <- cuteDev::get_message(data = "is.na(data)", 
            kind = "error", header = FALSE, env = get(env.name, 
                envir = sys.nframe(), inherits = FALSE))
        if (is.null(tempo.try.error)) {
            tempo <- list(NA.NB = sum(is.na(data)))
            output <- c(output, tempo)
        }
    }
    tempo.try.error <- cuteDev::get_message(data = "head(data)", 
        kind = "error", header = FALSE, env = get(env.name, envir = sys.nframe(), 
            inherits = FALSE))
    if (is.null(tempo.try.error)) {
        tempo <- list(HEAD = head(data))
        output <- c(output, tempo)
        tempo <- list(TAIL = tail(data))
        output <- c(output, tempo)
    }
    tempo.try.error <- cuteDev::get_message(data = "dim(data)", 
        kind = "error", header = FALSE, env = get(env.name, envir = sys.nframe(), 
            inherits = FALSE))
    if (is.null(tempo.try.error)) {
        if (length(dim(data)) > 0) {
            tempo <- list(DIMENSION = dim(data))
            if (length(tempo[[1]]) == 2L) {
                names(tempo[[1]]) <- c("NROW", "NCOL")
            }
            output <- c(output, tempo)
        }
    }
    if (all(class(data) == "data.frame") | all(class(data) \%in\% 
        c("matrix", "array")) | all(class(data) == "table")) {
        if (length(dim(data)) > 1) {
            tempo <- list(ROW_NAMES = dimnames(data)[[1]])
            output <- c(output, tempo)
            tempo <- list(COLUM_NAMES = dimnames(data)[[2]])
            output <- c(output, tempo)
        }
    }
    tempo.try.error <- cuteDev::get_message(data = "summary(data)", 
        kind = "error", header = FALSE, env = get(env.name, envir = sys.nframe(), 
            inherits = FALSE))
    if (is.null(tempo.try.error)) {
        tempo <- list(SUMMARY = summary(data))
        output <- c(output, tempo)
    }
    tempo.try.error <- cuteDev::get_message(data = "noquote(matrix(capture.output(str(data))))", 
        kind = "error", header = FALSE, env = get(env.name, envir = sys.nframe(), 
            inherits = FALSE))
    if (is.null(tempo.try.error)) {
        tempo <- capture.output(str(data))
        tempo <- list(STRUCTURE = noquote(matrix(tempo, dimnames = list(rep("", 
            length(tempo)), ""))))
        output <- c(output, tempo)
    }
    if (all(class(data) == "data.frame")) {
        tempo <- list(COLUMN_TYPE = sapply(data, FUN = "typeof"))
        if (any(sapply(data, FUN = "class") \%in\% "factor")) {
            tempo.class <- sapply(data, FUN = "class")
            if (any(unlist(tempo.class) \%in\% "ordered")) {
                tempo2 <- sapply(tempo.class, paste, collapse = " ")
            }
            else {
                tempo2 <- unlist(tempo.class)
            }
            tempo[["COLUMN_TYPE"]][grepl(x = tempo2, pattern = "factor")] <- tempo2[grepl(x = tempo2, 
                pattern = "factor")]
        }
        output <- c(output, tempo)
    }
    if (all(class(data) == "list")) {
        tempo <- list(COMPARTMENT_NAMES = names(data))
        output <- c(output, tempo)
        tempo <- list(COMPARTMENT_TYPE = sapply(data, FUN = "typeof"))
        if (any(unlist(sapply(data, FUN = "class")) \%in\% "factor")) {
            tempo.class <- sapply(data, FUN = "class")
            if (any(unlist(tempo.class) \%in\% "ordered")) {
                tempo2 <- sapply(tempo.class, paste, collapse = " ")
            }
            else {
                tempo2 <- unlist(tempo.class)
            }
            tempo[["COMPARTMENT_TYPE"]][grepl(x = tempo2, pattern = "factor")] <- tempo2[grepl(x = tempo2, 
                pattern = "factor")]
        }
        output <- c(output, tempo)
    }
    if (!is.null(n)) {
        output[names(output) != "STRUCTURE"] <- lapply(X = output[names(output) != 
            "STRUCTURE"], FUN = head, n = n, simplify = FALSE)
    }
    if (warn.print == TRUE & !is.null(warn)) {
        on.exit(warning(paste0("FROM ", function.name, ":\n\n", 
            warn), call. = FALSE))
    }
    on.exit(expr = options(warning.length = ini.warning.length), 
        add = TRUE)
    if (warn.print == FALSE) {
        output <- c(output, WARNING = warn)
    }
    else if (warn.print == TRUE & !is.null(warn)) {
        on.exit(warning(paste0("FROM ", function.name, ":\n\n", 
            warn), call. = FALSE))
    }
    on.exit(expr = options(warning.length = ini.warning.length), 
        add = TRUE)
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
