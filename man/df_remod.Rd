\name{df_remod}
\alias{df_remod}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
df_remod(data, quanti.col.name = "quanti", quali.col.name = "quali")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{quanti.col.name}{
%%     ~~Describe \code{quanti.col.name} here~~
}
  \item{quali.col.name}{
%%     ~~Describe \code{quali.col.name} here~~
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
function (data, quanti.col.name = "quanti", quali.col.name = "quali") 
{
    ini <- match.call(expand.dots = FALSE)
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], 
        "()")
    if (function.name[1] == "::()") {
        function.name <- function.name[3]
    }
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2))))
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1]
    .pack_and_function_check <- function(req.package = c("cuteDev::arg_check"), 
        lib.path = NULL, external.function.name = function.name) mandat.args <- c("data")
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
    tempo <- cuteDev::arg_check(data = quanti.col.name, class = "character", 
        length = 1, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = quali.col.name, class = "character", 
        length = 1, fun.name = function.name)
    eval(ee)
    if (!is.null(argum.check)) {
        if (any(argum.check, na.rm = TRUE) == TRUE) {
            stop(paste0("\n\n================\n\n", paste(text.check[argum.check], 
                collapse = "\n"), "\n\n================\n\n"), 
                call. = FALSE)
        }
    }
    if (!(all(class(arg.user.setting) == "list", na.rm = TRUE) & 
        length(arg.user.setting) == 0)) {
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
    tempo.arg <- c("data", "quanti.col.name", "quali.col.name")
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
    if (!any(class(data) \%in\% "data.frame")) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": THE data ARGUMENT MUST BE A DATA FRAME")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    tempo.factor <- unlist(lapply(data, class))
    for (i in 1:length(tempo.factor)) {
        if (all(tempo.factor[i] == "factor", na.rm = TRUE)) {
            data[, i] <- as.character(data[, i])
        }
    }
    tempo.factor <- unlist(lapply(data, mode))
    if (length(data) == 2L) {
        if (!((base::mode(data[, 1]) == "character" & base::mode(data[, 
            2]) == "numeric") | base::mode(data[, 2]) == "character" & 
            base::mode(data[, 1]) == "numeric" | base::mode(data[, 
            2]) == "numeric" & base::mode(data[, 1]) == "numeric")) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": IF data ARGUMENT IS A DATA FRAME MADE OF 2 COLUMNS, EITHER A COLUMN MUST BE NUMERIC AND THE OTHER CHARACTER, OR THE TWO COLUMNS MUST BE NUMERIC")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
        if ((base::mode(data[, 1]) == "character" | base::mode(data[, 
            2]) == "character") & (quanti.col.name != "quanti" | 
            quali.col.name != "quali")) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": IMPROPER quanti.col.name OR quali.col.name RESETTINGS. THESE ARGUMENTS ARE RESERVED FOR DATA FRAMES MADE OF n NUMERIC COLUMNS ONLY")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    else {
        if (!all(tempo.factor \%in\% "numeric")) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": IF data ARGUMENT IS A DATA FRAME MADE OF ONE COLUMN, OR MORE THAN 2 COLUMNS, THESE COLUMNS MUST BE NUMERIC")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    if ((!any(tempo.factor \%in\% "character")) & is.null(names(data))) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": NUMERIC DATA FRAME in the data ARGUMENT MUST HAVE COLUMN NAMES")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (all(tempo.factor \%in\% "numeric")) {
        quanti <- NULL
        for (i in 1:length(data)) {
            quanti <- c(quanti, data[, i])
        }
        quali <- rep(names(data), each = nrow(data))
        output.data <- data.frame(quanti, quali, stringsAsFactors = TRUE, 
            check.names = FALSE)
        names(output.data) <- c(quanti.col.name, quali.col.name)
        ini.rownames <- rownames(data)
        tempo.data <- data
        rownames(tempo.data) <- NULL
        null.rownames <- (tempo.data)
        if (!identical(ini.rownames, null.rownames)) {
            ini_rowname <- rep(ini.rownames, times = ncol(data))
            output.data <- cbind(output.data, ini_rowname, stringsAsFactors = TRUE)
        }
    }
    else {
        if (class(data[, 1]) == "character") {
            data <- cbind(data[2], data[1], stringsAsFactors = TRUE)
        }
        nc.max <- max(table(data[, 2]))
        nb.na <- nc.max - table(data[, 2])
        tempo <- split(data[, 1], data[, 2])
        for (i in 1:length(tempo)) {
            tempo[[i]] <- append(tempo[[i]], rep(NA, nb.na[i]))
        }
        output.data <- data.frame(tempo, stringsAsFactors = TRUE, 
            check.names = FALSE)
    }
    return(output.data)
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
