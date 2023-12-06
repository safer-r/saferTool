\name{comp_list}
\alias{comp_list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
comp_list(data1, data2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data1}{
%%     ~~Describe \code{data1} here~~
}
  \item{data2}{
%%     ~~Describe \code{data2} here~~
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
function (data1, data2) 
{
    ini <- match.call(expand.dots = FALSE)
    function.name <- paste0(as.list(match.call(expand.dots = FALSE))[[1]], 
        "()")
    if (function.name[1] == "::()") {
        function.name <- function.name[3]
    }
    arg.names <- names(formals(fun = sys.function(sys.parent(n = 2))))
    arg.user.setting <- as.list(match.call(expand.dots = FALSE))[-1]
    mandat.args <- c("data1", "data2")
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
    tempo.arg <- c("data1", "data2")
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
    if (!any(class(data1) \%in\% "list")) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": THE data1 ARGUMENT MUST BE A LIST")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (!any(class(data2) \%in\% "list")) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": THE data2 ARGUMENT MUST BE A LIST")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    same.length <- NULL
    length <- NULL
    same.names <- NULL
    name <- NULL
    any.id.name <- NULL
    same.names.pos1 <- NULL
    same.names.pos2 <- NULL
    any.id.compartment <- NULL
    same.compartment.pos1 <- NULL
    same.compartment.pos2 <- NULL
    identical.object <- NULL
    identical.content <- NULL
    if (identical(data1, data2)) {
        same.length <- TRUE
        length <- length(data1)
        if (!is.null(names(data1))) {
            same.names <- TRUE
            name <- names(data1)
            any.id.name <- TRUE
            same.names.pos1 <- 1:length(data1)
            same.names.pos2 <- 1:length(data2)
        }
        any.id.compartment <- TRUE
        same.compartment.pos1 <- 1:length(data1)
        same.compartment.pos2 <- 1:length(data2)
        identical.object <- TRUE
        identical.content <- TRUE
    }
    else {
        identical.object <- FALSE
        if (!identical(length(data1), length(data2))) {
            same.length <- FALSE
        }
        else {
            same.length <- TRUE
            length <- length(data1)
        }
        if (!(is.null(names(data1)) & is.null(names(data2)))) {
            if (!identical(names(data1), names(data2))) {
                same.names <- FALSE
            }
            else {
                same.names <- TRUE
                name <- names(data1)
            }
            any.id.name <- FALSE
            if (any(names(data1) \%in\% names(data2))) {
                any.id.name <- TRUE
                same.names.pos1 <- which(names(data1) \%in\% names(data2))
            }
            if (any(names(data2) \%in\% names(data1))) {
                any.id.name <- TRUE
                same.names.pos2 <- which(names(data2) \%in\% names(data1))
            }
        }
        names(data1) <- NULL
        names(data2) <- NULL
        any.id.compartment <- FALSE
        if (any(data1 \%in\% data2)) {
            any.id.compartment <- TRUE
            same.compartment.pos1 <- which(data1 \%in\% data2)
        }
        if (any(data2 \%in\% data1)) {
            any.id.compartment <- TRUE
            same.compartment.pos2 <- which(data2 \%in\% data1)
        }
        if (same.length == TRUE & !all(is.null(same.compartment.pos1), 
            is.null(same.compartment.pos2), na.rm = TRUE)) {
            if (identical(same.compartment.pos1, same.compartment.pos2)) {
                identical.content <- TRUE
            }
            else {
                identical.content <- FALSE
            }
        }
        else {
            identical.content <- FALSE
        }
    }
    output <- list(same.length = same.length, length = length, 
        same.names = same.names, name = name, any.id.name = any.id.name, 
        same.names.pos1 = same.names.pos1, same.names.pos2 = same.names.pos2, 
        any.id.compartment = any.id.compartment, same.compartment.pos1 = same.compartment.pos1, 
        same.compartment.pos2 = same.compartment.pos2, identical.object = identical.object, 
        identical.content = identical.content)
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
