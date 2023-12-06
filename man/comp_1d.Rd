\name{comp_1d}
\alias{comp_1d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
comp_1d(data1, data2)
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
    if (!any(class(data1) \%in\% c("logical", "integer", "numeric", 
        "character", "factor", "table"))) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": THE data1 ARGUMENT MUST BE A NON NULL VECTOR, FACTOR OR 1D TABLE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    else if (all(class(data1) \%in\% "table")) {
        if (length(dim(data1)) > 1) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": THE data1 ARGUMENT MUST BE A 1D TABLE")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    if (!any(class(data2) \%in\% c("logical", "integer", "numeric", 
        "character", "factor", "table"))) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": THE data2 ARGUMENT MUST BE A NON NULL VECTOR, FACTOR OR 1D TABLE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    else if (all(class(data2) \%in\% "table")) {
        if (length(dim(data2)) > 1) {
            tempo.cat <- paste0("ERROR IN ", function.name, ": THE data2 ARGUMENT MUST BE A 1D TABLE")
            stop(paste0("\n\n================\n\n", tempo.cat, 
                "\n\n================\n\n"), call. = FALSE)
        }
    }
    same.class <- FALSE
    class <- NULL
    same.length <- FALSE
    length <- NULL
    same.levels <- NULL
    levels <- NULL
    any.id.levels <- FALSE
    same.levels.pos1 <- NULL
    same.levels.pos2 <- NULL
    same.levels.match1 <- NULL
    same.levels.match2 <- NULL
    common.levels <- NULL
    same.names <- NULL
    name <- NULL
    any.id.name <- FALSE
    same.names.pos1 <- NULL
    same.names.pos2 <- NULL
    same.names.match1 <- NULL
    same.names.match2 <- NULL
    common.names <- NULL
    any.id.element <- FALSE
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
    if (identical(data1, data2)) {
        same.class <- TRUE
        class <- class(data1)
        same.length <- TRUE
        length <- length(data1)
        if (any(class(data1) \%in\% "factor")) {
            same.levels <- TRUE
            levels <- levels(data1)
            any.id.levels <- TRUE
            same.levels.pos1 <- 1:length(levels(data1))
            same.levels.pos2 <- 1:length(levels(data2))
            same.levels.match1 <- 1:length(levels(data1))
            same.levels.match2 <- 1:length(levels(data2))
            common.levels <- levels(data1)
        }
        if (!is.null(names(data1))) {
            same.names <- TRUE
            name <- names(data1)
            any.id.name <- TRUE
            same.names.pos1 <- 1:length(data1)
            same.names.pos2 <- 1:length(data2)
            same.names.match1 <- 1:length(data1)
            same.names.match2 <- 1:length(data2)
            common.names <- names(data1)
        }
        any.id.element <- TRUE
        same.elements.pos1 <- 1:length(data1)
        same.elements.pos2 <- 1:length(data2)
        same.elements.match1 <- 1:length(data1)
        same.elements.match2 <- 1:length(data2)
        common.elements <- data1
        same.order <- TRUE
        order1 <- order(data1)
        order2 <- order(data2)
        identical.object <- TRUE
        identical.content <- TRUE
    }
    else {
        if (identical(class(data1), class(data2))) {
            same.class <- TRUE
            class <- class(data1)
        }
        if (identical(length(data1), length(data2))) {
            same.length <- TRUE
            length <- length(data1)
        }
        if (any(class(data1) \%in\% "factor") & any(class(data2) \%in\% 
            "factor")) {
            if (identical(levels(data1), levels(data2))) {
                same.levels <- TRUE
                levels <- levels(data1)
            }
            else {
                same.levels <- FALSE
            }
            if (any(levels(data1) \%in\% levels(data2))) {
                any.id.levels <- TRUE
                same.levels.pos1 <- which(levels(data1) \%in\% 
                  levels(data2))
                same.levels.match1 <- match(levels(data1), levels(data2))
            }
            if (any(levels(data2) \%in\% levels(data1))) {
                any.id.levels <- TRUE
                same.levels.pos2 <- which(levels(data2) \%in\% 
                  levels(data1))
                same.levels.match2 <- match(levels(data2), levels(data1))
            }
            if (any.id.levels == TRUE) {
                common.levels <- unique(c(levels(data1)[same.levels.pos1], 
                  levels(data2)[same.levels.pos2]))
            }
        }
        if (any(class(data1) \%in\% "factor")) {
            data1 <- as.character(data1)
        }
        if (any(class(data2) \%in\% "factor")) {
            data2 <- as.character(data2)
        }
        if (!(is.null(names(data1)) & is.null(names(data2)))) {
            if (identical(names(data1), names(data2))) {
                same.names <- TRUE
                name <- names(data1)
            }
            else {
                same.names <- FALSE
            }
            if (any(names(data1) \%in\% names(data2))) {
                any.id.name <- TRUE
                same.names.pos1 <- which(names(data1) \%in\% names(data2))
                same.names.match1 <- match(names(data1), names(data2))
            }
            if (any(names(data2) \%in\% names(data1))) {
                any.id.name <- TRUE
                same.names.pos2 <- which(names(data2) \%in\% names(data1))
                same.names.match2 <- match(names(data2), names(data1))
            }
            if (any.id.name == TRUE) {
                common.names <- unique(c(names(data1)[same.names.pos1], 
                  names(data2)[same.names.pos2]))
            }
        }
        names(data1) <- NULL
        names(data2) <- NULL
        if (any(data1 \%in\% data2)) {
            any.id.element <- TRUE
            same.elements.pos1 <- which(data1 \%in\% data2)
            same.elements.match1 <- match(data1, data2)
        }
        if (any(data2 \%in\% data1)) {
            any.id.element <- TRUE
            same.elements.pos2 <- which(data2 \%in\% data1)
            same.elements.match2 <- match(data2, data1)
        }
        if (any.id.element == TRUE) {
            common.elements <- unique(c(data1[same.elements.pos1], 
                data2[same.elements.pos2]))
        }
        if (identical(data1, data2)) {
            identical.content <- TRUE
            same.order <- TRUE
        }
        else if (identical(sort(data1), sort(data2))) {
            same.order <- FALSE
            order1 <- order(data1)
            order2 <- order(data2)
        }
    }
    output <- list(same.class = same.class, class = class, same.length = same.length, 
        length = length, same.levels = same.levels, levels = levels, 
        any.id.levels = any.id.levels, same.levels.pos1 = same.levels.pos1, 
        same.levels.pos2 = same.levels.pos2, same.levels.match1 = same.levels.match1, 
        same.levels.match2 = same.levels.match2, common.levels = common.levels, 
        same.names = same.names, name = name, any.id.name = any.id.name, 
        same.names.pos1 = same.names.pos1, same.names.pos2 = same.names.pos2, 
        same.names.match1 = same.names.match1, same.names.match2 = same.names.match2, 
        common.names = common.names, any.id.element = any.id.element, 
        same.elements.pos1 = same.elements.pos1, same.elements.pos2 = same.elements.pos2, 
        same.elements.match1 = same.elements.match1, same.elements.match2 = same.elements.match2, 
        common.elements = common.elements, same.order = same.order, 
        order1 = order1, order2 = order2, identical.object = identical.object, 
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
