\name{comp_2d}
\alias{comp_2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
comp_2d(data1, data2)
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
    if (!(any(class(data1) \%in\% c("data.frame", "table")) | all(class(data1) \%in\% 
        c("matrix", "array")))) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": THE data1 ARGUMENT MUST BE A MATRIX, DATA FRAME OR TABLE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (!(any(class(data2) \%in\% c("data.frame", "table")) | all(class(data2) \%in\% 
        c("matrix", "array")))) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": THE data2 ARGUMENT MUST BE A MATRIX, DATA FRAME OR TABLE")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (all(class(data1) == "table") & length(dim(data1)) == 
        1L) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": THE data1 ARGUMENT IS A 1D TABLE. USE THE comp_1d FUNCTION")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    if (all(class(data2) == "table", na.rm = TRUE) & length(dim(data2)) == 
        1L) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": THE data2 ARGUMENT IS A 1D TABLE. USE THE comp_1d FUNCTION")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
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
    if (!identical(class(data1), class(data2))) {
        same.class <- FALSE
    }
    else {
        same.class <- TRUE
        class <- class(data1)
    }
    if (!identical(mode(data1), mode(data2))) {
        same.mode <- FALSE
    }
    else {
        same.mode <- TRUE
        mode <- mode(data1)
    }
    if (!identical(typeof(data1), typeof(data2))) {
        same.type <- FALSE
    }
    else {
        same.type <- TRUE
        type <- typeof(data1)
    }
    if (!identical(dim(data1), dim(data2))) {
        same.dim <- FALSE
    }
    else {
        same.dim <- TRUE
        dim <- dim(data1)
    }
    if (!identical(nrow(data1), nrow(data2))) {
        same.row.nb <- FALSE
    }
    else {
        same.row.nb <- TRUE
        row.nb <- nrow(data1)
    }
    if (!identical(ncol(data1), ncol(data2))) {
        same.col.nb <- FALSE
    }
    else {
        same.col.nb <- TRUE
        col.nb <- ncol(data1)
    }
    if (all(class(data1) \%in\% c("data.frame"))) {
        data1 <- apply(data1, 2, function(x) {
            gsub("\\s+", "", x)
        })
    }
    else if (all(class(data1) \%in\% c("table"))) {
        data1 <- matrix(data1, ncol = ncol(data1), dimnames = dimnames(data1))
        mode(data1) <- "character"
    }
    if (all(class(data2) \%in\% c("data.frame"))) {
        data2 <- apply(data2, 2, function(x) {
            gsub("\\s+", "", x)
        })
    }
    else if (all(class(data2) \%in\% c("table"))) {
        data2 <- matrix(data2, ncol = ncol(data2), dimnames = dimnames(data2))
        mode(data2) <- "character"
    }
    if (identical(data1, data2)) {
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
    }
    else {
        identical.object <- FALSE
        if (is.null(dimnames(data1)) & is.null(dimnames(data2))) {
            same.row.name <- NULL
            same.col.name <- NULL
        }
        else if ((is.null(dimnames(data1)) & !is.null(dimnames(data2))) | 
            (!is.null(dimnames(data1)) & is.null(dimnames(data2)))) {
            same.row.name <- FALSE
            same.col.name <- FALSE
            any.id.row.name <- FALSE
            any.id.col.name <- FALSE
        }
        else {
            if (is.null(dimnames(data1)[[1]]) & is.null(dimnames(data2)[[1]])) {
                same.row.name <- NULL
            }
            else if ((is.null(dimnames(data1)[[1]]) & !is.null(dimnames(data2)[[1]])) | 
                (!is.null(dimnames(data1)[[1]]) & is.null(dimnames(data2)[[1]]))) {
                same.row.name <- FALSE
                any.id.row.name <- FALSE
            }
            else if (identical(dimnames(data1)[[1]], dimnames(data2)[[1]])) {
                same.row.name <- TRUE
                row.name <- dimnames(data1)[[1]]
                any.id.row.name <- TRUE
                same.row.names.pos1 <- 1:nrow(data1)
                same.row.names.pos2 <- 1:nrow(data1)
                same.row.names.match1 <- 1:nrow(data1)
                same.row.names.match2 <- 1:nrow(data1)
                common.row.names <- dimnames(data1)[[1]]
            }
            else {
                same.row.name <- FALSE
                any.id.row.name <- FALSE
                if (any(dimnames(data1)[[1]] \%in\% dimnames(data2)[[1]])) {
                  any.id.row.name <- TRUE
                  same.row.names.pos1 <- which(dimnames(data1)[[1]] \%in\% 
                    dimnames(data2)[[1]])
                  same.row.names.match1 <- match(dimnames(data1)[[1]], 
                    dimnames(data2)[[1]])
                }
                if (any(dimnames(data2)[[1]] \%in\% dimnames(data1)[[1]])) {
                  any.id.row.name <- TRUE
                  same.row.names.pos2 <- which(dimnames(data2)[[1]] \%in\% 
                    dimnames(data1)[[1]])
                  same.row.names.match2 <- match(dimnames(data2)[[1]], 
                    dimnames(data1)[[1]])
                }
                if (any.id.row.name == TRUE) {
                  common.row.names <- unique(c(dimnames(data1)[[1]][same.row.names.pos1], 
                    dimnames(data2)[[1]][same.row.names.pos2]))
                }
            }
            if (is.null(dimnames(data1)[[2]]) & is.null(dimnames(data2)[[2]])) {
                same.col.name <- NULL
            }
            else if ((is.null(dimnames(data1)[[2]]) & !is.null(dimnames(data2)[[2]])) | 
                (!is.null(dimnames(data1)[[2]]) & is.null(dimnames(data2)[[2]]))) {
                same.col.name <- FALSE
                any.id.col.name <- FALSE
            }
            else if (identical(dimnames(data1)[[2]], dimnames(data2)[[2]])) {
                same.col.name <- TRUE
                col.name <- dimnames(data1)[[2]]
                any.id.col.name <- TRUE
                same.col.names.pos1 <- 1:ncol(data1)
                same.col.names.pos2 <- 1:ncol(data1)
                same.col.names.match1 <- 1:ncol(data1)
                same.col.names.match2 <- 1:ncol(data1)
                common.col.names <- dimnames(data1)[[2]]
            }
            else {
                same.col.name <- FALSE
                any.id.col.name <- FALSE
                if (any(dimnames(data1)[[2]] \%in\% dimnames(data2)[[2]])) {
                  any.id.col.name <- TRUE
                  same.col.names.pos1 <- which(dimnames(data1)[[2]] \%in\% 
                    dimnames(data2)[[2]])
                  same.col.names.match1 <- match(dimnames(data1)[[2]], 
                    dimnames(data2)[[2]])
                }
                if (any(dimnames(data2)[[2]] \%in\% dimnames(data1)[[2]])) {
                  any.id.col.name <- TRUE
                  same.col.names.pos2 <- which(dimnames(data2)[[2]] \%in\% 
                    dimnames(data1)[[2]])
                  same.col.names.match2 <- match(dimnames(data2)[[2]], 
                    dimnames(data1)[[2]])
                }
                if (any.id.col.name == TRUE) {
                  common.col.names <- unique(c(dimnames(data1)[[2]][same.col.names.pos1], 
                    dimnames(data2)[[2]][same.col.names.pos2]))
                }
            }
        }
        row.names(data1) <- paste0("A", 1:nrow(data1))
        row.names(data2) <- paste0("A", 1:nrow(data2))
        colnames(data1) <- paste0("A", 1:ncol(data1))
        colnames(data2) <- paste0("A", 1:ncol(data2))
        if (same.col.nb == TRUE) {
            if (as.double(nrow(data1)) * as.double(nrow(data2)) <= 
                1e+06) {
                tempo1 <- c(as.data.frame(t(data1), stringsAsFactors = FALSE))
                tempo2 <- c(as.data.frame(t(data2), stringsAsFactors = FALSE))
                same.row.pos1 <- which(tempo1 \%in\% tempo2)
                same.row.pos2 <- which(tempo2 \%in\% tempo1)
                if ((length(same.row.pos1) == 0L & length(same.row.pos2) == 
                  0L) | all(is.na(same.row.pos1)) | all(is.na(same.row.pos2))) {
                  any.id.row <- FALSE
                  same.row.pos1 <- NULL
                  same.row.pos2 <- NULL
                }
                else {
                  any.id.row <- TRUE
                  same.row.pos1 <- same.row.pos1[!is.na(same.row.pos1)]
                  same.row.pos2 <- same.row.pos2[!is.na(same.row.pos2)]
                  same.row.match1 <- match(tempo1, tempo2)
                  same.row.match2 <- match(tempo2, tempo1)
                }
            }
            else {
                same.row.pos1 <- "TOO BIG FOR EVALUATION"
                same.row.pos2 <- "TOO BIG FOR EVALUATION"
                same.row.match1 <- "TOO BIG FOR EVALUATION"
                same.row.match2 <- "TOO BIG FOR EVALUATION"
            }
        }
        else {
            any.id.row <- FALSE
        }
        if (same.row.nb == TRUE) {
            if (as.double(ncol(data1)) * as.double(ncol(data2)) <= 
                1e+06) {
                tempo1 <- c(as.data.frame(data1, stringsAsFactors = FALSE))
                tempo2 <- c(as.data.frame(data2, stringsAsFactors = FALSE))
                same.col.pos1 <- which(tempo1 \%in\% tempo2)
                same.col.pos2 <- which(tempo2 \%in\% tempo1)
                if ((length(same.col.pos1) == 0L & length(same.col.pos2) == 
                  0L) | all(is.na(same.col.pos1)) | all(is.na(same.col.pos2))) {
                  any.id.col <- FALSE
                  same.col.pos1 <- NULL
                  same.col.pos2 <- NULL
                }
                else {
                  any.id.col <- TRUE
                  same.col.pos1 <- same.col.pos1[!is.na(same.col.pos1)]
                  same.col.pos2 <- same.col.pos2[!is.na(same.col.pos2)]
                  same.col.match1 <- match(tempo1, tempo2)
                  same.col.match2 <- match(tempo2, tempo1)
                }
            }
            else {
                same.col.pos1 <- "TOO BIG FOR EVALUATION"
                same.col.pos2 <- "TOO BIG FOR EVALUATION"
                same.col.match1 <- "TOO BIG FOR EVALUATION"
                same.col.match2 <- "TOO BIG FOR EVALUATION"
            }
        }
        else {
            any.id.col <- FALSE
        }
        if (same.dim == TRUE) {
            if (all(data1 == data2, na.rm = TRUE)) {
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
    output <- list(same.class = same.class, class = class, same.mode = same.mode, 
        mode = mode, same.type = same.type, type = type, same.dim = same.dim, 
        dim = dim, same.row.nb = same.row.nb, row.nb = row.nb, 
        same.col.nb = same.col.nb, col.nb = col.nb, same.row.name = same.row.name, 
        row.name = row.name, any.id.row.name = any.id.row.name, 
        same.row.names.pos1 = same.row.names.pos1, same.row.names.pos2 = same.row.names.pos2, 
        same.row.names.match1 = same.row.names.match1, same.row.names.match2 = same.row.names.match2, 
        common.row.names = common.row.names, same.col.name = same.col.name, 
        col.name = col.name, any.id.col.name = any.id.col.name, 
        same.col.names.pos1 = same.col.names.pos1, same.col.names.pos2 = same.col.names.pos2, 
        same.col.names.match1 = same.col.names.match1, same.col.names.match2 = same.col.names.match2, 
        common.col.names = common.col.names, any.id.row = any.id.row, 
        same.row.pos1 = same.row.pos1, same.row.pos2 = same.row.pos2, 
        same.row.match1 = same.row.match1, same.row.match2 = same.row.match2, 
        any.id.col = any.id.col, same.col.pos1 = same.col.pos1, 
        same.col.pos2 = same.col.pos2, same.col.match1 = same.col.match1, 
        same.col.match2 = same.col.match2, identical.content = identical.content, 
        identical = identical.object)
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
