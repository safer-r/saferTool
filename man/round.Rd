\name{round}
\alias{round}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
round(data, dec.nb = 2, after.lead.zero = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{dec.nb}{
%%     ~~Describe \code{dec.nb} here~~
}
  \item{after.lead.zero}{
%%     ~~Describe \code{after.lead.zero} here~~
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
function (data, dec.nb = 2, after.lead.zero = TRUE) 
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
    arg.check <- NULL
    text.check <- NULL
    checked.arg.names <- NULL
    ee <- expression(arg.check <- c(arg.check, tempo$problem), 
        text.check <- c(text.check, tempo$text), checked.arg.names <- c(checked.arg.names, 
            tempo$object.name))
    tempo <- cuteDev::arg_check(data = data, class = "vector", 
        na.contain = TRUE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = dec.nb, class = "vector", 
        typeof = "integer", length = 1, double.as.integer.allowed = TRUE, 
        neg.values = FALSE, fun.name = function.name)
    eval(ee)
    tempo <- cuteDev::arg_check(data = after.lead.zero, class = "logical", 
        length = 1, fun.name = function.name)
    eval(ee)
    if (!is.null(arg.check)) {
        if (any(arg.check, na.rm = TRUE) == TRUE) {
            stop(paste0("\n\n================\n\n", paste(text.check[arg.check], 
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
    tempo.arg <- c("data", "dec.nb", "after.lead.zero")
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
    if (!(all(typeof(data) == "character") | all(typeof(data) == 
        "double") | all(typeof(data) == "integer"))) {
        tempo.cat <- paste0("ERROR IN ", function.name, ": data ARGUMENT MUST BE A VECTOR OF NUMBERS (IN NUMERIC OR CHARACTER MODE)")
        stop(paste0("\n\n================\n\n", tempo.cat, "\n\n================\n\n"), 
            call. = FALSE)
    }
    tempo <- grepl(x = data, pattern = "\\.")
    ini.mode <- base::mode(data)
    data <- as.character(data)
    for (i in 1:length(data)) {
        if (tempo[i] == TRUE) {
            if (after.lead.zero == TRUE) {
                zero.pos <- unlist(gregexpr(text = data[i], pattern = 0))
            }
            else {
                zero.pos <- -1
            }
            dot.pos <- unlist(gregexpr(text = data[i], pattern = "\\."))
            digit.pos <- unlist(gregexpr(text = data[i], pattern = "[[:digit:]]"))
            dec.pos <- digit.pos[digit.pos > dot.pos]
            count <- 0
            while ((dot.pos + count + 1) \%in\% zero.pos & (dot.pos + 
                count + 1) <= max(dec.pos) & (count + dec.nb) < 
                length(dec.pos)) {
                count <- count + 1
            }
            data[i] <- formatC(as.numeric(data[i]), digits = (count + 
                dec.nb), format = "f")
        }
    }
    if (ini.mode != "character") {
        data <- as.numeric(data)
    }
    return(data)
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
