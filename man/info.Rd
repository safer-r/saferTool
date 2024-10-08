% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/info.R
\name{info}
\alias{info}
\title{info}
\usage{
info(data, n = NULL, warn.print = TRUE, safer_check = TRUE)
}
\arguments{
\item{data}{Object to analyse.}

\item{n}{Single positive integer value indicating the n first number of elements to display per compartment of the output list (i.e., head(..., n)). Write NULL to return all the elements. Does not apply for the $STRUCTURE compartment output.}

\item{warn.print}{Single logical value. Print potential warnings at the end of the execution? If FALSE the warning messages are added in the output list as an additional compartment (or NULL if no message).}

\item{safer_check}{Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.}
}
\value{
A list containing information, depending on the class and type of data. The backbone is generally:

- $NAME: name of the object.

- $CLASS: class of the object (class() value).

- $TYPE: type of the object (typeof() value).

- $LENGTH: length of the object (length() value).

- $NA.NB: number of NA and NaN (only for type "logical", "integer", "double", "complex", "character" or "list").

- $HEAD: head of the object (head() value).

- $TAIL: tail of the object (tail() value).

- $DIMENSION: dimension (only for object with dimensions).

- $SUMMARY: object summary (summary() value).

- $STRUCTURE: object structure (str() value).

- $WARNING: warning messages (only if the warn.print argument is FALSE).

If data is made of numerics, provide also:

- $INF.NB: number of Inf and -Inf.

- $RANGE: range after removing Inf and NA.

- $SUM: sum after removing Inf and NA.

- $MEAN: mean after removing Inf and NA.

If data is a 2D object, provide also:

- $ROW_NAMES: row names.

- $COL_NAMES: column names.

If data is a data frame, provide also:

- $COLUMN_TYPE: type of each column (typeof() value).

If data is a list, provide also:

- $COMPARTMENT_NAMES: names of the comprtments.

- $COMPARTMENT_TYPE: type of each compartment (typeof() value).
}
\description{
Provide a broad description of an object.
}
\examples{
info(data = 1:3)
}
\author{
Gael Millot <gael.millot@pasteur.fr>

Yushi Han <yushi.han2000@gmail.com>

Haiding Wang <wanghaiding442@gmail.com>
}
