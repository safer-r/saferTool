% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comp_1d.R
\name{comp_1d}
\alias{comp_1d}
\title{comp_1d}
\usage{
comp_1d(data1, data2, safer_check = TRUE)
}
\arguments{
\item{data1}{Vector or factor or 1D table, or 1D matrix or 1D array.}

\item{data2}{Vector or factor or 1D table, or 1D matrix or 1D array.}

\item{safer_check}{Single logical value. Perform some "safer" checks (see https://github.com/safer-r)? If TRUE, checkings are performed before main code running: 1) R classical operators (like "<-") not overwritten by another package because of the R scope and 2) required functions and related packages effectively present in local R lybraries. Must be set to FALSE if this fonction is used inside another "safer" function to avoid pointless multiple checkings.}
}
\value{
A list containing:

- $same.class: logical. Are class identical?

- $class: class of the 2 datasets (NULL otherwise).

- $same.length: logical. Are number of elements identical?

- $length: number of elements in the 2 datasets (NULL otherwise).

- $same.levels: logical. Are levels identical? NULL if data1 and data2 are not factors.

- $levels: levels of the 2 datasets if identical (NULL otherwise or NULL if data1 and data2 are not factors).

- $any.id.levels: logical. Is there any identical levels? (NULL if data1 and data2 are not factors).

- $same.levels.pos1: positions, in data1, of the levels identical in data2 (NULL otherwise or NULL if data1 and data2 are not factors).

- $same.levels.pos2: positions, in data2, of the levels identical in data1 (NULL otherwise or NULL if data1 and data2 are not factors).

- $same.levels.match1: positions, in data2, of the levels that match the levels in data1, as given by match(data1, data2) (NULL otherwise or NULL if data1 and data2 are not factors).

- $same.levels.match2: positions, in data1, of the levels that match the levels in data2, as given by match(data1, data2) (NULL otherwise or NULL if data1 and data2 are not factors).

- $common.levels: common levels between data1 and data2 (can be a subset of $levels or not). NULL if no common levels or if data1 and data2 are not factors.

- $same.names: logical. Are element names identical? NULL if data1 and data2 have no names.

- $name: name of elements of the 2 datasets if identical (NULL otherwise).

- $any.id.names: logical. Is there any element names identical ?

- $same.names.pos1: positions, in data1, of the element names identical in data2. NULL if no identical names.

- $same.names.pos2: positions, in data2, of the element names identical in data1. NULL if no identical names.

- $same.names.match1: positions, in data2, of the names that match the names in data1, as given by match(data1, data2) (NULL otherwise).

- $same.names.match2: positions, in data1, of the names that match the names in data2, as given by match(data1, data2) (NULL otherwise).

- $common.names: common element names between data1 and data2 (can be a subset of $name or not). NULL if no common element names.

- $any.id.elements: logical. is there any identical elements ?

- $same.elements.pos1: positions, in data1, of the elements identical in data2. NULL if no identical elements.

- $same.elements.pos2: positions, in data2, of the elements identical in data1. NULL if no identical elements.

- $same.elements.match1: positions, in data2, of the elements that match the elements in data1, as given by match(data1, data2) (NULL otherwise).

- $same.elements.match2: positions, in data1, of the elements that match the elements in data2, as given by match(data1, data2) (NULL otherwise).

- $common.elements: common elements between data1 and data2. NULL if no common elements.

- $same.order: logical. Are all elements in the same order? TRUE or FALSE if elements of data1 and data2 are identical but not necessary in the same order. NULL otherwise (different length for instance).

- $order1: order of all elements of data1. NULL if $same.order is NULL.

- $order2: order of all elements of data2. NULL if $same.order is NULL.

- $identical.object: logical. Are objects identical (kind of object, element names, content, including content order)?

- $identical.content: logical. Are content objects identical (identical elements, including order, excluding kind of object and element names)?
}
\description{
Compare two 1D datasets (vector or factor or 1D table, or 1D matrix or 1D array) of the same class or not. Check and report in a list if the 2 datasets have:

- same class

- common elements

- common element names (except factors)

- common levels (factors only)
}
\examples{
obs1 = 1:5 ; obs2 = 1:5 ; names(obs1) <- LETTERS[1:5] ; names(obs2) <- LETTERS[1:5] ; comp_1d(obs1, obs2)
obs1 = factor(LETTERS[1:5]) ; obs2 = factor(LETTERS[1:5]) ; comp_1d(obs1, obs2)
obs1 = factor(c(LETTERS[1:4], "E")) ; obs2 = factor(c(LETTERS[1:4], "F")) ; comp_1d(obs1, obs2)
obs1 = 1:5 ; obs2 = 1.1:6.1 ; comp_1d(obs1, obs2)
}
\author{
Gael Millot <gael.millot@pasteur.fr>

Yushi Han <yushi.han2000@gmail.com>

Haiding Wang <wanghaiding442@gmail.com>
}
