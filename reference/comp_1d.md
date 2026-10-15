# comp_1d

Compare two 1D datasets (vector or factor or 1D table, or 1D matrix or
1D array) of the same class or not. Check and report in a list if the 2
datasets have:

\- same class

\- common elements

\- common element names (except factors)

\- common levels (factors only)

## Usage

``` r
comp_1d(data1, data2, safer_check = TRUE, lib_path = NULL, error_text = "")
```

## Arguments

- data1:

  Vector or factor or 1D table, or 1D matrix or 1D array.

- data2:

  Vector or factor or 1D table, or 1D matrix or 1D array.

- safer_check:

  Single logical value. Perform some "safer" checks? If `TRUE`,
  checkings are performed before main code running (see the [safer-r
  project](https://github.com/safer-r)): 1) correct `lib_path` argument
  value 2) required functions and related packages effectively present
  in local R libraries and 3) R classical operators (like `"<-"`) not
  overwritten by another package because of the R scope. Must be set to
  `FALSE` if this function is used inside another "safer" function to
  avoid pointless multiple checkings.

- lib_path:

  Vector of characters specifying the absolute pathways of the
  directories containing the required packages for the function, if not
  in the default directories. Useful when R packages are not installed
  in the default directories because of lack of admin rights. More
  precisely, `lib_path` is passed through the `new` argument of
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) so that the new
  library paths are `unique(c(new, .Library.site, .Library))`. Warning:
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) is restored to
  the initial paths, after function execution. Ignored if `NULL`
  (default) or if the `safer_check` argument is `FALSE`: only the
  pathways specified by the current
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) are used for
  package calling.

- error_text:

  Single character string used to add information in error messages
  returned by the function, notably if the function is inside other
  functions, which is practical for debugging. Example:
  `error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."`.
  If `NULL`, converted into `""`. Of note, in `arg_check()`,
  `error_text` is also used at the end of the string returned when no
  problem is detected.

## Value

A list containing:

\- \$same.class: logical. Are class identical?

\- \$class: class of the 2 datasets (NULL otherwise).

\- \$same.length: logical. Are number of elements identical?

\- \$length: number of elements in the 2 datasets (NULL otherwise).

\- \$same.levels: logical. Are levels identical? NULL if data1 and data2
are not factors.

\- \$levels: levels of the 2 datasets if identical (NULL otherwise or
NULL if data1 and data2 are not factors).

\- \$any.id.levels: logical. Is there any identical levels? (NULL if
data1 and data2 are not factors).

\- \$same.levels.pos1: positions, in data1, of the levels identical in
data2 (NULL otherwise or NULL if data1 and data2 are not factors).

\- \$same.levels.pos2: positions, in data2, of the levels identical in
data1 (NULL otherwise or NULL if data1 and data2 are not factors).

\- \$same.levels.match1: positions, in data2, of the levels that match
the levels in data1, as given by match(data1, data2) (NULL otherwise or
NULL if data1 and data2 are not factors).

\- \$same.levels.match2: positions, in data1, of the levels that match
the levels in data2, as given by match(data1, data2) (NULL otherwise or
NULL if data1 and data2 are not factors).

\- \$common.levels: common levels between data1 and data2 (can be a
subset of \$levels or not). NULL if no common levels or if data1 and
data2 are not factors.

\- \$same.names: logical. Are element names identical? NULL if data1 and
data2 have no names.

\- \$name: name of elements of the 2 datasets if identical (NULL
otherwise).

\- \$any.id.names: logical. Is there any element names identical ?

\- \$same.names.pos1: positions, in data1, of the element names
identical in data2. NULL if no identical names.

\- \$same.names.pos2: positions, in data2, of the element names
identical in data1. NULL if no identical names.

\- \$same.names.match1: positions, in data2, of the names that match the
names in data1, as given by match(data1, data2) (NULL otherwise).

\- \$same.names.match2: positions, in data1, of the names that match the
names in data2, as given by match(data1, data2) (NULL otherwise).

\- \$common.names: common element names between data1 and data2 (can be
a subset of \$name or not). NULL if no common element names.

\- \$any.id.elements: logical. is there any identical elements ?

\- \$same.elements.pos1: positions, in data1, of the elements identical
in data2. NULL if no identical elements.

\- \$same.elements.pos2: positions, in data2, of the elements identical
in data1. NULL if no identical elements.

\- \$same.elements.match1: positions, in data2, of the elements that
match the elements in data1, as given by match(data1, data2) (NULL
otherwise).

\- \$same.elements.match2: positions, in data1, of the elements that
match the elements in data2, as given by match(data1, data2) (NULL
otherwise).

\- \$common.elements: common elements between data1 and data2. NULL if
no common elements.

\- \$same.order: logical. Are all elements in the same order? TRUE or
FALSE if elements of data1 and data2 are identical but not necessary in
the same order. NULL otherwise (different length for instance).

\- \$order1: order of all elements of data1. NULL if \$same.order is
NULL.

\- \$order2: order of all elements of data2. NULL if \$same.order is
NULL.

\- \$identical.object: logical. Are objects identical (kind of object,
element names, content, including content order)?

\- \$identical.content: logical. Are content objects identical
(identical elements, including order, excluding kind of object and
element names)?

## Author

Gael Millot \<gael.millot@pasteur.fr\>

Yushi Han \<yushi.han2000@gmail.com\>

Haiding Wang \<wanghaiding442@gmail.com\>

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link 
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferTool/articles/comp_1d.html
obs1 = 1:5 ; obs2 = 1:5 ; names(obs1) <- LETTERS[1:5] ; names(obs2) <- LETTERS[1:5] ; comp_1d(obs1, obs2)
#> $same.class
#> [1] TRUE
#> 
#> $class
#> [1] "integer"
#> 
#> $same.length
#> [1] TRUE
#> 
#> $length
#> [1] 5
#> 
#> $same.levels
#> NULL
#> 
#> $levels
#> NULL
#> 
#> $any.id.levels
#> [1] FALSE
#> 
#> $same.levels.pos1
#> NULL
#> 
#> $same.levels.pos2
#> NULL
#> 
#> $same.levels.match1
#> NULL
#> 
#> $same.levels.match2
#> NULL
#> 
#> $common.levels
#> NULL
#> 
#> $same.names
#> [1] TRUE
#> 
#> $name
#> [1] "A" "B" "C" "D" "E"
#> 
#> $any.id.names
#> [1] TRUE
#> 
#> $same.names.pos1
#> [1] 1 2 3 4 5
#> 
#> $same.names.pos2
#> [1] 1 2 3 4 5
#> 
#> $same.names.match1
#> [1] 1 2 3 4 5
#> 
#> $same.names.match2
#> [1] 1 2 3 4 5
#> 
#> $common.names
#> [1] "A" "B" "C" "D" "E"
#> 
#> $any.id.elements
#> [1] TRUE
#> 
#> $same.elements.pos1
#> [1] 1 2 3 4 5
#> 
#> $same.elements.pos2
#> [1] 1 2 3 4 5
#> 
#> $same.elements.match1
#> [1] 1 2 3 4 5
#> 
#> $same.elements.match2
#> [1] 1 2 3 4 5
#> 
#> $common.elements
#> A B C D E 
#> 1 2 3 4 5 
#> 
#> $same.order
#> [1] TRUE
#> 
#> $order1
#> [1] 1 2 3 4 5
#> 
#> $order2
#> [1] 1 2 3 4 5
#> 
#> $identical.object
#> [1] TRUE
#> 
#> $identical.content
#> [1] TRUE
#> 
obs1 = factor(LETTERS[1:5]) ; obs2 = factor(LETTERS[1:5]) ; comp_1d(obs1, obs2)
#> $same.class
#> [1] TRUE
#> 
#> $class
#> [1] "factor"
#> 
#> $same.length
#> [1] TRUE
#> 
#> $length
#> [1] 5
#> 
#> $same.levels
#> [1] TRUE
#> 
#> $levels
#> [1] "A" "B" "C" "D" "E"
#> 
#> $any.id.levels
#> [1] TRUE
#> 
#> $same.levels.pos1
#> [1] 1 2 3 4 5
#> 
#> $same.levels.pos2
#> [1] 1 2 3 4 5
#> 
#> $same.levels.match1
#> [1] 1 2 3 4 5
#> 
#> $same.levels.match2
#> [1] 1 2 3 4 5
#> 
#> $common.levels
#> [1] "A" "B" "C" "D" "E"
#> 
#> $same.names
#> NULL
#> 
#> $name
#> NULL
#> 
#> $any.id.names
#> [1] FALSE
#> 
#> $same.names.pos1
#> NULL
#> 
#> $same.names.pos2
#> NULL
#> 
#> $same.names.match1
#> NULL
#> 
#> $same.names.match2
#> NULL
#> 
#> $common.names
#> NULL
#> 
#> $any.id.elements
#> [1] TRUE
#> 
#> $same.elements.pos1
#> [1] 1 2 3 4 5
#> 
#> $same.elements.pos2
#> [1] 1 2 3 4 5
#> 
#> $same.elements.match1
#> [1] 1 2 3 4 5
#> 
#> $same.elements.match2
#> [1] 1 2 3 4 5
#> 
#> $common.elements
#> [1] A B C D E
#> Levels: A B C D E
#> 
#> $same.order
#> [1] TRUE
#> 
#> $order1
#> [1] 1 2 3 4 5
#> 
#> $order2
#> [1] 1 2 3 4 5
#> 
#> $identical.object
#> [1] TRUE
#> 
#> $identical.content
#> [1] TRUE
#> 
obs1 = factor(c(LETTERS[1:4], "E")) ; obs2 = factor(c(LETTERS[1:4], "F")) ; comp_1d(obs1, obs2)
#> $same.class
#> [1] TRUE
#> 
#> $class
#> [1] "factor"
#> 
#> $same.length
#> [1] TRUE
#> 
#> $length
#> [1] 5
#> 
#> $same.levels
#> [1] FALSE
#> 
#> $levels
#> NULL
#> 
#> $any.id.levels
#> [1] TRUE
#> 
#> $same.levels.pos1
#> [1] 1 2 3 4
#> 
#> $same.levels.pos2
#> [1] 1 2 3 4
#> 
#> $same.levels.match1
#> [1]  1  2  3  4 NA
#> 
#> $same.levels.match2
#> [1]  1  2  3  4 NA
#> 
#> $common.levels
#> [1] "A" "B" "C" "D"
#> 
#> $same.names
#> NULL
#> 
#> $name
#> NULL
#> 
#> $any.id.names
#> [1] FALSE
#> 
#> $same.names.pos1
#> NULL
#> 
#> $same.names.pos2
#> NULL
#> 
#> $same.names.match1
#> NULL
#> 
#> $same.names.match2
#> NULL
#> 
#> $common.names
#> NULL
#> 
#> $any.id.elements
#> [1] TRUE
#> 
#> $same.elements.pos1
#> [1] 1 2 3 4
#> 
#> $same.elements.pos2
#> [1] 1 2 3 4
#> 
#> $same.elements.match1
#> [1]  1  2  3  4 NA
#> 
#> $same.elements.match2
#> [1]  1  2  3  4 NA
#> 
#> $common.elements
#> [1] "A" "B" "C" "D"
#> 
#> $same.order
#> NULL
#> 
#> $order1
#> NULL
#> 
#> $order2
#> NULL
#> 
#> $identical.object
#> [1] FALSE
#> 
#> $identical.content
#> [1] FALSE
#> 
obs1 = 1:5 ; obs2 = 1.1:6.1 ; comp_1d(obs1, obs2)
#> $same.class
#> [1] FALSE
#> 
#> $class
#> NULL
#> 
#> $same.length
#> [1] FALSE
#> 
#> $length
#> NULL
#> 
#> $same.levels
#> NULL
#> 
#> $levels
#> NULL
#> 
#> $any.id.levels
#> [1] FALSE
#> 
#> $same.levels.pos1
#> NULL
#> 
#> $same.levels.pos2
#> NULL
#> 
#> $same.levels.match1
#> NULL
#> 
#> $same.levels.match2
#> NULL
#> 
#> $common.levels
#> NULL
#> 
#> $same.names
#> NULL
#> 
#> $name
#> NULL
#> 
#> $any.id.names
#> [1] FALSE
#> 
#> $same.names.pos1
#> NULL
#> 
#> $same.names.pos2
#> NULL
#> 
#> $same.names.match1
#> NULL
#> 
#> $same.names.match2
#> NULL
#> 
#> $common.names
#> NULL
#> 
#> $any.id.elements
#> [1] FALSE
#> 
#> $same.elements.pos1
#> NULL
#> 
#> $same.elements.pos2
#> NULL
#> 
#> $same.elements.match1
#> NULL
#> 
#> $same.elements.match2
#> NULL
#> 
#> $common.elements
#> NULL
#> 
#> $same.order
#> NULL
#> 
#> $order1
#> NULL
#> 
#> $order2
#> NULL
#> 
#> $identical.object
#> [1] FALSE
#> 
#> $identical.content
#> [1] FALSE
#> 
```
