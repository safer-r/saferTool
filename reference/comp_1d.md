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

\- \$same_class: logical. Are class identical?

\- \$class: class of the 2 datasets (NULL otherwise).

\- \$same_length: logical. Are number of elements identical?

\- \$length: number of elements in the 2 datasets (NULL otherwise).

\- \$same_levels: logical. Are levels identical? NULL if data1 and data2
are not factors.

\- \$levels: levels of the 2 datasets if identical (NULL otherwise or
NULL if data1 and data2 are not factors).

\- \$any_id_levels: logical. Is there any identical levels? (NULL if
data1 and data2 are not factors).

\- \$same_levels_pos1: positions, in data1, of the levels identical in
data2 (NULL otherwise or NULL if data1 and data2 are not factors).

\- \$same_levels_pos2: positions, in data2, of the levels identical in
data1 (NULL otherwise or NULL if data1 and data2 are not factors).

\- \$same_levels_match1: positions, in data2, of the levels that match
the levels in data1, as given by match(data1, data2) (NULL otherwise or
NULL if data1 and data2 are not factors).

\- \$same_levels_match2: positions, in data1, of the levels that match
the levels in data2, as given by match(data1, data2) (NULL otherwise or
NULL if data1 and data2 are not factors).

\- \$common_levels: common levels between data1 and data2 (can be a
subset of \$levels or not). NULL if no common levels or if data1 and
data2 are not factors.

\- \$same_names: logical. Are element names identical? NULL if data1 and
data2 have no names.

\- \$name: name of elements of the 2 datasets if identical (NULL
otherwise).

\- \$any_id_names: logical. Is there any element names identical ?

\- \$same_names_pos1: positions, in data1, of the element names
identical in data2. NULL if no identical names.

\- \$same_names_pos2: positions, in data2, of the element names
identical in data1. NULL if no identical names.

\- \$same_names_match1: positions, in data2, of the names that match the
names in data1, as given by match(data1, data2) (NULL otherwise).

\- \$same_names_match2: positions, in data1, of the names that match the
names in data2, as given by match(data1, data2) (NULL otherwise).

\- \$common_names: common element names between data1 and data2 (can be
a subset of \$name or not). NULL if no common element names.

\- \$any_id_elements: logical. is there any identical elements ?

\- \$same_elements_pos1: positions, in data1, of the elements identical
in data2. NULL if no identical elements.

\- \$same_elements_pos2: positions, in data2, of the elements identical
in data1. NULL if no identical elements.

\- \$same_elements_match1: positions, in data2, of the elements that
match the elements in data1, as given by match(data1, data2) (NULL
otherwise).

\- \$same_elements_match2: positions, in data1, of the elements that
match the elements in data2, as given by match(data1, data2) (NULL
otherwise).

\- \$common_elements: common elements between data1 and data2. NULL if
no common elements.

\- \$same_order: logical. Are all elements in the same order? TRUE or
FALSE if elements of data1 and data2 are identical but not necessary in
the same order. NULL otherwise (different length for instance).

\- \$order1: order of all elements of data1. NULL if \$same_order is
NULL.

\- \$order2: order of all elements of data2. NULL if \$same_order is
NULL.

\- \$identical_object: logical. Are objects identical (kind of object,
element names, content, including content order)?

\- \$identical_content: logical. Are content objects identical
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
#> $same_class
#> [1] TRUE
#> 
#> $class
#> [1] "integer"
#> 
#> $same_length
#> [1] TRUE
#> 
#> $length
#> [1] 5
#> 
#> $same_levels
#> NULL
#> 
#> $levels
#> NULL
#> 
#> $any_id_levels
#> [1] FALSE
#> 
#> $same_levels_pos1
#> NULL
#> 
#> $same_levels_pos2
#> NULL
#> 
#> $same_levels_match1
#> NULL
#> 
#> $same_levels_match2
#> NULL
#> 
#> $common_levels
#> NULL
#> 
#> $same_names
#> [1] TRUE
#> 
#> $name
#> [1] "A" "B" "C" "D" "E"
#> 
#> $any_id_names
#> [1] TRUE
#> 
#> $same_names_pos1
#> [1] 1 2 3 4 5
#> 
#> $same_names_pos2
#> [1] 1 2 3 4 5
#> 
#> $same_names_match1
#> [1] 1 2 3 4 5
#> 
#> $same_names_match2
#> [1] 1 2 3 4 5
#> 
#> $common_names
#> [1] "A" "B" "C" "D" "E"
#> 
#> $any_id_elements
#> [1] TRUE
#> 
#> $same_elements_pos1
#> [1] 1 2 3 4 5
#> 
#> $same_elements_pos2
#> [1] 1 2 3 4 5
#> 
#> $same_elements_match1
#> [1] 1 2 3 4 5
#> 
#> $same_elements_match2
#> [1] 1 2 3 4 5
#> 
#> $common_elements
#> A B C D E 
#> 1 2 3 4 5 
#> 
#> $same_order
#> [1] TRUE
#> 
#> $order1
#> [1] 1 2 3 4 5
#> 
#> $order2
#> [1] 1 2 3 4 5
#> 
#> $identical_object
#> [1] TRUE
#> 
#> $identical_content
#> [1] TRUE
#> 
obs1 = factor(LETTERS[1:5]) ; obs2 = factor(LETTERS[1:5]) ; comp_1d(obs1, obs2)
#> $same_class
#> [1] TRUE
#> 
#> $class
#> [1] "factor"
#> 
#> $same_length
#> [1] TRUE
#> 
#> $length
#> [1] 5
#> 
#> $same_levels
#> [1] TRUE
#> 
#> $levels
#> [1] "A" "B" "C" "D" "E"
#> 
#> $any_id_levels
#> [1] TRUE
#> 
#> $same_levels_pos1
#> [1] 1 2 3 4 5
#> 
#> $same_levels_pos2
#> [1] 1 2 3 4 5
#> 
#> $same_levels_match1
#> [1] 1 2 3 4 5
#> 
#> $same_levels_match2
#> [1] 1 2 3 4 5
#> 
#> $common_levels
#> [1] "A" "B" "C" "D" "E"
#> 
#> $same_names
#> NULL
#> 
#> $name
#> NULL
#> 
#> $any_id_names
#> [1] FALSE
#> 
#> $same_names_pos1
#> NULL
#> 
#> $same_names_pos2
#> NULL
#> 
#> $same_names_match1
#> NULL
#> 
#> $same_names_match2
#> NULL
#> 
#> $common_names
#> NULL
#> 
#> $any_id_elements
#> [1] TRUE
#> 
#> $same_elements_pos1
#> [1] 1 2 3 4 5
#> 
#> $same_elements_pos2
#> [1] 1 2 3 4 5
#> 
#> $same_elements_match1
#> [1] 1 2 3 4 5
#> 
#> $same_elements_match2
#> [1] 1 2 3 4 5
#> 
#> $common_elements
#> [1] A B C D E
#> Levels: A B C D E
#> 
#> $same_order
#> [1] TRUE
#> 
#> $order1
#> [1] 1 2 3 4 5
#> 
#> $order2
#> [1] 1 2 3 4 5
#> 
#> $identical_object
#> [1] TRUE
#> 
#> $identical_content
#> [1] TRUE
#> 
obs1 = factor(c(LETTERS[1:4], "E")) ; obs2 = factor(c(LETTERS[1:4], "F")) ; comp_1d(obs1, obs2)
#> $same_class
#> [1] TRUE
#> 
#> $class
#> [1] "factor"
#> 
#> $same_length
#> [1] TRUE
#> 
#> $length
#> [1] 5
#> 
#> $same_levels
#> [1] FALSE
#> 
#> $levels
#> NULL
#> 
#> $any_id_levels
#> [1] TRUE
#> 
#> $same_levels_pos1
#> [1] 1 2 3 4
#> 
#> $same_levels_pos2
#> [1] 1 2 3 4
#> 
#> $same_levels_match1
#> [1]  1  2  3  4 NA
#> 
#> $same_levels_match2
#> [1]  1  2  3  4 NA
#> 
#> $common_levels
#> [1] "A" "B" "C" "D"
#> 
#> $same_names
#> NULL
#> 
#> $name
#> NULL
#> 
#> $any_id_names
#> [1] FALSE
#> 
#> $same_names_pos1
#> NULL
#> 
#> $same_names_pos2
#> NULL
#> 
#> $same_names_match1
#> NULL
#> 
#> $same_names_match2
#> NULL
#> 
#> $common_names
#> NULL
#> 
#> $any_id_elements
#> [1] TRUE
#> 
#> $same_elements_pos1
#> [1] 1 2 3 4
#> 
#> $same_elements_pos2
#> [1] 1 2 3 4
#> 
#> $same_elements_match1
#> [1]  1  2  3  4 NA
#> 
#> $same_elements_match2
#> [1]  1  2  3  4 NA
#> 
#> $common_elements
#> [1] "A" "B" "C" "D"
#> 
#> $same_order
#> NULL
#> 
#> $order1
#> NULL
#> 
#> $order2
#> NULL
#> 
#> $identical_object
#> [1] FALSE
#> 
#> $identical_content
#> [1] FALSE
#> 
obs1 = 1:5 ; obs2 = 1.1:6.1 ; comp_1d(obs1, obs2)
#> $same_class
#> [1] FALSE
#> 
#> $class
#> NULL
#> 
#> $same_length
#> [1] FALSE
#> 
#> $length
#> NULL
#> 
#> $same_levels
#> NULL
#> 
#> $levels
#> NULL
#> 
#> $any_id_levels
#> [1] FALSE
#> 
#> $same_levels_pos1
#> NULL
#> 
#> $same_levels_pos2
#> NULL
#> 
#> $same_levels_match1
#> NULL
#> 
#> $same_levels_match2
#> NULL
#> 
#> $common_levels
#> NULL
#> 
#> $same_names
#> NULL
#> 
#> $name
#> NULL
#> 
#> $any_id_names
#> [1] FALSE
#> 
#> $same_names_pos1
#> NULL
#> 
#> $same_names_pos2
#> NULL
#> 
#> $same_names_match1
#> NULL
#> 
#> $same_names_match2
#> NULL
#> 
#> $common_names
#> NULL
#> 
#> $any_id_elements
#> [1] FALSE
#> 
#> $same_elements_pos1
#> NULL
#> 
#> $same_elements_pos2
#> NULL
#> 
#> $same_elements_match1
#> NULL
#> 
#> $same_elements_match2
#> NULL
#> 
#> $common_elements
#> NULL
#> 
#> $same_order
#> NULL
#> 
#> $order1
#> NULL
#> 
#> $order2
#> NULL
#> 
#> $identical_object
#> [1] FALSE
#> 
#> $identical_content
#> [1] FALSE
#> 
```
