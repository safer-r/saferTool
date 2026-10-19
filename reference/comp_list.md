# comp_list

Compare two lists. Check and report in a list if the 2 datasets have:

\- same length

\- common names

\- common compartments

## Usage

``` r
comp_list(data1, data2, safer_check = TRUE, lib_path = NULL, error_text = "")
```

## Arguments

- data1:

  List.

- data2:

  List.

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

\- \$same_length: logical. Are number of compartments identical?

\- \$length: number of compartments in the 2 datasets (NULL otherwise).

\- \$same_names: logical. Are compartment names identical? NULL if data1
and data2 have no names.

\- \$name: name of compartments of the 2 datasets if identical (NULL
otherwise).

\- \$any_id_names: logical. Is there any compartment names identical ?

\- \$same_names_pos1: positions, in data1, of the compartment names
identical in data2. NULL if no identical names.

\- \$same_names_pos2: positions, in data2, of the compartment names
identical in data1. NULL if no identical names.

\- \$same_names_match1: positions, in data2, of the names that match the
names in data1, as given by match(data1, data2) (NULL otherwise).

\- \$same_names_match2: positions, in data1, of the names that match the
names in data2, as given by match(data1, data2) (NULL otherwise).

\- \$common_names: common compartment names between data1 and data2 (can
be a subset of \$name or not). NULL if no common compartment names.

\- \$any_id_compartments: logical. is there any identical compartments ?

\- \$same_compartments_pos1: positions, in data1, of the compartments
identical in data2. NULL if no identical compartments.

\- \$same_compartments_pos2: positions, in data2, of the compartments
identical in data1. NULL if no identical compartments.

\- \$same_compartments_match1: positions, in data2, of the compartments
that match the compartments in data1, as given by match(data1, data2)
(NULL otherwise).

\- \$same_compartments_match2: positions, in data1, of the compartments
that match the compartments in data2, as given by match(data1, data2)
(NULL otherwise).

\- \$common_compartments: common compartments between data1 and data2.
NULL if no common compartments.

\- \$same_order: logical. Are all compartments in the same order? TRUE
if compartments of data1 and data2 are identical. FALSE if compartments
of data1 and data2 are made of the same compartments but not in the same
order. NULL otherwise (different length or no common compartment for
instance).

\- \$identical_object: logical. Are lists identical (compartment names,
content, including content order)?

\- \$identical_content: logical. Are content list identical (identical
compartments, including order, excluding kind of object and compartment
names)?

## Author

Gael Millot \<gael.millot@pasteur.fr\>

Yushi Han \<yushi.han2000@gmail.com\>

Haiding Wang \<wanghaiding442@gmail.com\>

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link 
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferTool/articles/comp_list.html
obs1 = list(1:5, LETTERS[1:2]) ; 
obs2 = list(a = 1:5, b = LETTERS[1:2]) ; 
obs3 = list(LETTERS[1:2], 1:5) ; 
comp_list(obs1, obs2) ;
#> $same_length
#> [1] TRUE
#> 
#> $length
#> [1] 2
#> 
#> $same_names
#> [1] FALSE
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
#> $any_id_compartments
#> [1] TRUE
#> 
#> $same_compartments_pos1
#> [1] 1 2
#> 
#> $same_compartments_pos2
#> [1] 1 2
#> 
#> $same_compartments_match1
#> [1] 1 2
#> 
#> $same_compartments_match2
#> [1] 1 2
#> 
#> $common_compartments
#> $common_compartments[[1]]
#> [1] 1 2 3 4 5
#> 
#> $common_compartments[[2]]
#> [1] "A" "B"
#> 
#> 
#> $same_order
#> [1] TRUE
#> 
#> $identical_object
#> [1] FALSE
#> 
#> $identical_content
#> [1] TRUE
#> 
comp_list(obs1, obs3)
#> $same_length
#> [1] TRUE
#> 
#> $length
#> [1] 2
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
#> $any_id_compartments
#> [1] TRUE
#> 
#> $same_compartments_pos1
#> [1] 1 2
#> 
#> $same_compartments_pos2
#> [1] 1 2
#> 
#> $same_compartments_match1
#> [1] 2 1
#> 
#> $same_compartments_match2
#> [1] 2 1
#> 
#> $common_compartments
#> $common_compartments[[1]]
#> [1] 1 2 3 4 5
#> 
#> $common_compartments[[2]]
#> [1] "A" "B"
#> 
#> 
#> $same_order
#> [1] FALSE
#> 
#> $identical_object
#> [1] FALSE
#> 
#> $identical_content
#> [1] FALSE
#> 
```
