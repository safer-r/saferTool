# name_change

This function checks if a vector of character strings, like column names
of a data frame, has elements present in another vector (vector of
reserved words or column names of another data frame before merging).

## Usage

``` r
name_change(
  data1,
  data2,
  added_string = "_modif",
  duplicate = TRUE,
  lib_path = NULL,
  safer_check = TRUE,
  error_text = ""
)
```

## Arguments

- data1:

  Vector of character strings to check and modify.

- data2:

  Reference vector of character strings (duplicated elements not
  authorized).

- added_string:

  Single character string added at the end of the modified string in
  elements of data1 if present in data2.

- duplicate:

  Single logical value. Return the elements of data1 still with
  duplicated elements? Defaut to TRUE, i.e., duplicated elements remain
  duplicated.

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

- safer_check:

  Single logical value. Perform some "safer" checks? If `TRUE`,
  checkings are performed before main code running (see the [safer-r
  project](https://github.com/safer-r)): 1) correct `lib_path` argument
  value 2) required functions and related packages effectively present
  in local R libraries and 3) R classical operators (like `"<-"`) not
  overwritten by another package because of the R scope. Must be set to
  `FALSE` if this function is used inside another "safer" function to
  avoid pointless multiple checkings.

- error_text:

  Single character string used to add information in error messages
  returned by the function, notably if the function is inside other
  functions, which is practical for debugging. Example:
  `error_text = " INSIDE <PACKAGE_1>::<FUNCTION_1> INSIDE <PACKAGE_2>::<FUNCTION_2>."`.
  If `NULL`, converted into `""`. Of note, in `arg_check()`,
  `error_text` is also used at the end of the string returned when no
  problem is detected.

## Value

A list containing :

\- \$data: the modified data1 (in the same order as in the initial
data1).

\- \$posi: the modified positions in data1. NULL if no modification.

\- \$ini: the initial elements before modification. NULL if no
modification.

\- \$post: the modified elements in the same order as in ini. NULL if no
modification.

## Details

\- If elements are duplicated in data1 and match an element of data2,
then the value of added_string is added at the end of the matched
element of data1. - If the same occurs with the argument duplicate =
FALSE, then value of added_string is added at the end of the matched
element of data1, together with an incremented number so that elements
in data1 are not anymore duplicated. - An error message is returned if
any of the data1 elements end with the string in added_string.

## Author

Gael Millot \<gael.millot@pasteur.fr\>

Yushi Han \<yushi.han2000@gmail.com\>

Haiding Wang \<wanghaiding442@gmail.com\>

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link 
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferTool/articles/name_change.html
obs1 <- c("A", "B", "C", "A", "D") ; 
obs2 <- c("A", "C") ; 
name_change(obs1, obs2)
#> $data
#> [1] "A_modif" "B"       "C_modif" "A_modif" "D"      
#> 
#> $posi
#> [1] 1 3 4
#> 
#> $ini
#> [1] "A" "C" "A"
#> 
#> $post
#> [1] "A_modif" "C_modif" "A_modif"
#> 
```
