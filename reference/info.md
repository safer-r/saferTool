# info

Provide a broad description of an object.

## Usage

``` r
info(
  data,
  n = NULL,
  warn_print = TRUE,
  safer_check = TRUE,
  lib_path = NULL,
  error_text = ""
)
```

## Arguments

- data:

  Object to analyse.

- n:

  Single positive integer value indicating the n first number of
  elements to display per compartment of the output list (i.e.,
  head(..., n)). Write NULL to return all the elements. Does not apply
  for the \$STRUCTURE compartment output.

- warn_print:

  Single logical value. Print potential warnings at the end of the
  execution? If FALSE the warning messages are added in the output list
  as an additional compartment (or NULL if no message).

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

A list containing information, depending on the class and type of data.
The backbone is generally:

\- \$NAME: name of the object.

\- \$CLASS: class of the object (class() value).

\- \$TYPE: type of the object (typeof() value).

\- \$LENGTH: length of the object (length() value).

\- \$NA.NB: number of NA and NaN (only for type "logical", "integer",
"double", "complex", "character" or "list").

\- \$HEAD: head of the object (head() value).

\- \$TAIL: tail of the object (tail() value).

\- \$DIMENSION: dimension (only for object with dimensions).

\- \$SUMMARY: object summary (summary() value).

\- \$STRUCTURE: object structure (str() value).

\- \$WARNING: warning messages (only if the warn_print argument is
FALSE).

If data is made of numerics, provide also:

\- \$INF.NB: number of Inf and -Inf.

\- \$RANGE: range after removing Inf and NA.

\- \$SUM: sum after removing Inf and NA.

\- \$MEAN: mean after removing Inf and NA.

If data is a 2D object, provide also:

\- \$ROW_NAMES: row names.

\- \$COL_NAMES: column names.

If data is a data frame, provide also:

\- \$COLUMN_TYPE: type of each column (typeof() value).

If data is a list, provide also:

\- \$COMPARTMENT_NAMES: names of the compartments.

\- \$COMPARTMENT_TYPE: type of each compartment (typeof() value).

## Author

Gael Millot \<gael.millot@pasteur.fr\>

Yushi Han \<yushi.han2000@gmail.com\>

Haiding Wang \<wanghaiding442@gmail.com\>

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link 
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferTool/articles/info.html
info(data = 1:3)
#> $NAME
#> [1] "1:3"
#> 
#> $CLASS
#> [1] "integer"
#> 
#> $TYPE
#> [1] "integer"
#> 
#> $LENGTH
#> [1] 3
#> 
#> $INF.NB
#> [1] 0
#> 
#> $RANGE
#> [1] 1 3
#> 
#> $SUM
#> [1] 6
#> 
#> $MEAN
#> [1] 2
#> 
#> $NA.NB
#> [1] 0
#> 
#> $HEAD
#> [1] 1 2 3
#> 
#> $TAIL
#> [1] 1 2 3
#> 
#> $SUMMARY
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>     1.0     1.5     2.0     2.0     2.5     3.0 
#> 
#> $STRUCTURE
#>                  
#>   int [1:3] 1 2 3
#> 
```
