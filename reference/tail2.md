# tail2

As utils::tail() but display the left or right tail of big 2D objects.

## Usage

``` r
tail2(
  data1,
  n = 6,
  side = "l",
  safer_check = TRUE,
  lib_path = NULL,
  error_text = ""
)
```

## Arguments

- data1:

  Any object but more dedicated for matrix, data frame or table. NULL
  and empty non-NULL objects (like an empty matrix) are rejected by the
  checkings.

- n:

  As in utils::tail() but for matrix, data frame or table, number of
  dimension to print (10 means 10 rows and columns). Ignored if data1 is
  not a matrix, data frame or table.

- side:

  Either "l" or "r" for the left or right side of the 2D object (only
  for matrix, data frame or table). Ignored if data1 is not a matrix,
  data frame or table.

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

The tail.

## Details

Warning: other arguments of utils::tail() not used.

For 2D objects, rows are always displayed from the bottom. Columns are
displayed from the left if `side = "l"` and from the right if
`side = "r"`.

## See also

[`tail`](https://rdrr.io/r/utils/head.html)

## Author

Gael Millot \<gael.millot@pasteur.fr\>

Yushi Han \<yushi.han2000@gmail.com\>

Haiding Wang \<wanghaiding442@gmail.com\>

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link 
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferTool/articles/tail2.html
obs1 = matrix(1:30, ncol = 5, dimnames = list(letters[1:6], LETTERS[1:5])) ; 
obs1 ; 
#>   A  B  C  D  E
#> a 1  7 13 19 25
#> b 2  8 14 20 26
#> c 3  9 15 21 27
#> d 4 10 16 22 28
#> e 5 11 17 23 29
#> f 6 12 18 24 30
tail2(obs1, 3, "r")
#>    C  D  E
#> d 16 22 28
#> e 17 23 29
#> f 18 24 30
tail2(letters, 3)
#> [1] "x" "y" "z"
```
