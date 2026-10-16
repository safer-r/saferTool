# max2

Find and return the maximum value of a set of data.

## Usage

``` r
max2(
  x,
  na_rm = FALSE,
  finite = FALSE,
  safer_check = TRUE,
  lib_path = NULL,
  error_text = ""
)
```

## Arguments

- x:

  Numeric or logical vector or matrix or numeric table where find the
  initial values to use.

- na_rm:

  Single logical value. Should missing values (NA and NaN) be removed ?

- finite:

  Single logical value. Should infinite values (Inf and -Inf) be removed
  ? Warning: this argument does not remove NA and NaN. Please use the
  na_rm argument.

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

The maximum value of the given arguments.

## See also

[`max`](https://rdrr.io/r/base/Extremes.html)

## Author

Gael Millot \<gael.millot@pasteur.fr\>

Yushi Han \<yushi.han2000@gmail.com\>

Haiding Wang \<wanghaiding442@gmail.com\>

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link 
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferTool/articles/max2.html
vec <- c(1:3) ; max2(x = vec)
#> [1] 3

vec <- c(1,3,5,TRUE, FALSE, NA) ; max2(x = vec, na_rm = TRUE)
#> [1] 5

# This example returns an error because of the character in the vector
# vec <- c(1,3,5,TRUE,"apple") ; max2(x = vec)

# This example returns an error because of the argument na_rm,
# when the function max2() of the package base doesn't return an error
# vec <- c(1,3,5,TRUE) ; max2(x = vec, na.rm = TRUE)
```
