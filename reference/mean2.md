# mean2

Calculate the mean of numeric and logical value.

## Usage

``` r
mean2(
  x,
  trim = 0,
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

- trim:

  Single numeric value between 0 and 0.5 indicating the fraction (0 to
  0.5) of observations to be trimmed from each end of `x` before the
  mean is computed (as in
  [`base::mean()`](https://rdrr.io/r/base/mean.html)). A value of 0.5
  returns the median (as in
  [`base::mean()`](https://rdrr.io/r/base/mean.html)). Warning: a factor
  value is rejected with an error, even if factors have mode "numeric".

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

The mean of the given arguments. Returns NA if the input contains NA and
the argument na_rm = FALSE, else returns a value.

## Details

Same as [`mean`](https://rdrr.io/r/base/mean.html) (which is called
internally, using `base::mean(x = x, trim = trim, na.rm = na_rm)`), with
the following differences:

\- `x` must be a numeric or logical vector, matrix or table. Character
values, factors (for which
[`base::mean()`](https://rdrr.io/r/base/mean.html) returns NA with a
warning), NULL, empty objects (like `numeric(0)`, for which
[`base::mean()`](https://rdrr.io/r/base/mean.html) returns `NaN`) and
objects made of NA only are rejected with an explicit error message.

\- `trim` must be a single numeric value between 0 and 0.5
([`base::mean()`](https://rdrr.io/r/base/mean.html) silently treats
negative values as 0 and values above 0.5 as 0.5, i.e., returns the
median).

\- The `na.rm` argument is named `na_rm`, and no `...` argument is
available.

\- The additional `finite` argument removes the `Inf` and `-Inf` values
before computation.

\- The "safer" checkings of the arguments and environment (see the
`safer_check`, `lib_path` and `error_text` arguments).

## See also

[`mean`](https://rdrr.io/r/base/mean.html)

## Author

Gael Millot \<gael.millot@pasteur.fr\>

Yushi Han \<yushi.han2000@gmail.com\>

Haiding Wang \<wanghaiding442@gmail.com\>

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link 
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferTool/articles/mean2.html
vec <- c(1:3) ; mean2(x = vec)
#> [1] 2

vec <- c(1,3,5,TRUE) ; mean2(x = vec)
#> [1] 2.5

vec <- c(1,2,3,4,100) ; mean2(x = vec, trim = 0.2)
#> [1] 3

# This example returns an error because of the character in the vector
# vec <- c(1,3,5,TRUE,"apple") ; mean2(x = vec)
```
