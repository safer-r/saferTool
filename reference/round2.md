# round2

Round a vector of values, if decimal, with the desired number of decimal
digits after the decimal leading zeros.

## Usage

``` r
round2(
  data,
  dec_nb = 2,
  after_lead_zero = TRUE,
  safer_check = TRUE,
  lib_path = NULL,
  error_text = ""
)
```

## Arguments

- data:

  A vector of numbers (numeric or character mode).

- dec_nb:

  Single integer value. Number of required decimal digits.

- after_lead_zero:

  Single logical value. If FALSE, rounding is performed for all the
  decimal numbers, whatever the leading zeros (e.g., 0.123 -\> 0.12 and
  0.00128 -\> 0.00). If TRUE, dec_nb are taken after the leading zeros
  (e.g., 0.123 -\> 0.12 and 0.00128 -\> 0.0013).

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

The modified vector.

## Details

WARNINGS

Work well with numbers as character strings, but not always with
numerical numbers because of the floating point.

Numeric values are really truncated from a part of their decimal digits,
whatever options(digits) settings.

See ?.Machine or
https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r,
with the interexting formula: abs(x - round(x)) \>
.Machine\$double.eps^0.5.

## See also

[`round`](https://rdrr.io/r/base/Round.html)

## Author

Gael Millot \<gael.millot@pasteur.fr\>

Yushi Han \<yushi.han2000@gmail.com\>

Haiding Wang \<wanghaiding442@gmail.com\>

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link 
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferTool/articles/round2.html
ini.options <- options()$digits ; 
options(digits = 8) ; 
cat(round2(data = c(NA, 10, 100.001, 333.0001254, 12312.1235), dec_nb = 2, after_lead_zero = FALSE), "\n\n") ; 
#> NA 10 100 333 12312.12 
#> 
options(digits = ini.options)
```
