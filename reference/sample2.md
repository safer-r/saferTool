# sample2

Randomly sample elements from an object `x`, with a corrected behavior
when `x` is made of a single element (see the details below).

## Usage

``` r
sample2(
  x,
  size = NULL,
  replace = FALSE,
  prob = NULL,
  set_seed = NULL,
  safer_check = TRUE,
  lib_path = NULL,
  error_text = ""
)
```

## Arguments

- x:

  Object from which to sample elements. Any object that has a length
  (see [`length`](https://rdrr.io/r/base/length.html)) and that can be
  subsetted using the `[` operator (vector, factor, matrix, data frame,
  table, list, etc.).

- size:

  Either `NULL` (default), meaning that all the elements of `x` are
  sampled exactly once (like in
  [`sample`](https://rdrr.io/r/base/sample.html) when `x` is not made of
  a single element), or a single non negative integer (integers of type
  `double` are accepted when they have a 0 modulo, i.e., `2.0` is
  accepted like `2L`, but `2.5` is not). Sampling without replacement
  more elements than the length of `x` returns an error, like in
  [`sample`](https://rdrr.io/r/base/sample.html).

- replace:

  Single logical value. Should sampling be with replacement?

- prob:

  Either `NULL` (default) or a vector of numeric probability weights of
  the same length as `x`. No negative, infinite, NA or factor value
  allowed (unlike [`sample`](https://rdrr.io/r/base/sample.html), which
  accepts a factor).

- set_seed:

  Either `NULL` (default) or a single positive or negative integer
  (integers of type `double` are accepted when they have a 0 modulo)
  used in [`base::set.seed()`](https://rdrr.io/r/base/Random.html)
  before sampling. Warning: unlike
  [`base::set.seed()`](https://rdrr.io/r/base/Random.html) called in the
  global environment, the `.Random.seed` object of the user global
  environment is restored at the end of the function execution (the
  random generator state existing before the function call), so that the
  function does not modify the random generator state of the user. If
  `NULL`, the current random generator state of the user is directly
  used and modified, like in
  [`sample`](https://rdrr.io/r/base/sample.html).

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

The sampled elements, with the subsetting semantics of the `[` operator,
i.e., the same class as `x`, with the dimensions possibly dropped like
in [`sample`](https://rdrr.io/r/base/sample.html) (for instance,
sampling a matrix returns a vector).

## Details

The [`sample`](https://rdrr.io/r/base/sample.html) function behaves in a
confusing way when `x` is made of a single positive numeric value:
`base::sample(x = 4, size = 3, replace = TRUE)` draws 3 values between 1
and 4 (the single value 4 is used as the size of the population 1:4),
and `base::sample(x = 4, size = 3, replace = FALSE)` returns 3 distinct
values between 1 and 4 without error, although a single value cannot be
sampled 3 times without replacement. `sample2()` corrects these
behaviors: the single value is used as the population itself, made of
this single element. Thus, `sample2(x = 4, size = 3, replace = TRUE)`
returns `4 4 4`, and `sample2(x = 4, size = 3, replace = FALSE)` returns
an error because the single value 4 cannot be sampled 3 times without
replacement. Note that this also implies `sample2(x = 4)` (i.e., with
`size = NULL`) returns the single value 4, unlike `base::sample(x = 4)`
which draws 4 values between 1 and 4. For any other `x` value (length
superior to 1, or length 1 but not made of a positive numeric value),
`sample2()` behaves exactly like
[`sample`](https://rdrr.io/r/base/sample.html).

## See also

[`sample`](https://rdrr.io/r/base/sample.html)

## Author

Gael Millot \<gael.millot@pasteur.fr\>

Yushi Han \<yushi.han2000@gmail.com\>

Haiding Wang \<wanghaiding442@gmail.com\>

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferTool/articles/sample2.html
vec <- c(1:5) ; sample2(x = vec, size = 3, set_seed = 1)
#> [1] 1 4 3

sample2(x = 4, size = 3, replace = TRUE, set_seed = 1) # 4 4 4 (see the details below)
#> [1] 4 4 4

# This example returns an error: the single value 4 cannot be sampled 3 times without replacement
# sample2(x = 4, size = 3, replace = FALSE)

# This example returns an error because of the negative probability
# sample2(x = c(1:5), size = 3, replace = TRUE, prob = c(-1, 1, 1, 1, 1))
```
