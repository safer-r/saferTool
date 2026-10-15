# df_remod

If the data frame is made of n numeric columns, a new data frame is
created, with the 1st column gathering all the numeric values, and the
2nd column being the name of the columns of the initial data frame. If
row names were present in the initial data frame, then a new ini_rowname
column is added with the names of the rows.

If the data frame is made of one numeric column and one character or
factor column, a new data frame is created, with the new columns
corresponding to the split numeric values (according to the character
column). NA are added a the end of each column to have the same number
of rows. BEWARE: in such data frame, rows are not individuals. This
means that in the example below, values 10 and 20 are associated on the
same row but that means nothing in term of association.

## Usage

``` r
df_remod(
  df,
  quanti_col_name = "quanti",
  quali_col_name = "quali",
  safer_check = TRUE,
  lib_path = NULL,
  error_text = ""
)
```

## Arguments

- df:

  Data frame object to convert.

- quanti_col_name:

  Single character string. Optional name for the quanti column of the
  new data frame.

- quali_col_name:

  Single character string. Optional name for the quali column of the new
  data frame.

- safer_check:

  Single logical value. Perform the "safer" checks? If `TRUE`, checkings
  are performed before main code running (see the [safer-r
  project](https://github.com/safer-r)): 1) correct `lib_path` argument
  value 2) required functions and related packages effectively present
  in local R libraries and 3) R classical operators (like `"<-"`) not
  overwritten by another package because of the R scope. Warning: must
  be set to `FALSE` if this function is used inside another "safer"
  function to avoid pointless multiple checkings.

- lib_path:

  Vector of characters specifying the absolute pathways of the
  directories containing the required packages for the function, if not
  in the default directories. Useful when R packages are not installed
  in the default directories because of lack of admin rights. More
  precisely, `lib_path` is passed through the `new` argument of
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) so that the new
  library paths are `c(lib_path, .libPaths())`. Warning:
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
  If `NULL`, converted into `""`.

## Value

The modified data frame.

## Author

Gael Millot \<gael.millot@pasteur.fr\>

Yushi Han \<yushi.han2000@gmail.com\>

Haiding Wang \<wanghaiding442@gmail.com\>

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link 
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferTool/articles/df_remod.html
obs <- data.frame(col1 = (1:4)*10, col2 = 5:8, stringsAsFactors = TRUE) ; 
rownames(obs) <- paste0("row", 1:4) ; 
obs ;
#>      col1 col2
#> row1   10    5
#> row2   20    6
#> row3   30    7
#> row4   40    8
df_remod(obs, quanti_col_name = "quanti", quali_col_name = "quali")
#>   quanti quali ini_rowname
#> 1     10  col1        row1
#> 2     20  col1        row2
#> 3     30  col1        row3
#> 4     40  col1        row4
#> 5      5  col2        row1
#> 6      6  col2        row2
#> 7      7  col2        row3
#> 8      8  col2        row4
```
