# comp_2d

Compare two 2D datasets of the same class or not. Check and report in a
list if the 2 datasets have:

\- same class

\- same type

\- common row names

\- common column names

\- same row number

\- same column number

\- potential identical rows between the 2 datasets

\- potential identical columns between the 2 datasets

## Usage

``` r
comp_2d(data1, data2, safer_check = TRUE, lib_path = NULL, error_text = "")
```

## Arguments

- data1:

  Matrix, data frame or table.

- data2:

  Matrix, data frame or table.

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

A list containing:

\- \$same_class: logical. Are classes identical ?

\- \$class: identical class of the 2 datasets (NULL otherwise).

\- \$same_mode: logical. Are modes identical ?

\- \$mode: identical mode of the 2 datasets (NULL otherwise).

\- \$same_type: logical. Are types identical ?

\- \$type: identical type of the 2 datasets (NULL otherwise).

\- \$same_dim: logical. Are dimension identical ?

\- \$dim: dimension of the 2 datasets (NULL otherwise).

\- \$same_row_nb: logical. Are number of rows identical ?

\- \$row_nb: nb of rows of the 2 datasets if identical (NULL otherwise).

\- \$same_col_nb: logical. Are number of columns identical ?

\- \$col_nb: nb of columns of the 2 datasets if identical (NULL
otherwise).

\- \$same_row_name: logical. Are row names identical ? NULL if no row
names in the two 2D datasets.

\- \$row_name: name of rows of the 2 datasets if identical (NULL
otherwise).

\- \$any_id_row_name: logical. Is there any row names identical ? NULL
if no row names in the two 2D datasets.

\- \$same_row_names_pos1: positions, in data1, of the row names
identical in data2.

\- \$same_row_names_pos2: positions, in data2, of the row names
identical in data1.

\- \$same_row_names_match1: positions, in data2, of the row names that
match the row names in data1, as given by match(data1, data2) (NULL
otherwise).

\- \$same_row_names_match2: positions, in data1, of the row names that
match the row names in data2, as given by match(data1, data2) (NULL
otherwise).

\- \$common_row_names: common row names between data1 and data2 (can be
a subset of \$name or not). NULL if no common row names.

\- \$same_col_name: logical. Are column names identical ? NULL if no col
names in the two 2D datasets.

\- \$col_name: name of columns of the 2 datasets if identical (NULL
otherwise).

\- \$any_id_col_name: logical. Is there any column names identical ?
NULL if no col names in the two 2D datasets.

\- \$same_col_names_pos1: positions, in data1, of the column names
identical in data2.

\- \$same_col_names_pos2: positions, in data2, of the column names
identical in data1.

\- \$same_col_names_match1: positions, in data2, of the column names
that match the column names in data1, as given by match(data1, data2)
(NULL otherwise).

\- \$same_col_names_match2: positions, in data1, of the column names
that match the column names in data2, as given by match(data1, data2)
(NULL otherwise).

\- \$common_col_names: common column names between data1 and data2 (can
be a subset of \$name or not). NULL if no common column names.

\- \$any_id_row: logical. is there identical rows (not considering row
names)? NULL if nrow(data1) \* nrow(data2) \> 1e6. Warning: class, mode
and type are not considered (comparison of content is performed after
conversion of the elements into character.

\- \$same_row_pos1: positions, in data1, of the rows identical in data2
(not considering row names). Return "TOO BIG FOR EVALUATION" if
nrow(data1) \* nrow(data2) \> 1e6. Warning: class, mode and type are not
considered (comparison of content is performed after conversion of the
elements into character.

\- \$same_row_pos2: positions, in data2, of the rows identical in data1
(not considering row names). Return "TOO BIG FOR EVALUATION" if
nrow(data1) \* nrow(data2) \> 1e6. Warning: class, mode and type are not
considered (comparison of content is performed after conversion of the
elements into character.

\- \$same_row_match1: positions, in data2, of the rows that match the
rows in data1, as given by match(data1, data2) (NULL otherwise).
Warning: class, mode and type are not considered (comparison of content
is performed after conversion of the elements into character.

\- \$same_row_match2: positions, in data1, of the rows that match the
rows in data2, as given by match(data1, data2) (NULL otherwise).
Warning: class, mode and type are not considered (comparison of content
is performed after conversion of the elements into character.

\- \$any_id_col: logical. is there identical columns (not considering
column names)? NULL if ncol(data1) \* ncol(data2) \> 1e6. Warning:
class, mode and type are not considered (comparison of content is
performed after conversion of the elements into character.

\- \$same_col_pos1: position in data1 of the cols identical in data2
(not considering column names). Return "TOO BIG FOR EVALUATION" if
ncol(data1) \* ncol(data2) \> 1e6. Warning: class, mode and type are not
considered (comparison of content is performed after conversion of the
elements into character.

\- \$same_col_pos2: position in data2 of the cols identical in data1
(not considering column names). Return "TOO BIG FOR EVALUATION" if
ncol(data1) \* ncol(data2) \> 1e6. Warning: class, mode and type are not
considered (comparison of content is performed after conversion of the
elements into character.

\- \$same_col_match1: positions, in data2, of the columns that match the
columns in data1, as given by match(data1, data2) (NULL otherwise).
Warning: class, mode and type are not considered (comparison of content
is performed after conversion of the elements into character.

\- \$same_row_match2: positions, in data1, of the columns that match the
columns in data2, as given by match(data1, data2) (NULL otherwise).
Warning: class, mode and type are not considered (comparison of content
is performed after conversion of the elements into character.

\- \$identical_content: logical. Are contents identical ? Row and column
names are not considered. Warning: class, mode and type are not
considered (comparison of content is performed after conversion of the
elements into character).

\- \$identical: logical. Idem as \$identical_content but including row &
column names.

## Details

WARNINGS

The results in:

\$any_id_row,

\$same_row_pos1,

\$same_row_pos2,

\$same_row_match1,

\$same_row_match2,

\$any_id_col,

\$same_col_pos1,

\$same_col_pos2,

\$same_col_match1,

\$same_col_match2,

\$identical_content,

\$identical

Does not take into account the mode and type (integer, double,
character, etc.) of the matrix, data frame and table content. Indeed,
comparisons are performed after conversion of the content into
characters. This allows the 2 by 2 comparisons of data frame rows.
However, the same mode and same type information is provided with the
\$same_mode and \$same_type result, which is convenient when dealing
with matrices and tables. But the different modes and types between
column of a data frame is never considered. Thus, be careful when
concluding that columns of two different data frames are the same,
because the values can be identical but not the mode or type (integer in
the first data frame column, and double in the second data frame column,
for instance).

"TOO BIG FOR EVALUATION" returned in \$same_row_pos1, \$same_row_pos2,
\$same_row_match1 and \$same_row_match2 when nrow(data1) \* nrow(data2)
\> 1e6 and \$any_id_row is returned NULL.

"TOO BIG FOR EVALUATION" returned in \$same_col_pos1, \$same_col_pos2,
\$same_col_match1 and \$same_col_match2 when ncol(data1) \* ncol(data2)
\> 1e6 and \$any_id_col is returned NULL.

## Author

Gael Millot \<gael.millot@pasteur.fr\>

Yushi Han \<yushi.han2000@gmail.com\>

Haiding Wang \<wanghaiding442@gmail.com\>

## Examples

``` r
# Warning: these examples may not work well when using the "Run examples" link 
# because of a particular environment. Please, copy-paste in a local environment.
# See also https://safer-r.github.io/saferTool/articles/comp_2d.html
obs1 = matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])) ; 
obs2 = as.data.frame(matrix(1:10, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5])), stringsAsFactors = TRUE) ;
obs1 ; 
#>   A B C D  E
#> a 1 3 5 7  9
#> b 2 4 6 8 10
obs2 ; 
#>   A B C D  E
#> a 1 3 5 7  9
#> b 2 4 6 8 10
comp_2d(obs1, obs2)
#> $same_class
#> [1] FALSE
#> 
#> $class
#> NULL
#> 
#> $same_mode
#> [1] FALSE
#> 
#> $mode
#> NULL
#> 
#> $same_type
#> [1] FALSE
#> 
#> $type
#> NULL
#> 
#> $same_dim
#> [1] TRUE
#> 
#> $dim
#> [1] 2 5
#> 
#> $same_row_nb
#> [1] TRUE
#> 
#> $row_nb
#> [1] 2
#> 
#> $same_col_nb
#> [1] TRUE
#> 
#> $col_nb
#> [1] 5
#> 
#> $same_row_name
#> [1] TRUE
#> 
#> $row_name
#> [1] "a" "b"
#> 
#> $any_id_row_name
#> [1] TRUE
#> 
#> $same_row_names_pos1
#> [1] 1 2
#> 
#> $same_row_names_pos2
#> [1] 1 2
#> 
#> $same_row_names_match1
#> [1] 1 2
#> 
#> $same_row_names_match2
#> [1] 1 2
#> 
#> $common_row_names
#> [1] "a" "b"
#> 
#> $same_col_name
#> [1] TRUE
#> 
#> $col_name
#> [1] "A" "B" "C" "D" "E"
#> 
#> $any_id_col_name
#> [1] TRUE
#> 
#> $same_col_names_pos1
#> [1] 1 2 3 4 5
#> 
#> $same_col_names_pos2
#> [1] 1 2 3 4 5
#> 
#> $same_col_names_match1
#> [1] 1 2 3 4 5
#> 
#> $same_col_names_match2
#> [1] 1 2 3 4 5
#> 
#> $common_col_names
#> [1] "A" "B" "C" "D" "E"
#> 
#> $any_id_row
#> [1] FALSE
#> 
#> $same_row_pos1
#> NULL
#> 
#> $same_row_pos2
#> NULL
#> 
#> $same_row_match1
#> NULL
#> 
#> $same_row_match2
#> NULL
#> 
#> $any_id_col
#> [1] FALSE
#> 
#> $same_col_pos1
#> NULL
#> 
#> $same_col_pos2
#> NULL
#> 
#> $same_col_match1
#> NULL
#> 
#> $same_col_match2
#> NULL
#> 
#> $identical_content
#> [1] TRUE
#> 
#> $identical
#> [1] FALSE
#> 


# Matrices: same row content but not same row names

obs1 = t(matrix(1:10, byrow = TRUE, ncol = 5, dimnames = list(letters[1:2], LETTERS[1:5]))) ; 
obs2 = t(matrix(c(1:5, 101:105, 6:10), byrow = TRUE, ncol = 5, dimnames = list(c("a", "z", "b"), c(LETTERS[1:2], "k", LETTERS[5:4])))) ; 
obs1 ; 
#>   a  b
#> A 1  6
#> B 2  7
#> C 3  8
#> D 4  9
#> E 5 10
obs2 ; 
#>   a   z  b
#> A 1 101  6
#> B 2 102  7
#> k 3 103  8
#> E 4 104  9
#> D 5 105 10
comp_2d(obs1, obs2)
#> $same_class
#> [1] TRUE
#> 
#> $class
#> [1] "matrix" "array" 
#> 
#> $same_mode
#> [1] TRUE
#> 
#> $mode
#> [1] "numeric"
#> 
#> $same_type
#> [1] TRUE
#> 
#> $type
#> [1] "integer"
#> 
#> $same_dim
#> [1] FALSE
#> 
#> $dim
#> NULL
#> 
#> $same_row_nb
#> [1] TRUE
#> 
#> $row_nb
#> [1] 5
#> 
#> $same_col_nb
#> [1] FALSE
#> 
#> $col_nb
#> NULL
#> 
#> $same_row_name
#> [1] FALSE
#> 
#> $row_name
#> NULL
#> 
#> $any_id_row_name
#> [1] TRUE
#> 
#> $same_row_names_pos1
#> [1] 1 2 4 5
#> 
#> $same_row_names_pos2
#> [1] 1 2 4 5
#> 
#> $same_row_names_match1
#> [1]  1  2 NA  5  4
#> 
#> $same_row_names_match2
#> [1]  1  2 NA  5  4
#> 
#> $common_row_names
#> [1] "A" "B" "D" "E"
#> 
#> $same_col_name
#> [1] FALSE
#> 
#> $col_name
#> NULL
#> 
#> $any_id_col_name
#> [1] TRUE
#> 
#> $same_col_names_pos1
#> [1] 1 2
#> 
#> $same_col_names_pos2
#> [1] 1 3
#> 
#> $same_col_names_match1
#> [1] 1 3
#> 
#> $same_col_names_match2
#> [1]  1 NA  2
#> 
#> $common_col_names
#> [1] "a" "b"
#> 
#> $any_id_row
#> [1] FALSE
#> 
#> $same_row_pos1
#> NULL
#> 
#> $same_row_pos2
#> NULL
#> 
#> $same_row_match1
#> NULL
#> 
#> $same_row_match2
#> NULL
#> 
#> $any_id_col
#> [1] TRUE
#> 
#> $same_col_pos1
#> [1] 1 2
#> 
#> $same_col_pos2
#> [1] 1 3
#> 
#> $same_col_match1
#> [1] 1 3
#> 
#> $same_col_match2
#> [1]  1 NA  2
#> 
#> $identical_content
#> [1] FALSE
#> 
#> $identical
#> [1] FALSE
#> 
```
