# df_remod(): more examples

\

``` r

library(saferTool)
```

\

### Dataset

``` r

df1 <- data.frame(col1 = (1:4)*10, col2 = 5:8, stringsAsFactors = TRUE) ; 
rownames(df1) <- paste0("row", 1:4) # data frame of numeric columns with row names
df2 <- data.frame(a = c("x", "x", "y"), b = c(1, 2, 3), stringsAsFactors = TRUE) # data frame made of one character column and one numeric column
df3 <- data.frame(a = c("x", "x", "y"), b = c(1, 2), stringsAsFactors = TRUE) # unbalanced mixed data frame
```

    ## Error in `data.frame()`:
    ## ! arguments imply differing number of rows: 3, 2

``` r

df4 <- data.frame(a = 1:2, b = 3:4, c = 5:6, stringsAsFactors = TRUE) # data frame of 3 numeric columns
df5 <- data.frame(a = c("x", "y"), stringsAsFactors = TRUE) # data frame of a single character column
```

\

### Dataset info

``` r

df1 # data frame of numeric columns with row names
```

    ##      col1 col2
    ## row1   10    5
    ## row2   20    6
    ## row3   30    7
    ## row4   40    8

``` r

df2 # data frame made of one character column and one numeric column
```

    ##   a b
    ## 1 x 1
    ## 2 x 2
    ## 3 y 3

``` r

df3 # unbalanced mixed data frame
```

    ## Error:
    ## ! object 'df3' not found

``` r

df4 # data frame of 3 numeric columns
```

    ##   a b c
    ## 1 1 3 5
    ## 2 2 4 6

``` r

df5 # data frame of a single character column
```

    ##   a
    ## 1 x
    ## 2 y

\

### Simple examples

``` r

df_remod(df = df1) # transfo 1: all numeric columns, with row names -> quanti column + ini_rowname column
```

    ##   quanti quali ini_rowname
    ## 1     10  col1        row1
    ## 2     20  col1        row2
    ## 3     30  col1        row3
    ## 4     40  col1        row4
    ## 5      5  col2        row1
    ## 6      6  col2        row2
    ## 7      7  col2        row3
    ## 8      8  col2        row4

``` r

df_remod(df = df2) # transfo 2: one numeric column and one character column -> split numeric values
```

    ##   x  y
    ## 1 1  3
    ## 2 2 NA

\

### Argument quanti_col_name and quali_col_name

``` r

df_remod(df = df1, quanti_col_name = "numbers", quali_col_name = "source")
```

    ##   numbers source ini_rowname
    ## 1      10   col1        row1
    ## 2      20   col1        row2
    ## 3      30   col1        row3
    ## 4      40   col1        row4
    ## 5       5   col2        row1
    ## 6       6   col2        row2
    ## 7       7   col2        row3
    ## 8       8   col2        row4

``` r

df_remod(df = df2, quanti_col_name = "numbers", quali_col_name = "label")
```

    ## Error:
    ## ! 
    ## 
    ## ================
    ## 
    ## ERROR IN saferTool::df_remod().
    ## 
    ## IMPROPER quanti_col_name OR quali_col_name RESETTINGS. THESE ARGUMENTS ARE RESERVED FOR DATA FRAMES MADE OF n NUMERIC COLUMNS ONLY
    ## 
    ## ================

\

### Argument safer_check

``` r

df_remod(df = df1, safer_check = TRUE) # checkings performed before main code running
```

    ##   quanti quali ini_rowname
    ## 1     10  col1        row1
    ## 2     20  col1        row2
    ## 3     30  col1        row3
    ## 4     40  col1        row4
    ## 5      5  col2        row1
    ## 6      6  col2        row2
    ## 7      7  col2        row3
    ## 8      8  col2        row4

``` r

df_remod(df = df2, safer_check = FALSE) # checkings not performed (faster, but insecure)
```

    ##   x  y
    ## 1 1  3
    ## 2 2 NA

\

### All the arguments

``` r

df_remod(df = df1, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = "")
```

    ##   quanti quali ini_rowname
    ## 1     10  col1        row1
    ## 2     20  col1        row2
    ## 3     30  col1        row3
    ## 4     40  col1        row4
    ## 5      5  col2        row1
    ## 6      6  col2        row2
    ## 7      7  col2        row3
    ## 8      8  col2        row4

``` r

df_remod(df = df2, quanti_col_name = "quanti", quali_col_name = "quali", safer_check = TRUE, lib_path = NULL, error_text = "")
```

    ##   x  y
    ## 1 1  3
    ## 2 2 NA
