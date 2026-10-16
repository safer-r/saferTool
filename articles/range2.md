# range2

\

``` r

library(saferTool)
```

\

### Datasets

``` r

vec1 <- -1:3 # vector of integers
vec2 <- c(1:3, NA) # vector of integers with NA
vec3 <- c(1:3, Inf) # vector of integers with Inf
vec4 <- 1:3 / 3 # vector of proportions
vec5 <- c(1, 2, NA, -Inf) # vector of integers but stored as "double", with NA and Inf
log1 <- c(TRUE, FALSE, NA) # logical vector with NA
log2 <- c(TRUE, FALSE, FALSE) # logical vector                    
mat1 <- matrix(vec1) # 1D matrix of integers
mat2 <- matrix(vec5) # 1D matrix of integers, NA and Inf
mat3 <- matrix(log1) # 1D matrix of logical values
mat4 <- matrix(log2) # 1D matrix of logical values and NA
fac1 <- factor(vec5) # 1D factor
tab1 <- table(fac1) # 1D table
```

\

### Datasets info

``` r
vec1 # vector of integers
>  [1] -1  0  1  2  3
vec2 # vector of integers with NA
>  [1]  1  2  3 NA
vec3 # vector of integers with Inf
>  [1]   1   2   3 Inf
vec4 # vector of proportions
>  [1] 0.3333333 0.6666667 1.0000000
vec5 # vector of integers but stored as "double", with NA and Inf
>  [1]    1    2   NA -Inf
log1 # logical vector with NA
>  [1]  TRUE FALSE    NA
log2 # logical vector                    
>  [1]  TRUE FALSE FALSE
mat1 # 1D matrix of integers
>       [,1]
>  [1,]   -1
>  [2,]    0
>  [3,]    1
>  [4,]    2
>  [5,]    3
mat2 # 1D matrix of integers, NA and Inf
>       [,1]
>  [1,]    1
>  [2,]    2
>  [3,]   NA
>  [4,] -Inf
mat3 # 1D matrix of logical values
>        [,1]
>  [1,]  TRUE
>  [2,] FALSE
>  [3,]    NA
mat4 # 1D matrix of logical values and NA
>        [,1]
>  [1,]  TRUE
>  [2,] FALSE
>  [3,] FALSE
fac1 # 1D factor
>  [1] 1    2    <NA> -Inf
>  Levels: -Inf 1 2
tab1 # 1D table
>  fac1
>  -Inf    1    2 
>     1    1    1
```

\

### Simple examples

``` r
range2(x = vec1) # vector of integers
>  [1] -1  3
range2(x = vec2) # vector of integers with NA
>  [1] NA NA
range2(x = vec3) # vector of integers with Inf
>  [1]   1 Inf
range2(x = vec4) # vector of proportions
>  [1] 0.3333333 1.0000000
range2(x = vec5) # vector of integers but stored as "double", with NA and Inf
>  [1] NA NA
range2(x = log1) # logical vector with NA
>  [1] NA NA
range2(x = log2) # logical vector
>  [1] 0 1
range2(x = mat1) # 1D matrix of integers
>  [1] -1  3
range2(x = mat2) # 1D matrix of integers, NA and Inf
>  [1] NA NA
range2(x = mat3) # 1D matrix of logical values
>  [1] NA NA
range2(x = mat4) # 1D matrix of logical values and NA
>  [1] 0 1
range2(x = tab1) # 1D table
>  [1] 1 1
```

\

### Argument na_rm

``` r
range2(
    x = vec1, 
    na_rm = TRUE
)
>  [1] -1  3

range2(
    x = vec2, 
    na_rm = TRUE
)
>  [1] 1 3

range2(
    x = log2, 
    na_rm = TRUE
)
>  [1] 0 1

range2(
    x = log1, 
    na_rm = TRUE
)
>  [1] 0 1

range2(
    x = mat3, 
    na_rm = TRUE
)
>  [1] 0 1
```

\

### Argument finite

``` r
range2(
    x = vec1,
    finite = TRUE
)
>  [1] -1  3

range2(
    x = vec3,
    finite = TRUE
)
>  [1] 1 3
```

\

### Argument safer_check

``` r
range2(
    x = mat3,
    safer_check = TRUE
)
>  [1] NA NA

range2(
    x = vec3,
    safer_check = TRUE
)
>  [1]   1 Inf
```

\

### All the arguments

``` r
range2(
    x = vec5,
    na_rm = TRUE,
    finite = FALSE,
    safer_check = TRUE
)
>  [1] -Inf    2

range2(
    x = mat4,
    na_rm = TRUE,
    finite = FALSE,
    safer_check = TRUE
)
>  [1] 0 1
```
