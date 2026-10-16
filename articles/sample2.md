# sample2

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
df1 <- data.frame(a = 1:4, b = c("a", "b", "c", "d"), stringsAsFactors = TRUE) # data frame
list1 <- list(a = 1, b = 2, c = 3) # list
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
df1 # data frame
>    a b
>  1 1 a
>  2 2 b
>  3 3 c
>  4 4 d
list1 # list
>  $a
>  [1] 1
>  
>  $b
>  [1] 2
>  
>  $c
>  [1] 3
```

\

### Simple examples

``` r
sample2(x = vec1, size = 2, set_seed = 1) # vector of integers
>  [1] -1  2
sample2(x = log2, size = 2, set_seed = 1) # logical vector
>  [1]  TRUE FALSE
sample2(x = mat1, size = 2, set_seed = 1) # 1D matrix of integers
>  [1] -1  2
sample2(x = fac1, size = 2, set_seed = 1) # 1D factor
>  [1] 1    <NA>
>  Levels: -Inf 1 2
sample2(x = tab1, size = 2, set_seed = 1) # 1D table
>  fac1
>  -Inf    1 
>     1    1
sample2(x = df1, size = 2, set_seed = 1) # data frame: 2 rows are sampled
>    a b
>  1 1 a
>  2 2 b
>  3 3 c
>  4 4 d
sample2(x = list1, size = 2, set_seed = 1) # list
>  $a
>  [1] 1
>  
>  $b
>  [1] 2
```

\

### The corrected behavior for a single value

``` r
# base::sample() uses a single positive numeric value as the size of the population 1:x:
base::sample(x = 4, size = 3, replace = TRUE) # returns 3 draws between 1 and 4
>  [1] 1 3 4
base::sample(x = 4, size = 3, replace = FALSE) # returns 3 distinct values between 1 and 4, although the single value 4 cannot be sampled 3 times without replacement
>  [1] 3 1 2

# sample2() always uses the value itself as the population:
sample2(x = 4, size = 3, replace = TRUE, set_seed = 1) # returns 4 4 4
>  [1] 4 4 4
sample2(x = 4.5, size = 3, replace = TRUE, set_seed = 1) # returns 4.5 4.5 4.5, even for decimal values
>  [1] 4.5 4.5 4.5
sample2(x = TRUE, size = 3, replace = TRUE, set_seed = 1) # logical single values are also sampled as themselves
>  [1] TRUE TRUE TRUE

# the following example returns an error because the single value 4 cannot be sampled 3 times without replacement
# sample2(x = 4, size = 3, replace = FALSE)

# with size = NULL, all the elements are sampled exactly once:
sample2(x = 4, set_seed = 1) # returns 4
>  [1] 4
base::sample(x = 4) # for comparison: base::sample() draws 4 values between 1 and 4
>  [1] 3 4 1 2
```

\

### Argument size

``` r
sample2(
    x = vec1,
    size = 2,
    set_seed = 1
)
>  [1] -1  2

sample2(
    x = vec1,
    size = 10, # larger than the length of x
    replace = TRUE, # replace = TRUE required
    set_seed = 1
)
>   [1] -1  2 -1  0  3  1  0  1  1 -1

# the following example returns an error because size = 10 is larger than the length of x and replace = FALSE
# sample2(x = vec1, size = 10, replace = FALSE, set_seed = 1)

sample2(
    x = vec1,
    size = 5, # all the elements exactly once
    replace = FALSE,
    set_seed = 1
)
>  [1] -1  2  1  3  0
```

\

### Argument replace

``` r
sample2(
    x = vec1,
    size = 5,
    replace = FALSE, # all the elements exactly once
    set_seed = 1
)
>  [1] -1  2  1  3  0

sample2(
    x = vec1,
    size = 10,
    replace = TRUE,
    set_seed = 1
)
>   [1] -1  2 -1  0  3  1  0  1  1 -1
```

\

### Argument prob

``` r
sample2(
    x = vec1,
    size = 10,
    replace = TRUE,
    prob = c(0, 1, 0, 0, 0), # only the second element (-1:3, i.e., 0) can be drawn
    set_seed = 1
)
>   [1] 0 0 0 0 0 0 0 0 0 0

# the following example returns an error because the prob length is not equal to the x length
# sample2(x = vec1, size = 10, replace = TRUE, prob = c(0.5, 0.5), set_seed = 1)
```

\

### Argument set_seed

``` r
sample2(
    x = vec1,
    size = 2,
    set_seed = 1
)
>  [1] -1  2

sample2(
    x = vec1,
    size = 2,
    set_seed = 1
) # same result, because of the set_seed argument
>  [1] -1  2

sample2(
    x = vec1,
    size = 2
) # potentially different result at each execution, like in base::sample()
>  [1] -1  3
```

\

### Argument safer_check

``` r
sample2(
    x = vec1,
    size = 2,
    set_seed = 1,
    safer_check = TRUE
)
>  [1] -1  2

sample2(
    x = vec1,
    size = 2,
    set_seed = 1,
    safer_check = FALSE
)
>  [1] -1  2
```

\

### All the arguments

``` r
sample2(
    x = vec1,
    size = 2,
    replace = FALSE,
    prob = NULL,
    set_seed = 1,
    safer_check = TRUE
)
>  [1] -1  2

sample2(
    x = vec1,
    size = 4,
    replace = TRUE,
    prob = c(0.2, 0.2, 0.2, 0.2, 0.2), # uniform weights, same length as x
    set_seed = 1,
    safer_check = TRUE
)
>  [1]  1  1  2 -1
```
