# head2

\

``` r

library(saferTool)
```

\

### Datasets

``` r

mat1 <- diag(1:20) ;  dimnames(mat1) <- list(letters[1:20], LETTERS[1:20]) # diagonal matrix 20 * 20 with row names and column names
```

\

### Datasets info

``` r
mat1 # diagonal matrix 20 * 20 with row names and column names
>    A B C D E F G H I  J  K  L  M  N  O  P  Q  R  S  T
>  a 1 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0
>  b 0 2 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0
>  c 0 0 3 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0
>  d 0 0 0 4 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0
>  e 0 0 0 0 5 0 0 0 0  0  0  0  0  0  0  0  0  0  0  0
>  f 0 0 0 0 0 6 0 0 0  0  0  0  0  0  0  0  0  0  0  0
>  g 0 0 0 0 0 0 7 0 0  0  0  0  0  0  0  0  0  0  0  0
>  h 0 0 0 0 0 0 0 8 0  0  0  0  0  0  0  0  0  0  0  0
>  i 0 0 0 0 0 0 0 0 9  0  0  0  0  0  0  0  0  0  0  0
>  j 0 0 0 0 0 0 0 0 0 10  0  0  0  0  0  0  0  0  0  0
>  k 0 0 0 0 0 0 0 0 0  0 11  0  0  0  0  0  0  0  0  0
>  l 0 0 0 0 0 0 0 0 0  0  0 12  0  0  0  0  0  0  0  0
>  m 0 0 0 0 0 0 0 0 0  0  0  0 13  0  0  0  0  0  0  0
>  n 0 0 0 0 0 0 0 0 0  0  0  0  0 14  0  0  0  0  0  0
>  o 0 0 0 0 0 0 0 0 0  0  0  0  0  0 15  0  0  0  0  0
>  p 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0 16  0  0  0  0
>  q 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0 17  0  0  0
>  r 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0 18  0  0
>  s 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0 19  0
>  t 0 0 0 0 0 0 0 0 0  0  0  0  0  0  0  0  0  0  0 20
```

\

### Simple examples

``` r
head2(data1 = mat1)
>    A B C D E F
>  a 1 0 0 0 0 0
>  b 0 2 0 0 0 0
>  c 0 0 3 0 0 0
>  d 0 0 0 4 0 0
>  e 0 0 0 0 5 0
>  f 0 0 0 0 0 6
head2(letters, 8)
>  [1] "a" "b" "c" "d" "e" "f" "g" "h"
```

\

### Argument n

``` r
head2(data1 = mat1, n = 5) # number of dimension to print (5 means 5 rows and columns)
>    A B C D E
>  a 1 0 0 0 0
>  b 0 2 0 0 0
>  c 0 0 3 0 0
>  d 0 0 0 4 0
>  e 0 0 0 0 5
```

\

### Argument side

``` r
head2(data1 = mat1, side = "r") # left or right side of the 2D object (only for matrix, data frame or table)
>    O P Q R S T
>  a 0 0 0 0 0 0
>  b 0 0 0 0 0 0
>  c 0 0 0 0 0 0
>  d 0 0 0 0 0 0
>  e 0 0 0 0 0 0
>  f 0 0 0 0 0 0
```

\

### Argument safer_check

``` r
head2(data1 = mat1, safer_check = TRUE) # safer_check = TRUE, checkings are performed before main code running
>    A B C D E F
>  a 1 0 0 0 0 0
>  b 0 2 0 0 0 0
>  c 0 0 3 0 0 0
>  d 0 0 0 4 0 0
>  e 0 0 0 0 5 0
>  f 0 0 0 0 0 6
```

\

### All the arguments

``` r
head2(
    data1 = mat1, 
    n = 6, 
    side = "l"
)
>    A B C D E F
>  a 1 0 0 0 0 0
>  b 0 2 0 0 0 0
>  c 0 0 3 0 0 0
>  d 0 0 0 4 0 0
>  e 0 0 0 0 5 0
>  f 0 0 0 0 0 6
```
