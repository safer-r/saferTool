# round2

\

``` r

library(saferTool)
```

\

### Datasets

``` r

vec1 <- 1:3 / 3 # vector of proportions
vec2 <- c(NA, 10, 100.001, 333.0001254, 12312.1235) # vector of integer, double and NA
vec3 <- c(NA, "10", "100.001", "333.0001254", "12312.1235") # vector of character values and NA
```

\

### Datasets info

``` r
vec1 # vector of proportions
>  [1] 0.3333333 0.6666667 1.0000000
vec2 # vector of integer, double and NA
>  [1]         NA    10.0000   100.0010   333.0001 12312.1235
```

\

### Simple examples

``` r
round2(data = vec1) # vector of proportions
>  [1] 0.33 0.67 1.00
round2(data = vec2) # vector of integer, double and NA
>  [1]         NA    10.0000   100.0010   333.0001 12312.1200
round2(data = vec3) # vector of character values and NA
>  [1] NA          "10"        "100.001"   "333.00013" "12312.12"
```

\

### Argument dec_nb

``` r
cat(round2(data = vec1, dec_nb = 2), "\n\n")
>  0.33 0.67 1

cat(round2(data = vec1, dec_nb = 3), "\n\n")
>  0.333 0.667 1

cat(round2(data = vec2, dec_nb = 2), "\n\n")
>  NA 10 100.001 333.0001 12312.12

cat(round2(data = vec2, dec_nb = 3), "\n\n")
>  NA 10 100.001 333.0001 12312.12

cat(round2(data = vec3, dec_nb = 2), "\n\n")
>  NA 10 100.001 333.00013 12312.12

cat(round2(data = vec3, dec_nb = 3), "\n\n")
>  NA 10 100.001 333.000125 12312.123
```

\

### Argument after_lead_zero

``` r
cat(round2(data = vec2, after_lead_zero = TRUE), "\n\n")
>  NA 10 100.001 333.0001 12312.12

cat(round2(data = vec2, after_lead_zero = FALSE), "\n\n")
>  NA 10 100 333 12312.12

cat(round2(data = vec3, after_lead_zero = TRUE), "\n\n")
>  NA 10 100.001 333.00013 12312.12

cat(round2(data = vec3, after_lead_zero = FALSE), "\n\n")
>  NA 10 100.00 333.00 12312.12
```

\

### Argument safer_check

``` r
round2(data = vec1, safer_check = TRUE)
>  [1] 0.33 0.67 1.00

round2(data = vec2, safer_check = TRUE)
>  [1]         NA    10.0000   100.0010   333.0001 12312.1200
```

\

### All the arguments

``` r
ini.options <- options()$digits ; 
options(digits = 8) ; 
cat(round2(data = vec2, dec_nb = 2, after_lead_zero = FALSE, safer_check = TRUE), "\n\n") ; 
>  NA 10 100 333 12312.12
options(digits = ini.options)


ini.options <- options()$digits ; 
options(digits = 8) ; 
cat(round2(data = vec3, dec_nb = 2, after_lead_zero = FALSE, safer_check = TRUE), "\n\n") ; 
>  NA 10 100.00 333.00 12312.12
options(digits = ini.options)


ini.options <- options()$digits ; 
options(digits = 8) ; 
cat(round2(data = c(NA, "10", "100.001", "333.0001254", "12312.1235"), dec_nb = 2, after_lead_zero = TRUE, safer_check = TRUE), "\n\n") ; 
>  NA 10 100.001 333.00013 12312.12
options(digits = ini.options)
```
