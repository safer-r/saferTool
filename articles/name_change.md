# name_change

\

``` r

library(saferTool)
```

\

### Datasets

``` r

char1 <- c("A", "B", "C", "D", "E", "F") # vector of character strings
char2 <- c("A", "C", "E") # vector of character strings
char3 <- c("A", "B", "C", "C_modif1", "D") # vector of character strings
char4 <- c("A", "A_modif1", "C") # vector of character strings
```

\

### Datasets info

``` r
char1 # vector of character strings
>  [1] "A" "B" "C" "D" "E" "F"
char2 # vector of character strings
>  [1] "A" "C" "E"
char3 # vector of character strings
>  [1] "A"        "B"        "C"        "C_modif1" "D"
char4 # vector of character strings
>  [1] "A"        "A_modif1" "C"
```

\

### Simple examples

``` r
name_change(char1,char2)
>  $data
>  [1] "A_modif" "B"       "C_modif" "D"       "E_modif" "F"      
>  
>  $posi
>  [1] 1 3 5
>  
>  $ini
>  [1] "A" "C" "E"
>  
>  $post
>  [1] "A_modif" "C_modif" "E_modif"
```

\

### All the arguments

``` r
name_change(char3, char4, safer_check = TRUE) # the function checks that the new names are neither in obs1 nor in obs2 (increment the number after the added string)
>  $data
>  [1] "A_modif"  "B"        "C_modif"  "C_modif1" "D"       
>  
>  $posi
>  [1] 1 3
>  
>  $ini
>  [1] "A" "C"
>  
>  $post
>  [1] "A_modif" "C_modif"
```
