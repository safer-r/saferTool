# comp_1d

\

``` r

library(saferTool)
```

\

### Datasets

``` r

vec1 <- 1:5 # vector of integers
vec2 <- 3:6 # vector of integers
fac1 <- factor(LETTERS[1:5]) # 1D factor
fac2 <- factor(LETTERS[4:7]) # 1D factor
fac3 <- factor(LETTERS[10:11]) # 1D factor
tab1 <- as.table(1:5) # 1D table
```

\

### Datasets info

``` r
vec1 # vector of integers
>  [1] 1 2 3 4 5
vec2 # vector of integers
>  [1] 3 4 5 6
fac1 # 1D factor
>  [1] A B C D E
>  Levels: A B C D E
fac2 # 1D factor
>  [1] D E F G
>  Levels: D E F G
tab1 # 1D table
>  A B C D E 
>  1 2 3 4 5
```

\

### All the arguments

``` r
names(vec1) <- LETTERS[1:5] ; 
comp_1d(vec1, vec2, safer_check = TRUE)
>  $same_class
>  [1] TRUE
>  
>  $class
>  [1] "integer"
>  
>  $same_length
>  [1] FALSE
>  
>  $length
>  NULL
>  
>  $same_levels
>  NULL
>  
>  $levels
>  NULL
>  
>  $any_id_levels
>  [1] FALSE
>  
>  $same_levels_pos1
>  NULL
>  
>  $same_levels_pos2
>  NULL
>  
>  $same_levels_match1
>  NULL
>  
>  $same_levels_match2
>  NULL
>  
>  $common_levels
>  NULL
>  
>  $same_names
>  [1] FALSE
>  
>  $name
>  NULL
>  
>  $any_id_names
>  [1] FALSE
>  
>  $same_names_pos1
>  NULL
>  
>  $same_names_pos2
>  NULL
>  
>  $same_names_match1
>  NULL
>  
>  $same_names_match2
>  NULL
>  
>  $common_names
>  NULL
>  
>  $any_id_elements
>  [1] TRUE
>  
>  $same_elements_pos1
>  [1] 3 4 5
>  
>  $same_elements_pos2
>  [1] 1 2 3
>  
>  $same_elements_match1
>  [1] NA NA  1  2  3
>  
>  $same_elements_match2
>  [1]  3  4  5 NA
>  
>  $common_elements
>  [1] 3 4 5
>  
>  $same_order
>  NULL
>  
>  $order1
>  NULL
>  
>  $order2
>  NULL
>  
>  $identical_object
>  [1] FALSE
>  
>  $identical_content
>  [1] FALSE

names(vec1) <- LETTERS[1:5] ; 
names(vec2) <- LETTERS[1:4] ; 
comp_1d(vec1, vec2, safer_check = TRUE)
>  $same_class
>  [1] TRUE
>  
>  $class
>  [1] "integer"
>  
>  $same_length
>  [1] FALSE
>  
>  $length
>  NULL
>  
>  $same_levels
>  NULL
>  
>  $levels
>  NULL
>  
>  $any_id_levels
>  [1] FALSE
>  
>  $same_levels_pos1
>  NULL
>  
>  $same_levels_pos2
>  NULL
>  
>  $same_levels_match1
>  NULL
>  
>  $same_levels_match2
>  NULL
>  
>  $common_levels
>  NULL
>  
>  $same_names
>  [1] FALSE
>  
>  $name
>  NULL
>  
>  $any_id_names
>  [1] TRUE
>  
>  $same_names_pos1
>  [1] 1 2 3 4
>  
>  $same_names_pos2
>  [1] 1 2 3 4
>  
>  $same_names_match1
>  [1]  1  2  3  4 NA
>  
>  $same_names_match2
>  [1] 1 2 3 4
>  
>  $common_names
>  [1] "A" "B" "C" "D"
>  
>  $any_id_elements
>  [1] TRUE
>  
>  $same_elements_pos1
>  [1] 3 4 5
>  
>  $same_elements_pos2
>  [1] 1 2 3
>  
>  $same_elements_match1
>  [1] NA NA  1  2  3
>  
>  $same_elements_match2
>  [1]  3  4  5 NA
>  
>  $common_elements
>  [1] 3 4 5
>  
>  $same_order
>  NULL
>  
>  $order1
>  NULL
>  
>  $order2
>  NULL
>  
>  $identical_object
>  [1] FALSE
>  
>  $identical_content
>  [1] FALSE

comp_1d(fac1, fac2, safer_check = TRUE)
>  $same_class
>  [1] TRUE
>  
>  $class
>  [1] "factor"
>  
>  $same_length
>  [1] FALSE
>  
>  $length
>  NULL
>  
>  $same_levels
>  [1] FALSE
>  
>  $levels
>  NULL
>  
>  $any_id_levels
>  [1] TRUE
>  
>  $same_levels_pos1
>  [1] 4 5
>  
>  $same_levels_pos2
>  [1] 1 2
>  
>  $same_levels_match1
>  [1] NA NA NA  1  2
>  
>  $same_levels_match2
>  [1]  4  5 NA NA
>  
>  $common_levels
>  [1] "D" "E"
>  
>  $same_names
>  NULL
>  
>  $name
>  NULL
>  
>  $any_id_names
>  [1] FALSE
>  
>  $same_names_pos1
>  NULL
>  
>  $same_names_pos2
>  NULL
>  
>  $same_names_match1
>  NULL
>  
>  $same_names_match2
>  NULL
>  
>  $common_names
>  NULL
>  
>  $any_id_elements
>  [1] TRUE
>  
>  $same_elements_pos1
>  [1] 4 5
>  
>  $same_elements_pos2
>  [1] 1 2
>  
>  $same_elements_match1
>  [1] NA NA NA  1  2
>  
>  $same_elements_match2
>  [1]  4  5 NA NA
>  
>  $common_elements
>  [1] "D" "E"
>  
>  $same_order
>  NULL
>  
>  $order1
>  NULL
>  
>  $order2
>  NULL
>  
>  $identical_object
>  [1] FALSE
>  
>  $identical_content
>  [1] FALSE

comp_1d(fac1, fac3, safer_check = TRUE)
>  $same_class
>  [1] TRUE
>  
>  $class
>  [1] "factor"
>  
>  $same_length
>  [1] FALSE
>  
>  $length
>  NULL
>  
>  $same_levels
>  [1] FALSE
>  
>  $levels
>  NULL
>  
>  $any_id_levels
>  [1] FALSE
>  
>  $same_levels_pos1
>  NULL
>  
>  $same_levels_pos2
>  NULL
>  
>  $same_levels_match1
>  NULL
>  
>  $same_levels_match2
>  NULL
>  
>  $common_levels
>  NULL
>  
>  $same_names
>  NULL
>  
>  $name
>  NULL
>  
>  $any_id_names
>  [1] FALSE
>  
>  $same_names_pos1
>  NULL
>  
>  $same_names_pos2
>  NULL
>  
>  $same_names_match1
>  NULL
>  
>  $same_names_match2
>  NULL
>  
>  $common_names
>  NULL
>  
>  $any_id_elements
>  [1] FALSE
>  
>  $same_elements_pos1
>  NULL
>  
>  $same_elements_pos2
>  NULL
>  
>  $same_elements_match1
>  NULL
>  
>  $same_elements_match2
>  NULL
>  
>  $common_elements
>  NULL
>  
>  $same_order
>  NULL
>  
>  $order1
>  NULL
>  
>  $order2
>  NULL
>  
>  $identical_object
>  [1] FALSE
>  
>  $identical_content
>  [1] FALSE

comp_1d(vec1, fac1, safer_check = TRUE)
>  $same_class
>  [1] FALSE
>  
>  $class
>  NULL
>  
>  $same_length
>  [1] TRUE
>  
>  $length
>  [1] 5
>  
>  $same_levels
>  NULL
>  
>  $levels
>  NULL
>  
>  $any_id_levels
>  [1] FALSE
>  
>  $same_levels_pos1
>  NULL
>  
>  $same_levels_pos2
>  NULL
>  
>  $same_levels_match1
>  NULL
>  
>  $same_levels_match2
>  NULL
>  
>  $common_levels
>  NULL
>  
>  $same_names
>  [1] FALSE
>  
>  $name
>  NULL
>  
>  $any_id_names
>  [1] FALSE
>  
>  $same_names_pos1
>  NULL
>  
>  $same_names_pos2
>  NULL
>  
>  $same_names_match1
>  NULL
>  
>  $same_names_match2
>  NULL
>  
>  $common_names
>  NULL
>  
>  $any_id_elements
>  [1] FALSE
>  
>  $same_elements_pos1
>  NULL
>  
>  $same_elements_pos2
>  NULL
>  
>  $same_elements_match1
>  NULL
>  
>  $same_elements_match2
>  NULL
>  
>  $common_elements
>  NULL
>  
>  $same_order
>  NULL
>  
>  $order1
>  NULL
>  
>  $order2
>  NULL
>  
>  $identical_object
>  [1] FALSE
>  
>  $identical_content
>  [1] FALSE

comp_1d(tab1, tab1, safer_check = TRUE)
>  $same_class
>  [1] TRUE
>  
>  $class
>  [1] "table"
>  
>  $same_length
>  [1] TRUE
>  
>  $length
>  [1] 5
>  
>  $same_levels
>  NULL
>  
>  $levels
>  NULL
>  
>  $any_id_levels
>  [1] FALSE
>  
>  $same_levels_pos1
>  NULL
>  
>  $same_levels_pos2
>  NULL
>  
>  $same_levels_match1
>  NULL
>  
>  $same_levels_match2
>  NULL
>  
>  $common_levels
>  NULL
>  
>  $same_names
>  [1] TRUE
>  
>  $name
>  [1] "A" "B" "C" "D" "E"
>  
>  $any_id_names
>  [1] TRUE
>  
>  $same_names_pos1
>  [1] 1 2 3 4 5
>  
>  $same_names_pos2
>  [1] 1 2 3 4 5
>  
>  $same_names_match1
>  [1] 1 2 3 4 5
>  
>  $same_names_match2
>  [1] 1 2 3 4 5
>  
>  $common_names
>  [1] "A" "B" "C" "D" "E"
>  
>  $any_id_elements
>  [1] TRUE
>  
>  $same_elements_pos1
>  [1] 1 2 3 4 5
>  
>  $same_elements_pos2
>  [1] 1 2 3 4 5
>  
>  $same_elements_match1
>  [1] 1 2 3 4 5
>  
>  $same_elements_match2
>  [1] 1 2 3 4 5
>  
>  $common_elements
>  A B C D E 
>  1 2 3 4 5 
>  
>  $same_order
>  [1] TRUE
>  
>  $order1
>  [1] 1 2 3 4 5
>  
>  $order2
>  [1] 1 2 3 4 5
>  
>  $identical_object
>  [1] TRUE
>  
>  $identical_content
>  [1] TRUE

comp_1d(tab1, vec1, safer_check = TRUE)
>  $same_class
>  [1] FALSE
>  
>  $class
>  NULL
>  
>  $same_length
>  [1] TRUE
>  
>  $length
>  [1] 5
>  
>  $same_levels
>  NULL
>  
>  $levels
>  NULL
>  
>  $any_id_levels
>  [1] FALSE
>  
>  $same_levels_pos1
>  NULL
>  
>  $same_levels_pos2
>  NULL
>  
>  $same_levels_match1
>  NULL
>  
>  $same_levels_match2
>  NULL
>  
>  $common_levels
>  NULL
>  
>  $same_names
>  [1] TRUE
>  
>  $name
>  [1] "A" "B" "C" "D" "E"
>  
>  $any_id_names
>  [1] TRUE
>  
>  $same_names_pos1
>  [1] 1 2 3 4 5
>  
>  $same_names_pos2
>  [1] 1 2 3 4 5
>  
>  $same_names_match1
>  [1] 1 2 3 4 5
>  
>  $same_names_match2
>  [1] 1 2 3 4 5
>  
>  $common_names
>  [1] "A" "B" "C" "D" "E"
>  
>  $any_id_elements
>  [1] TRUE
>  
>  $same_elements_pos1
>  [1] 1 2 3 4 5
>  
>  $same_elements_pos2
>  [1] 1 2 3 4 5
>  
>  $same_elements_match1
>  [1] 1 2 3 4 5
>  
>  $same_elements_match2
>  [1] 1 2 3 4 5
>  
>  $common_elements
>  [1] 1 2 3 4 5
>  
>  $same_order
>  NULL
>  
>  $order1
>  NULL
>  
>  $order2
>  NULL
>  
>  $identical_object
>  [1] FALSE
>  
>  $identical_content
>  [1] FALSE
```
