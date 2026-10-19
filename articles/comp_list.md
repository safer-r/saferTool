# comp_list

\

``` r

library(saferTool)
```

\

### Datasets

``` r

list1 <- list(a = 1:5, b = LETTERS[1:2], d = matrix(1:6)) # list
list2 <- list(1:5, LETTERS[1:2]) # list
list3 <- list(a = 1:5, b = LETTERS[1:2]) # list
list4 <- list(LETTERS[5:9], matrix(1:6), 1:5) # list
```

\

### Datasets info

``` r
list1 # list
>  $a
>  [1] 1 2 3 4 5
>  
>  $b
>  [1] "A" "B"
>  
>  $d
>       [,1]
>  [1,]    1
>  [2,]    2
>  [3,]    3
>  [4,]    4
>  [5,]    5
>  [6,]    6
list2 # list
>  [[1]]
>  [1] 1 2 3 4 5
>  
>  [[2]]
>  [1] "A" "B"
list3 # list
>  $a
>  [1] 1 2 3 4 5
>  
>  $b
>  [1] "A" "B"
list4 # list
>  [[1]]
>  [1] "E" "F" "G" "H" "I"
>  
>  [[2]]
>       [,1]
>  [1,]    1
>  [2,]    2
>  [3,]    3
>  [4,]    4
>  [5,]    5
>  [6,]    6
>  
>  [[3]]
>  [1] 1 2 3 4 5
```

\

### All the arguments

``` r
comp_list(list1, list1, safer_check = TRUE)
>  $same_length
>  [1] TRUE
>  
>  $length
>  [1] 3
>  
>  $same_names
>  [1] TRUE
>  
>  $name
>  [1] "a" "b" "d"
>  
>  $any_id_names
>  [1] TRUE
>  
>  $same_names_pos1
>  [1] 1 2 3
>  
>  $same_names_pos2
>  [1] 1 2 3
>  
>  $same_names_match1
>  [1] 1 2 3
>  
>  $same_names_match2
>  [1] 1 2 3
>  
>  $common_names
>  [1] "a" "b" "d"
>  
>  $any_id_compartments
>  [1] TRUE
>  
>  $same_compartments_pos1
>  [1] 1 2 3
>  
>  $same_compartments_pos2
>  [1] 1 2 3
>  
>  $same_compartments_match1
>  [1] 1 2 3
>  
>  $same_compartments_match2
>  [1] 1 2 3
>  
>  $common_compartments
>  $common_compartments$a
>  [1] 1 2 3 4 5
>  
>  $common_compartments$b
>  [1] "A" "B"
>  
>  $common_compartments$d
>       [,1]
>  [1,]    1
>  [2,]    2
>  [3,]    3
>  [4,]    4
>  [5,]    5
>  [6,]    6
>  
>  
>  $same_order
>  [1] TRUE
>  
>  $identical_object
>  [1] TRUE
>  
>  $identical_content
>  [1] TRUE

comp_list(list1, list2, safer_check = TRUE)
>  $same_length
>  [1] FALSE
>  
>  $length
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
>  $any_id_compartments
>  [1] TRUE
>  
>  $same_compartments_pos1
>  [1] 1 2
>  
>  $same_compartments_pos2
>  [1] 1 2
>  
>  $same_compartments_match1
>  [1]  1  2 NA
>  
>  $same_compartments_match2
>  [1] 1 2
>  
>  $common_compartments
>  $common_compartments[[1]]
>  [1] 1 2 3 4 5
>  
>  $common_compartments[[2]]
>  [1] "A" "B"
>  
>  
>  $same_order
>  NULL
>  
>  $identical_object
>  [1] FALSE
>  
>  $identical_content
>  [1] FALSE

comp_list(list1, list3, safer_check = TRUE)
>  $same_length
>  [1] FALSE
>  
>  $length
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
>  [1] 1 2
>  
>  $same_names_pos2
>  [1] 1 2
>  
>  $same_names_match1
>  [1]  1  2 NA
>  
>  $same_names_match2
>  [1] 1 2
>  
>  $common_names
>  [1] "a" "b"
>  
>  $any_id_compartments
>  [1] TRUE
>  
>  $same_compartments_pos1
>  [1] 1 2
>  
>  $same_compartments_pos2
>  [1] 1 2
>  
>  $same_compartments_match1
>  [1]  1  2 NA
>  
>  $same_compartments_match2
>  [1] 1 2
>  
>  $common_compartments
>  $common_compartments[[1]]
>  [1] 1 2 3 4 5
>  
>  $common_compartments[[2]]
>  [1] "A" "B"
>  
>  
>  $same_order
>  NULL
>  
>  $identical_object
>  [1] FALSE
>  
>  $identical_content
>  [1] FALSE

comp_list(list2, list4, safer_check = TRUE)
>  $same_length
>  [1] FALSE
>  
>  $length
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
>  $any_id_compartments
>  [1] TRUE
>  
>  $same_compartments_pos1
>  [1] 1
>  
>  $same_compartments_pos2
>  [1] 3
>  
>  $same_compartments_match1
>  [1]  3 NA
>  
>  $same_compartments_match2
>  [1] NA NA  1
>  
>  $common_compartments
>  $common_compartments[[1]]
>  [1] 1 2 3 4 5
>  
>  
>  $same_order
>  NULL
>  
>  $identical_object
>  [1] FALSE
>  
>  $identical_content
>  [1] FALSE
```
