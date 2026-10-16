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
>  $same.length
>  [1] TRUE
>  
>  $length
>  [1] 3
>  
>  $same.names
>  [1] TRUE
>  
>  $name
>  [1] "a" "b" "d"
>  
>  $any.id.names
>  [1] TRUE
>  
>  $same.names.pos1
>  [1] 1 2 3
>  
>  $same.names.pos2
>  [1] 1 2 3
>  
>  $same.names.match1
>  [1] 1 2 3
>  
>  $same.names.match2
>  [1] 1 2 3
>  
>  $common.names
>  [1] "a" "b" "d"
>  
>  $any.id.compartments
>  [1] TRUE
>  
>  $same.compartments.pos1
>  [1] 1 2 3
>  
>  $same.compartments.pos2
>  [1] 1 2 3
>  
>  $same.compartments.match1
>  [1] 1 2 3
>  
>  $same.compartments.match2
>  [1] 1 2 3
>  
>  $common.compartments
>  $common.compartments$a
>  [1] 1 2 3 4 5
>  
>  $common.compartments$b
>  [1] "A" "B"
>  
>  $common.compartments$d
>       [,1]
>  [1,]    1
>  [2,]    2
>  [3,]    3
>  [4,]    4
>  [5,]    5
>  [6,]    6
>  
>  
>  $same.order
>  [1] TRUE
>  
>  $identical.object
>  [1] TRUE
>  
>  $identical.content
>  [1] TRUE

comp_list(list1, list2, safer_check = TRUE)
>  $same.length
>  [1] FALSE
>  
>  $length
>  NULL
>  
>  $same.names
>  [1] FALSE
>  
>  $name
>  NULL
>  
>  $any.id.names
>  [1] FALSE
>  
>  $same.names.pos1
>  NULL
>  
>  $same.names.pos2
>  NULL
>  
>  $same.names.match1
>  NULL
>  
>  $same.names.match2
>  NULL
>  
>  $common.names
>  NULL
>  
>  $any.id.compartments
>  [1] TRUE
>  
>  $same.compartments.pos1
>  [1] 1 2
>  
>  $same.compartments.pos2
>  [1] 1 2
>  
>  $same.compartments.match1
>  [1]  1  2 NA
>  
>  $same.compartments.match2
>  [1] 1 2
>  
>  $common.compartments
>  $common.compartments[[1]]
>  [1] 1 2 3 4 5
>  
>  $common.compartments[[2]]
>  [1] "A" "B"
>  
>  
>  $same.order
>  NULL
>  
>  $identical.object
>  [1] FALSE
>  
>  $identical.content
>  [1] FALSE

comp_list(list1, list3, safer_check = TRUE)
>  $same.length
>  [1] FALSE
>  
>  $length
>  NULL
>  
>  $same.names
>  [1] FALSE
>  
>  $name
>  NULL
>  
>  $any.id.names
>  [1] TRUE
>  
>  $same.names.pos1
>  [1] 1 2
>  
>  $same.names.pos2
>  [1] 1 2
>  
>  $same.names.match1
>  [1]  1  2 NA
>  
>  $same.names.match2
>  [1] 1 2
>  
>  $common.names
>  [1] "a" "b"
>  
>  $any.id.compartments
>  [1] TRUE
>  
>  $same.compartments.pos1
>  [1] 1 2
>  
>  $same.compartments.pos2
>  [1] 1 2
>  
>  $same.compartments.match1
>  [1]  1  2 NA
>  
>  $same.compartments.match2
>  [1] 1 2
>  
>  $common.compartments
>  $common.compartments[[1]]
>  [1] 1 2 3 4 5
>  
>  $common.compartments[[2]]
>  [1] "A" "B"
>  
>  
>  $same.order
>  NULL
>  
>  $identical.object
>  [1] FALSE
>  
>  $identical.content
>  [1] FALSE

comp_list(list2, list4, safer_check = TRUE)
>  $same.length
>  [1] FALSE
>  
>  $length
>  NULL
>  
>  $same.names
>  NULL
>  
>  $name
>  NULL
>  
>  $any.id.names
>  [1] FALSE
>  
>  $same.names.pos1
>  NULL
>  
>  $same.names.pos2
>  NULL
>  
>  $same.names.match1
>  NULL
>  
>  $same.names.match2
>  NULL
>  
>  $common.names
>  NULL
>  
>  $any.id.compartments
>  [1] TRUE
>  
>  $same.compartments.pos1
>  [1] 1
>  
>  $same.compartments.pos2
>  [1] 3
>  
>  $same.compartments.match1
>  [1]  3 NA
>  
>  $same.compartments.match2
>  [1] NA NA  1
>  
>  $common.compartments
>  $common.compartments[[1]]
>  [1] 1 2 3 4 5
>  
>  
>  $same.order
>  NULL
>  
>  $identical.object
>  [1] FALSE
>  
>  $identical.content
>  [1] FALSE
```
