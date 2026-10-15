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
>  $same.class
>  [1] TRUE
>  
>  $class
>  [1] "integer"
>  
>  $same.length
>  [1] FALSE
>  
>  $length
>  NULL
>  
>  $same.levels
>  NULL
>  
>  $levels
>  NULL
>  
>  $any.id.levels
>  [1] FALSE
>  
>  $same.levels.pos1
>  NULL
>  
>  $same.levels.pos2
>  NULL
>  
>  $same.levels.match1
>  NULL
>  
>  $same.levels.match2
>  NULL
>  
>  $common.levels
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
>  $any.id.elements
>  [1] TRUE
>  
>  $same.elements.pos1
>  [1] 3 4 5
>  
>  $same.elements.pos2
>  [1] 1 2 3
>  
>  $same.elements.match1
>  [1] NA NA  1  2  3
>  
>  $same.elements.match2
>  [1]  3  4  5 NA
>  
>  $common.elements
>  [1] 3 4 5
>  
>  $same.order
>  NULL
>  
>  $order1
>  NULL
>  
>  $order2
>  NULL
>  
>  $identical.object
>  [1] FALSE
>  
>  $identical.content
>  [1] FALSE

names(vec1) <- LETTERS[1:5] ; 
names(vec2) <- LETTERS[1:4] ; 
comp_1d(vec1, vec2, safer_check = TRUE)
>  $same.class
>  [1] TRUE
>  
>  $class
>  [1] "integer"
>  
>  $same.length
>  [1] FALSE
>  
>  $length
>  NULL
>  
>  $same.levels
>  NULL
>  
>  $levels
>  NULL
>  
>  $any.id.levels
>  [1] FALSE
>  
>  $same.levels.pos1
>  NULL
>  
>  $same.levels.pos2
>  NULL
>  
>  $same.levels.match1
>  NULL
>  
>  $same.levels.match2
>  NULL
>  
>  $common.levels
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
>  [1] 1 2 3 4
>  
>  $same.names.pos2
>  [1] 1 2 3 4
>  
>  $same.names.match1
>  [1]  1  2  3  4 NA
>  
>  $same.names.match2
>  [1] 1 2 3 4
>  
>  $common.names
>  [1] "A" "B" "C" "D"
>  
>  $any.id.elements
>  [1] TRUE
>  
>  $same.elements.pos1
>  [1] 3 4 5
>  
>  $same.elements.pos2
>  [1] 1 2 3
>  
>  $same.elements.match1
>  [1] NA NA  1  2  3
>  
>  $same.elements.match2
>  [1]  3  4  5 NA
>  
>  $common.elements
>  [1] 3 4 5
>  
>  $same.order
>  NULL
>  
>  $order1
>  NULL
>  
>  $order2
>  NULL
>  
>  $identical.object
>  [1] FALSE
>  
>  $identical.content
>  [1] FALSE

comp_1d(fac1, fac2, safer_check = TRUE)
>  $same.class
>  [1] TRUE
>  
>  $class
>  [1] "factor"
>  
>  $same.length
>  [1] FALSE
>  
>  $length
>  NULL
>  
>  $same.levels
>  [1] FALSE
>  
>  $levels
>  NULL
>  
>  $any.id.levels
>  [1] TRUE
>  
>  $same.levels.pos1
>  [1] 4 5
>  
>  $same.levels.pos2
>  [1] 1 2
>  
>  $same.levels.match1
>  [1] NA NA NA  1  2
>  
>  $same.levels.match2
>  [1]  4  5 NA NA
>  
>  $common.levels
>  [1] "D" "E"
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
>  $any.id.elements
>  [1] TRUE
>  
>  $same.elements.pos1
>  [1] 4 5
>  
>  $same.elements.pos2
>  [1] 1 2
>  
>  $same.elements.match1
>  [1] NA NA NA  1  2
>  
>  $same.elements.match2
>  [1]  4  5 NA NA
>  
>  $common.elements
>  [1] "D" "E"
>  
>  $same.order
>  NULL
>  
>  $order1
>  NULL
>  
>  $order2
>  NULL
>  
>  $identical.object
>  [1] FALSE
>  
>  $identical.content
>  [1] FALSE

comp_1d(fac1, fac3, safer_check = TRUE)
>  $same.class
>  [1] TRUE
>  
>  $class
>  [1] "factor"
>  
>  $same.length
>  [1] FALSE
>  
>  $length
>  NULL
>  
>  $same.levels
>  [1] FALSE
>  
>  $levels
>  NULL
>  
>  $any.id.levels
>  [1] FALSE
>  
>  $same.levels.pos1
>  NULL
>  
>  $same.levels.pos2
>  NULL
>  
>  $same.levels.match1
>  NULL
>  
>  $same.levels.match2
>  NULL
>  
>  $common.levels
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
>  $any.id.elements
>  [1] FALSE
>  
>  $same.elements.pos1
>  NULL
>  
>  $same.elements.pos2
>  NULL
>  
>  $same.elements.match1
>  NULL
>  
>  $same.elements.match2
>  NULL
>  
>  $common.elements
>  NULL
>  
>  $same.order
>  NULL
>  
>  $order1
>  NULL
>  
>  $order2
>  NULL
>  
>  $identical.object
>  [1] FALSE
>  
>  $identical.content
>  [1] FALSE

comp_1d(vec1, fac1, safer_check = TRUE)
>  $same.class
>  [1] FALSE
>  
>  $class
>  NULL
>  
>  $same.length
>  [1] TRUE
>  
>  $length
>  [1] 5
>  
>  $same.levels
>  NULL
>  
>  $levels
>  NULL
>  
>  $any.id.levels
>  [1] FALSE
>  
>  $same.levels.pos1
>  NULL
>  
>  $same.levels.pos2
>  NULL
>  
>  $same.levels.match1
>  NULL
>  
>  $same.levels.match2
>  NULL
>  
>  $common.levels
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
>  $any.id.elements
>  [1] FALSE
>  
>  $same.elements.pos1
>  NULL
>  
>  $same.elements.pos2
>  NULL
>  
>  $same.elements.match1
>  NULL
>  
>  $same.elements.match2
>  NULL
>  
>  $common.elements
>  NULL
>  
>  $same.order
>  NULL
>  
>  $order1
>  NULL
>  
>  $order2
>  NULL
>  
>  $identical.object
>  [1] FALSE
>  
>  $identical.content
>  [1] FALSE

comp_1d(tab1, tab1, safer_check = TRUE)
>  $same.class
>  [1] TRUE
>  
>  $class
>  [1] "table"
>  
>  $same.length
>  [1] TRUE
>  
>  $length
>  [1] 5
>  
>  $same.levels
>  NULL
>  
>  $levels
>  NULL
>  
>  $any.id.levels
>  [1] FALSE
>  
>  $same.levels.pos1
>  NULL
>  
>  $same.levels.pos2
>  NULL
>  
>  $same.levels.match1
>  NULL
>  
>  $same.levels.match2
>  NULL
>  
>  $common.levels
>  NULL
>  
>  $same.names
>  [1] TRUE
>  
>  $name
>  [1] "A" "B" "C" "D" "E"
>  
>  $any.id.names
>  [1] TRUE
>  
>  $same.names.pos1
>  [1] 1 2 3 4 5
>  
>  $same.names.pos2
>  [1] 1 2 3 4 5
>  
>  $same.names.match1
>  [1] 1 2 3 4 5
>  
>  $same.names.match2
>  [1] 1 2 3 4 5
>  
>  $common.names
>  [1] "A" "B" "C" "D" "E"
>  
>  $any.id.elements
>  [1] TRUE
>  
>  $same.elements.pos1
>  [1] 1 2 3 4 5
>  
>  $same.elements.pos2
>  [1] 1 2 3 4 5
>  
>  $same.elements.match1
>  [1] 1 2 3 4 5
>  
>  $same.elements.match2
>  [1] 1 2 3 4 5
>  
>  $common.elements
>  A B C D E 
>  1 2 3 4 5 
>  
>  $same.order
>  [1] TRUE
>  
>  $order1
>  [1] 1 2 3 4 5
>  
>  $order2
>  [1] 1 2 3 4 5
>  
>  $identical.object
>  [1] TRUE
>  
>  $identical.content
>  [1] TRUE

comp_1d(tab1, vec1, safer_check = TRUE)
>  $same.class
>  [1] FALSE
>  
>  $class
>  NULL
>  
>  $same.length
>  [1] TRUE
>  
>  $length
>  [1] 5
>  
>  $same.levels
>  NULL
>  
>  $levels
>  NULL
>  
>  $any.id.levels
>  [1] FALSE
>  
>  $same.levels.pos1
>  NULL
>  
>  $same.levels.pos2
>  NULL
>  
>  $same.levels.match1
>  NULL
>  
>  $same.levels.match2
>  NULL
>  
>  $common.levels
>  NULL
>  
>  $same.names
>  [1] TRUE
>  
>  $name
>  [1] "A" "B" "C" "D" "E"
>  
>  $any.id.names
>  [1] TRUE
>  
>  $same.names.pos1
>  [1] 1 2 3 4 5
>  
>  $same.names.pos2
>  [1] 1 2 3 4 5
>  
>  $same.names.match1
>  [1] 1 2 3 4 5
>  
>  $same.names.match2
>  [1] 1 2 3 4 5
>  
>  $common.names
>  [1] "A" "B" "C" "D" "E"
>  
>  $any.id.elements
>  [1] TRUE
>  
>  $same.elements.pos1
>  [1] 1 2 3 4 5
>  
>  $same.elements.pos2
>  [1] 1 2 3 4 5
>  
>  $same.elements.match1
>  [1] 1 2 3 4 5
>  
>  $same.elements.match2
>  [1] 1 2 3 4 5
>  
>  $common.elements
>  [1] 1 2 3 4 5
>  
>  $same.order
>  NULL
>  
>  $order1
>  NULL
>  
>  $order2
>  NULL
>  
>  $identical.object
>  [1] FALSE
>  
>  $identical.content
>  [1] FALSE
```
