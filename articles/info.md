# info

\

``` r

library(saferTool)
```

\

### Datasets

``` r

vec1 <- -1:3 # vector of integers
vec2 <- 1:3 / 3 # vector of proportions
vec3 <- c(1, 2, NA, -Inf) # vector of double values, with NA and -Inf
vec4 <- "pearson" # vector of characters
vec5 <- c("a", "b","a", NA) # vector of characters with NA
cpx <- as.complex(1) # complex
mat1 <- matrix(vec1) # 1D matrix of integers
mat2 <- matrix(c(1:5, NA), ncol = 2, dimnames = list(c("ROW1", "ROW2", "ROW3"), c("M1", "M2"))) # 2D matrix of floats with NA
df1 <- as.data.frame(mat2) # data.frame
l1 <- list(L1 = 1:3, L2 = letters[1:3]) # list
fac1 <- factor(rep(letters[4:6], c(4:6))) # factor
tab1 <- table(fac1) # 1D table
tab2 <- table(fac1, fac1) # 2D table
exp1 <- expression("a") # object of class "expression", mode "expression" & type "expression"
name1 <- substitute(exp1) # object of class "name", mode "name" & type "symbol"
fun1 <- mean # closure function of class "function", mode "function" & type "closure"
fun2 <- sum # primitive function of class "function", mode "function" & type "builtin"
fun3 <- get("<-") # primitive function of class "function", mode "function" &  type "special"
env1 <- new.env() # environment
s4 <- show # S4 object
call1 <- call("call1") # object of class "call", mode "call" & type "language"
```

\

### Datasets info

``` r
vec1 # vector of integers
>  [1] -1  0  1  2  3
vec2 # vector of proportions
>  [1] 0.3333333 0.6666667 1.0000000
vec3 # vector of integers but stored as "double", with NA
>  [1]    1    2   NA -Inf
vec4 # vector of characters
>  [1] "pearson"
vec5 # vector of characters with NA
>  [1] "a" "b" "a" NA
mat1 # 1D matrix of integers
>       [,1]
>  [1,]   -1
>  [2,]    0
>  [3,]    1
>  [4,]    2
>  [5,]    3
mat2 # 2D matrix of floats with NA
>       M1 M2
>  ROW1  1  4
>  ROW2  2  5
>  ROW3  3 NA
df1 # data.frame
>       M1 M2
>  ROW1  1  4
>  ROW2  2  5
>  ROW3  3 NA
l1 # list
>  $L1
>  [1] 1 2 3
>  
>  $L2
>  [1] "a" "b" "c"
fac1 # factor
>   [1] d d d d e e e e e f f f f f f
>  Levels: d e f
tab1 # 1D table
>  fac1
>  d e f 
>  4 5 6
tab2 # 2D table
>      fac1
>  fac1 d e f
>     d 4 0 0
>     e 0 5 0
>     f 0 0 6
exp1 # object of class "expression", mode "expression" & type "expression"
>  expression("a")
name1 # object of class "name", mode "name" & type "symbol"
>  exp1
fun1 # closure function of class "function", mode "function" & type "closure"
>  function (x, ...) 
>  UseMethod("mean")
>  <bytecode: 0x561803223ba0>
>  <environment: namespace:base>
fun2 # primitive function of class "function", mode "function" & type "builtin"
>  function (..., na.rm = FALSE)  .Primitive("sum")
fun3 # primitive function of class "function", mode "function" &  type "special"
>  .Primitive("<-")
env1 # environment
>  <environment: 0x5618063a8e88>
s4 # S4 object
>  standardGeneric for "show" defined from package "methods"
>  
>  function (object) 
>  standardGeneric("show")
>  <bytecode: 0x561802e63dd0>
>  <environment: 0x5618019c8498>
>  Methods may be defined for arguments: object
>  Use  showMethods(show)  for currently available ones.
>  (This generic function excludes non-simple inheritance; see ?setIs)
call1 # object of class "call", mode "call" &  type "language"
>  call1()
```

\

### Simple examples

``` r
info(data = vec1) # vector of integers
>  $NAME
>  [1] "vec1"
>  
>  $CLASS
>  [1] "integer"
>  
>  $TYPE
>  [1] "integer"
>  
>  $LENGTH
>  [1] 5
>  
>  $INF.NB
>  [1] 0
>  
>  $RANGE
>  [1] -1  3
>  
>  $SUM
>  [1] 5
>  
>  $MEAN
>  [1] 1
>  
>  $NA.NB
>  [1] 0
>  
>  $HEAD
>  [1] -1  0  1  2  3
>  
>  $TAIL
>  [1] -1  0  1  2  3
>  
>  $SUMMARY
>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
>       -1       0       1       1       2       3 
>  
>  $STRUCTURE
>                        
>    int [1:5] -1 0 1 2 3
info(data = vec2) # vector of proportions
>  $NAME
>  [1] "vec2"
>  
>  $CLASS
>  [1] "numeric"
>  
>  $TYPE
>  [1] "double"
>  
>  $LENGTH
>  [1] 3
>  
>  $INF.NB
>  [1] 0
>  
>  $RANGE
>  [1] 0.3333333 1.0000000
>  
>  $SUM
>  [1] 2
>  
>  $MEAN
>  [1] 0.6666667
>  
>  $NA.NB
>  [1] 0
>  
>  $HEAD
>  [1] 0.3333333 0.6666667 1.0000000
>  
>  $TAIL
>  [1] 0.3333333 0.6666667 1.0000000
>  
>  $SUMMARY
>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
>   0.3333  0.5000  0.6667  0.6667  0.8333  1.0000 
>  
>  $STRUCTURE
>                           
>    num [1:3] 0.333 0.667 1
info(data = vec3) # vector of integers but stored as "double", with NA and Inf
>  $NAME
>  [1] "vec3"
>  
>  $CLASS
>  [1] "numeric"
>  
>  $TYPE
>  [1] "double"
>  
>  $LENGTH
>  [1] 4
>  
>  $INF.NB
>  [1] 1
>  
>  $RANGE
>  [1] 1 2
>  
>  $SUM
>  [1] 3
>  
>  $MEAN
>  [1] 1.5
>  
>  $NA.NB
>  [1] 1
>  
>  $HEAD
>  [1]    1    2   NA -Inf
>  
>  $TAIL
>  [1]    1    2   NA -Inf
>  
>  $SUMMARY
>     Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     NAs 
>     -Inf    -Inf     1.0    -Inf     1.5     2.0       1 
>  
>  $STRUCTURE
>                         
>    num [1:4] 1 2 NA -Inf
info(data = vec4) # vector of characters
>  $NAME
>  [1] "vec4"
>  
>  $CLASS
>  [1] "character"
>  
>  $TYPE
>  [1] "character"
>  
>  $LENGTH
>  [1] 1
>  
>  $NA.NB
>  [1] 0
>  
>  $HEAD
>  [1] "pearson"
>  
>  $TAIL
>  [1] "pearson"
>  
>  $SUMMARY
>     Length  N.unique   N.blank Min.nchar Max.nchar 
>          1         1         0         7         7 
>  
>  $STRUCTURE
>                 
>    chr "pearson"
info(data = vec5) # vector of characters with NA
>  $NAME
>  [1] "vec5"
>  
>  $CLASS
>  [1] "character"
>  
>  $TYPE
>  [1] "character"
>  
>  $LENGTH
>  [1] 4
>  
>  $NA.NB
>  [1] 1
>  
>  $HEAD
>  [1] "a" "b" "a" NA 
>  
>  $TAIL
>  [1] "a" "b" "a" NA 
>  
>  $SUMMARY
>     Length  N.unique   N.blank Min.nchar Max.nchar       NAs 
>          4         2         0         1         1         1 
>  
>  $STRUCTURE
>                            
>    chr [1:4] "a" "b" "a" NA
info(data = mat1) # 1D matrix of integers
>  $NAME
>  [1] "mat1"
>  
>  $CLASS
>  [1] "matrix" "array" 
>  
>  $TYPE
>  [1] "integer"
>  
>  $LENGTH
>  [1] 5
>  
>  $INF.NB
>  [1] 0
>  
>  $RANGE
>  [1] -1  3
>  
>  $SUM
>  [1] 5
>  
>  $MEAN
>  [1] 1
>  
>  $NA.NB
>  [1] 0
>  
>  $HEAD
>       [,1]
>  [1,]   -1
>  [2,]    0
>  [3,]    1
>  [4,]    2
>  [5,]    3
>  
>  $TAIL
>       [,1]
>  [1,]   -1
>  [2,]    0
>  [3,]    1
>  [4,]    2
>  [5,]    3
>  
>  $DIMENSION
>  NROW NCOL 
>     5    1 
>  
>  $ROW_NAMES
>  NULL
>  
>  $COL_NAMES
>  NULL
>  
>  $SUMMARY
>         V1    
>   Min.   :-1  
>   1st Qu.: 0  
>   Median : 1  
>   Mean   : 1  
>   3rd Qu.: 2  
>   Max.   : 3  
>  
>  $STRUCTURE
>                           
>    int [1:5, 1] -1 0 1 2 3
info(data = mat2) # 2D matrix of floats with NA
>  $NAME
>  [1] "mat2"
>  
>  $CLASS
>  [1] "matrix" "array" 
>  
>  $TYPE
>  [1] "integer"
>  
>  $LENGTH
>  [1] 6
>  
>  $INF.NB
>  [1] 0
>  
>  $RANGE
>  [1] 1 5
>  
>  $SUM
>  [1] 15
>  
>  $MEAN
>  [1] 3
>  
>  $NA.NB
>  [1] 1
>  
>  $HEAD
>       M1 M2
>  ROW1  1  4
>  ROW2  2  5
>  ROW3  3 NA
>  
>  $TAIL
>       M1 M2
>  ROW1  1  4
>  ROW2  2  5
>  ROW3  3 NA
>  
>  $DIMENSION
>  NROW NCOL 
>     3    2 
>  
>  $ROW_NAMES
>  [1] "ROW1" "ROW2" "ROW3"
>  
>  $COL_NAMES
>  [1] "M1" "M2"
>  
>  $SUMMARY
>         M1            M2      
>   Min.   :1.0   Min.   :4.00  
>   1st Qu.:1.5   1st Qu.:4.25  
>   Median :2.0   Median :4.50  
>   Mean   :2.0   Mean   :4.50  
>   3rd Qu.:2.5   3rd Qu.:4.75  
>   Max.   :3.0   Max.   :5.00  
>                 NAs    :1     
>  
>  $STRUCTURE
>                                         
>    int [1:3, 1:2] 1 2 3 4 5 NA          
>    - attr(*, "dimnames")=List of 2      
>     ..$ : chr [1:3] "ROW1" "ROW2" "ROW3"
>     ..$ : chr [1:2] "M1" "M2"
info(data = df1) # data.frame
>  $NAME
>  [1] "df1"
>  
>  $CLASS
>  [1] "data.frame"
>  
>  $TYPE
>  [1] "list"
>  
>  $LENGTH
>  [1] 2
>  
>  $NA.NB
>  [1] 1
>  
>  $HEAD
>       M1 M2
>  ROW1  1  4
>  ROW2  2  5
>  ROW3  3 NA
>  
>  $TAIL
>       M1 M2
>  ROW1  1  4
>  ROW2  2  5
>  ROW3  3 NA
>  
>  $DIMENSION
>  NROW NCOL 
>     3    2 
>  
>  $ROW_NAMES
>  [1] "ROW1" "ROW2" "ROW3"
>  
>  $COL_NAMES
>  [1] "M1" "M2"
>  
>  $SUMMARY
>         M1            M2      
>   Min.   :1.0   Min.   :4.00  
>   1st Qu.:1.5   1st Qu.:4.25  
>   Median :2.0   Median :4.50  
>   Mean   :2.0   Mean   :4.50  
>   3rd Qu.:2.5   3rd Qu.:4.75  
>   Max.   :3.0   Max.   :5.00  
>                 NAs    :1     
>  
>  $STRUCTURE
>                                         
>   'data.frame':\t3 obs. of  2 variables:
>    $ M1: int  1 2 3                     
>    $ M2: int  4 5 NA                    
>  
>  $COLUMN_TYPE
>         M1        M2 
>  "integer" "integer"
info(data = l1) # list
>  $NAME
>  [1] "l1"
>  
>  $CLASS
>  [1] "list"
>  
>  $TYPE
>  [1] "list"
>  
>  $LENGTH
>  [1] 2
>  
>  $NA.NB
>  [1] 0
>  
>  $HEAD
>  $HEAD$L1
>  [1] 1 2 3
>  
>  $HEAD$L2
>  [1] "a" "b" "c"
>  
>  
>  $TAIL
>  $TAIL$L1
>  [1] 1 2 3
>  
>  $TAIL$L2
>  [1] "a" "b" "c"
>  
>  
>  $SUMMARY
>     Length Class  Mode     
>  L1 3      -none- numeric  
>  L2 3      -none- character
>  
>  $STRUCTURE
>                               
>   List of 2                   
>    $ L1: int [1:3] 1 2 3      
>    $ L2: chr [1:3] "a" "b" "c"
>  
>  $COMPARTMENT_NAMES
>  [1] "L1" "L2"
>  
>  $COMPARTMENT_TYPE
>           L1          L2 
>    "integer" "character"
info(data = fac1) # factor
>  $NAME
>  [1] "fac1"
>  
>  $CLASS
>  [1] "factor"
>  
>  $TYPE
>  [1] "integer"
>  
>  $LENGTH
>  [1] 15
>  
>  $NA.NB
>  [1] 0
>  
>  $HEAD
>  [1] d d d d e e
>  Levels: d e f
>  
>  $TAIL
>  [1] f f f f f f
>  Levels: d e f
>  
>  $SUMMARY
>  d e f 
>  4 5 6 
>  
>  $STRUCTURE
>                                                           
>    Factor w/ 3 levels "d","e","f": 1 1 1 1 2 2 2 2 2 3 ...
info(data = tab1) # 1D table
>  $NAME
>  [1] "tab1"
>  
>  $CLASS
>  [1] "table"
>  
>  $TYPE
>  [1] "integer"
>  
>  $LENGTH
>  [1] 3
>  
>  $INF.NB
>  [1] 0
>  
>  $RANGE
>  [1] 4 6
>  
>  $SUM
>  [1] 15
>  
>  $MEAN
>  [1] 5
>  
>  $NA.NB
>  [1] 0
>  
>  $HEAD
>  fac1
>  d e f 
>  4 5 6 
>  
>  $TAIL
>  fac1
>  d e f 
>  4 5 6 
>  
>  $DIMENSION
>  [1] 3
>  
>  $SUMMARY
>  Number of cases in table: 15 
>  Number of factors: 1 
>  
>  $STRUCTURE
>                                    
>    'table' int [1:3(1d)] 4 5 6     
>    - attr(*, "dimnames")=List of 1 
>     ..$ fac1: chr [1:3] "d" "e" "f"
info(data = tab2) # 2D table
>  $NAME
>  [1] "tab2"
>  
>  $CLASS
>  [1] "table"
>  
>  $TYPE
>  [1] "integer"
>  
>  $LENGTH
>  [1] 9
>  
>  $INF.NB
>  [1] 0
>  
>  $RANGE
>  [1] 0 6
>  
>  $SUM
>  [1] 15
>  
>  $MEAN
>  [1] 1.666667
>  
>  $NA.NB
>  [1] 0
>  
>  $HEAD
>      fac1
>  fac1 d e f
>     d 4 0 0
>     e 0 5 0
>     f 0 0 6
>  
>  $TAIL
>      fac1
>  fac1 d e f
>     d 4 0 0
>     e 0 5 0
>     f 0 0 6
>  
>  $DIMENSION
>  NROW NCOL 
>     3    3 
>  
>  $ROW_NAMES
>  [1] "d" "e" "f"
>  
>  $COL_NAMES
>  [1] "d" "e" "f"
>  
>  $SUMMARY
>  Number of cases in table: 15 
>  Number of factors: 2 
>  Test for independence of all factors:
>   Chisq = 30, df = 4, p-value = 4.894e-06
>   Chi-squared approximation may be incorrect
>  
>  $STRUCTURE
>                                            
>    'table' int [1:3, 1:3] 4 0 0 0 5 0 0 0 6
>    - attr(*, "dimnames")=List of 2         
>     ..$ fac1: chr [1:3] "d" "e" "f"        
>     ..$ fac1: chr [1:3] "d" "e" "f"
info(data = exp1) # object of class "expression", mode "expression" & type "expression"
>  $NAME
>  [1] "exp1"
>  
>  $CLASS
>  [1] "expression"
>  
>  $TYPE
>  [1] "expression"
>  
>  $LENGTH
>  [1] 1
>  
>  $HEAD
>  expression("a")
>  
>  $TAIL
>  expression("a")
>  
>  $SUMMARY
>      Length      Class       Mode 
>           1 expression expression 
>  
>  $STRUCTURE
>                    
>     expression("a")
info(data = name1) # object of class "name", mode "name" & type "symbol"
>  $NAME
>  [1] "name1"
>  
>  $CLASS
>  [1] "name"
>  
>  $TYPE
>  [1] "symbol"
>  
>  $LENGTH
>  [1] 1
>  
>  $SUMMARY
>  Length  Class   Mode 
>       1   name   name 
>  
>  $STRUCTURE
>               
>    symbol exp1
info(data = fun1) # closure function of class "function", mode "function" & type "closure"
>  $NAME
>  [1] "fun1"
>  
>  $CLASS
>  [1] "function"
>  
>  $TYPE
>  [1] "closure"
>  
>  $LENGTH
>  [1] 1
>  
>  $HEAD
>                      
>  1 function (x, ...) 
>  2 UseMethod("mean") 
>  
>  $TAIL
>                      
>  1 function (x, ...) 
>  2 UseMethod("mean") 
>  
>  $STRUCTURE
>                      
>   function (x, ...)
info(data = fun2) # primitive function of class "function", mode "function" & type "builtin"
>  $NAME
>  [1] "fun2"
>  
>  $CLASS
>  [1] "function"
>  
>  $TYPE
>  [1] "builtin"
>  
>  $LENGTH
>  [1] 1
>  
>  $HEAD
>                     
>  1 .Primitive("sum")
>  
>  $TAIL
>                     
>  1 .Primitive("sum")
>  
>  $STRUCTURE
>                                  
>   function (..., na.rm = FALSE)
info(data = fun3) # primitive function of class "function", mode "function" &  type "special"
>  $NAME
>  [1] "fun3"
>  
>  $CLASS
>  [1] "function"
>  
>  $TYPE
>  [1] "special"
>  
>  $LENGTH
>  [1] 1
>  
>  $HEAD
>                    
>  1 .Primitive("<-")
>  
>  $TAIL
>                    
>  1 .Primitive("<-")
>  
>  $STRUCTURE
>                    
>   .Primitive("<-")
info(data = env1) # environment
>  $NAME
>  [1] "env1"
>  
>  $CLASS
>  [1] "environment"
>  
>  $TYPE
>  [1] "environment"
>  
>  $LENGTH
>  [1] 0
>  
>  $SUMMARY
>       Length       Class        Mode 
>            0 environment environment 
>  
>  $STRUCTURE
>                                 
>   <environment: 0x5618063a8e88>
info(data = s4) # S4 object
>  $NAME
>  [1] "s4"
>  
>  $CLASS
>  [1] "standardGeneric"
>  attr(,"package")
>  [1] "methods"
>  
>  $TYPE
>  [1] "closure"
>  
>  $LENGTH
>  [1] 1
>  
>  $HEAD
>                                                                                                  
>  1 new("standardGeneric", .Data = function (object)                                              
>  2 standardGeneric("show"), generic = structure("show", package = "methods"),                    
>  3     package = "methods", group = list(), valueClass = character(0),                           
>  4     signature = structure("object", simpleOnly = TRUE), default = new("derivedDefaultMethod", 
>  5         .Data = function (object)                                                             
>  6         showDefault(object), target = new("signature", .Data = "ANY",                         
>  
>  $TAIL
>                                                                                                        
>  9          generic = structure("show", package = "methods")), skeleton = (new("derivedDefaultMethod", 
>  10         .Data = function (object)                                                                  
>  11         showDefault(object), target = new("signature", .Data = "ANY",                              
>  12             names = "object", package = "methods"), defined = new("signature",                     
>  13             .Data = "ANY", names = "object", package = "methods"),                                 
>  14         generic = structure("show", package = "methods")))(object))                                
>  
>  $STRUCTURE
>                                                                                                                                                                  
>   Formal class 'standardGeneric' [package "methods"] with 8 slots                                                                                                
>     ..@ .Data     :function (object)                                                                                                                             
>     ..@ generic   : chr "show"                                                                                                                                   
>     .. ..- attr(*, "package")= chr "methods"                                                                                                                     
>     ..@ package   : chr "methods"                                                                                                                                
>     ..@ group     : list()                                                                                                                                       
>     ..@ valueClass: chr(0)                                                                                                                                       
>     ..@ signature : chr "object"                                                                                                                                 
>     .. ..- attr(*, "simpleOnly")= logi TRUE                                                                                                                      
>     ..@ default   :Formal class 'derivedDefaultMethod' [package "methods"] with 4 slots                                                                          
>     .. .. ..@ .Data  :function (object)                                                                                                                          
>     .. .. ..@ target :Formal class 'signature' [package "methods"] with 3 slots                                                                                  
>     .. .. .. .. ..@ .Data  : chr "ANY"                                                                                                                           
>     .. .. .. .. ..@ names  : chr "object"                                                                                                                        
>     .. .. .. .. ..@ package: chr "methods"                                                                                                                       
>     .. .. ..@ defined:Formal class 'signature' [package "methods"] with 3 slots                                                                                  
>     .. .. .. .. ..@ .Data  : chr "ANY"                                                                                                                           
>     .. .. .. .. ..@ names  : chr "object"                                                                                                                        
>     .. .. .. .. ..@ package: chr "methods"                                                                                                                       
>     .. .. ..@ generic: chr "show"                                                                                                                                
>     .. .. .. ..- attr(*, "package")= chr "methods"                                                                                                               
>     ..@ skeleton  : language (new("derivedDefaultMethod", .Data = function (object)  showDefault(object), target = new("signature", .Data = "A| __truncated__ ...
info(data = call1) # object of class "call", mode "call" &  type "language"
>  $NAME
>  [1] "call1"
>  
>  $CLASS
>  [1] "call"
>  
>  $TYPE
>  [1] "language"
>  
>  $LENGTH
>  [1] 1
>  
>  $HEAD
>  call1()
>  
>  $TAIL
>  call1()
>  
>  $SUMMARY
>  Length  Class   Mode 
>       1   call   call 
>  
>  $STRUCTURE
>                    
>    language call1()
```

\

### All the arguments

``` r
info(
    data = vec1, 
    n = 1, # number of element to display per compartment of the output list (i.e., head(..., n))
    warn_print = FALSE,
    safer_check = TRUE
)
>  $NAME
>  [1] "vec1"
>  
>  $CLASS
>  [1] "integer"
>  
>  $TYPE
>  [1] "integer"
>  
>  $LENGTH
>  [1] 5
>  
>  $INF.NB
>  [1] 0
>  
>  $RANGE
>  [1] -1
>  
>  $SUM
>  [1] 5
>  
>  $MEAN
>  [1] 1
>  
>  $NA.NB
>  [1] 0
>  
>  $HEAD
>  [1] -1
>  
>  $TAIL
>  [1] -1
>  
>  $SUMMARY
>  Min. 
>    -1 
>  
>  $STRUCTURE
>                        
>    int [1:5] -1 0 1 2 3
>  
>  $WARNING
>  [1] "SOME COMPARTMENTS CAN BE TRUNCATED (n ARGUMENT IS 1)"
```
