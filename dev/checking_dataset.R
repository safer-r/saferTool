################ CREATION OF THE CHECKING DATASET


######## object list t25_20201108 (25 objects created 20201108)


vec1 <- -1:3 # vector of integers
vec2 <- 1:3 / 3 # vector of proportions
vec3 <- c(1, 2, NA, -Inf) # vector of integers but stored as "double", with NA and Inf
vec4 <- "pearson" # vector of characters
vec5 <- c("a", "b","a", NA) # vector of characters with NA
cpx1 <- as.complex(1) # complex
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
s4.1 <- show # S4 object
call1 <- call("call1") # object of class "call", mode "call" &  type "language"


###################### 20231205

vec6 <- c(NA,NA) #vector with only NA
vec7 <- c(0.8, 12.55, 1.5) # vector with positive floats
vec8 <- c(0, 12.55, 1.5) # vector with positive floats and 0
vec9 <- c(-1.6,0, 7.5) # vector with floats and 0
vec10 <- c(-1.6, -12.5, -0.8, 0) # vector with negative floats and 0
vec11 <- c(-1.6, -12.5, -0.8) # vector with negative floats
vec12 <- 1:3 # vector with positive integers
vec13 <- 0:3 # vector with positive integers and 0
vec14 <- -3:0 # vector with negative integers and 0
mat3 <- matrix(vec6) # matix with only NA
l2 <- list(vec6) # list with only NA
df2 <- as.data.frame(mat3) # dataframe with only NA
fac2 <- factor(rep(NA,2)) # factor with only NA
tab3 <- table(fac2) # 1D table with only NA
tab4 <- table(fac2, fac2) # 2D table with only NA

angle1 <- list(-90, 0, 45, 90, 180) # vector of angles
log1 <- list("no", "log2", "log10") # vector of characters
logic1 <- list(TRUE, FALSE) # vector of logic


t25_20201108 <- list( # test list
    NULL, 
    NA, 
    1, 
    TRUE, 
    vec1, 
    vec2, 
    vec3, 
    vec4, 
    vec5, 
    vec6,
    vec7,
    vec8,
    vec9,
    vec10,
    vec11,
    vec12,
    vec13,
    vec14,
    cpx1, 
    mat1, 
    mat2, 
    mat3,
    df1, 
    df2,
    l1,
    l2,
    fac1, 
    fac2,
    tab1, 
    tab2, 
    tab3,
    tab4,
    exp1, 
    name1, 
    angle1,
    log1,
    logic1,
    fun1, 
    fun2, 
    fun3, 
    env1, 
    s4.1, 
    call1
)