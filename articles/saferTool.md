# saferTool

## Description

Set of functions for object analysis and object modification, written
according to the [safer-R project](https://github.com/safer-r)
specifications.

\

## Content

\

| Function | Description |
|:---|:---|
| **comp_1d()** | Compare two 1D datasets (vector or factor or 1D table, or 1D matrix or 1D array) of the same class or not. Check and report in a list if the 2 datasets have: 1. same class 2. common elements 3. common element names (except factors) 4. common levels (factors only) |
| **comp_2d()** | Compare two 2D datasets of the same class or not. Check and report in a list if the 2 datasets have: 1. same class 2. same type 3. common row names 4. common column names 5. same row number 6. same column number 7. potential identical rows between the 2 datasets 8. potential identical columns between the 2 datasets |
| **df_remod()** | If the data frame is made of n numeric columns, a new data frame is created, with the 1st column gathering all the numeric values, and the 2nd column being the name of the columns of the initial data frame. If row names were present in the initial data frame, then a new ini_rowname column is added with the names of the rows. If the data frame is made of one numeric column and one character or factor column, a new data frame is created, with the new columns corresponding to the split numeric values (according to the character column). NA are added a the end of each column to have the same number of rows. |
| **name_change()** | This function allow to check if a vector of character strings, like column names of a data frame, has elements present in another vector (vector of reserved words or column names of another data frame before merging). |
| **round2()** | Round a vector of values, if decimal, with the desired number of decimal digits after the decimal leading zeros. |

\

See the articles for more examples of each function:
[comp_1d](https://safer-r.github.io/saferTool/articles/comp_1d.html),
[comp_2d](https://safer-r.github.io/saferTool/articles/comp_2d.html),
[df_remod](https://safer-r.github.io/saferTool/articles/df_remod.html),
[name_change](https://safer-r.github.io/saferTool/articles/name_change.html),
[round2](https://safer-r.github.io/saferTool/articles/round2.html).
