
# saferTool <a href="">[<img src="man/figures/saferTool.png" align="right" height="140" />](https://safer-r.github.io/saferTool)</a>

<br />

<!-- badges: start -->

[![Codecov test coverage](https://codecov.io/github/safer-r/saferTool/coverage.svg?branch=master)](https://app.codecov.io/github/safer-r/saferTool?branch=master)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/saferTool)](https://cran.r-project.org/package=saferTool)
[![downloads](https://cranlogs.r-pkg.org/badges/saferTool)](https://www.rdocumentation.org/trends)
[![](https://img.shields.io/badge/license-GPL3.0-green.svg)](https://opensource.org/licenses/MITgpl-3-0)
[![rworkflows](https://github.com/safer-r/saferTool/actions/workflows/rworkflows.yml/badge.svg)](https://github.com/safer-r/saferTool/actions/workflows/rworkflows.yml)
<!-- badges: end -->

<br />

## Table of content

   - [Description](#description)
   - [Content](#content)
   - [Versions](#versions)
   - [Installation](#installation)
   - [Licence](#licence)
   - [Citations](#citations)
   - [Credits](#credits)
   - [Acknowledgements](#acknowledgements)

<br />

## Description

Set of R functions for the development of R functions, written according to the [safer project](https://github.com/safer-r) specifications.

<br />

## Content
<br />

| Function | Description |
| :--- | :--- |
| **comp_1d()** | Compare two 1D datasets (vector or factor or 1D table, or 1D matrix or 1D array) of the same class or not. Check and report in a list if the 2 datasets have: 1. same class 2. common elements 3. common element names (except factors) 4. common levels (factors only) |
| **comp_2d()** | Compare two 2D datasets of the same class or not. Check and report in a list if the 2 datasets have: 1. same class 2. same type 3. common row names 4. common column names 5. same row number 6. same column number 7. potential identical rows between the 2 datasets 8. potential identical columns between the 2 datasets |
| **comp_list()** | Compare two lists. Check and report in a list if the 2 datasets have: 1. same length 2. common names 3. common compartments |
| **df_remod()** | If the data frame is made of n numeric columns, a new data frame is created, with the 1st column gathering all the numeric values, and the 2nd column being the name of the columns of the initial data frame. If row names were present in the initial data frame, then a new ini_rowname column is added with the names of the rows. If the data frame is made of one numeric column and one character or factor column, a new data frame is created, with the new columns corresponding to the split numeric values (according to the character column). NA are added a the end of each column to have the same number of rows. |
| **head2()** | As utils::head() but display the left or right head of big 2D objects. |
| **info()** | Provide a broad description of an object. |
| **max2()** | Find and return the maximum value of a set of data. As base::max() but check the arguments with more details. |
| **min2()** | Find and return the minimum value of a set of data. As base::min() but check the arguments with more details.|
| **name_change()** | This function allow to check if a vector of character strings, like column names of a data frame, has elements present in another vector (vector of reserved words or column names of another data frame before merging). |
| **range2()** | Find and return the minimum value and the maximum value of a set of data. As base::range() but check the arguments with more details. |
| **round2()** | Round a vector of values, if decimal, with the desired number of decimal digits after the decimal leading zeros. |
| **sum2()** | Calculate the sum of numeric and logical value. As base::sum() but check the arguments with more details. |
| **tail2()** | As utils::tail() but display the left or right head of big 2D objects. |

<br />

Read `vignette("saferTool")` for more details.

<br />

## Versions

The different *saferTool* releases are tagged [here](https://github.com/safer-r/saferTool/tags).

<br />

## Installation

*saferTool* can be currently be installed from GitHub:

```r
install.packages("remotes")
remotes::install_github("https://github.com/safer-r/saferTool")
```

Older versions can be installed like this:

```r
v <- "v1.0" # desired tag version
remotes::install_github(paste0("https://github.com/safer-r/saferTool/tree/", v))
```

<br />

## Licence

This package can be redistributed and/or modified under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
Distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchandability or fitness for a particular purpose.
See the GNU General Public License for more details at https://www.gnu.org/licenses.

<br />

## Citation

If you are using functions of *saferTool*, please cite: 

> Han Y, Serizay J, Millot GA (2023). _The R saferTool package_.
> <https://github.com/safer-r/saferTool/>.

<br />

## Credits

[Yushi Han](https://github.com/yushiHn/), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France

[Jacques Serizai](https://github.com/js2264), Spatial Regulation of Genomes team, Institut Pasteur, Paris, France

[Gael A. Millot](https://github.com/gael-millot), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France

<br />

## Acknowledgements

The developers & maintainers of the mentioned softwares and packages, including:

- [R](https://www.r-project.org/)
- [Git](https://git-scm.com/)
- [Github](https://github.com/)

