
# saferTool <a href="https://safer-r.github.io/saferTool">[<img src="man/figures/new_saferTool.png" align="right" height="140" />](https://safer-r.github.io/saferTool/)</a>

<br />

<!-- badges: start -->

[![CRAN status](https://img.shields.io/cran/v/saferTool.svg)](https://cran.r-project.org/package=saferTool)
[![R Version](https://img.shields.io/badge/R-%3E%3D%204.5.0-blue?logo=R)](https://cran.r-project.org/)
[![Downloads](https://cranlogs.r-pkg.org/badges/saferTool)](https://r-pkg.org/pkg/saferTool)<br />
[![Rworkflows](https://github.com/safer-r/saferTool/actions/workflows/rworkflows.yml/badge.svg)](https://github.com/safer-r/saferTool/actions/workflows/rworkflows.yml)
[![Codecov](https://codecov.io/github/safer-r/saferTool/coverage.svg?branch=master)](https://app.codecov.io/github/safer-r/saferTool?branch=master)
[![safer-R Status](https://img.shields.io/badge/Safer--R%20status-backbone%20v20.0-brightgreen)](https://github.com/safer-r/.github/blob/main/profile/backbone.R)<br />
[![](https://img.shields.io/badge/license-GPL3.0-lightgrey.svg)](https://opensource.org/license/gpl-3.0)
<!-- badges: end -->

<br />

## Table of content

   - [Description](#description)
   - [Content](#content)
   - [Versions](#versions)
   - [Installation](#installation)
   - [Docker](#docker-image)
   - [Licence](#licence)
   - [Citations](#citations)
   - [Credits](#credits)
   - [acknowledgments](#acknowledgments)


<br />

## Description

Set of R functions for the development of R functions, written according to the [safer-R project](https://github.com/safer-r) specifications.

<br />

## Content
<br />

| Function | Description |
| :--- | :--- |
| **comp_1d()** | Compare two 1D datasets (vector or factor or 1D table, or 1D matrix or 1D array) of the same class or not. Check and report in a list if the 2 datasets have: 1. same class 2. common elements 3. common element names (except factors) 4. common levels (factors only) |
| **comp_2d()** | Compare two 2D datasets of the same class or not. Check and report in a list if the 2 datasets have: 1. same class 2. same type 3. common row names 4. common column names 5. same row number 6. same column number 7. potential identical rows between the 2 datasets 8. potential identical columns between the 2 datasets |
| **comp_list()** | Compare two lists. Check and report in a list if the 2 datasets have: same length, common names, common compartments, same order, and if identical or identical in content. |
| **df_remod()** | If the data frame is made of n numeric columns, a new data frame is created, with the 1st column gathering all the numeric values, and the 2nd column being the name of the columns of the initial data frame. If row names were present in the initial data frame, then a new ini_rowname column is added with the names of the rows. If the data frame is made of one numeric column and one character or factor column, a new data frame is created, with the new columns corresponding to the split numeric values (according to the character column). NA are added a the end of each column to have the same number of rows. |
| **head2()** | As utils::head() but display the left or right head of big 2D objects. |
| **info()** | Provide the class, the type, the length, the head, the tail and other info of an object in a list (does not print, except the optional truncation warning). |
| **max2()** | As base::max() but accepting logical values. |
| **min2()** | As base::min() but accepting logical values. |
| **name_change()** | This function allow to check if a vector of character strings, like column names of a data frame, has elements present in another vector (vector of reserved words or column names of another data frame before merging). |
| **range2()** | As base::range() but accepting logical values. |
| **round2()** | Round a vector of values, if decimal, with the desired number of decimal digits after the decimal leading zeros. |
| **sample2()** | As base::sample() but with a corrected behavior when x is made of a single positive numeric value (the single value is sampled as itself, instead of being used as the size of the population 1:x), and a set_seed argument that restores the initial .Random.seed state after execution. |
| **sum2()** | As base::sum() but accepting logical values. |
| **tail2()** | As utils::tail() but display the left or right tail of big 2D objects. |

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
remotes::install_github("https://github.com/safer-r/saferTool") # or open R as admin and remotes::install_github("https://github.com/safer-r/saferTool", lib = "C:/Program Files/R/R-4.5.2/library")
```

Older versions can be installed like this:

```r
v <- "v1.0.0" # desired tag version
remotes::install_github(paste0("https://github.com/safer-r/saferTool/tree/", v))
```

<br />

## Licence

This package can be redistributed and/or modified under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
Distributed in the hope that it will be useful, but without any warranty; without even the implied warranty of merchandability or fitness for a particular purpose.
See the GNU General Public License for more details at https://www.gnu.org/licenses or in the Licence.txt attached file.

<br />

## Citation

If you are using functions of *saferTool*, please cite: 

> Wang H, Han Y, Serizay J, Millot GA (2023). _The R saferTool package_.
> <https://github.com/safer-r/saferTool/>.

<br />

## Credits

[Haiding Wang](https://github.com/Tintin2710), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France

[Yushi Han](https://github.com/yushiHn/), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France

[Mia Legras](https://github.com/mialgrs), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France

[Jacques Serizay](https://github.com/js2264), Spatial Regulation of Genomes team, Institut Pasteur, Paris, France

[Gael A. Millot](https://github.com/gael-millot), Bioinformatics and Biostatistics Hub, Institut Pasteur, Paris, France

<br />

## Acknowledgements

The developers & maintainers of the mentioned softwares and packages, including:

- [R](https://www.r-project.org/)
- [Git](https://git-scm.com/)
- [Github](https://github.com/)



