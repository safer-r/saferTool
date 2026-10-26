# saferTool 1.2.0

* BREAKING: the output field names of `comp_1d()`, `comp_2d()` and `comp_list()` use underscores instead of dots (`$same_class`, `$identical_content`, `$same_row_nb`, ...). Packages reading these fields (saferGG, saferMatrix) require saferTool >= 1.2.0, since builds labelled 1.1.0 may still return the dotted names.
* DESCRIPTION: Description rewritten for saferTool (it was the description of saferDev), BugReports added, maintainer roles aut and cre, saferDev (>= 3.0.0) and R (>= 4.6.0) required (saferDev 3.0.0 requires R >= 4.6.0 and returns the object_name output component used here).
* Documentation: lists written with \itemize, error examples placed in \dontrun{} instead of commented out, examples written with the saferTool:: prefix and lines under 100 characters, @details of the base function rewritings condensed, authors written as in saferDev.

# saferTool 1.0.0

* Initial version (backbone v21.0).
