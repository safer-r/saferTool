# saferTool 1.2.0

* Backbone v21.1: the invented `x` argument names (absent from the called function signature, for instance `base::any(x = ...)`, `base::all(x = ...)`, `base::sum(x = ...)`, `base::order(x = ...)`) are removed from all the function calls.
* Backbone v21.2: the lib_path argument is now checked whatever the value of safer_check (the `if(safer_check == TRUE){` and `}` lines around the check of lib_path are commented, because lib_path is used outside the safer_check checkings, for instance in the `saferDev::arg_check()` calls): an incorrect lib_path now returns an error even with `safer_check = FALSE`. The documentation of the safer_check argument no longer lists the lib_path check, and the documentation of the lib_path argument no longer says that lib_path is ignored if safer_check is FALSE.
* BREAKING: the output field names of `comp_1d()`, `comp_2d()` and `comp_list()` use underscores instead of dots (`$same_class`, `$identical_content`, `$same_row_nb`, ...). Packages reading these fields (saferGG, saferMatrix) require saferTool >= 1.2.0, since builds labelled 1.1.0 may still return the dotted names.
* DESCRIPTION: Description rewritten for saferTool (it was the description of saferDev), BugReports added, maintainer roles aut and cre, saferDev (>= 3.0.0) and R (>= 4.6.0) required (saferDev 3.0.0 requires R >= 4.6.0 and returns the object_name output component used here).
* Documentation: lists written with \itemize, error examples placed in \dontrun{} instead of commented out, examples written with the saferTool:: prefix and lines under 100 characters, @details of the base function rewritings condensed, authors written as in saferDev.

# saferTool 1.0.0

* Initial version (backbone v21.0).
