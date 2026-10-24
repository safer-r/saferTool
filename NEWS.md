# saferTool 1.2.0

* BREAKING: the output field names of `comp_1d()`, `comp_2d()` and `comp_list()` use underscores instead of dots (`$same_class`, `$identical_content`, `$same_row_nb`, ...). Packages reading these fields (saferGG, saferMatrix) require saferTool >= 1.2.0, since builds labelled 1.1.0 may still return the dotted names.

# saferTool 1.0.0

* Initial version (backbone v21.0).
