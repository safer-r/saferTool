# Changelog

## saferTool 1.2.0

- BREAKING: the output field names of
  [`comp_1d()`](../reference/comp_1d.md),
  [`comp_2d()`](../reference/comp_2d.md) and
  [`comp_list()`](../reference/comp_list.md) use underscores instead of
  dots (`$same_class`, `$identical_content`, `$same_row_nb`, …).
  Packages reading these fields (saferGG, saferMatrix) require saferTool
  \>= 1.2.0, since builds labelled 1.1.0 may still return the dotted
  names.

## saferTool 1.0.0

- Initial version (backbone v21.0).
