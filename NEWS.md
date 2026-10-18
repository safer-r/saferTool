# saferTool 1.1.0

* 8 new functions migrated to the safer-r backbone v20.0 and now part of the package: comp_list(), head2(), info(), max2(), min2(), range2(), sum2() and tail2(). All follow the saferDev 2.0.0 structure (arg_check() v2.0.0 semantics, reserved lib_path/safer_check/error_text arguments, # nocov markers on unreachable internal-error branches).
* sample2: new function. As base::sample() but with a corrected behavior when x is made of a single positive numeric value (the single value is sampled as itself, instead of being used as the size of the population 1:x: sample2(x = 4, size = 3, replace = TRUE) returns 4 4 4 and sample2(x = 4, size = 3, replace = FALSE) returns an error), and a set_seed argument that applies base::set.seed() before sampling while restoring the initial .Random.seed state of the user after execution. The dedicated test file replicates the saferDev patterns, and the vignette article vignettes/articles/sample2.Rmd has been added.
* Dotted argument names renamed to underscores (na.rm -> na_rm, warn.print -> warn_print).
* comp_list: same.order bug fix (kept NULL when no compartment is in common, instead of a spurious FALSE).
* info: $COLUM_NAMES typo fixed into $COL_NAMES; environments, functions and symbols are accepted as data input again (dev behavior restored, consistent with saferDev::arg_check()).
* head2/tail2: 1D objects (arrays with a single dim, and any non 2D object) dispatched to utils::head()/utils::tail(); the 2D branch keeps the 2D nature of the output with drop = FALSE; n < 1 now returns an explicit error.
* Each new function has its dedicated test file (tests/testthat/test-*.R replicating the saferDev patterns) and its vignette article (vignettes/articles/).
* test-mandatory_args.R extended to the 8 new functions and to sample2().

# saferTool 1.0.0

* Migrated to the safer-r backbone v20.0: every function now follows the saferDev 2.0.0 structure, with arg_check() v2.0.0 semantics (exact class/typeof/mode comparison; class = c("matrix", "array") for matrices, mode = "numeric" preferred for numeric arguments).
* All functions carry the mandatory lib_path, safer_check and error_text reserved arguments.
* Bug fixes found during the migration: stale error blocks in comp_1d, comp_2d and df_remod that referenced undefined objects (crashed with "object 'function.name' not found" instead of printing the intended message).
* Complete test suite rewritten in the saferDev style (tests/testthat), including a test-mandatory_args.R replicating the saferDev pattern; code coverage brought to 100% with # nocov markers on unreachable internal-error branches.
* Orphaned functions removed from the documentation (comp_list, df_mod, head2, info, max2, min2, range2, sum2, tail2 had no R code anymore).

# saferTool 1.0.0

* Initial version (backbone v19.0).
