# saferTool 2.0.0

* Migrated to the safer-r backbone v20.0: every function now follows the saferDev 2.0.0 structure, with arg_check() v2.0.0 semantics (exact class/typeof/mode comparison; class = c("matrix", "array") for matrices, mode = "numeric" preferred for numeric arguments).
* All functions carry the mandatory lib_path, safer_check and error_text reserved arguments.
* Bug fixes found during the migration: stale error blocks in comp_1d, comp_2d and df_remod that referenced undefined objects (crashed with "object 'function.name' not found" instead of printing the intended message).
* Complete test suite rewritten in the saferDev style (tests/testthat), including a test-mandatory_args.R replicating the saferDev pattern; code coverage brought to 100% with # nocov markers on unreachable internal-error branches.
* Orphaned functions removed from the documentation (comp_list, df_mod, head2, info, max2, min2, range2, sum2, tail2 had no R code anymore).

# saferTool 1.0.0

* Initial version (backbone v19.0).
