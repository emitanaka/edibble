## Release summary

This update makes a number of bug fixes, addition of new features and 
quality of life improvements as shown below.

Bug fixes

* Bug fix for export_design when no record factor exists
* Bug fix for order assignment

Quality of life improvements

* Format change for the title page in the export
* Change behaviour of `fct_attrs()` when levels supplied as numeric or vector 
  instead of `lvls()`. 
* Change the print out of edibble table.
* Improve the assignment algorithm.

New features 

* Added new functions `count_by()` and `split_by()`.
* Added ability to specify conditional treatment.
* The `simuluate_rcrds()` has now a facelift with delineation of the process specification to `simulate_process()`. 
* `autofill_rcrds()` implemented.
* Added ability to add two designs by `+`.
* Ability to add metadata through design().

## R CMD check results

0 errors | 0 warnings | 0 note

R CMD check succeeded
