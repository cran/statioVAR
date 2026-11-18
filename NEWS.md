# statioVAR NEWS

## Version 0.1.0

* Initial release:
  - `detrender()`: Within-person linear detrending for multilevel VAR analysis.
  - `pooled()`: Pooled polynomial detrending for multivariate panel data.
* Console summaries (the `cat()` calls) are only shown in interactive sessions.
* Added a CITATION file to cite the package properly.

## Version 0.1.1

* Title now in title case, CITATION switched to `bibentry()`.
* Added inst/WORDLIST.
* Removed Unicode symbols.

## Version 0.1.2

* Fixed DESCRIPTION wording.
* Added references.

## Version 0.1.3

* Clarified in `detrender()` description the risk of Nickell-type small-T bias, and recommended T>=10.
* Added in `detrender()` description the K+4 rule for VAR(1).
* Added references in `detrender()`.
* Replaced 'clusters' with 'units' in docs and output; return element renamed to `n_units` (was `n_clusters`). 

## Version 0.1.4

* `detrender()`: the default `alpha` is now 1, so every estimated within-unit linear trend is removed by default.
* `detrender()`: added a Warning section to the documentation on Nickell-type bias, minimum series length, and the low power of significance-based trend removal in short series.
* `pooled()`: `alpha` now accepts the value 1, which retains all estimated terms. The default remains 0.05.
