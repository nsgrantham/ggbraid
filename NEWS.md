# ggbraid 0.2.3

* Fix error when using `geom_braid(..., na.rm = FALSE)` in combination with `facet_grid()`.

* Fix error in `compute_layer()` that may arise when using `geom_braid(..., na.rm = NA)`.

* Fix HTML validation problems.

* Convert vignettes to articles so they appear on the pkgdown website but are not included in the package itself.

* Minor README and article edits.


# ggbraid 0.2.2

* Add documentation for datasets `temps` and `hoops`.

* Fix warnings and notes from R CMD check.


# ggbraid 0.2.1

* Fix problem where braiding fails when `x` is non-numeric.


# ggbraid 0.2.0

* Handle `NA`s with `na.rm = NA` (the default), `na.rm = FALSE`, and 
  `na.rm = TRUE`.

* Add support for flipped aesthetics `y`, `xmin`, `xmax`.

* Add support for `after_stat()` — use `fill = after_stat(braid)`.

* Update documentation on `stat_braid()` and `geom_braid()` with examples.

* Add "US Supreme Court" vignette and finish "NBA Finals Game" vignette.


# ggbraid 0.1.0

* Add `stat_braid()` and `geom_braid()`.

* Add "Average Daily Temperatures" and "NBA Finals Game" vignettes.

