#' @encoding UTF-8
#' @family utils
#' @title Split a Property Combos into a Vector of Single Properties
#' @description Splits a single property combo (underscore separated) into its constituent single properties.
#' @param x A character scalar.
#' @return Either `NULL` or a character vector (returns `NULL` when `x` is not of mode `'character'` or is of length zero).
#' @export
combo2props <- function(x) {
  if (base::is.character(x)) {
    if (base::length(ppp::spec2combos(x)) == 1) {
      x <- ppp::av(base::strsplit(x, "_", fixed = T))
      x <- x[x != ""]
      if (base::length(x) > 0) {
        if (base::any(base::is.na(x))) {NULL} else if (base::any(base::nchar(x) != 3)) {NULL} else {base::unique(base::tolower(x))}
      } else {NULL}
    } else {NULL}
  } else {NULL}
}
