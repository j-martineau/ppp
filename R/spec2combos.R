#' @encoding UTF-8
#' @family utils
#' @title Convert a Property Spec to a Vector of Property Combos
#' @description Splits a property spec along pipes to place each single property or property combo in a separate element (property combos are multiple properties separated by underscores).
#' @param x A character vector or scalar.
#' @return A character vector or `NULL` (returns `NULL` when `x` is not of mode `'character'` or is of length zero).
#' @export
spec2_combos <- function(x) {
  if (base::is.character(x)) {
    x <- ppp::av(base::strsplit(x, "|", fixed = T))
    x <- x[x != ""]
    if (base::length(x) > 0) {base::unique(base::tolower(x))} else {NULL}
  } else {NULL}
}
