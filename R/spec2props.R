#' @encoding UTF-8
#' @family utils
#' @title Convert a Properties Specification Character Scalar or Vector to a Character Vector of Single Properties
#' @description Splits each element of `x` along both `|` and `_` to create a character vector of single constituent
#'   properties, removes any blank values, reduces the result to unique values, converts all remaining values to
#'   lowercase, and sorts the remaining values.
#' @param x A character vector
#' @return Either `NULL` or a non-empty character vector of unique, sorted, lowercase, 3-character values.
#'
#'   Returns `NULL` if any of the following criteria are met:
#'   \itemize{
#'     \item `x` is not of mode `'character'`.
#'     \item Any resulting value is `NA`.
#'     \item Any resulting value is not a 3-character string.
#'     \item `x` resolves to a zero-length character vector.
#'   }
#' @export
spec2props <- function(x) {
  if (base::is.character(x)) {
    x <- ppp::av(base::strsplit(x, "|", fixed = T))
    x <- ppp::av(base::strsplit(x, "_", fixed = T))
    x <- x[x != ""]
    if (base::length(x) > 0) {
      if (base::any(base::is.na(x))) {NULL} else if (base::any(base::nchar(x) != 3)) {NULL} else {base::unique(base::tolower(x))}
    } else {NULL}
  } else {NULL}
}
