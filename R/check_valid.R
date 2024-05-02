#' @encoding UTF-8
#' @family utils
#' @title Check `x` Against valid Values and Against a Test
#' @param x An object.
#' @param test A non-missing logical scalar.
#' @param valid Either `NULL` or an atomic vector of valid values.
#' @param return A logical scalar.
#' @export
check_valid <- function(x, test, valid) {
  if (test) {
    x <- ppp::av(x)
    valid <- ppp::av(valid)
    if (base::length(valid) == 0) {T} else {base::all(x %in% valid)}
  } else {F}
}
