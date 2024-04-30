#' @encoding UTF-8
#' @family utils
#' @title Flatten `...` Into an Atomic Vector of the Consituent Atomic Elements of `...`
#' @param ... Objects to be flattened to an attomic vector.
#' @return An atomic vector (or `NULL`)
#' @export
av <- function(...) {
  x <- base::unlist(base::list(...), use.names = F)
  attributes(x) <- NULL
  x
}
