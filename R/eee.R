#' @encoding UTF-8
#' @title Effective Dimensionality Properties
#' @description An object's effective dimensionality is defined as the number of dimensions in which it has multiple populated index positions (i.e., if a dimension has only one index position, that is not an effective dimension).
#' \tabular{ll}{  `'eud', 'EUD'`   \tab *Effectively* `NaN` *dimensional*.                                                                                                                                                                                      \cr
#'                                 \tab Length-`0` objects including `NULL` (of undefined effective dimensionality).                                                                                                                                            \cr   \tab   \cr
#'                `'e0d', 'E0D'`   \tab *Effectively* `0` *dimensional*                                                                                                                                                                                         \cr
#'                                 \tab `1x1` data.frames and length-`1` vectors, \code{\link[=VLS]{vlists}}, and arrays.                                                                                                                          \cr   \tab   \cr
#'                `'e1d', 'E1D'`   \tab *Effectively* `1` *dimensional*                                                                                                                                                                                         \cr
#'                                 \tab \code{\link[=MVC]{multivecs}}, length-`2+` vlists, \code{\link[=ROW]{row}} data.frames, \code{\link[=COL]{column}} data.frames, and length-`2+` arrays with `2+` index positions in exactly `1` dimension. \cr   \tab   \cr
#'                `'e2d', 'E2D'`   \tab *Effectively* `2` *dimensional*                                                                                                                                                                                         \cr
#'                                 \tab `2+ x 2+ `data.frames and length-`4+` arrays with `2+` positions in exactly `2` dimensions.                                                                                                                \cr   \tab   \cr
#'                `'ehd', 'EHD'`   \tab *Effectively hyper dimensional*                                                                                                                                                                            \cr   \tab   \cr
#'                                 \tab Length-`8+` arrays with `2+` positions in `3+` dimensions.                                                                                                                                                                }
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more effective dimensionality properties from `eee_props()`. Effective dimensionality properties may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @examples
#' a. <- "a"
#' abc. <- c("a", "b", "c")
#' udf1 <- NULL
#' udf2 <- character(0)
#' udf3 <- matrix(nrow = 0, ncol = 0)
#' udf4 <- list()
#' zro1 <- a.
#' zro2 <- list(abc = abc.)
#' zro3 <- data.frame(a = a.)
#' zro4 <- array(a., dim = c(1, 1, 1))
#' one1 <- matrix(abc., nrow = 1)
#' one2 <- matrix(abc., ncol = 1)
#' one3 <- data.frame(a. = abc.)
#' one4 <- array(abc., dim = c(3, 1, 1))
#' two1 <- matrix(rep.int(abc., 2), nrow = 2)
#' two2 <- data.frame(a = abc., b = abc.)
#' two3 <- array(1:4, dim = c(2, 2, 1))
#' two4 <- array(1:4, dim = c(1, 2, 2, 1))
#' hyp1 <- array(1:8, dim = c(2, 2, 2))
#' hyp2 <- array(1:8, dim = c(2, 2, 2, 1))
#' hyp3 <- array(1:8, dim = c(1, 2, 2, 2, 1))
#' hyp4 <- array(1:16, dim = c(2, 2, 2, 2))
#'
#' eee_funs()
#' eee_props()
#' is_eee_spec("eUD")
#' is_eee_spec("d1D")
#' c(neee(udf1), neee(udf2), neee(udf3), neee(udf4))
#' c(neee(zro1), neee(zro2), neee(zro3), neee(zro4))
#' c(neee(one1), neee(one2), neee(one3), neee(one4))
#' c(neee(two1), neee(two2), neee(two3), neee(two4))
#' c(neee(hyp1), neee(hyp2), neee(hyp3), neee(hyp4))
#' c(EEE(one1, "eud"), EEE(one1, "e0d"), EEE(one1, "e1d"), EEE(one1, "e2d"), EEE(one1, "ehd"))
#' c(EUD(one1), E0D(one1), E1D(one1), E2D(one1), EHD(one1))
#' c(eee(udf1), eee(zro1), eee(one1), eee(two1), eee(hyp1), eee(hyp4))
#' @export
eee_PROPS <- function() {utils::help("eee_PROPS", package = "ppp")}

#' @describeIn eee_PROPS Lists all effective dimensionality properties possessed by `x`. Returns a lowercase character scalar.
eee <- function(x) {
  y <- NULL
  for (EEE in ppp::eee_funs(EEE)) {if (base::eval(base::parse(text = base::paste0("ppp:::.", EEE, "(x)")))) {y <- base::c(y, base::tolower(EEE))}}
  y
}

#' @describeIn eee_PROPS Lists all effective dimensionality checking functions. Returns a sorted, uppercase, character vector.
#' @export
eee_funs <- function() {base::c("E0D", "E1D", "E2D", "EHD", "EUD")}

#' @describeIn eee_PROPS Lists all effective dimensionality properties. Returns a sorted, lowercase, character vector.
#' @export
eee_props <- function() {base::c("e0d", "e1d", "e2d", "ehd", "eud")}

#' @describeIn eee_PROPS Checks whether `spec` is an effective dimensionality property spec. Returns a logical scalar. See \code{\link{ppp}} for a definition of a property spec.
#' @export
is_eee_spec <- function(spec) {
  spec <- ppp::spec2props(spec)
  if (base::length(spec) == 0) {F} else {base::all(spec %in% ppp::eee_props(eee))}
}

#' @describeIn eee_PROPS Checks whether `x` matches the effective dimensionality property spec in `spec` subject to any count or value restrictions in `...`. Returns a logical scalar. See \code{\link{ppp}} for a definition of a property spec.
#' @export
EEE <- function(x, spec, ...) {
  errs <- ppp::meets_errs(x, ...)
  if (!ppp::is_eee_spec(spec)) {errs <- base::c(errs, "[spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from eee_props().")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, .PKG = 'aj')}
  if (ppp::meets(x, ...)) {
    props <- ppp::spec2props(spec)
    for (prop in base::toupper(props)) {if (base::eval(base::parse(text = base::paste0("ppp:::.", prop, "(x)")))) {return(T)}}
  }
  F
}

#' @describeIn eee_PROPS Checks whether `x` is of zero effective dimensions subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
E0D <- function(x, ...) {ppp::EEE(x, 'e0D', ...)}

#' @describeIn eee_PROPS Checks whether `x` is of one effective dimension subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
E1D <- function(x, ...) {ppp::EEE(x, 'e1D', ...)}

#' @describeIn eee_PROPS Checks whether `x` is of two effective dimensions subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
E2D <- function(x, ...) {ppp::EEE(x, 'e2D', ...)}

#' @describeIn eee_PROPS Checks whether `x` is of three or more effective dimensions subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
EHD <- function(x, ...) {ppp::EEE(x, 'eHD', ...)}

#' @describeIn eee_PROPS Checks whether `x` is of undefined effective dimensionality subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
EUD <- function(x, ...) {ppp::EEE(x, 'eUD', ...)}

#' @describeIn eee_PROPS Gets the effective dimensionality of `x`. Returns either `NaN` or a non-negative integer scalar.
#' @export
neee <- function(x) {
  if (base::length(x) == 0) {NaN}
  else if (base::NCOL(x) * base::NROW(x) == 1) {0}
  else if (base::is.vector(x)) {1}
  else {base::length(base::which(base::dim(x) > 1))}
}
