#' @encoding UTF-8
#' @title Combined Completeness Plus Extended Mode Properties
#' @description Check for combination of \link[=CMP]{completeness} and \link[=mmm]{extended mode}.
#' @param x An R object.
#' @param .mmm A character scalar extended mode property from `mmm_props()`.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @examples
#' cmp_mmm_funs()
#' cmp_mmm(letters, "ch1")
#' cmp_mmm(c("abc", "def"), "ch3")
#' cmp_psw(0:10)
#' cmp_ord(factor(letters, ordered = T))
#' @export
cmp_mmm_PROPS <- function() {utils::help("cmp_mmm_PROPS", package = "ppp")}

#' @describeIn cmp_mmm_PROPS Check `x` for completeness and for the extended mode in `.mmm` subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_mmm <- function(x, .mmm, ...) {
  Errors <- ppp::meets_errs(x, ...)
  if (!ppp::.cmp_chr_scl(.mmm, .valid = base::c(ppp::mmm_funs(), ppp::mmm_props()))) {Errors <- base::c(Errors, '[.mmm] is not a scalar value from mmm_props().')}
  if (!base::is.null(Errors)) {ppp::stopperr(Errors, .PKG = "ppp")}
  if (!ppp::meets(x, ...)) {F}
  else if (!base::is.atomic(x) | base::length(x) == 0) {F}
  else if (base::any(base::is.na(x))) {F}
  else {base::eval(base::parse(text = base::paste0("ppp::.", base::toupper(.mmm), "(x)")))}
}

#' @describeIn cmp_mmm_PROPS Lists completeness plus extended mode property checking functions. Returns a character vector.
#' @export
cmp_mmm_funs <- function() {base::paste0('cmp_', ppp::mmm_funs())}

#' @describeIn cmp_mmm_PROPS Checks for completeness and atomic-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_atm <- function(x, ...) {if (base::is.atomic(x) & base::length(x) > 0) {!base::any(base::is.na(x))} else {F}}

#' @describeIn cmp_mmm_PROPS Checks for completeness and onechar-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_ch1 <- function(x, ...) {ppp::cmp_mmm(x, 'ch1', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and threechar-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_ch3 <- function(x, ...) {ppp::cmp_mmm(x, 'ch3', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and character-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_chr <- function(x, ...) {ppp::cmp_mmm(x, 'chr', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and color-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_clr <- function(x, ...) {ppp::cmp_mmm(x, 'clr', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and whole-number-even-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_evn <- function(x, ...) {ppp::cmp_mmm(x, 'evn', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and factor-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_fac <- function(x, ...) {ppp::cmp_mmm(x, 'fac', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and fractional-numeric-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_frc <- function(x, ...) {ppp::cmp_mmm(x, 'frc', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and indexer-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_ind <- function(x, ...) {ppp::cmp_mmm(x, 'ind', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and logical-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_lgl <- function(x, ...) {ppp::cmp_mmm(x, 'lgl', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and negative-numeric-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_neg <- function(x, ...) {ppp::cmp_mmm(x, 'neg', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and negative-whole-number-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_ngw <- function(x, ...) {ppp::cmp_mmm(x, 'ngw', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and non-negative-numeric-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_nng <- function(x, ...) {ppp::cmp_mmm(x, 'nng', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and non-negative-whole-number-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_nnw <- function(x, ...) {ppp::cmp_mmm(x, 'nnw', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and non-positive-numeric-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_nps <- function(x, ...) {ppp::cmp_mmm(x, 'nps', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and non-positive-whole-number-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_npw <- function(x, ...) {ppp::cmp_mmm(x, 'npw', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and non-sortable-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_nst <- function(x, ...) {ppp::cmp_mmm(x, 'nst', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and numeric-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_num <- function(x, ...) {ppp::cmp_mmm(x, 'num', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and whole-numer-odd-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_odd <- function(x, ...) {ppp::cmp_mmm(x, 'odd', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and ordered-factor-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_ord <- function(x, ...) {ppp::cmp_mmm(x, 'ord', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and percent-valued-numeric-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_pct <- function(x, ...) {ppp::cmp_mmm(x, 'pct', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and positive-numeric-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_pos <- function(x, ...) {ppp::cmp_mmm(x, 'pos', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and positive-whole-number-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_psw <- function(x, ...) {ppp::cmp_mmm(x, 'psw', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and sortable-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_srt <- function(x, ...) {ppp::cmp_mmm(x, 'srt', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and string-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_str <- function(x, ...) {ppp::cmp_mmm(x, 'str', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and unordered-factor-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_uno <- function(x, ...) {ppp::cmp_mmm(x, 'uno', ...)}

#' @describeIn cmp_mmm_PROPS Checks for completeness and whole-number-ness subject to any count or value restrictions in .... Returns a logical scalar.
#' @export
cmp_whl <- function(x, ...) {ppp::cmp_mmm(x, 'whl', ...)}
