#' @encoding UTF-8
#' @title Combo Uniqueness and Extended Mode Properties
#' @description Functions for checking combinations of \link[=UNQ]{uniqueness} and \link[=mmm]{extended mode} subject to any count or value restrictions in `...`.
#' @param x An R object.
#' @param .mmm A character scalar extended mode property from \code{\link{mmm_props}()}`.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @examples
#' UnqClr <- c("red", "blue", "#FFFFFF")
#' UnqCh3 <- c("atm", "ch1", "ch3")
#' UnqCh1 <- c("a", "b", "c")
#' UnqAtm <- c("", "8", "a")
#' UnqChr <- c("", " ", "a")
#' UnqFac <- factor(UnqAtm)
#' UnqPct <- c(0, 50, 100)
#' UnqEvn <- c(-2, 0, 2)
#' UnqWhl <- c(-1, 0, 1)
#' UnqIn1 <- c(T, F)
#' UnqPsw <- 1:3
#' UnqNnw <- 0:2
#'
#' UnqLgl <-  UnqIn1
#' UnqIn2 <-  UnqPsw
#' UnqSrt <-  UnqNum
#' UnqStr <-  UnqCh1
#' UnqOdd <-  UnqEvn + 1
#' UnqPos <-  UnqPsw / 2
#' UnqNum <-  UnqOdd / 2
#' UnqNeg <- -UnqPsw / 2
#' UnqPsw <- -UnqNnw
#' UnqNst <-  factor(UnqEvn, ordered = F)
#' UnqUno <-  factor(UnqEvn, ordered = T)
#'
#' unq_mm_funs()
#' unq_mmm(UnqAtm, "atm")
#' unq_mmm(UnqWhl, "ch1")
#' c(unq_atm(UnqAtm), unq_atm(NULL))
#' c(unq_ch1(UnqCh1), unq_ch1(UnqCh3), unq_ch1(c(UnqCh1, UnqCh1))
#' c(unq_ch3(UnqCh3), unq_ch3(UnqCh1), unq_ch3(c(UnqCh3, UnqCh3))
#' c(unq_chr(UnqChr), unq_chr(UnqNum), unq_chr(c(UnqChr, UnqChr))
#' c(unq_clr(UnqClr), unq_clr(UnqCh1), unq_clr(c(UnqClr, UnqClr))
#' c(unq_evn(UnqEvn), unq_evn(UnqOdd), unq_evn(c(UnqEvn, UnqEvn))
#' c(unq_fac(UnqFac), unq_fac(UnqStr), unq_fac(c(UnqFac, UnqFac))
#' c(unq_frc(UnqFrc), unq_frc(UnqWhl), unq_frc(c(UnqFrc, UnqFrc))
#' c(unq_ind(UnqIn1), unq_ind(UnqNum), unq_ind(c(UnqIn1, UnqIn1))
#' c(unq_ind(UnqIn2), unq_ind(UnqFrc), unq_ind(c(UnqIn2, UnqIn2))
#' c(unq_lgl(UnqLgl), unq_lgl(UnqCh1), unq_lgl(c(UnqLgl, UnqLgl))
#' c(unq_neg(UnqNeg), unq_neg(UnqNps), unq_neg(c(UnqNeg, UnqNeg))
#' c(unq_neg(UnqNgw), unq_ngw(UnqNeg), unq_ngw(c(UnqNgw, UnqNgw))
#' c(unq_nng(UnqNng), unq_nng(UnqNps), unq_nng(c(UnqNng, UnqNng))
#' c(unq_nnw(UnqNnw), unq_nnw(UnqNng), unq_nnw(c(UnqNnw, UnqNnw))
#' c(unq_nps(UnqNps), unq_nps(UnqNeg), unq_npw(c(UnqNps, UnqNps))
#' c(unq_npw(UnqNpw), unq_npw(UnqNps), unq_npw(c(UnqNpw, UnqNpw))
#' c(unq_nst(UnqNst), unq_nst(UnqSrt), unq_nst(c(UnqNst, UnqNst))
#' c(unq_num(UnqNum), unq_num(UnqCh1), unq_num(c(UnqNum, UnqNum))
#' c(unq_odd(UnqOdd), unq_odd(UnqEvn), unq_odd(c(UnqOdd, UnqOdd))
#' c(unq_ord(UnqOrd), unq_ord(UnqUno), unq_ord(c(UnqOrd, UnqOrd))
#' c(unq_pct(UnqPct), unq_pct(UnqNeg), unq_pct(c(UnqPct, UnqPct))
#' c(unq_pos(UnqPos), unq_pos(UnqNng), unq_pos(c(UnqPos, UnqPos))
#' c(unq_ppn(UnqPpn), unq_ppn(UnqPct), unq_ppn(c(UnqPpn, UnqPpn))
#' c(unq_psw(UnqPsw), unq_psw(UnqPos), unq_psw(c(UnqPsw, UnqPsw))
#' c(unq_srt(UnqSrt), unq_srt(UnqNst), unq_srt(c(UnqSrt, UnqSrt))
#' c(unq_str(UnqStr), unq_str(UnqChr), unq_str(c(UnqStr, UnqStr))
#' c(unq_uno(UnqUno), unq_uno(UnqOrd), unq_uno(c(UnqUno, UnqUno))
#' c(unq_whl(UnqWhl), unq_whl(UnqNum), unq_whl(c(UnqWhl, UnqWhl))
#' @export
unq_mmm_PROPS <- function() {utils::help("unq_mmm_PROPS", package = "ppp")}

#' @describeIn unq_mmm_PROPS Checks `x` for completened and against the extended mode in `.mmm` subject to any count and/or value restrictions in `.mmm`. Returns a logical scalar.
#' @export
unq_mmm <- function(x, .mmm, ...) {if (ppp::cmp_mmm(x, .mmm, ...)) {base::length(x) == base::length(base::unique(x))} else {F}}

#' @describeIn unq_mmm_PROPS Lists all combo uniqueness plus extended mode property checking functions. Returns a character vector.
#' @export
unq_mmm_funs <- function() {base::paste0('unq_', ppp::mmm_funs())}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for atomic-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_atm <- function(x, ...) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {base::length(x) == base::length(base::unique(x))} else {F}
  } else {F}
}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for onechar-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ch1 <- function(x, ...) {ppp::unq_mmm(x, 'ch1', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for threechar-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ch3 <- function(x, ...) {ppp::unq_mmm(x, 'ch3', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for character-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_chr <- function(x, ...) {ppp::unq_mmm(x, 'chr', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for color-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_clr <- function(x, ...) {ppp::unq_mmm(x, 'clr', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for even-whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_evn <- function(x, ...) {ppp::unq_mmm(x, 'evn', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for factor-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_fac <- function(x, ...) {ppp::unq_mmm(x, 'fac', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for fractional-whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_frc <- function(x, ...) {ppp::unq_mmm(x, 'frc', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for indexer-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ind <- function(x, ...) {ppp::unq_mmm(x, 'ind', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for logical-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_lgl <- function(x, ...) {ppp::unq_mmm(x, 'lgl', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for negative-numeric-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_neg <- function(x, ...) {ppp::unq_mmm(x, 'neg', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for negative-whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ngw <- function(x, ...) {ppp::unq_mmm(x, 'ngw', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for non-negative-numeric-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nng <- function(x, ...) {ppp::unq_mmm(x, 'nng', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for non-negative-whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nnw <- function(x, ...) {ppp::unq_mmm(x, 'nnw', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for non-positive-numeric-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nps <- function(x, ...) {ppp::unq_mmm(x, 'nps', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for non-positive-whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_npw <- function(x, ...) {ppp::unq_mmm(x, 'npw', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for non-sortable-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nst <- function(x, ...) {ppp::unq_mmm(x, 'nst', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for odd-whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_odd <- function(x, ...) {ppp::unq_mmm(x, 'odd', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for ordered-factor-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ord <- function(x, ...) {ppp::unq_mmm(x, 'ord', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for percent-valued-numeric-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_pct <- function(x, ...) {ppp::unq_mmm(x, 'pct', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for positive-numeric-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_pos <- function(x, ...) {ppp::unq_mmm(x, 'pos', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for proportion-value-numeric-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ppn <- function(x, ...) {ppp::unq_mmm(x, 'ppn', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for positive-whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_psw <- function(x, ...) {ppp::unq_mmm(x, 'psw', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for sortable-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_srt <- function(x, ...) {ppp::unq_mmm(x, 'srt', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for string-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_str <- function(x, ...) {ppp::unq_mmm(x, 'str', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for unordered-factor-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_uno <- function(x, ...) {ppp::unq_mmm(x, 'uno', ...)}

#' @describeIn unq_mmm_PROPS Checks `x` for uniqueness and for whole-number-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_whl <- function(x, ...) {ppp::unq_mmm(x, 'whl', ...)}
