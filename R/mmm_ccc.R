#' @encoding UTF-8
#' @family props
#' @title Combo Extended Mode Plus Extended Class Combination Properties
#' @description Combo extended mode plus extended class combination properties are the combination of a single \link[=mmm]{extended mode} property, and a single \link[=ccc]{extended class} property.
#' @param x An R object.
#' @param mmm A character scalar single extended mode property from \code{\link{mmm_props}()}.
#' @param ccc A character scalar single extended class property from \code{\link{ccc_props}()}.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @examples
#' mmm_ccc_funs()
#' mmm_ccc(letters, "ch1", "vec")
#' mmm_ccc(letters, "str", "scl")
#' ch1_vec(letters)
#' str_scl("a")
#' @export
mmm_ccc_PROPS <- function() {utils::help("mmm_ccc_PROPS", package = "ppp")}

#' @describeIn mmm_ccc_PROPS Checks `x` for extended mode `mmm` and for extended class `ccc` subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
mmm_ccc <- function(x, mmm, ccc, ...) {
  if (base::is.character(mmm)) {mmm <- base::tolower(mmm)}
  if (base::is.character(ccc)) {ccc <- base::tolower(ccc)}
  errMMM <- "[mmm] is not a scalar value from mmm_props()."
  errCCC <- "[ccc] is not a scalar value from ccc_props()."
  errs <- ppp::meets_errs(x, ...)
  if (base::length(mmm) != 1) {errs <- base::c(errs, errMMM)} else if (!(mmm %in% ppp::mmm_props())) {errs <- base::c(errs, errMMM)}
  if (base::length(ccc) != 1) {errs <- base::c(errs, errCCC)} else if (!(ccc %in% ppp::ccc_props())) {errs <- base::c(errs, errCCC)}
  if (!base::is.null(errs)) {ppp::stopperr(errs, .PKG = "ppp")}
  if (ppp::bbb_ccc(x, "atm", ccc, ...)) {
    if      (ppp::.VLS(x)) {base::all(base::sapply(x,    ppp::MMM(x, mmm, ...)))}
    else if (ppp::.DTF(x)) {base::all(base::apply( x, 2, ppp::MMM(x, mmm, ...)))}
    else                   {                             ppp::MMM(x, mmm, ...)  }
  } else {F}
}

#' @describeIn mmm_ccc_PROPS Lists all combo extended mode plus extended class property checking functions. Returns a character vector.
#' @export
mmm_ccc_funs <- function() {
  x <- base::expand.grid(mmm = ppp::mmm_props(), ccc = ppp::ccc_props())
  x <- base::apply(x, 1, paste0, collapse = "_")
  base::paste0('', base::sort(ppp::av(x)))
}

#' @describeIn mmm_ccc_PROPS Checks `x` for onechar-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ch1_arr <- function(x, ...) {ppp::mmm_ccc(x, 'ch1', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for onechar-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ch1_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'ch1', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for onechar-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ch1_gen <- function(x, ...) {ppp::mmm_ccc(x, 'ch1', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for onechar-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ch1_mat <- function(x, ...) {ppp::mmm_ccc(x, 'ch1', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for onechar-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ch1_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'ch1', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for onechar-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ch1_scl <- function(x, ...) {ppp::mmm_ccc(x, 'ch1', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for onechar-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ch1_vec <- function(x, ...) {ppp::mmm_ccc(x, 'ch1', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for onechar-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ch1_vls <- function(x, ...) {ppp::mmm_ccc(x, 'ch1', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for threechar-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ch3_arr <- function(x, ...) {ppp::mmm_ccc(x, 'ch3', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for threechar-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ch3_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'ch3', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for threechar-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ch3_gen <- function(x, ...) {ppp::mmm_ccc(x, 'ch3', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for threechar-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ch3_mat <- function(x, ...) {ppp::mmm_ccc(x, 'ch3', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for threechar-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ch3_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'ch3', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for threechar-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ch3_scl <- function(x, ...) {ppp::mmm_ccc(x, 'ch3', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for threechar-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ch3_vec <- function(x, ...) {ppp::mmm_ccc(x, 'ch3', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for threechar-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ch3_vls <- function(x, ...) {ppp::mmm_ccc(x, 'ch3', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for character-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
chr_arr <- function(x, ...) {ppp::mmm_ccc(x, 'chr', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for character-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
chr_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'chr', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for character-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
chr_gen <- function(x, ...) {ppp::mmm_ccc(x, 'chr', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for character-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
chr_mat <- function(x, ...) {ppp::mmm_ccc(x, 'chr', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for character-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
chr_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'chr', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for character-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
chr_scl <- function(x, ...) {ppp::mmm_ccc(x, 'chr', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for character-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
chr_vec <- function(x, ...) {ppp::mmm_ccc(x, 'chr', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for character-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
chr_vls <- function(x, ...) {ppp::mmm_ccc(x, 'chr', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for color-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
clr_arr <- function(x, ...) {ppp::mmm_ccc(x, 'clr', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for color-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
clr_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'clr', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for color-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
clr_gen <- function(x, ...) {ppp::mmm_ccc(x, 'clr', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for color-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
clr_mat <- function(x, ...) {ppp::mmm_ccc(x, 'clr', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for color-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
clr_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'clr', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for color-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
clr_scl <- function(x, ...) {ppp::mmm_ccc(x, 'clr', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for color-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
clr_vec <- function(x, ...) {ppp::mmm_ccc(x, 'clr', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for color-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
clr_vls <- function(x, ...) {ppp::mmm_ccc(x, 'clr', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for even-whole-number-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
evn_arr <- function(x, ...) {ppp::mmm_ccc(x, 'evn', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for even-whole-number-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
evn_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'evn', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for even-whole-number-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
evn_gen <- function(x, ...) {ppp::mmm_ccc(x, 'evn', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for even-whole-number-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
evn_mat <- function(x, ...) {ppp::mmm_ccc(x, 'evn', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for even-whole-number-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
evn_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'evn', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for even-whole-number-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
evn_scl <- function(x, ...) {ppp::mmm_ccc(x, 'evn', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for even-whole-number-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
evn_vec <- function(x, ...) {ppp::mmm_ccc(x, 'evn', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for even-whole-number-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
evn_vls <- function(x, ...) {ppp::mmm_ccc(x, 'evn', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for factor-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
fac_arr <- function(x, ...) {ppp::mmm_ccc(x, 'fac', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for factor-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
fac_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'fac', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for factor-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
fac_gen <- function(x, ...) {ppp::mmm_ccc(x, 'fac', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for factor-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
fac_mat <- function(x, ...) {ppp::mmm_ccc(x, 'fac', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for factor-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
fac_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'fac', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for factor-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
fac_scl <- function(x, ...) {ppp::mmm_ccc(x, 'fac', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for factor-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
fac_vec <- function(x, ...) {ppp::mmm_ccc(x, 'fac', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for factor-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
fac_vls <- function(x, ...) {ppp::mmm_ccc(x, 'fac', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for fractional-numeric-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
frc_arr <- function(x, ...) {ppp::mmm_ccc(x, 'frc', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for fractional-numeric-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
frc_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'frc', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for fractional-numeric-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
frc_gen <- function(x, ...) {ppp::mmm_ccc(x, 'frc', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for fractional-numeric-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
frc_mat <- function(x, ...) {ppp::mmm_ccc(x, 'frc', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for fractional-numeric-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
frc_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'frc', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for fractional-numeric-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
frc_scl <- function(x, ...) {ppp::mmm_ccc(x, 'frc', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for fractional-numeric-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
frc_vec <- function(x, ...) {ppp::mmm_ccc(x, 'frc', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for fractional-numeric-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
frc_vls <- function(x, ...) {ppp::mmm_ccc(x, 'frc', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for indexer-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ind_arr <- function(x, ...) {ppp::mmm_ccc(x, 'ind', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for indexer-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ind_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'ind', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for indexer-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ind_gen <- function(x, ...) {ppp::mmm_ccc(x, 'ind', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for indexer-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ind_mat <- function(x, ...) {ppp::mmm_ccc(x, 'ind', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for indexer-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ind_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'ind', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for indexer-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ind_scl <- function(x, ...) {ppp::mmm_ccc(x, 'ind', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for indexer-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ind_vec <- function(x, ...) {ppp::mmm_ccc(x, 'ind', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for indexer-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ind_vls <- function(x, ...) {ppp::mmm_ccc(x, 'ind', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for logical-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
lgl_arr <- function(x, ...) {ppp::mmm_ccc(x, 'lgl', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for -logicalness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
lgl_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'lgl', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for logical-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
lgl_gen <- function(x, ...) {ppp::mmm_ccc(x, 'lgl', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for logical-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
lgl_mat <- function(x, ...) {ppp::mmm_ccc(x, 'lgl', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for logical-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
lgl_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'lgl', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for logical-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
lgl_scl <- function(x, ...) {ppp::mmm_ccc(x, 'lgl', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for logical-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
lgl_vec <- function(x, ...) {ppp::mmm_ccc(x, 'lgl', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for logical-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
lgl_vls <- function(x, ...) {ppp::mmm_ccc(x, 'lgl', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for negative-numeric-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
neg_arr <- function(x, ...) {ppp::mmm_ccc(x, 'neg', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for negative-numeric-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
neg_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'neg', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for negative-numeric-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
neg_gen <- function(x, ...) {ppp::mmm_ccc(x, 'neg', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for negative-numeric-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
neg_mat <- function(x, ...) {ppp::mmm_ccc(x, 'neg', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for negative-numeric-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
neg_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'neg', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for negative-numeric-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
neg_scl <- function(x, ...) {ppp::mmm_ccc(x, 'neg', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for negative-numeric-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
neg_vec <- function(x, ...) {ppp::mmm_ccc(x, 'neg', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for negative-numeric-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
neg_vls <- function(x, ...) {ppp::mmm_ccc(x, 'neg', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for negative-whole-number-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ngw_arr <- function(x, ...) {ppp::mmm_ccc(x, 'ngw', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for negative-whole-number-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ngw_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'ngw', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for negative-whole-number-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ngw_gen <- function(x, ...) {ppp::mmm_ccc(x, 'ngw', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for negative-whole-number-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ngw_mat <- function(x, ...) {ppp::mmm_ccc(x, 'ngw', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for negative-whole-number-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ngw_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'ngw', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for negative-whole-number-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ngw_scl <- function(x, ...) {ppp::mmm_ccc(x, 'ngw', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for negative-whole-number-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ngw_vec <- function(x, ...) {ppp::mmm_ccc(x, 'ngw', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for negative-whole-number-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ngw_vls <- function(x, ...) {ppp::mmm_ccc(x, 'ngw', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-negative-numeric-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nng_arr <- function(x, ...) {ppp::mmm_ccc(x, 'nng', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-negative-numeric-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nng_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'nng', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-negative-numeric-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nng_gen <- function(x, ...) {ppp::mmm_ccc(x, 'nng', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-negative-numeric-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nng_mat <- function(x, ...) {ppp::mmm_ccc(x, 'nng', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-negative-numeric-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nng_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'nng', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-negative-numeric-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nng_scl <- function(x, ...) {ppp::mmm_ccc(x, 'nng', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-negative-numeric-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nng_vec <- function(x, ...) {ppp::mmm_ccc(x, 'nng', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-negative-numeric-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nng_vls <- function(x, ...) {ppp::mmm_ccc(x, 'nng', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-negative-whole-number-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nnw_arr <- function(x, ...) {ppp::mmm_ccc(x, 'nnw', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-negative-whole-number-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nnw_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'nnw', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-negative-whole-number-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nnw_gen <- function(x, ...) {ppp::mmm_ccc(x, 'nnw', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-negative-whole-number-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nnw_mat <- function(x, ...) {ppp::mmm_ccc(x, 'nnw', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-negative-whole-number-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nnw_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'nnw', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-negative-whole-number-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nnw_scl <- function(x, ...) {ppp::mmm_ccc(x, 'nnw', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-negative-whole-number-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nnw_vec <- function(x, ...) {ppp::mmm_ccc(x, 'nnw', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-negative-whole-number-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nnw_vls <- function(x, ...) {ppp::mmm_ccc(x, 'nnw', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-positive-numeric-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nps_arr <- function(x, ...) {ppp::mmm_ccc(x, 'nps', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-positive-numeric-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nps_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'nps', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-positive-numeric-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nps_gen <- function(x, ...) {ppp::mmm_ccc(x, 'nps', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-positive-numeric-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nps_mat <- function(x, ...) {ppp::mmm_ccc(x, 'nps', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-positive-numeric-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nps_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'nps', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-positive-numeric-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nps_scl <- function(x, ...) {ppp::mmm_ccc(x, 'nps', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-positive-numeric-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nps_vec <- function(x, ...) {ppp::mmm_ccc(x, 'nps', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-positive-numeric-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nps_vls <- function(x, ...) {ppp::mmm_ccc(x, 'nps', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-positive-whole-number-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
npw_arr <- function(x, ...) {ppp::mmm_ccc(x, 'npw', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-positive-whole-number-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
npw_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'npw', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-positive-whole-number-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
npw_gen <- function(x, ...) {ppp::mmm_ccc(x, 'npw', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-positive-whole-number-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
npw_mat <- function(x, ...) {ppp::mmm_ccc(x, 'npw', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-positive-whole-number-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
npw_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'npw', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-positive-whole-number-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
npw_scl <- function(x, ...) {ppp::mmm_ccc(x, 'npw', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-positive-whole-number-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
npw_vec <- function(x, ...) {ppp::mmm_ccc(x, 'npw', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for non-positive-whole-number-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
npw_vls <- function(x, ...) {ppp::mmm_ccc(x, 'npw', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for nonsortable-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nst_arr <- function(x, ...) {ppp::mmm_ccc(x, 'nst', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for nonsortable-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nst_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'nst', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for nonsortable-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nst_gen <- function(x, ...) {ppp::mmm_ccc(x, 'nst', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for nonsortable-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nst_mat <- function(x, ...) {ppp::mmm_ccc(x, 'nst', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for nonsortable-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nst_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'nst', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for nonsortable-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nst_scl <- function(x, ...) {ppp::mmm_ccc(x, 'nst', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for nonsortable-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nst_vec <- function(x, ...) {ppp::mmm_ccc(x, 'nst', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for nonsortable-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
nst_vls <- function(x, ...) {ppp::mmm_ccc(x, 'nst', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for numeric-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
num_arr <- function(x, ...) {ppp::mmm_ccc(x, 'num', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for numeric-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
num_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'num', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for numeric-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
num_gen <- function(x, ...) {ppp::mmm_ccc(x, 'num', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for numeric-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
num_mat <- function(x, ...) {ppp::mmm_ccc(x, 'num', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for numeric-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
num_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'num', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for numeric-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
num_scl <- function(x, ...) {ppp::mmm_ccc(x, 'num', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for numeric-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
num_vec <- function(x, ...) {ppp::mmm_ccc(x, 'num', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for numeric-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
num_vls <- function(x, ...) {ppp::mmm_ccc(x, 'num', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for odd-whole-number-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
odd_arr <- function(x, ...) {ppp::mmm_ccc(x, 'odd', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for odd-whole-number-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
odd_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'odd', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for odd-whole-number-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
odd_gen <- function(x, ...) {ppp::mmm_ccc(x, 'odd', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for odd-whole-number-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
odd_mat <- function(x, ...) {ppp::mmm_ccc(x, 'odd', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for odd-whole-number-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
odd_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'odd', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for odd-whole-number-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
odd_scl <- function(x, ...) {ppp::mmm_ccc(x, 'odd', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for odd-whole-number-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
odd_vec <- function(x, ...) {ppp::mmm_ccc(x, 'odd', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for odd-whole-number-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
odd_vls <- function(x, ...) {ppp::mmm_ccc(x, 'odd', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for ordered-factor-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ord_arr <- function(x, ...) {ppp::mmm_ccc(x, 'ord', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for ordered-factor-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ord_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'ord', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for ordered-factor-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ord_gen <- function(x, ...) {ppp::mmm_ccc(x, 'ord', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for ordered-factor-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ord_mat <- function(x, ...) {ppp::mmm_ccc(x, 'ord', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for ordered-factor-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ord_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'ord', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for ordered-factor-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ord_scl <- function(x, ...) {ppp::mmm_ccc(x, 'ord', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for -ordered-factorness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ord_vec <- function(x, ...) {ppp::mmm_ccc(x, 'ord', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for ordered-factor-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ord_vls <- function(x, ...) {ppp::mmm_ccc(x, 'ord', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for percent-valued-numeric-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
pct_arr <- function(x, ...) {ppp::mmm_ccc(x, 'pct', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for percent-valued-numeric-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
pct_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'pct', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for percent-valued-numeric-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
pct_gen <- function(x, ...) {ppp::mmm_ccc(x, 'pct', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for percent-valued-numeric-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
pct_mat <- function(x, ...) {ppp::mmm_ccc(x, 'pct', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for percent-valued-numeric-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
pct_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'pct', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for percent-valued-numeric-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
pct_scl <- function(x, ...) {ppp::mmm_ccc(x, 'pct', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for percent-valued-numeric-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
pct_vec <- function(x, ...) {ppp::mmm_ccc(x, 'pct', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for percent-valued-numeric-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
pct_vls <- function(x, ...) {ppp::mmm_ccc(x, 'pct', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for positive-numeric-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
pos_arr <- function(x, ...) {ppp::mmm_ccc(x, 'pos', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for positive-numeric-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
pos_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'pos', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for positive-numeric-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
pos_gen <- function(x, ...) {ppp::mmm_ccc(x, 'pos', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for positive-numeric-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
pos_mat <- function(x, ...) {ppp::mmm_ccc(x, 'pos', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for positive-numeric-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
pos_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'pos', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for positive-numeric-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
pos_scl <- function(x, ...) {ppp::mmm_ccc(x, 'pos', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for positive-numeric-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
pos_vec <- function(x, ...) {ppp::mmm_ccc(x, 'pos', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for positive-numeric-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
pos_vls <- function(x, ...) {ppp::mmm_ccc(x, 'pos', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for proportion-valued-numeric-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ppn_arr <- function(x, ...) {ppp::mmm_ccc(x, 'ppn', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for proportion-valued-numeric-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ppn_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'ppn', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for proportion-valued-numeric-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ppn_gen <- function(x, ...) {ppp::mmm_ccc(x, 'ppn', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for proportion-valued-numeric-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ppn_mat <- function(x, ...) {ppp::mmm_ccc(x, 'ppn', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for proportion-valued-numeric-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ppn_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'ppn', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for proportion-valued-numeric-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ppn_scl <- function(x, ...) {ppp::mmm_ccc(x, 'ppn', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for proportion-valued-numeric-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ppn_vec <- function(x, ...) {ppp::mmm_ccc(x, 'ppn', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for proportion-valued-numeric-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
ppn_vls <- function(x, ...) {ppp::mmm_ccc(x, 'ppn', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for positive-whole-number-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
psw_arr <- function(x, ...) {ppp::mmm_ccc(x, 'psw', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for positive-whole-number-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
psw_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'psw', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for positive-whole-number-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
psw_gen <- function(x, ...) {ppp::mmm_ccc(x, 'psw', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for positive-whole-number-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
psw_mat <- function(x, ...) {ppp::mmm_ccc(x, 'psw', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for positive-whole-number-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
psw_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'psw', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for positive-whole-number-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
psw_scl <- function(x, ...) {ppp::mmm_ccc(x, 'psw', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for positive-whole-number-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
psw_vec <- function(x, ...) {ppp::mmm_ccc(x, 'psw', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for positive-whole-number-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
psw_vls <- function(x, ...) {ppp::mmm_ccc(x, 'psw', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for sortable-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
srt_arr <- function(x, ...) {ppp::mmm_ccc(x, 'srt', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for sortable-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
srt_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'srt', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for sortable-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
srt_gen <- function(x, ...) {ppp::mmm_ccc(x, 'srt', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for sortable-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
srt_mat <- function(x, ...) {ppp::mmm_ccc(x, 'srt', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for sortable-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
srt_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'srt', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for sortable-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
srt_scl <- function(x, ...) {ppp::mmm_ccc(x, 'srt', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for sortable-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
srt_vec <- function(x, ...) {ppp::mmm_ccc(x, 'srt', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for sortable-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
srt_vls <- function(x, ...) {ppp::mmm_ccc(x, 'srt', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for string-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
str_arr <- function(x, ...) {ppp::mmm_ccc(x, 'str', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for string-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
str_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'str', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for string-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
str_gen <- function(x, ...) {ppp::mmm_ccc(x, 'str', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for string-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
str_mat <- function(x, ...) {ppp::mmm_ccc(x, 'str', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for string-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
str_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'str', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for string-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
str_scl <- function(x, ...) {ppp::mmm_ccc(x, 'str', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for string-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
str_vec <- function(x, ...) {ppp::mmm_ccc(x, 'str', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for string-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
str_vls <- function(x, ...) {ppp::mmm_ccc(x, 'str', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for unordered-factor-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
uno_arr <- function(x, ...) {ppp::mmm_ccc(x, 'uno', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for unordered-factor-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
uno_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'uno', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for unordered-factor-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
uno_gen <- function(x, ...) {ppp::mmm_ccc(x, 'uno', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for unordered-factor-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
uno_mat <- function(x, ...) {ppp::mmm_ccc(x, 'uno', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for unordered-factor-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
uno_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'uno', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for unordered-factor-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
uno_scl <- function(x, ...) {ppp::mmm_ccc(x, 'uno', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for unordered-factor-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
uno_vec <- function(x, ...) {ppp::mmm_ccc(x, 'uno', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for unordered-factor-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
uno_vls <- function(x, ...) {ppp::mmm_ccc(x, 'uno', 'vls', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for whole-number-ness and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
whl_arr <- function(x, ...) {ppp::mmm_ccc(x, 'whl', 'arr', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for whole-number-ness and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
whl_dtf <- function(x, ...) {ppp::mmm_ccc(x, 'whl', 'dtf', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for whole-number-ness and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
whl_gen <- function(x, ...) {ppp::mmm_ccc(x, 'whl', 'gen', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for whole-number-ness and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
whl_mat <- function(x, ...) {ppp::mmm_ccc(x, 'whl', 'mat', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for whole-number-ness and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
whl_mvc <- function(x, ...) {ppp::mmm_ccc(x, 'whl', 'mvc', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for whole-number-ness and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
whl_scl <- function(x, ...) {ppp::mmm_ccc(x, 'whl', 'scl', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for whole-number-ness and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
whl_vec <- function(x, ...) {ppp::mmm_ccc(x, 'whl', 'vec', ...)}

#' @describeIn mmm_ccc_PROPS Checks `x` for whole-number-ness and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
whl_vls <- function(x, ...) {ppp::mmm_ccc(x, 'whl', 'vls', ...)}
