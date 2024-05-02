#' @encoding UTF-8
#' @family props
#' @title Combo Completeness Plus Extended Mode Plus Extended Class Combination Properties
#' @description Combo completeness plus extended mode plus extended class combination properties are the combination of \link[=UNQ]{completeness}, a single \link[=mmm]{extended mode} property, and a single \link[=ccc]{extended class} property.
#' @param x An R object.
#' @param mmm A character scalar single extended mode property from \code{\link{mmm_props}()}.
#' @param ccc A character scalar single extended class property from \code{\link{ccc_props}()}.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @examples
#' cmp_mmm_ccc_funs()
#' cmp_mmm_ccc(letters, "ch1", "vec")
#' cmp_mmm_ccc(letters, "str", "scl")
#' cmp_ch1_vec(letters)
#' cmp_str_scl("a")
#' @export
cmp_mmm_ccc_PROPS <- function() {utils::help("cmp_mmm_ccc_PROPS", package = "ppp")}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for extended mode `mmm`, and for extended class `ccc` subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_mmm_ccc <- function(x, mmm, ccc, ...) {
  if (ppp::mmm_ccc(x, mmm, ccc, ...)) {
    if      (ccc == "dtf") {base::nrow(x) == base::nrow(base::unique(x))}
    else if (ccc == "vls") {
      n  <- base::lengths(x)
      nu <- base::sapply(x, function(x) {base::length(base::unique(x))})
      base::all(n == nu)
    } else {base::length(x) == base::length(base::unique(x))}
  } else {F}
}

#' @describeIn cmp_mmm_ccc_PROPS Lists all combo completeness plus extended mode plus extended class property checking functions. Returns a character vector.
#' @export
cmp_mmm_ccc_funs <- function() {
  x <- base::expand.grid(mmm = ppp::mmm_props(), ccc = ppp::ccc_props())
  x <- base::apply(x, 1, paste0, collapse = "_")
  base::paste0('cmp_', base::sort(ppp::av(x)))
}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for atomicness-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_atm_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'atm', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for atomicness-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_atm_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'atm', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for atomicness-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_atm_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'atm', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for atomicness-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_atm_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'atm', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for atomicness-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_atm_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'atm', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for atomicness-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_atm_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'atm', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for atomicness-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_atm_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'atm', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for atomicness-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_atm_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'atm', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for onechar-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ch1_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ch1', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for onechar-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ch1_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ch1', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for onechar-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ch1_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ch1', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for onechar-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ch1_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ch1', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for onechar-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ch1_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ch1', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for onechar-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ch1_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ch1', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for onechar-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ch1_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ch1', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for onechar-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ch1_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ch1', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for threechar-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ch3_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ch3', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for threechar-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ch3_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ch3', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for threechar-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ch3_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ch3', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for threechar-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ch3_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ch3', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for threechar-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ch3_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ch3', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for threechar-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ch3_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ch3', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for threechar-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ch3_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ch3', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for threechar-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ch3_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ch3', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for character-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_chr_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'chr', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for character-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_chr_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'chr', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for character-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_chr_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'chr', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for character-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_chr_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'chr', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for character-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_chr_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'chr', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for character-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_chr_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'chr', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for character-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_chr_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'chr', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for character-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_chr_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'chr', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for color-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_clr_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'clr', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for color-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_clr_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'clr', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for color-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_clr_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'clr', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for color-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_clr_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'clr', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for color-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_clr_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'clr', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for color-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_clr_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'clr', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for color-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_clr_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'clr', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for color-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_clr_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'clr', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for even-whole-number-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_evn_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'evn', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for even-whole-number-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_evn_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'evn', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for even-whole-number-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_evn_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'evn', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for even-whole-number-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_evn_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'evn', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for even-whole-number-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_evn_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'evn', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for even-whole-number-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_evn_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'evn', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for even-whole-number-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_evn_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'evn', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for even-whole-number-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_evn_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'evn', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for factor-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_fac_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'fac', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for factor-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_fac_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'fac', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for factor-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_fac_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'fac', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for factor-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_fac_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'fac', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for factor-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_fac_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'fac', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for factor-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_fac_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'fac', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for factor-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_fac_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'fac', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for factor-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_fac_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'fac', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for fractional-numeric-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_frc_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'frc', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for fractional-numeric-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_frc_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'frc', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for fractional-numeric-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_frc_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'frc', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for fractional-numeric-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_frc_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'frc', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for fractional-numeric-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_frc_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'frc', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for fractional-numeric-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_frc_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'frc', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for fractional-numeric-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_frc_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'frc', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for fractional-numeric-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_frc_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'frc', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for indexer-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ind_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ind', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for indexer-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ind_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ind', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for indexer-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ind_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ind', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for indexer-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ind_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ind', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for indexer-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ind_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ind', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for indexer-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ind_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ind', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for indexer-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ind_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ind', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for indexer-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ind_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ind', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for logical-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_lgl_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'lgl', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for -logicalness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_lgl_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'lgl', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for logical-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_lgl_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'lgl', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for logical-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_lgl_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'lgl', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for logical-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_lgl_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'lgl', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for logical-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_lgl_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'lgl', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for logical-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_lgl_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'lgl', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for logical-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_lgl_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'lgl', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for negative-numeric-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_neg_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'neg', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for negative-numeric-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_neg_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'neg', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for negative-numeric-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_neg_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'neg', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for negative-numeric-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_neg_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'neg', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for negative-numeric-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_neg_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'neg', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for negative-numeric-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_neg_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'neg', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for negative-numeric-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_neg_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'neg', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for negative-numeric-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_neg_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'neg', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for negative-whole-number-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ngw_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ngw', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for negative-whole-number-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ngw_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ngw', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for negative-whole-number-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ngw_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ngw', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for negative-whole-number-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ngw_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ngw', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for negative-whole-number-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ngw_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ngw', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for negative-whole-number-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ngw_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ngw', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for negative-whole-number-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ngw_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ngw', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for negative-whole-number-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ngw_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ngw', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-negative-numeric-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nng_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nng', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-negative-numeric-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nng_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nng', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-negative-numeric-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nng_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nng', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-negative-numeric-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nng_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nng', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-negative-numeric-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nng_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nng', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-negative-numeric-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nng_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nng', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-negative-numeric-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nng_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nng', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-negative-numeric-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nng_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nng', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-negative-whole-number-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nnw_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nnw', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-negative-whole-number-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nnw_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nnw', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-negative-whole-number-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nnw_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nnw', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-negative-whole-number-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nnw_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nnw', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-negative-whole-number-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nnw_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nnw', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-negative-whole-number-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nnw_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nnw', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-negative-whole-number-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nnw_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nnw', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-negative-whole-number-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nnw_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nnw', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-positive-numeric-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nps_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nps', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-positive-numeric-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nps_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nps', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-positive-numeric-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nps_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nps', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-positive-numeric-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nps_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nps', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-positive-numeric-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nps_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nps', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-positive-numeric-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nps_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nps', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-positive-numeric-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nps_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nps', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-positive-numeric-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nps_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nps', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-positive-whole-number-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_npw_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'npw', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-positive-whole-number-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_npw_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'npw', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-positive-whole-number-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_npw_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'npw', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-positive-whole-number-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_npw_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'npw', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-positive-whole-number-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_npw_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'npw', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-positive-whole-number-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_npw_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'npw', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-positive-whole-number-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_npw_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'npw', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for non-positive-whole-number-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_npw_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'npw', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for nonsortable-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nst_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nst', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for nonsortable-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nst_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nst', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for nonsortable-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nst_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nst', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for nonsortable-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nst_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nst', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for nonsortable-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nst_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nst', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for nonsortable-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nst_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nst', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for nonsortable-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nst_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nst', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for nonsortable-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_nst_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'nst', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for numeric-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_num_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'num', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for numeric-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_num_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'num', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for numeric-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_num_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'num', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for numeric-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_num_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'num', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for numeric-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_num_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'num', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for numeric-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_num_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'num', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for numeric-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_num_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'num', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for numeric-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_num_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'num', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for odd-whole-number-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_odd_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'odd', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for odd-whole-number-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_odd_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'odd', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for odd-whole-number-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_odd_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'odd', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for odd-whole-number-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_odd_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'odd', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for odd-whole-number-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_odd_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'odd', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for odd-whole-number-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_odd_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'odd', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for odd-whole-number-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_odd_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'odd', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for odd-whole-number-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_odd_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'odd', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for ordered-factor-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ord_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ord', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for ordered-factor-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ord_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ord', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for ordered-factor-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ord_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ord', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for ordered-factor-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ord_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ord', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for ordered-factor-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ord_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ord', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for ordered-factor-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ord_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ord', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for -ordered-factorness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ord_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ord', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for ordered-factor-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ord_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ord', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for percent-valued-numeric-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_pct_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'pct', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for percent-valued-numeric-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_pct_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'pct', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for percent-valued-numeric-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_pct_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'pct', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for percent-valued-numeric-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_pct_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'pct', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for percent-valued-numeric-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_pct_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'pct', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for percent-valued-numeric-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_pct_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'pct', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for percent-valued-numeric-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_pct_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'pct', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for percent-valued-numeric-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_pct_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'pct', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for positive-numeric-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_pos_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'pos', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for positive-numeric-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_pos_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'pos', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for positive-numeric-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_pos_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'pos', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for positive-numeric-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_pos_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'pos', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for positive-numeric-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_pos_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'pos', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for positive-numeric-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_pos_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'pos', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for positive-numeric-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_pos_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'pos', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for positive-numeric-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_pos_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'pos', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for proportion-valued-numeric-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ppn_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ppn', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for proportion-valued-numeric-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ppn_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ppn', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for proportion-valued-numeric-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ppn_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ppn', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for proportion-valued-numeric-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ppn_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ppn', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for proportion-valued-numeric-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ppn_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ppn', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for proportion-valued-numeric-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ppn_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ppn', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for proportion-valued-numeric-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ppn_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ppn', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for proportion-valued-numeric-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_ppn_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'ppn', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for positive-whole-number-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_psw_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'psw', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for positive-whole-number-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_psw_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'psw', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for positive-whole-number-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_psw_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'psw', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for positive-whole-number-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_psw_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'psw', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for positive-whole-number-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_psw_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'psw', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for positive-whole-number-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_psw_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'psw', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for positive-whole-number-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_psw_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'psw', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for positive-whole-number-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_psw_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'psw', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for sortable-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_srt_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'srt', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for sortable-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_srt_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'srt', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for sortable-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_srt_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'srt', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for sortable-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_srt_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'srt', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for sortable-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_srt_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'srt', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for sortable-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_srt_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'srt', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for sortable-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_srt_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'srt', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for sortable-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_srt_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'srt', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for string-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_str_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'str', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for string-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_str_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'str', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for string-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_str_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'str', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for string-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_str_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'str', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for string-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_str_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'str', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for string-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_str_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'str', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for string-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_str_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'str', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for string-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_str_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'str', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for unordered-factor-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_uno_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'uno', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for unordered-factor-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_uno_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'uno', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for unordered-factor-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_uno_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'uno', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for unordered-factor-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_uno_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'uno', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for unordered-factor-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_uno_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'uno', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for unordered-factor-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_uno_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'uno', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for unordered-factor-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_uno_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'uno', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for unordered-factor-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_uno_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'uno', 'vls', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for whole-number-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_whl_arr <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'whl', 'arr', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for whole-number-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_whl_dtf <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'whl', 'dtf', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for whole-number-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_whl_gen <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'whl', 'gen', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for whole-number-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_whl_mat <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'whl', 'mat', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for whole-number-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_whl_mvc <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'whl', 'mvc', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for whole-number-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_whl_scl <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'whl', 'scl', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for whole-number-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_whl_vec <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'whl', 'vec', ...)}

#' @describeIn cmp_mmm_ccc_PROPS Checks `x` for completeness, for whole-number-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
cmp_whl_vls <- function(x, ...) {ppp::cmp_mmm_ccc(x, 'whl', 'vls', ...)}
