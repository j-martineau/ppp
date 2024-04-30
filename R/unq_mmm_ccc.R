#' @encoding UTF-8
#' @family props
#' @title Combo Uniqueness Plus Extended Mode Plus Extended Class Combination Properties
#' @description Combo uniqueness plus extended mode plus extended class combination properties are the combination of \link[=UNQ]{uniqueness}, a single \link[=mmm]{extended mode} property, and a single \link[=ccc]{extended class} property.
#' @param x An R object.
#' @param .mmm A character scalar single extended mode property from \code{\link{mmm_props}()}.
#' @param .ccc A character scalar single extended class property from \code{\link{ccc_props}()}.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @examples
#' unq_mmm_ccc_funs()
#' unq_mmm_ccc(letters, "ch1", "vec")
#' unq_mmm_ccc(letters, "str", "scl")
#' unq_ch1_vec(letters)
#' unq_str_scl("a")
#' @export
unq_mmm_ccc_PROPS <- function() {utils::help("unq_mmm_ccc_PROPS", package = "ppp")}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for extended mode `.mmm`, and for extended class `.ccc` subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
unq_mmm_ccc <- function(x, .mmm, .ccc, ...) {
  if (ppp::cmp_mmm_ccc(x, .mmm, .ccc, ...)) {
    if      (.ccc == "dtf") {base::nrow(x) == base::nrow(base::unique(x))}
    else if (.ccc == "vls") {
      N  <- base::lengths(x)
      NU <- base::sapply(x, function(x) {base::length(base::unique(x))})
      base::all(N == NU)
    } else {base::length(x) == base::length(base::unique(x))}
  } else {F}
}

#' @describeIn unq_mmm_ccc_PROPS Lists all combo uniqueness plus extended mode plus extended class property checking functions. Returns a character vector.
#' @export
unq_mmm_ccc_funs <- function() {
  x <- base::expand.grid(mmm = ppp::mmm_props(), ccc = ppp::ccc_props())
  x <- base::apply(x, 1, paste0, collapse = "_")
  base::paste0('unq_', base::sort(ppp::av(x)))
}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for atomicness-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_atm_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'atm', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for atomicness-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_atm_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'atm', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for atomicness-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_atm_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'atm', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for atomicness-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_atm_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'atm', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for atomicness-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_atm_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'atm', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for atomicness-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_atm_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'atm', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for atomicness-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_atm_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'atm', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for atomicness-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_atm_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'atm', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for onechar-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ch1_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ch1', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for onechar-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ch1_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ch1', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for onechar-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ch1_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ch1', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for onechar-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ch1_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ch1', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for onechar-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ch1_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ch1', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for onechar-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ch1_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ch1', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for onechar-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ch1_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ch1', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for onechar-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ch1_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ch1', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for threechar-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ch3_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ch3', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for threechar-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ch3_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ch3', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for threechar-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ch3_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ch3', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for threechar-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ch3_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ch3', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for threechar-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ch3_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ch3', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for threechar-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ch3_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ch3', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for threechar-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ch3_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ch3', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for threechar-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ch3_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ch3', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for character-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_chr_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'chr', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for character-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_chr_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'chr', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for character-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_chr_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'chr', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for character-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_chr_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'chr', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for character-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_chr_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'chr', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for character-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_chr_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'chr', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for character-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_chr_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'chr', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for character-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_chr_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'chr', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for color-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_clr_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'clr', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for color-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_clr_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'clr', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for color-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_clr_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'clr', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for color-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_clr_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'clr', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for color-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_clr_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'clr', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for color-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_clr_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'clr', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for color-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_clr_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'clr', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for color-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_clr_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'clr', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for even-whole-number-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_evn_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'evn', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for even-whole-number-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_evn_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'evn', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for even-whole-number-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_evn_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'evn', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for even-whole-number-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_evn_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'evn', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for even-whole-number-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_evn_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'evn', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for even-whole-number-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_evn_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'evn', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for even-whole-number-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_evn_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'evn', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for even-whole-number-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_evn_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'evn', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for factor-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_fac_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'fac', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for factor-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_fac_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'fac', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for factor-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_fac_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'fac', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for factor-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_fac_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'fac', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for factor-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_fac_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'fac', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for factor-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_fac_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'fac', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for factor-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_fac_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'fac', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for factor-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_fac_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'fac', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for fractional-numeric-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_frc_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'frc', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for fractional-numeric-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_frc_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'frc', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for fractional-numeric-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_frc_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'frc', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for fractional-numeric-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_frc_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'frc', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for fractional-numeric-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_frc_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'frc', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for fractional-numeric-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_frc_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'frc', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for fractional-numeric-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_frc_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'frc', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for fractional-numeric-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_frc_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'frc', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for indexer-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ind_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ind', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for indexer-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ind_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ind', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for indexer-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ind_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ind', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for indexer-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ind_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ind', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for indexer-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ind_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ind', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for indexer-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ind_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ind', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for indexer-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ind_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ind', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for indexer-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ind_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ind', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for logical-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_lgl_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'lgl', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for -logicalness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_lgl_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'lgl', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for logical-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_lgl_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'lgl', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for logical-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_lgl_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'lgl', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for logical-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_lgl_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'lgl', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for logical-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_lgl_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'lgl', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for logical-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_lgl_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'lgl', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for logical-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_lgl_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'lgl', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for negative-numeric-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_neg_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'neg', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for negative-numeric-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_neg_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'neg', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for negative-numeric-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_neg_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'neg', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for negative-numeric-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_neg_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'neg', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for negative-numeric-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_neg_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'neg', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for negative-numeric-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_neg_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'neg', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for negative-numeric-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_neg_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'neg', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for negative-numeric-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_neg_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'neg', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for negative-whole-number-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ngw_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ngw', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for negative-whole-number-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ngw_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ngw', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for negative-whole-number-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ngw_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ngw', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for negative-whole-number-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ngw_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ngw', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for negative-whole-number-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ngw_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ngw', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for negative-whole-number-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ngw_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ngw', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for negative-whole-number-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ngw_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ngw', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for negative-whole-number-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ngw_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ngw', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-negative-numeric-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nng_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nng', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-negative-numeric-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nng_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nng', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-negative-numeric-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nng_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nng', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-negative-numeric-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nng_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nng', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-negative-numeric-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nng_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nng', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-negative-numeric-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nng_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nng', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-negative-numeric-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nng_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nng', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-negative-numeric-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nng_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nng', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-negative-whole-number-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nnw_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nnw', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-negative-whole-number-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nnw_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nnw', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-negative-whole-number-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nnw_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nnw', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-negative-whole-number-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nnw_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nnw', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-negative-whole-number-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nnw_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nnw', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-negative-whole-number-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nnw_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nnw', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-negative-whole-number-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nnw_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nnw', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-negative-whole-number-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nnw_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nnw', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-positive-numeric-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nps_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nps', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-positive-numeric-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nps_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nps', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-positive-numeric-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nps_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nps', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-positive-numeric-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nps_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nps', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-positive-numeric-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nps_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nps', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-positive-numeric-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nps_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nps', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-positive-numeric-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nps_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nps', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-positive-numeric-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nps_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nps', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-positive-whole-number-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_npw_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'npw', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-positive-whole-number-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_npw_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'npw', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-positive-whole-number-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_npw_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'npw', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-positive-whole-number-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_npw_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'npw', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-positive-whole-number-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_npw_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'npw', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-positive-whole-number-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_npw_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'npw', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-positive-whole-number-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_npw_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'npw', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for non-positive-whole-number-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_npw_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'npw', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for nonsortable-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nst_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nst', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for nonsortable-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nst_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nst', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for nonsortable-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nst_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nst', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for nonsortable-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nst_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nst', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for nonsortable-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nst_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nst', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for nonsortable-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nst_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nst', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for nonsortable-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nst_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nst', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for nonsortable-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_nst_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'nst', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for numeric-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_num_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'num', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for numeric-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_num_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'num', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for numeric-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_num_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'num', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for numeric-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_num_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'num', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for numeric-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_num_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'num', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for numeric-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_num_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'num', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for numeric-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_num_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'num', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for numeric-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_num_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'num', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for odd-whole-number-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_odd_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'odd', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for odd-whole-number-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_odd_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'odd', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for odd-whole-number-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_odd_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'odd', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for odd-whole-number-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_odd_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'odd', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for odd-whole-number-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_odd_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'odd', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for odd-whole-number-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_odd_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'odd', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for odd-whole-number-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_odd_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'odd', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for odd-whole-number-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_odd_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'odd', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for ordered-factor-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ord_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ord', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for ordered-factor-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ord_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ord', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for ordered-factor-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ord_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ord', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for ordered-factor-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ord_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ord', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for ordered-factor-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ord_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ord', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for ordered-factor-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ord_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ord', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for -ordered-factorness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ord_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ord', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for ordered-factor-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ord_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ord', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for percent-valued-numeric-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_pct_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'pct', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for percent-valued-numeric-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_pct_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'pct', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for percent-valued-numeric-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_pct_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'pct', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for percent-valued-numeric-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_pct_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'pct', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for percent-valued-numeric-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_pct_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'pct', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for percent-valued-numeric-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_pct_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'pct', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for percent-valued-numeric-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_pct_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'pct', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for percent-valued-numeric-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_pct_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'pct', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for positive-numeric-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_pos_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'pos', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for positive-numeric-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_pos_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'pos', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for positive-numeric-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_pos_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'pos', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for positive-numeric-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_pos_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'pos', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for positive-numeric-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_pos_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'pos', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for positive-numeric-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_pos_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'pos', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for positive-numeric-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_pos_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'pos', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for positive-numeric-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_pos_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'pos', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for proportion-valued-numeric-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ppn_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ppn', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for proportion-valued-numeric-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ppn_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ppn', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for proportion-valued-numeric-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ppn_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ppn', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for proportion-valued-numeric-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ppn_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ppn', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for proportion-valued-numeric-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ppn_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ppn', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for proportion-valued-numeric-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ppn_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ppn', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for proportion-valued-numeric-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ppn_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ppn', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for proportion-valued-numeric-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_ppn_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'ppn', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for positive-whole-number-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_psw_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'psw', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for positive-whole-number-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_psw_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'psw', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for positive-whole-number-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_psw_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'psw', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for positive-whole-number-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_psw_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'psw', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for positive-whole-number-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_psw_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'psw', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for positive-whole-number-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_psw_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'psw', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for positive-whole-number-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_psw_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'psw', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for positive-whole-number-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_psw_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'psw', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for sortable-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_srt_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'srt', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for sortable-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_srt_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'srt', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for sortable-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_srt_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'srt', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for sortable-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_srt_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'srt', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for sortable-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_srt_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'srt', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for sortable-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_srt_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'srt', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for sortable-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_srt_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'srt', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for sortable-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_srt_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'srt', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for string-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_str_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'str', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for string-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_str_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'str', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for string-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_str_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'str', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for string-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_str_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'str', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for string-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_str_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'str', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for string-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_str_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'str', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for string-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_str_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'str', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for string-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_str_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'str', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for unordered-factor-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_uno_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'uno', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for unordered-factor-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_uno_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'uno', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for unordered-factor-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_uno_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'uno', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for unordered-factor-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_uno_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'uno', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for unordered-factor-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_uno_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'uno', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for unordered-factor-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_uno_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'uno', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for unordered-factor-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_uno_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'uno', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for unordered-factor-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_uno_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'uno', 'vls', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for whole-number-ness, and for array-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_whl_arr <- function(x, ...) {ppp::unq_mmm_ccc(x, 'whl', 'arr', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for whole-number-ness, and for data.frame-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_whl_dtf <- function(x, ...) {ppp::unq_mmm_ccc(x, 'whl', 'dtf', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for whole-number-ness, and for generic-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_whl_gen <- function(x, ...) {ppp::unq_mmm_ccc(x, 'whl', 'gen', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for whole-number-ness, and for matrix-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_whl_mat <- function(x, ...) {ppp::unq_mmm_ccc(x, 'whl', 'mat', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for whole-number-ness, and for multivec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_whl_mvc <- function(x, ...) {ppp::unq_mmm_ccc(x, 'whl', 'mvc', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for whole-number-ness, and for scalar-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_whl_scl <- function(x, ...) {ppp::unq_mmm_ccc(x, 'whl', 'scl', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for whole-number-ness, and for vec-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_whl_vec <- function(x, ...) {ppp::unq_mmm_ccc(x, 'whl', 'vec', ...)}

#' @describeIn unq_mmm_ccc_PROPS Checks `x` for uniqueness, for whole-number-ness, and for vector-list-ness subject to any count/value restrictions in `...`. Returns a logical scalar.
#' @export
unq_whl_vls <- function(x, ...) {ppp::unq_mmm_ccc(x, 'whl', 'vls', ...)}
