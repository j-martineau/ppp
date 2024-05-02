#' @encoding UTF-8
#' @title Combo Basic Plus Extended Class Properties
#' @description Some combinations of basic + extended class properties are nonsensical. For this reason, the basic properties represented in this family of functions are `c('atm', 'nil',  'pop')`, or atomic, nil (non-`NULL` and of length 0), and populated (of length `1+`).
#' \cr\cr In addition, the base property `'nil'` is nonsensical in combination with extended classes `'mvc'` (multivec) and `'scl'` (scalar), which thus do not have combined `bbb_ccc` property functions.
#' @param x An R object.
#' @param bbb A character scalar single basic property from \code{\link{bbb_props}()}.
#' @param ccc A character scalar single extendec class property from \code{\link{ccc_props}()}.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @examples
#' bbb_ccc_funs()
#' bbb_ccc(letters, "atm", "mvc")
#' bbb_ccc(1, "nil", "vec")
#' atm_gen(letters)
#' atm_scl(1)
#' @export
bbb_ccc_PROPS <- function() {utils::help("bbb_ccc_PROPS", package = "ppp")}

#' @describeIn bbb_ccc_PROPS Checks `x` for the basic property in `bbb` and for the extended class property in `ccc` subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
bbb_ccc <- function(x, bbb, ccc, ...) {
  if (base::is.character(bbb)) {bbb <- base::tolower(bbb)}
  if (base::is.character(ccc)) {ccc <- base::tolower(ccc)}
  errBBB <- "[bbb] is not a scalar value from bbb_props()."
  errCCC <- "[ccc] is not a scalar value from ccc_props()."
  errs <- ppp::meets_errs(x, ...)
  if (base::length(bbb) != 1) {errs <- base::c(errs, errBBB)} else if (!(bbb %in% ppp::bbb_props())) {errs <- base::c(errs, errBBB)}
  if (base::length(ccc) != 1) {errs <- base::c(errs, errCCC)} else if (!(ccc %in% ppp::ccc_props())) {errs <- base::c(errs, errCCC)}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "ppp")}
  arr <- ccc == "arr"; ARR <- ppp::.ARR(x)
  dtf <- ccc == "dtf"; DTF <- ppp::.DTF(x)
  gen <- ccc == "gen"; GEN <- ppp::.GEN(x)
  mat <- ccc == "mat"; MAT <- ppp::.MAT(x)
  mvc <- ccc == "mvc"; MVC <- ppp::.MVC(x)
  scl <- ccc == "scl"; SCL <- ppp::.SCL(x)
  vec <- ccc == "vec"; VEC <- ppp::.VEC(x)
  vls <- ccc == "vls"; VLS <- ppp::.VLS(x)
  pop <- bbb == "pop"; POP <- ppp::.POP(x)
  nil <- bbb == "nil"; NIL <- !POP & !base::is.null(x)
  atm <- bbb == "atm"
  if (!ppp::meets(x, ...)) {F}
  else if ((pop & !POP) | (nil & !NIL) | (arr & !ARR) | (dtf & !DTF) | (gen & !GEN) | (mat & !MAT) | (mvc & !MVC) | (scl & !SCL) | (vec & !VEC) | (vls & !VLS)) {F}
  else if (atm & dtf) {base::all(base::apply(x, 2, base::is.atomic))}
  else if (atm & vls) {base::all(base::sapply(x, base::is.atomic))}
  else if (atm) {base::is.atomic(x)}
  else {base::eval(base::parse(text = base::paste0("ppp::.", base::toupper(bbb), "(x)")))}
}

#' @describeIn bbb_ccc_PROPS Lists all combo basic plus extended class property checking functions.
#' @export
bbb_ccc_funs <- function() {base::sort(base::c("atm_arr", "atm_dtf", "atm_gen", "atm_mat", "atm_mvc", "atm_scl", "atm_vec", "atm_vls",
                                               "nil_arr", "nil_dtf", "nil_gen", "nil_mat",                       "nil_vec", "nil_vls",
                                               "pop_arr", "pop_dtf", "pop_gen", "pop_mat", "pop_mvc", "pop_scl", "pop_vec", "pop_vls"))}

#' @describeIn bbb_ccc_PROPS Checks `x` for atomic-ness and array-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_arr <- function(x, ...) {ppp::bbb_ccc(x, 'atm', 'arr', ...)}

#' @describeIn bbb_ccc_PROPS Checks `x` for atomic-ness and data.frame-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_dtf <- function(x, ...) {ppp::bbb_ccc(x, 'atm', 'dtf', ...)}

#' @describeIn bbb_ccc_PROPS Checks `x` for atomic-ness and generic-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_gen <- function(x, ...) {ppp::bbb_ccc(x, 'atm', 'gen', ...)}

#' @describeIn bbb_ccc_PROPS Checks `x` for atomic-ness and matrix-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_mat <- function(x, ...) {ppp::bbb_ccc(x, 'atm', 'mat', ...)}

#' @describeIn bbb_ccc_PROPS Checks `x` for atomic-ness and multivec-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_mvc <- function(x, ...) {ppp::bbb_ccc(x, 'atm', 'mvc', ...)}

#' @describeIn bbb_ccc_PROPS Checks `x` for atomic-ness and scalar-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_scl <- function(x, ...) {ppp::bbb_ccc(x, 'atm', 'scl', ...)}

#' @describeIn bbb_ccc_PROPS Checks `x` for atomic-ness and vec-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_vec <- function(x, ...) {ppp::bbb_ccc(x, 'atm', 'vec', ...)}

#' @describeIn bbb_ccc_PROPS Checks `x` for atomic-ness and vector-list-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
atm_vls <- function(x, ...) {ppp::bbb_ccc(x, 'atm', 'vls', ...)}

#' @describeIn bbb_ccc_PROPS Checks `x` for nil-ness and array-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
nil_arr <- function(x, ...) {ppp::bbb_ccc(x, "nil", "arr", ...)}

#' @describeIn bbb_ccc_PROPS Checks `x` for nil-ness and data.frame-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
nil_dtf <- function(x, ...) {ppp::bbb_ccc(x, "nil", "dtf", ...)}

#' @describeIn bbb_ccc_PROPS Checks `x` for nil-ness and generic-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
nil_gen <- function(x, ...) {ppp::bbb_ccc(x, "nil", "gen", ...)}

#' @describeIn bbb_ccc_PROPS Checks `x` for nil-ness and matrix-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
nil_mat <- function(x, ...) {ppp::bbb_ccc(x, "nil", "mat", ...)}

#' @describeIn bbb_ccc_PROPS Checks `x` for nil-ness and vec-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
nil_vec <- function(x, ...) {ppp::bbb_ccc(x, "nil", "vec", ...)}

#' @describeIn bbb_ccc_PROPS Checks `x` for nil-ness and vector-list-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
nil_vls <- function(x, ...) {ppp::bbb_ccc(x, "nil", "vls", ...)}

#' @describeIn bbb_ccc_PROPS Checks `x` for populated-ness and array-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_arr <- function(x, ...) {ppp::bbb_ccc(x, 'pop', 'arr', ...)}

#' @describeIn bbb_ccc_PROPS Checks `x` for populated-ness and data.frame-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_dtf <- function(x, ...) {ppp::bbb_ccc(x, 'pop', 'dtf', ...)}

#' @describeIn bbb_ccc_PROPS Checks `x` for populated-ness and generic-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_gen <- function(x, ...) {ppp::bbb_ccc(x, 'pop', 'gen', ...)}

#' @describeIn bbb_ccc_PROPS Checks `x` for populated-ness and matrix-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_mat <- function(x, ...) {ppp::bbb_ccc(x, 'pop', 'mat', ...)}

#' @describeIn bbb_ccc_PROPS Checks `x` for populated-ness and multivec-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_mvc <- function(x, ...) {ppp::bbb_ccc(x, 'pop', 'mvc', ...)}

#' @describeIn bbb_ccc_PROPS Checks `x` for populated-ness and scalar-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_scl <- function(x, ...) {ppp::bbb_ccc(x, 'pop', 'scl', ...)}

#' @describeIn bbb_ccc_PROPS Checks `x` for populated-ness and vec-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_vec <- function(x, ...) {ppp::bbb_ccc(x, 'pop', 'vec', ...)}

#' @describeIn bbb_ccc_PROPS Checks `x` for populated-ness and vector-list-ness subject to any count and/or value restrictions in `...`. Returns a logical scalar.
#' @export
pop_vls <- function(x, ...) {ppp::bbb_ccc(x, 'pop', 'vls', ...)}
