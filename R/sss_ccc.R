#' @encoding UTF-8
#' @family props
#' @title Combo Shape and Extended Class Properties
#' @description These functions check for the combination of \link[=sss]{shape} and \link[=ccc]{extended class} properties.
#'
#' Some combinations of shape and extended class are nonsensical. That includes:
#'
#' \itemize{\item Shapes `emp` (empty) and `pnt` (point) combined with extended classes `mvc` (multivec) or `scl` (scalar).
#'          \item Shapes `row`, `col` (column), or `rct` (rectangular) combined with extended classes other than `dtf` (data.frame) or `mat` (matrix),
#'          \item Shape `sqr` (square) combined with any extended class other than `mat` (matrix).}
#'
#' Nonsensical combinations do not have corresponding `sss_ccc` property functions.
#' @param x An R object.
#' @param sss A character scalar single basic property from \code{link{sss_props}()}.
#' @param ccc A character scalar single extended class property from \code{link{ccc_props}()}
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @examples
#' sss_cccfuns()
#' sss_cccprops()
#' sss_ccc(letters, "lin", "vec")
#' sss_ccc(1, "emp", "vec")
#' lin_gen(letters)
#' pnt_vec(1)
#' @export
sss_ccc_PROPS <- function() {utils::help("shape_plus_extended_class", package = "ppp")}

#' @describeIn sss_ccc_PROPS Checks `x` for a combination of the single shape property `sss` and the single extended class property `ccc` subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
sss_ccc <- function(x, sss, ccc, ...) {
  if (!ppp:::.cmp_chr_scl(sss)) {pkSSS <- F} else {okSSS <- base::tolower(sss) %in% ppp::sss_props()}
  if (!ppp:::.cmp_chr_scl(ccc)) {okCCC <- F} else {okCCC <- base::tolower(ccc) %in% ppp::ccc_props()}
  errs <- ppp::meets_errs(x, ...)
  if (!okSSS) {errs <- base::c(errs, "[sss] is not a scalar value from sss_props().")}
  if (!okCCC) {errs <- base::c(errs, "[ccc] is not a scalar value from ccc_props().")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, .PKG = "ppp")}
  if (!ppp::meets(x, ...)) {return(F)}
  sss <- base::toupper(sss)
  ccc <- base::toupper(ccc)
  if (sss == "emp") {
    if (base::length(base::dim(x)) != 2) {
      if (base::NROW(x) * base::NCOL(x) != 0) {F}
      else if (ccc == "ARR") {ppp::.ARR(x)}
      else if (ccc == "DTF") {ppp::.DTF(x)}
      else if (ccc == "GEN") {ppp::.GEN(x)}
      else if (ccc == "MAT") {ppp::.MAT(x)}
      else if (ccc == "VEC") {ppp::.VEC(x)}
      else if (ccc == "VLS") {ppp::.VLS(x)}
      else {F}
    } else if (base::NROW(x) * base::NCOL(x) == 0) {ccc %in% base::c("ARR", "VEC", "VLS")}
  } else {base::eval(base::parse(text = base::paste0("ppp::.", sss, "(x) & ppp::.", ccc, "(x)")))}
}

#' @describeIn sss_ccc_PROPS Lists all shape-plus-extended class combo property checking functions. Returns a character vector.
#' @export
sss_ccc_funs <- function() {base::sort(base::c("emp_arr", "emp_dtf", "emp_gen", "emp_mat", "emp_vec", "emp_vls", "pnt_arr", "pnt_dtf", "pnt_gen", "pnt_mat", "pnt_vec", "pnt_vls", "lin_arr", "lin_dtf", "lin_gen", "lin_mat", "lin_mvc", "lin_vec", "lin_vls", "col_dtf", "col_mat", "row_dtf", "row_mat", "rct_dtf", "rct_mat", "sld_arr", "sqr_mat"))}

#' @describeIn sss_ccc_PROPS Checks `x` for empty-ness and array-ness. Returns a logical scalar.
#' @export
emp_arr <- function(x, ...) {ppp::sss_ccc(x, "emp", "arr", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for empty-ness and data.frame-ness. Returns a logical scalar.
#' @export
emp_dtf <- function(x, ...) {ppp::sss_ccc(x, "emp", "dtf", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for empty-ness and generic-ness. Returns a logical scalar.
#' @export
emp_gen <- function(x, ...) {ppp::sss_ccc(x, "emp", "gen", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for empty-ness and matrix-ness. Returns a logical scalar.
#' @export
emp_mat <- function(x, ...) {ppp::sss_ccc(x, "emp", "mat", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for empty-ness and vec-ness. Returns a logical scalar.
#' @export
emp_vec <- function(x, ...) {ppp::sss_ccc(x, "emp", "vec", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for empty-ness and vector-list-ness. Returns a logical scalar.
#' @export
emp_vls <- function(x, ...) {ppp::sss_ccc(x, "emp", "vls", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for point-ness and array-ness. Returns a logical scalar.
#' @export
pnt_arr <- function(x, ...) {ppp::sss_ccc(x, "pnt", "arr", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for point-ness and data.frame-ness. Returns a logical scalar.
#' @export
pnt_dtf <- function(x, ...) {ppp::sss_ccc(x, "pnt", "dtf", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for point-ness and generic-ness. Returns a logical scalar.
#' @export
pnt_gen <- function(x, ...) {ppp::sss_ccc(x, "pnt", "gen", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for point-ness and matrix-ness. Returns a logical scalar.
#' @export
pnt_mat <- function(x, ...) {ppp::sss_ccc(x, "pnt", "mat", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for point-ness and scalar-ness. Returns a logical scalar.
#' @export
pnt_scl <- function(x, ...) {ppp::sss_ccc(x, "pnt", "scl", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for point-ness and vec-ness. Returns a logical scalar.
#' @export
pnt_vec <- function(x, ...) {ppp::sss_ccc(x, "pnt", "vec", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for point-ness and vector-list-ness. Returns a logical scalar.
#' @export
pnt_vls <- function(x, ...) {ppp::sss_ccc(x, "pnt", "vls", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for linear-ness and array-ness. Returns a logical scalar.
#' @export
lin_arr <- function(x, ...) {ppp::sss_ccc(x, "lin", "arr", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for linear-ness and data.frame-ness. Returns a logical scalar.
#' @export
lin_dtf <- function(x, ...) {ppp::sss_ccc(x, "lin", "dtf", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for linear-ness and generic-ness. Returns a logical scalar.
#' @export
lin_gen <- function(x, ...) {ppp::sss_ccc(x, "lin", "gen", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for linear-ness and matrix-ness. Returns a logical scalar.
#' @export
lin_mat <- function(x, ...) {ppp::sss_ccc(x, "lin", "mat", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for linear-ness and multivec-ness. Returns a logical scalar.
#' @export
lin_mvc <- function(x, ...) {ppp::sss_ccc(x, "lin", "mvc", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for linear-ness and vec-ness. Returns a logical scalar.
#' @export
lin_vec <- function(x, ...) {ppp::sss_ccc(x, "lin", "vec", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for linear-ness and vector-list-ness. Returns a logical scalar.
#' @export
lin_vls <- function(x, ...) {ppp::sss_ccc(x, "lin", "vls", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for column-ness and data.frame-ness. Returns a logical scalar.
#' @export
col_dtf <- function(x, ...) {ppp::sss_ccc(x, "col", "dtf", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for column-ness and matrix-ness. Returns a logical scalar.
#' @export
col_mat <- function(x, ...) {ppp::sss_ccc(x, "col", "mat", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for row-ness and data.frame-ness. Returns a logical scalar.
#' @export
row_dtf <- function(x, ...) {ppp::sss_ccc(x, "row", "dtf", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for row-ness and matrix-ness. Returns a logical scalar.
#' @export
row_mat <- function(x, ...) {ppp::sss_ccc(x, "row", "mat", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for rectangle-ness and data.frame-ness. Returns a logical scalar.
#' @export
rct_dtf <- function(x, ...) {ppp::sss_ccc(x, "rct", "dtf", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for rectangle-ness and matrix-ness. Returns a logical scalar.
#' @export
rct_mat <- function(x, ...) {ppp::sss_ccc(x, "rct", "mat", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for square-ness and matrix-ness. Returns a logical scalar.
#' @export
sqr_mat <- function(x, ...) {ppp::sss_ccc(x, "sqr", "mat", ...)}

#' @describeIn sss_ccc_PROPS Checks `x` for solid-ness and array-ness. Returns a logical scalar.
#' @export
sld_arr <- function(x, ...) {ppp::sss_ccc(x, "sld", "arr", ...)}
