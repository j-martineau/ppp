#' @encoding UTF-8
#' @family utils
#' @title Checks for Meta-Errors in Count and/or Value-Restriction Arguments, If There Are Any
#' @description Checks whether any count- or value-restrictions are carried in `...` arguments. If so, evaluates whether those restriction arguments are valid. If not, throws errors describing invalid restriction arguments.
#' @inheritParams meets
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return No return value. Called for the side effect of evaluating the validity of any count- or value-restriction arguments in `...`.
#' @export
meets_errs <- function(x, ...) {
  if (base::...length() == 0) {return(NULL)}
  dots <- base::list(...)
  names <- base::names(dots)
  valid <- base::c('.n', '.nr', '.nc', '.min', '.minr', '.minc', '.max', '.maxr', '.maxc', '.vals', '.lt', '.le', '.ge', '.gt')
  errors <- NULL
  if (base::length(names) != base::length(dots)) {errors <- base::c(errors, "All [...] arguments must be named.")}
  if (base::length(names) != base::length(base::unique(names))) {errors <- base::c(errors, "names of [...] arguments must be unique.")}
  if (!base::all(names %in% valid)) {errors <- base::c(errors, base::paste0("All names of [...] arguments must be from c('.n', '.nr', '.nc', '.min', '.minr', '.minc', '.max', '.maxr', '.maxc', '.vals', '.lt', '.le', '.ge', '.gt')."))}
  if (!base::is.null(errors)) {ppp::stopper(errors, fun = "meets", pkg = "ppp")}
  if (".n" %in% names) {if (!ppp:::.cmp_nnw_vec(dots$.n)) {errors <- base::c(errors, "[.n] must be a non-negative whole-number vector (?cmp_nnw_vec) if supplied.")}}
  if (".min" %in% names) {if (!ppp:::cmp_nnw_scl(dots$.min)) {errors <- base::c(errors, "[.min] must be a non-negative whole-number scalar (?cmp_nnw_scl) if supplied.")}}
  if (".max" %in% names) {if (!ppp:::cmp_nnw_scl(dots$.max)) {errors <- base::c(errors, "[.max] must be a non-negative whole-number scalar (?cmp_nnw_scl) if supplied.")}}
  if (".nr" %in% names) {
    if (!base::is.data.frame(x) & !is.matrix(x)) {errors <- base::c(errors, "[x] must be a data.frame or matrix when [.nr] is supplied")}
    if (!ppp:::.cmp_nnw_vec(dots$.nr)) {errors <- base::c(errors, "[.nr] must be a non-negative whole-number vector (?cmp_nnw_vec) if supplied.")}
  }
  if (".minr" %in% names) {
    if (!base::is.data.frame(x) & !is.matrix(x)) {errors <- base::c(errors, "[x] must be a data.frame or matrix when [.minr] is supplied")}
    if (!ppp:::cmp_nnw_scl(dots$.minr)) {errors <- base::c(errors, "[.minr] must be a non-negative whole-number scalar (?cmp_nnw_scl) if supplied.")}
  }
  if (".maxr" %in% names) {
    if (!base::is.data.frame(x) & !is.matrix(x)) {errors <- base::c(errors, "[x] must be a data.frame or matrix when [.maxr] is supplied")}
    if (!ppp:::cmp_nnw_scl(dots$.maxr)) {errors <- base::c(errors, "[.maxr] must be a non-negative whole-number scalar (?cmp_nnw_scl) if supplied.")}
  }
  if (".nc" %in% names) {
    if (!base::is.data.frame(x) & !is.matrix(x)) {errors <- base::c(errors, "[x] must be a data.frame or matrix when [.nc] is supplied")}
    if (!ppp:::.cmp_nnw_vec(dots$.nc)) {errors <- base::c(errors, "[.nc] must be a non-negative whole-number vector (?cmp_nnw_vec) if supplied.")}
  }
  if (".minc" %in% names) {
    if (!base::is.data.frame(x) & !is.matrix(x)) {errors <- base::c(errors, "[x] must be a data.frame or matrix when [.minc] is supplied")}
    if (!ppp:::cmp_nnw_scl(dots$.minc)) {errors <- base::c(errors, "[.minc] must be a non-negative whole-number scalar (?cmp_nnw_scl) if supplied.")}
  }
  if (".maxc" %in% names) {
    if (!base::is.data.frame(x) & !is.matrix(x)) {errors <- base::c(errors, "[x] must be a data.frame or matrix when [.maxc] is supplied")}
    if (!ppp:::cmp_nnw_scl(dots$.maxc)) {errors <- base::c(errors, "[.maxc] must be a non-negative whole-number scalar (?cmp_nnw_scl) if supplied.")}
  }
  if (".vals" %in% names) {
    if (!ppp:::.cmp_atm_vec(dots$.vals)) {errors <- base::c(errors, "[.vals] must be a complete atomic vector (?cmp_atm_vec) if supplied.")}
    else if (!ppp:::.compat(x, dots$.vals)) {errors <- base::c(errors, "[x] and [.vals] must be compatible modes.")}
  }
  if (".lt" %in% names) {
    if (!ppp:::.cmp_srt_scl(dots$.lt)) {errors <- base::c(errors, "[.lt] must be a complete sortable scalar (?cmp_srt_scl).")}
    else if (!ppp:::.compar(x, dots$.lt)) {errors <- base::c(errors, "[x] and [.lt] must be of comparable, sortable modes.")}
  }
  if (".le" %in% names) {
    if (!ppp:::.cmp_srt_scl(dots$.le)) {errors <- base::c(errors, "[.le] must be a complete sortable scalar (?cmp_srt_scl).")}
    else if (!ppp:::.compar(x, dots$.le)) {errors <- base::c(errors, "[x] and [.le] must be of comparable, sortable modes.")}
  }
  if (".ge" %in% names) {
    if (!ppp:::.cmp_srt_scl(dots$.ge)) {errors <- base::c(errors, "[.ge] must be a complete sortable scalar (?cmp_srt_scl).")}
    else if (!ppp:::.compar(x, dots$.ge)) {errors <- base::c(errors, "[x] and [.vals] must be of comparable, sortable modes.")}
  }
  if (".gt" %in% names) {
    if (!ppp:::.cmp_srt_scl(dots$.gt)) {errors <- base::c(errors, "[.gt] must be a complete sortable scalar (?cmp_srt_scl).")}
    else if (!ppp:::.compar(x, dots$.gt)) {errors <- base::c(errors, "[x] and [.gt] must be of comparable, sortable modes.")}
  }
  if (!base::is.null(errors)) {ppp::stopper(errors, .FUN = "meets", .PKG = "ppp")}
}
