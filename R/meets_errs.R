#' @encoding UTF-8
#' @family utils
#' @title Checks for Meta-Errors in Value-Restriction Arguments, If There Are Any
#' @description Checks whether any count- or value-restrictions are carried in `...` arguments. If so, evaluates whether those restriction arguments are valid. If not, throws errors describing invalid restriction arguments.
#' @inheritParams meets
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @return No return value. Called for the side effect of evaluating the validity of any count- or value-restriction arguments in `...`.
#' @export
meets_errs <- function(x, ...) {
  if (base::...length() == 0) {return(NULL)}
  Dots <- base::list(...)
  Names <- base::names(Dots)
  Valid <- base::c('.N', '.NR', '.NC', '.MIN', '.MINR', '.MINC', '.MAX', '.MAXR', '.MAXC', '.VALS', '.LT', '.LE', '.GE', '.GT')
  Errors <- NULL
  if (base::length(Names) != base::length(Dots)) {Errors <- base::c(Errors, "All [...] arguments must be named.")}
  if (base::length(Names) != base::length(base::unique(Names))) {Errors <- base::c(Errors, "Names of [...] arguments must be unique.")}
  if (!base::all(Names %in% Valid)) {Errors <- base::c(Errors, base::paste0("All names of [...] arguments must be from c('.N', '.NR', '.NC', '.MIN', '.MINR', '.MINC', '.MAX', '.MAXR', '.MAXC', '.VALS', '.LT', '.LE', '.GE', '.GT')."))}
  if (!base::is.null(Errors)) {ppp::stopper(Errors, .FUN = "meets", .PKG = "ppp")}
  if (".N" %in% Names) {if (!ppp:::.cmp_nnw_vec(Dots$.N)) {Errors <- base::c(Errors, "[.N] must be a non-negative whole-number vector (?ppp::cmp_nnw_vec) if supplied.")}}
  if (".MIN" %in% Names) {if (!ppp:::cmp_nnw_scl(Dots$.MIN)) {Errors <- base::c(Errors, "[.MIN] must be a non-negative whole-number scalar (?ppp::cmp_nnw_scl) if supplied.")}}
  if (".MAX" %in% Names) {if (!ppp:::cmp_nnw_scl(Dots$.MAX)) {Errors <- base::c(Errors, "[.MAX] must be a non-negative whole-number scalar (?ppp::cmp_nnw_scl) if supplied.")}}
  if (".NR" %in% Names) {
    if (!base::is.data.frame(x) & !is.matrix(x)) {Errors <- base::c(Errors, "[x] must be a data.frame or matrix when [.NR] is supplied")}
    if (!ppp:::.cmp_nnw_vec(Dots$.NR)) {Errors <- base::c(Errors, "[.NR] must be a non-negative whole-number vector (?ppp::cmp_nnw_vec) if supplied.")}
  }
  if (".MINR" %in% Names) {
    if (!base::is.data.frame(x) & !is.matrix(x)) {Errors <- base::c(Errors, "[x] must be a data.frame or matrix when [.MINR] is supplied")}
    if (!ppp:::cmp_nnw_scl(Dots$.MINR)) {Errors <- base::c(Errors, "[.MINR] must be a non-negative whole-number scalar (?ppp::cmp_nnw_scl) if supplied.")}
  }
  if (".MAXR" %in% Names) {
    if (!base::is.data.frame(x) & !is.matrix(x)) {Errors <- base::c(Errors, "[x] must be a data.frame or matrix when [.MAXR] is supplied")}
    if (!ppp:::cmp_nnw_scl(Dots$.MAXR)) {Errors <- base::c(Errors, "[.MAXR] must be a non-negative whole-number scalar (?ppp::cmp_nnw_scl) if supplied.")}
  }
  if (".NC" %in% Names) {
    if (!base::is.data.frame(x) & !is.matrix(x)) {Errors <- base::c(Errors, "[x] must be a data.frame or matrix when [.NC] is supplied")}
    if (!ppp:::.cmp_nnw_vec(Dots$.NC)) {Errors <- base::c(Errors, "[.NC] must be a non-negative whole-number vector (?ppp::cmp_nnw_vec) if supplied.")}
  }
  if (".MINC" %in% Names) {
    if (!base::is.data.frame(x) & !is.matrix(x)) {Errors <- base::c(Errors, "[x] must be a data.frame or matrix when [.MINC] is supplied")}
    if (!ppp:::cmp_nnw_scl(Dots$.MINC)) {Errors <- base::c(Errors, "[.MINC] must be a non-negative whole-number scalar (?ppp::cmp_nnw_scl) if supplied.")}
  }
  if (".MAXC" %in% Names) {
    if (!base::is.data.frame(x) & !is.matrix(x)) {Errors <- base::c(Errors, "[x] must be a data.frame or matrix when [.MAXC] is supplied")}
    if (!ppp:::cmp_nnw_scl(Dots$.MAXC)) {Errors <- base::c(Errors, "[.MAXC] must be a non-negative whole-number scalar (?ppp::cmp_nnw_scl) if supplied.")}
  }
  if (".VALS" %in% Names) {
    if (!ppp:::.cmp_atm_vec(Dots$.VALS)) {Errors <- base::c(Errors, "[.VALS] must be a complete atomic vector (?ppp::cmp_atm_vec) if supplied.")}
    else if (!ppp:::.compat(x, Dots$.VALS)) {Errors <- base::c(Errors, "[x] and [.VALS] must be compatible modes.")}
  }
  if (".LT" %in% Names) {
    if (!ppp:::.cmp_srt_scl(Dots$.LT)) {Errors <- base::c(Errors, "[.LT] must be a complete sortable scalar (?ppp::cmp_srt_scl).")}
    else if (!ppp:::.compar(x, Dots$.LT)) {Errors <- base::c(Errors, "[x] and [.LT] must be of comparable, sortable modes.")}
  }
  if (".LE" %in% Names) {
    if (!ppp:::.cmp_srt_scl(Dots$.LE)) {Errors <- base::c(Errors, "[.LE] must be a complete sortable scalar (?ppp::cmp_srt_scl).")}
    else if (!ppp:::.compar(x, Dots$.LE)) {Errors <- base::c(Errors, "[x] and [.LE] must be of comparable, sortable modes.")}
  }
  if (".GE" %in% Names) {
    if (!ppp:::.cmp_srt_scl(Dots$.GE)) {Errors <- base::c(Errors, "[.GE] must be a complete sortable scalar (?ppp::cmp_srt_scl).")}
    else if (!ppp:::.compar(x, Dots$.GE)) {Errors <- base::c(Errors, "[x] and [.VALS] must be of comparable, sortable modes.")}
  }
  if (".GT" %in% Names) {
    if (!ppp:::.cmp_srt_scl(Dots$.GT)) {Errors <- base::c(Errors, "[.GT] must be a complete sortable scalar (?ppp::cmp_srt_scl).")}
    else if (!ppp:::.compar(x, Dots$.GT)) {Errors <- base::c(Errors, "[x] and [.GT] must be of comparable, sortable modes.")}
  }
  if (!base::is.null(Errors)) {ppp::stopper(Errors, .FUN = "meets", .PKG = "ppp")}
}
