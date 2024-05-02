#' @encoding UTF-8
#' @family properties
#' @title Extended Mode Properties
#' @description Extended modes are defined for non-empty atomic objects. For all other objects, the extended mode is `NULL`. These are not formally defined modes, but are evaluated dynamically based on the current characteristics of an object.
#'
#' Atomic objects that contain only `NA` values are of every extended mode (they can be coerced to any mode without introducing new `NA` values).
#'
#' ## Generic extended modes
#'
#' \tabular{ll}{  `'atm', 'ATM'`   \tab atomic (both a \code{\link[=bbb]{basic}} and an extended mode property).}
#'
#' ## Character extended modes
#'
#' \tabular{ll}{  `'chr', 'CHR'`   \tab character                          \cr
#'                `'ch1', 'CH1'`   \tab onechar, or `all(nchar(x) == 1)`   \cr
#'                `'ch3', 'CH3'`   \tab threechar, or `all(nchar(x) == 3)` \cr
#'                `'clr', 'CLR'`   \tab color (valid color values)         \cr
#'                `'str', 'STR'`   \tab string (no blanks)                   }
#'
#' ## Categorical extended modes
#'
#' \tabular{ll}{  `'fac', 'FAC'`   \tab factor         \cr
#'                `'lgl', 'LGL'`   \tab logical        \cr
#'                `'ord', 'ORD'`   \tab ordered factor \cr
#'                `'uno', 'UNO'`   \tab unordered factor }
#'
#' ## Basic numeric extended modes
#'
#' \tabular{ll}{  `'num', 'NUM'`   \tab numeric    \cr
#'                `'frc', 'FRC'`   \tab fractional \cr
#'                `'whl', 'WHL'`   \tab whole number }
#'
#' ## Value-restricted numeric extended modes
#'
#' \tabular{ll}{  `'pct', 'PCT'`   \tab percentage (`0` to `100`) \cr
#'                `'ppn', 'PPN'`   \tab proportion (`0-1`)        \cr
#'                `'pos', 'POS'`   \tab positive                  \cr
#'                `'neg', 'NEG'`   \tab negative                  \cr
#'                `'nng', 'NNG'`   \tab non-negative              \cr
#'                `'nps', 'NPS'`   \tab non-positive                }
#'
#' ## Whole-number value-restricted extended modes
#'
#' \tabular{ll}{  `'evn', 'EVN'`   \tab even               \cr
#'                `'odd', 'ODD'`   \tab odd                \cr
#'                `'ngw', 'NGW'`   \tab negative whole     \cr
#'                `'psw', 'PSW'`   \tab positive whole     \cr
#'                `'npw', 'NPW'`   \tab non-positive whole \cr
#'                `'nnw', 'NNW'`   \tab non-negative whole   }
#'
#' ## Combination extended modes
#' \tabular{ll}{  `'ind', 'IND'`   \tab indexer (see `'lgl'`, `'psw'`)           \cr
#'                `'srt', 'SRT'`   \tab sortable (see `'chr'`, `'num'`, `'ord'`) \cr
#'                `'nst', 'NST'`   \tab non-sortable atomic                        }
#'
#' @param x An R object.
#' @param spec `NULL` or a \link[=cmp_chr_scl]{complete character vec} containing one or more extended mode properties from `mmm_props()`. extended mode specs may be pipe-delimited. If there are multiple properties in `spec`, `x` is inspected for a match to any of the specified properties.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @examples
#' mmm_funs()
#' mmm_props()
#' mmm(letters)
#' mmm(1:10)
#' mmm(c(pi, log(10), exp(1)))
#' is_mmm_spec("invalid")
#' is_mmm_spec("psw|srt")
#' MMM(1:10, "psw|srt")
#' MMM(letters, "ch1")
#' MMM(letters, "srt")
#' MMM(1:10, "ch3")
#' CH1(letters)
#' SRT(letters)
#' CLR(1:10)
#' CLR("blue")
#' CLR("#1077ACFF")
#' @export
mmm_PROPS <- function() {utils::help("mmm_PROPS", package = "ppp")}

#' @describeIn mmm_PROPS Lists all extended mode properties possessed by `x`. Returns a character vector.
#' @export
mmm <- function(x) {
  y <- NULL
  for (mmm in ppp::mmm_funs()) {if (base::eval(base::parse(text = base::paste0("ppp::.", mmm, "(x)")))) {y <- base::c(y, base::tolower(mmm))}}
  y
}

#' @describeIn mmm_PROPS Lists all extended-mode checking functions. Returns a sorted, uppercase, character vector.
#' @export
mmm_funs <- function() {base::c("ATM", "CH1", "CH3", "CHR", "CLR", "EVN", "FAC", "FRC", "IND", "LGL", "NEG", "NGW", "NNG", "NNW", "NPS", "NPW", "NST", "NUM", "ODD", "ORD", "PCT", "POS", "PPN", "PSW", "SRT", "STR", "UNO", "WHL")}

#' @describeIn mmm_PROPS Lists all extended-mode properties. Returns a sorted, lowercase, character vector.
#' @export
mmm_props <- function() {base::c("atm", "ch1", "ch3", "chr", "clr", "evn", "fac", "frc", "ind", "lgl", "neg", "ngw", "nng", "nnw", "nps", "npw", "nst", "num", "odd", "ord", "pct", "pos", "ppn", "psw", "srt", "str", "uno", "whl")}

#' @describeIn mmm_PROPS Checks whether a `spec` is an extended-mode property spec. Returns a logical scalar. See \code{\link{ppp}} for a definition of a property spec.
#' @export
is_mmm_spec <- function(spec) {
  spec <- ppp::spec2props(spec)
  if (base::length(spec) == 0) {F} else {base::all(spec %in% ppp::mmm_props())}
}

#' @describeIn mmm_PROPS Checks whether the object `x` is a match to extended mode spec in `spec` subject to any count or value restriction arguments in `...`. Returns a logical scalar. See \code{\link{ppp}} for a definition of a property spec.
#' @export
MMM <- function(x, spec, ...) {
  errs <- ppp::meets_errs(x, ...)
  if (!ppp::is_mmm_spec(spec)) {errs <- base::c(errs, '[spec] must be a complete character vec (?cmp_chr_vec) containing one or more (possible pipe-separated) values exclusively from mmm_props().')}
  if (!base::is.null(errs)) {ppp::stopperr(errs, .PKG = "ppp")}
  props <- ppp::spec2props(spec)
  for (prop in base::toupper(props)) {if (base::eval(base::parse(text = base::paste0("ppp::.", prop, "(x)")))) {return(T)}}
  F
}

#' @describeIn mmm_PROPS Checks whether `x` is a onechar vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
CH1 <- function(x, ...) {ppp::MMM(x, 'ch1', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is a threechar vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
CH3 <- function(x, ...) {ppp::MMM(x, 'ch3', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is a character vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
CHR <- function(x, ...) {ppp::MMM(x, 'chr', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is a color character vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
CLR <- function(x, ...) {ppp::MMM(x, 'clr', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is an even, whole-number vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
EVN <- function(x, ...) {ppp::MMM(x, 'evn', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is a factor vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
FAC <- function(x, ...) {ppp::MMM(x, 'fac', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is a fractional numeric vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
FRC <- function(x, ...) {ppp::MMM(x, 'frc', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is an indexing vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
IND <- function(x, ...) {ppp::MMM(x, 'ind', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is a logical vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
LGL <- function(x, ...) {ppp::MMM(x, 'lgl', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is a negative numeric vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
NEG <- function(x, ...) {ppp::MMM(x, 'neg', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is a negative whole-number vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
NGW <- function(x, ...) {ppp::MMM(x, 'ngw', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is a non-negative numeric vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
NNG <- function(x, ...) {ppp::MMM(x, 'nng', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is a non-negative whole-number vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
NNW <- function(x, ...) {ppp::MMM(x, 'nnw', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is a non-positive numeric vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
NPS <- function(x, ...) {ppp::MMM(x, 'nps', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is a non-positive whole-number vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
NPW <- function(x, ...) {ppp::MMM(x, 'npw', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is a non-sortable vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
NST <- function(x, ...) {ppp::MMM(x, 'nst', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is a numeric vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
NUM <- function(x, ...) {ppp::MMM(x, 'num', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is an odd, whole-number vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
ODD <- function(x, ...) {ppp::MMM(x, 'odd', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is an ordered factor vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
ORD <- function(x, ...) {ppp::MMM(x, 'ord', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is a percentage-valued numeric vector (i.e., in the range 0-100) subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
PCT <- function(x, ...) {ppp::MMM(x, 'pct', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is a positive numeric vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
POS <- function(x, ...) {ppp::MMM(x, 'pos', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is a proportion-valued numeric vector (i.e., in the range 0-1) subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
PPN <- function(x, ...) {ppp::MMM(x, 'ppn', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is a positive whole-number vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
PSW <- function(x, ...) {ppp::MMM(x, 'psw', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is a sortable vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
SRT <- function(x, ...) {ppp::MMM(x, 'srt', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is a string vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
STR <- function(x, ...) {ppp::MMM(x, 'str', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is an unordered factor vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
UNO <- function(x, ...) {ppp::MMM(x, 'uno', ...)}

#' @describeIn mmm_PROPS Checks whether `x` is a whole-number vector subject to any count or value restriction arguments in `...`. Returns a logical scalar.
#' @export
WHL <- function(x, ...) {ppp::MMM(x, 'whl', ...)}
