#' @encoding UTF-8
#' @title Evaluate whether an object meets count and/or value restrictions
#' @description Evaluates whether `x` meets any count and value restrictions are provided in `...`. If none are provided, returns `TRUE`.
#' @param x An object.
#' @param ... Optional named arguments count and/or value restrictions for `x`. See the *specifying count and value restrictions* section.
#' @section Specifying count and value restrictions: Specifying restrictions in `...` is optional. The full set of recognized arguments names are defined in the following table along with the properties each specifies:
#' \tabular{ll}{  `.max, .maxr, .maxc`   \tab Scalar maximum valid numbers of elements, rows, and columns, respectively.                                                                 \cr   \tab   \cr
#'                `.min, .minr, .minc`   \tab Scalar minimum valid numbers of elements, rows, and columns, respectively.                                                                 \cr   \tab   \cr
#'                `.lt, .le, .ge, .gt`   \tab \link[=cmp_srt_scl]{Complete sortable scalar} less-than, less-than-or-equal, greater-than-or-equal, and greater-than bounds, respectively. \cr   \tab   \cr
#'                `.n, .nr, .nc`         \tab A vector of valid numbers of elements, rows, and columns, respectively.                                                                    \cr   \tab   \cr
#'                `.vals`                \tab A vector of valid values.                                                                                                                                 }
#' @return A logical scalar.
#' @examples
#' chrs <- c("a", "b", "c")
#' nums <- 1:3
#' sqrd <- matrix(1:16, nrow = 4)
#' meets(chrs, .n = 1:5)
#' meets(chrs, .max = 2)
#' meets(chrs, .min = 3)
#' meets(sqrd, .nr = 3, .nr = 2:10)
#' meets(sqrd, .minr = 2, .maxr = 5, .minc = 4, .maxc = 5)
#' meets(chrs, .vals = letters)
#' meets(chrs, .gt = "a", .lt = "c")
#' meets(sqrd, .ge = 1, .le = 16)
#' @export
meets <- function(x, ...) {
  if (base::...length() == 0) {return(T)}
  Errors <- ppp::meets_errs(x, ...)
  if (!base::is.null(Errors)) {ppp::stopperr(Errors, .PKG = "ppp")}
  Atoms <- ppp::av(x)
  if (ppp::.D1D(x)) {NX <- base::length(x)} else {NX <- base::prod(base::dim(x))}
  Atoms <- Atoms[!base::is.na(Atoms)]
  NR    <- base::NROW(x)
  NC    <- base::NCOL(x)
  Dots  <- base::list(...)
  if (base::length(Atoms) == 0) {return(T)}
  else if (!base::is.null(Dots$.n)) {if (!(NX %in% Dots$.n)) {return(F)}}
  else if (!base::is.null(Dots$.nr)) {if (!(NR %in% Dots$.nr)) {return(F)}}
  else if (!base::is.null(Dots$.nc)) {if (!(NC %in% Dots$.nc)) {return(F)}}
  else if (!base::is.null(Dots$.le)) {if (base::any(Atoms > Dots$.le)) {return(F)}}
  else if (!base::is.null(Dots$.ge)) {if (base::any(Atoms < Dots$.ge)) {return(F)}}
  else if (!base::is.null(Dots$.lt)) {if (base::any(Atoms >= Dots$.lt)) {return(F)}}
  else if (!base::is.null(Dots$.gt)) {if (base::any(Atoms <= Dots$.gt)) {return(F)}}
  else if (!base::is.null(Dots$.min)) {if (!(NX >= Dots$.min)) {return(F)}}
  else if (!base::is.null(Dots$.max)) {if (!(NX <= Dots$.max)) {return(F)}}
  else if (!base::is.null(Dots$.minr)) {if (!(NR >= Dots$.minr)) {return(F)}}
  else if (!base::is.null(Dots$.maxr)) {if (!(NR <= Dots$.maxr)) {return(F)}}
  else if (!base::is.null(Dots$.minc)) {if (!(NC >= Dots$.minc)) {return(F)}}
  else if (!base::is.null(Dots$.maxc)) {if (!(NC <= Dots$.maxc)) {return(F)}}
  else if (!base::is.null(Dots$.vals)) {if (base::all(Atoms %in% Dots$.vals)) {return(F)}}
  T
}
