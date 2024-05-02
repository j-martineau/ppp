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
  errs <- ppp::meets_errs(x, ...)
  if (!base::is.null(errs)) {ppp::stopperr(errs, .PKG = "ppp")}
  atoms <- ppp::av(x)
  if (ppp::.D1D(x)) {nx <- base::length(x)} else {nx <- base::prod(base::dim(x))}
  atoms <- atoms[!base::is.na(atoms)]
  nr    <- base::NROW(x)
  nc    <- base::NCOL(x)
  dots  <- base::list(...)
  if (base::length(atoms) == 0) {return(T)}
  else if (!base::is.null(dots$.n   )) {if (!(nx %in% dots$.n              )) {return(F)}}
  else if (!base::is.null(dots$.nr  )) {if (!(nr %in% dots$.nr             )) {return(F)}}
  else if (!base::is.null(dots$.nc  )) {if (!(nc %in% dots$.nc             )) {return(F)}}
  else if (!base::is.null(dots$.le  )) {if (base::any(atoms > dots$.le     )) {return(F)}}
  else if (!base::is.null(dots$.ge  )) {if (base::any(atoms < dots$.ge     )) {return(F)}}
  else if (!base::is.null(dots$.lt  )) {if (base::any(atoms >= dots$.lt    )) {return(F)}}
  else if (!base::is.null(dots$.gt  )) {if (base::any(atoms <= dots$.gt    )) {return(F)}}
  else if (!base::is.null(dots$.min )) {if (!(nx >= dots$.min              )) {return(F)}}
  else if (!base::is.null(dots$.max )) {if (!(nx <= dots$.max              )) {return(F)}}
  else if (!base::is.null(dots$.minr)) {if (!(nr >= dots$.minr             )) {return(F)}}
  else if (!base::is.null(dots$.maxr)) {if (!(nr <= dots$.maxr             )) {return(F)}}
  else if (!base::is.null(dots$.minc)) {if (!(nc >= dots$.minc             )) {return(F)}}
  else if (!base::is.null(dots$.maxc)) {if (!(nc <= dots$.maxc             )) {return(F)}}
  else if (!base::is.null(dots$.vals)) {if (base::all(atoms %in% dots$.vals)) {return(F)}}
  T
}
