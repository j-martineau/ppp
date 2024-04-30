#' @encoding UTF-8
#' @title Combo Uniqueness and Extended Class Properties
#' @description Functions checking for combinations of \link[=UNQ]{uniqueness} and \link[=ccc]{extended class}.
#' @param x An R object.
#' @param .ccc A character scalar single extended property from \code{\link{ccc_props}()}.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @examples
#' UnqArr <- array(1:27, dim = c(3, 3, 3))
#' UnqDtf <- data.frame(a = c("a", "b", "c"), b = 1:3)
#' UnqMat <- matrix(1:9, nrow = 3)
#' UnqMvc <- 1:2
#' UnqScl <- 1
#' UnqVec <- "a"
#' UnqVls <- list(a = letters, b = 1:3)
#'
#' unq_ccc_funs()
#' unq_ccc(UnqArr, "arr")
#' unq_ccc(UnqVls, "dtf")
#' c(unq_dtf(UnqDtf), unq_arr(UnqArr), unq_arr(rbind(UnqDtf, UnqDtf)))
#' c(unq_mat(UnqMat), unq_arr(UnqVls), unq_arr(rbind(UnqMat, UnqMat)))
#' c(unq_arr(UnqArr), unq_arr(UnqDtf), unq_arr(c(UnqArr, UnqArr)))
#' c(unq_gen(UnqArr), unq_arr(UnqDtf), unq_arr(c(UnqArr, UnqArr)))
#' c(unq_gen(UnqVec), unq_arr(UnqArr), unq_arr(c(UnqVec, UnqCec)))
#' c(unq_gen(UnqMvc), unq_arr(UnqScl), unq_arr(c(UnqMvc, UnqMvc)))
#' c(unq_gen(UnqScl), unq_arr(UnqMvc), unq_arr(c(UnqScl, UnqScl)))
#' c(unq_mvc(UnqMvc), unq_arr(UnqVec), unq_arr(c(UnqMvc, UnqMvc)))
#' c(unq_scl(UnqScl), unq_arr(UnqMvc), unq_arr(c(UnqScl, UnqScl)))
#' c(unq_vec(UnqVec), unq_arr(UnqDtf), unq_arr(c(UnqVec, UnqVec)))
#' c(unq_vls(UnqVls), unq_arr(UnqVec), unq_arr(c(UnqVls, UnqVls)))
#' @export
unq_ccc_PROPS <- function() {utils::help("unq_ccc_PROPS", package = "ppp")}

#' @describeIn unq_ccc_PROPS Checks `x` for uniqueness and against the extended class in `.ccc`. Return a logical scalar.
#' @export
unq_ccc <- function(x, .ccc, ...) {
  UNQ <- function(x) {base::length(x) == base::length(base::unique(x))}
  if (ppp::cmp_ccc(x, .ccc, ...)) {
    .ccc <- base::tolower(.ccc)
    if (.ccc == "dtf") {base::all(base::apply(x, 2, UNQ))}
    else if (.ccc == "vls") {base::all(base::sapply(x, UNQ))}
    else {base::length(x) == base::length(base::unique(x))}
  } else {F}
}

#' @describeIn unq_ccc_PROPS Lists all combo uniqueness and extended class property checking functions. Returns a sorted, lowercase, character vector.
#' @export
unq_ccc_funs <- function() {base::paste0('unq_', ppp::ccc_props())}

#' @describeIn unq_ccc_PROPS Checks `x` for completeness and array-ness, subject to any count or content restrictions in `...`. Returns a logical scalar.
#' @export
unq_arr <- function(x, ...) {ppp::unq_ccc(x, 'arr', ...)}

#' @describeIn unq_ccc_PROPS Checks `x` for completeness and data.frame-ness, subject to any count or content restrictions in `...`. Returns a logical scalar.
#' @export
unq_dtf <- function(x, ...) {ppp::unq_ccc(x, 'dtf', ...)}

#' @describeIn unq_ccc_PROPS Checks `x` for completeness and generic-ness, subject to any count or content restrictions in `...`. Returns a logical scalar.
#' @export
unq_gen <- function(x, ...) {ppp::unq_ccc(x, 'gen', ...)}

#' @describeIn unq_ccc_PROPS Checks `x` for completeness and matrix-ness, subject to any count or content restrictions in `...`. Returns a logical scalar.
#' @export
unq_mat <- function(x, ...) {ppp::unq_ccc(x, 'mat', ...)}

#' @describeIn unq_ccc_PROPS Checks `x` for completeness and multivec-ness, subject to any count or content restrictions in `...`. Returns a logical scalar.
#' @export
unq_mvc <- function(x, ...) {ppp::unq_ccc(x, 'mvc', ...)}

#' @describeIn unq_ccc_PROPS Checks `x` for completeness and scalar-ness, subject to any count or content restrictions in `...`. Returns a logical scalar.
#' @export
unq_scl <- function(x, ...) {ppp::unq_ccc(x, 'scl', ...)}

#' @describeIn unq_ccc_PROPS Checks `x` for completeness and vec-ness, subject to any count or content restrictions in `...`. Returns a logical scalar.
#' @export
unq_vec <- function(x, ...) {ppp::unq_ccc(x, 'vec', ...)}

#' @describeIn unq_ccc_PROPS Checks `x` for completeness and vector-list-ness, subject to any count or content restrictions in `...`. Returns a logical scalar.
#' @export
unq_vls <- function(x, ...) {ppp::unq_ccc(x, 'vls', ...)}
