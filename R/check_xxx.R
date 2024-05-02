# check whether Dots are named
.all_named <- function(...) {
  n    <- base::...length()
  labs <- base::...names()
  if (n == 0) {F} else if (base::length(labs) != n) {F} else {!base::any(base::is.na(labs))}
}

# simple oxford comma lists
.ox_vals <- function(x, join) {
  n <- base::length(x)
  if (n == 1) {x} else if (n == 2) {base::paste0(x[1], " ", join, " ", x[2])} else {base::paste0(base::paste0(x[1:(n - 1)], collapse = ", "), ", ", join, " ", x[n])}
}

#' @encoding UTF-8
#' @family Errs
#' @title Incremental Error Checking
#' @description Conditionally bank error messages in the immediate environment of a function to allow for exhaustive error checking before throwing an exception. Results in a possibly multiple-error, accumulated message to be processed upon completion of error checking.
#' @param ... Differs by function in terms of whether they are named, how many there are, and their \link[=ppp]{property requirements} as described in the *functions* section.
#' @param .a A logical scalar indicating whether to \link[=av]{atomize} `...` args.
#' @param .d A non-`NA` character scalar delimiter for collapsing `...` into an error message.
#' @param .na A logical scalar indicating whether `NA` values qualify as `'logical'`.
#' @param .fun A character scalar naming the function generating an error or errors.
#' @param .funs A \link[=cmp_chr_vec]{complete character vec} containing `1` or more \link[=prop_funs]{property function} names.
#' @param .spec A \link[=cmp_chr_scl]{complete character scalar} containing a \link[=is_prop_spec]{property spec}.
#' @param .vals A \link[=cmp_atm]{complete atomic object} of length `length(.whens)`.
#' @param .chars A \link[=cmp_str]{complete string object} containing
#' @param .named A logical scalar indicating whether `...` args must uniquely named without using `""`.
#' @param .whens A \link[=pop_atm]{populated atomic object} of length `length(.vals)`.
#' @param .extras `NULL` or a \link[=cmp_atm]{complete atomic object} containing additional valid values.
#' @return **A **\code{\link[base]{simpleError}} **object** \cr\cr `getterr`
#' \cr\cr  **A character vector**                           \cr\cr `banked_Errs`
#' \cr\cr  All others are called for their side effects.
#' @examples
#' egStopper <- function() {stopperr('stopper demo', pkg = 'ppp')}
#' egErrs    <- function() {Errs('Errs demo1', 'Errs demo2', pkg = 'ppp')}
#' egErr     <- function() {err('err', 'demo', pkg = 'ppp')}
#' egErrors  <- function(..., tf = NA, lgl = 42, not = FALSE, pop = NULL,
#'                            fail = simpleError('error'), funs = 2:4, spec = 42,
#'                            vals = 42, class = 42, nas.or = NULL, nll.or = NA,
#'                            chars = '5', when.a = "error.a", when.b = "error.b") {
#'   bankerr(...elt(1))
#'   bankErrs(...elt(2), ...elt(3))
#'   check_tf(tf = tf)
#'   check_lgl(lgl = lgl)
#'   check_t(not = not)
#'   check_pop(pop = pop)
#'   check_fail(fail = fail)
#'   check_funs(c('cmp_ch1_vec', 'cmp_ngw_vec'), funs = funs)
#'   check_spec('cmp_ch1_vec|nll|nas', spec = spec)
#'   check_vals(letters, vals = vals)
#'   check_cls('data.frame', class)
#'   check_nas_or(c('cmp_ch1_vec', 'cmp_ngw_vec'), nas.or = nas.or)
#'   check_nll_or(c('cmp_ch1_vec', 'cmp_ngw_vec'), nll.or = nll.or)
#'   check_chars(letters, chars = chars)
#'   check_when(when.a = when.a, when.b = when.b, c('error.a', ''), c('error.b', ''))
#'   checkerr(pkg = 'ppp')
#' }
#' \dontrun{
#'   egstopperr()
#'   getterr()
#'   purgerr()
#'   getter()
#'   egErrs()
#'   egErrs()
#'   egErrors()
#' }
#' @export
check_xxx <- function() {utils::help("check_xxx", package = "ppp")}

#' @describeIn check_xxx Checks named `...` arguments for scalar `TRUE`-ness. If any named `...` argument is not scalar `TRUE`, collapses *unnamed* `...` args to an error message template, replacing the escape sequence `'{@@}'` with each non-qualifying *named* `...` arg's name.
#' @export
check_t <- function(..., .d = " ") {
  if (uj::is_err(.d)) {.d <- " "} else if (!base::is.character(.d) | base::length(.d) != 1) {.d <- " "} else if (base::is.na(.d)) {.d <- " "}
  dots <- uj::named_dots(...)
  anon <- uj::anon_dots(...)
  mssg <- base::paste0(uj::av(anon), collapse = .d)
  labs <- base::names(dots)
  errs <- NULL
  if (base::length(dots) == 0                               ) {errs <- base::c(errs, "There are no named [...] args."                                                                                  )}
  if (base::length(anon) != base::length(base::unique(labs))) {errs <- base::c(errs, "Named [...] args must be uniquely named."                                                                        )}
  if (base::length(anon) == 0                               ) {errs <- base::c(errs, "There are no unnamed [...] args."                                                                                )}
  if (!base::any(base::grepl("{@}", mssg, fixed = TRUE))    ) {errs <- base::c(errs, "At least 1 unnamed [...] arg must contain the escape sequence '{@}' for inserting the names of named [...] args.")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "ppp")}
  for (i in 1:base::length(dots)) {if (!dots[[1]]) {ppp::bankerr(base::gsub("{@}", base::paste0("[", labs[i], "]"), mssg, fixed = T), gens = 1)}}
}

#' @describeIn check_xxx Checks named `...` arguments for scalar `TRUE`-ness or scalar `FALSE`-ness. If any named `...` argument is neither scalar `TRUE` nor scalar `FALSE`, banks an error indicating that the argument must be scalar `TRUE` or scalar `FALSE`. NOTE: unnamed `...` arguments are not valid.
#' @export
check_tf <- function(...) {
  errs <- NULL
  if (base::...length() == 0) {errs <- base::c(errs, "There are no [...] args")}
  if (!ppp:::.all_named(...)) {errs <- base::c(errs, "All [...] args must be uniquely named without using \"\".")}
  if (!base::is.null(errs)  ) {ppp::stopperr(errs, pkg = "ppp")}
  labs <- base::...names()
  dots <- base::list(...)
  ok   <- base::sapply(dots, base::isTRUE) | base::sapply(dots, base::isFALSE)
  for (i in 1:base::length(ok)) {if (!ok[i]) {ppp::bankerr("[", labs[i], "] must be scalar TRUE or scalar FALSE.", gens = 1, d = "")}}
}

#' @describeIn check_xxx Checks each named `...` arg for scalar `TRUE`-ness, scalar `FALSE`-ness, scalar `NA`-ness (if `.na` is scalar `TRUE`), or scalar membership in `.extras` (when `.extras` is a \link[=cmp_atm]{complete atomic object}). Banks an error for each `...` that does not qualify. NOTE: unnamed `...` arguments are not valid.
#' @export
check_lgl <- function(..., .na = FALSE, .extras = NULL) {
  if (ppp::.cmp_lgl_scl(.na)) {    .na <- .na    } else {.na     <- F   }
  if (ppp::.cmp_atm(.extras)) {.extras <- .extras} else {.extras <- NULL}
  errs    <- NULL
  if (base::...length() > 0) {errs <- base::c(errs, "There are no [...] args")}
  if (ppp:::.all_named(...)) {errs <- base::c(errs, "All [...] args must be uniquely named without using \"\".")}
  if (!base::is.null(errs) ) {ppp::stopperr(errs, pkg = "ppp")}
  X <- base::list(...)
  if (!(base::all(base::sapply(X, ppp:::.atm_scl)))) {ppp::stopperr("All [...] args must be atomic scalars.", pkg = "ppp")}
  ok <- base::sapply(X, base::isFALSE)
  if (.na) {ok <- ok & base::all(base::is.na(X))}
  if (!base::all(ok)) {
    labs <- base::paste0("[", base::...names()[!ok], "]")
    if (base::length(labs) > 1) {mults <- base::c("s", "")} else {mults <- base::c("", "s")}
    vals <- base::c("TRUE", "FALSE")
    if (.na) {vals <- base::c(vals, "NA")}
    if (!base::is.null(.extras)) {
      if (base::is.character(.extras)) {vals <- base::c(vals, base::paste0("'", .extras, "'"))}
      else {vals <- base::c(vals, base::as.character(.extras))}
    }
    err <- base::paste0("Argument", mults[1], ppp:::.ox_vals(labs, "and"), "contain", mults[2], " one or more values not in {", ppp:::.ox_vals(vals, "and"), "}.")
    ppp::bankerr(err, gens = 1)
  }
}

#' @describeIn check_xxx Checks each named `...` argument for `NULL`-ness or for any property describe by any property function named in character argument `.funs`. Banks an automatically-generated error message for each `...` argument that does not qualify. NOTE: Unnamed `...` arguments are not valid.
#' @export
check_nll_or <- function(.funs, ..., .vals = NULL) {
  .funs  <- uj::failsafe(.funs)
  .vals  <- uj::failsafe(.vals)
  errs <- NULL
  if (uj::is_err(.funs)         ) {errs <- base::c(errs, "[.funs] must be a complete string vec (?cmp_str_vec).")}
  if (uj::is_err(.vals)         ) {errs <- base::c(errs, "[.vals] must be NULL or a valid R object.")}
  if (!base::is.null(errs)      ) {ppp::stopperr(errs, pkg = "ppp")}
  if (!ppp:::.cmp_chr_vec(.funs)) {ppp::stopperr("[.funs] must be a complete string vec (?cmp_str_vec).")}
  .funs  <- ppp::av(base::strsplit(.funs, "|", TRUE))
  okFuns <- base::all(.funs %in% ppp::prop_funs())
  labs   <- base::...names()
  nX     <- base::...length()
  okX    <- nX > 0
  if (!okX                ) {okLabs <- T} else {okLabs <- ppp:::.all_named(...)}
  if (base::is.null(.vals)) {okVals <- T} else {okVals <- ppp::.cmp_vals(.vals)}
  if (!okX   ) {errs <- base::c(errs, "[...] arguments must be supplied.")}
  if (!okLabs) {errs <- base::c(errs, "[...] args must be uniquely named without using blank strings.")}
  if (!okFuns) {errs <- base::c(errs, "[.funs] must contain 1+ function names found in prop_funs().")}
  if (!okVals) {errs <- base::c(errs, "[.vals] must be NULL or complete and atomic (?cmp_atm).")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "ppp")}
  errs <- base::paste0("[", labs, "] must be NULL or ", ppp::spec_concise(.funs))
  for (i in 1:nX) {if (!base::is.null(base::...elt(i))) {
    ok <- F
    for (fun in .funs) {
      ok <- ok | base::eval(base::parse(text = base::paste0(Fun, "(base::...elt(i))")))
      if (ok & !base::is.null(.vals)) {ok <- ok & base::all(base::...elt(i) %in% .vals)}
    }
    if (!ok) {ppp::bankerr(errs[i], gens = 1)}
  }}
}

#' @describeIn check_xxx Checks each named `...` argument for scalar `NA`-ness or for any property describe by any property function named in character argument `.funs`. Banks an automatically-generated error message for each `...` argument that does not qualify. NOTE: Unnamed `...` arguments are not valid.
#' @export
check_nas_or <- function(.funs, ..., .vals = NULL) {
  .funs  <- uj::failsafe(.funs)
  .vals  <- uj::failsafe(.vals)
  errs <- NULL
  if (uj::is_err(.funs)        ) {errs <- base::c(errs, "[.funs] must be a complete string vec (?cmp_str_vec).")}
  if (uj::is_err(.vals)        ) {errs <- base::c(errs, "[.vals] must be NULL or a valid R object.")}
  if (!base::is.null(errs)     ) {ppp::stopperr(errs, pkg = "ppp")}
  if (!ppp::.cmp_str_vec(.funs)) {ppp::stopperr("[.funs] must be a complete string vec (?cmp_str_vec).")}
  .funs  <- ppp::av(base::strsplit(.funs, "|", TRUE))
  okFuns <- base::all(.funs %in% ppp::prop_funs())
  labs   <- base::...names()
  nX     <- base::...length()
  okX    <- nX > 0
  if (!okX                ) {okLabs <- T} else {OkLabs <- ppp:::.all_named(...)}
  if (base::is.null(.vals)) {okVals <- T} else {okVals <- ppp::.cmp_atm(.vals) }
  if (!okX   ) {errs <- base::c(errs, "[...] okVals must be supplied.")}
  if (!okLabs) {errs <- base::c(errs, "[...] args must be uniquely named without using blank strings.")}
  if (!okFuns) {errs <- base::c(errs, "[.funs] must contain 1+ function names found in prop_funs().")}
  if (!okVals) {errs <- base::c(errs, "[.vals] must be NULL or complete and atomic (?cmp_atm).")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "ppp")}
  errs <- base::paste0("[", labs, "] must be scalar NA or ", ppp::spec_concise(.funs))
  for (i in 1:nX) {if (!ppp::.NA0(base::...elt(i))) {
    ok <- F
    for (fun in .funs) {
      ok <- ok | base::eval(base::parse(text = base::paste0(fun, "(base::...elt(i))")))
      if (ok & !base::is.null(.vals)) {ok <- ok & base::all(base::...elt(i) %in% .vals)}
    }
    if (!ok) {ppp::bankerr(errs[i], gens = 1)}
  }}
}

#' @describeIn check_xxx Checks each named `...` argument for any one the classes named in character argument `.cls`. Banks an automatically-generated error message for each `...` argument that does not qualify. NOTE: Unnamed `...` arguments are not valid.
#' @export
check_cls <- function(.cls, ...) {
  if (uj::is_err(.cls)) {ppp::stopperr("[.cls] must be a complete character vec (?cmp_chr_vec)", pkg = "ppp")}
  dots   <- uj::named_dots(...)
  anon   <- uj::anon_dots(...)
  labs   <- base::names(dots)
  okN    <- (base::length(dots) + base::length(anon)) > 0
  okLabs <- base::length(anon) == 0
  okUnq  <- base::length(labs) == base::length(base::unique(labs))
  okCls  <- ppp::.cmp_chr_vec(.cls)
  errs   <- NULL
  if (!okN   ) {errs <- base::c(errs, "There are no [...] args.")}
  if (!okUnq ) {errs <- base::c(errs, "[...] arg names must be unique.")}
  if (!okCls ) {errs <- base::c(errs, "[.cls] must be a complete character vec (?cmp_chr_vec).")}
  if (!okLabs) {errs <- base::c(errs, "All [...] args must be named.")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "ppp")}
  .cls <- ppp::av(base::strsplit(.cls, "|", fixed = T))
  mssg <- base::paste0("'", .cls, "'")
  n    <- base::length(mssg)
  if      (n == 1) {mssg <- base::paste0("class ", mssg)}
  else if (n == 2) {mssg <- base::paste0("class ", mssg[1], " or ", mssg[2])}
  else             {mssg <- base::paste0("any class in c(", paste0(mssg, collapse = ", "), ")")}
  for (i in 1:base::length(dots)) {
    classes <- base::class(dots[[i]])
    if (!base::any(.cls %in% classes)) {ppp::bankerr("[", labs[i], "] must be of ", mssg, ".", gens = 1, .d = "")}
  }
}

#' @describeIn check_xxx Checks each named `...` for \link[=POP]{populated-ness} (i.e., non-`NULL` and not of length `0`). Banks an error for each `...` argument that does not qualify. NOTE: Unnamed `...` arguments are not valid.
#' @export
check_pop <- function(...) {
  dots   <- uj::named_dots(...)
  anon   <- uj::anon_dots(...)
  labs   <- base::names(dots)
  okN    <- base::...length() > 0
  okDots <- base::length(anon) == 0
  okAnon <- !base::any(labs == "")
  okUnq  <- base::length(labs) == base::length(base::unique(labs))
  errs   <- NULL
  if (!okN   ) {errs <- base::c(errs, "There are no [...] args.")}
  if (!okDots) {errs <- base::c(errs, "All [...] args must be named.")}
  if (!okAnon) {errs <- base::c(errs, "[...] arg names may not be blank strings (\"\").")}
  if (!okUnq ) {errs <- base::c(errs, "[...] arg names must be unique.")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "ppp")}
  for (i in 1:base::length(dots)) {if (base::length(dots[[i]]) == 0) {ppp::bankerr("[", labs[i], "] is NULL or empty.", gens = 1, .d = "")}}
}

#' @describeIn check_xxx Checks each named `...` argument for a match to any of the property functions named in `.funs`, and if there is a match and atomic argument `.vals` is non-`NULL`, whether the values are all contained in atomic argument `.vals`. Banks an error for each `...` argument that does not qualify. NOTE: Unname `...` arguments are not valid.
#' @export
check_funs <- function(.funs, ..., .vals = NULL) {
  if (!ppp::.cmp_chr_vec(.funs)) {ppp::stopperr("[.funs] must be a complete character vec (?cmp_chr_vec).", pkg = "ppp")}
  .funs   <- ppp::av(base::strsplit(.funs, "|", TRUE))
  labs    <- base::...names()
  n       <- base::...length()
  okN     <- n > 0
  okDots  <- base::length(labs) == n
  okAnon  <- !base::any(labs == "")
  okUnq   <- base::length(labs) == base::length(base::unique(labs))
  okFuns  <- base::all(base::sapply(.funs, ppp::is_prop_fun))
  okVals  <- base::ifelse(base::is.null(.vals), T, ppp::.cmp_atm(.vals))
  errs    <- NULL
  if (!okN   ) {errs <- base::c(errs, "[...] arguments must be supplied.")}
  if (!okUnq ) {errs <- base::c(errs, "[...] arg names must be unique.")}
  if (!okFuns) {errs <- base::c(errs, "[.funs] contains a function name not found in ppp::prop_funs().")}
  if (!okVals) {errs <- base::c(errs, "[.vals] must be NULL or complete and atomic (?cmp_atm).")}
  if (!okDots) {errs <- base::c(errs, "all [...] args must be named.")}
  if (!okAnon) {errs <- base::c(errs, "[...] arg names may not be blank strings (\"\")")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "ppp")}
  errs <- base::paste0("[", labs, "] must have the following properties: ", ppp::spec_concise(.funs))
  for (i in 1:n) {
    dot <- base::...elt(i)
    ok  <- FALSE
    for (fun in .funs) {if (!ok) {ok <- base::eval(base::parse(text = base::paste0("ppp::", fun, "(dot)")))}}
    if (!ok) {ppp::bankerr(errs[i], gens = 1)}
  }
}

#' @describeIn check_xxx Checks each named `...` argument for a match to the property spec in character argument `.spec` with the additional check for disallowed `NA` values if logical scalar argument `.na = FALSE`. Banks an error for each `...` argument that does not qualify. NOTE: Unnamed `...` arguments are not valid.
#' @export
check_spec <- function(.spec, ..., .na = F) {
  n      <- base::...length()
  labs   <- base::...names()
  okN    <- n > 0
  okNas  <- uj::fs_na(.na)
  okDots <- base::length(labs) == n
  okAnon <- !base::any(labs == "")
  okUnq  <- base::length(labs) == base::length(base::unique(labs))
  okSpec <- ppp::.cmp_chr_scl(.spec)
  if (okSpec) {
    combos <- ppp::av(base::strsplit(.spec, "|", fixed = TRUE))
    for (combo in combos) {
      props <- ppp::av(base::strsplit(combo, "_", fixed = T))
      if (!base::all(base::tolower(props) %in% ppp::all_props())) {okSpec <- F}
      else if (base::length(props) != base::length(base::unique(props))) {okSpec <- F}
    }
  }
  errs <- NULL
  if (!okN   ) {errs <- base::c(errs, "[...] is empty.")}
  if (!okNas ) {errs <- base::c(errs, "[.na] must be TRUE or FALSE.")}
  if (!okDots) {errs <- base::c(errs, "all [...] args mut be named.")}
  if (!okAnon) {errs <- base::c(errs, "[...] arg names may not be blank.")}
  if (!okUnq ) {errs <- base::c(errs, "[...] arg names must be unique.")}
  if (!okSpec) {errs <- base::c(errs, "[.spec] is not a valid property specification (?is_prop_spec).")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "ppp")}
  errs <- base::paste0("[", labs, "] must be ", ppp::spec_concise(.spec), ".")
  for (i in 1:n) {
    val <- F
    if (.na ) {val <- base::is.na(base::...elt(i))}
    if (!val) {val <- ppp::PPP(base::...elt(i), .spec)}
    if (!val) {ppp::bankerr(errs[i], gens = 1)}
  }
}

#' @describeIn check_xxx Checks each named `...` argument for atomic-ness and for containing only the atomic values given in `.vals`. Banks an error for each non-qualifying `...` argument. NOTE: Unnamed `...` arguments are not valid.
#' @export
check_vals <- function(.vals, ..., .a = TRUE, .na = FALSE) {
  valid <- function(y) {
    if (base::is.atomic(y)) {base::all(base::sapply(ppp::av(y), base::`%in%`, table = .vals))}
    else if (base::is.data.frame(y)) {base::all(base::apply(y, 2, valid))}
    else if (base::is.list(y)) {base::all(base::sapply(y, valid))}
    else {F}
  }
  n      <- base::...length()
  labs   <- base::...names()
  okN    <- n > 0
  okA    <- ppp::.cmp_lgl_scl(.a)
  okNas  <- ppp::.cmp_lgl_scl(.na)
  okDots <- base::length(labs) == n
  okAnon <- !base::any(labs == "")
  okUnq  <- base::length(labs) == base::length(base::unique(labs))
  okVals <- ppp::.cmp_atm(.vals)
  errs   <- NULL
  if (!okN   ) {errs <- base::c(errs, "[...] is empty.")}
  if (!okA   ) {errs <- base::c(errs, "[.a] must be TRUE or FALSE.")}
  if (!okNas ) {errs <- base::c(errs, "[.na] must be TRUE or FALSE.")}
  if (!okVals) {errs <- base::c(errs, "[.vals] must be complete and atomic (?cmp_atm).")}
  if (!okDots) {errs <- base::c(errs, "all [...] args must be named.")}
  if (!okAnon) {errs <- base::c(errs, "[...] arg names may not be blank strings (\"\").")}
  if (!okUnq ) {errs <- base::c(errs, "[...] arg names must be unique.")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "ppp")}
  dots <- base::list(...)
  atm  <- base::sapply(dots, ppp::.POP) & base::sapply(dots, ppp::.aTM)
  if (.a & !base::all(atm)) {ppp::stopperr("When [.a = TRUE], all [...] args must be populated and atomic (?pop_atm).", pkg = "ppp")}
  if (!.a) {
    vls <- base::sapply(dots, ppp::atm_vls)
    dtf <- base::sapply(dots, ppp::atm_dtf)
    if (!base::all(atm | vls | dtf)) {ppp::stopperr("All [...] args must be populated and atomic (?pop_atm), atomic vlists (?atm_vls), or atomic data.frames (?atm_dtf).", pkg = "ppp")}
  }
  for (i in 1:n) {if (!valid(dots[[i]])) {ppp::bankerr("[", labs[i], "] contains 1 or more values not in [.vals].", gens = 1, .d = "")}}
}

#' @describeIn check_xxx Checks each named `...` argument (assumed to be of mode `'character'`) for whether it contains only the characters contained in the character argument `.chars`. When `.a = TRUE`, \link[=av]{atomizes} `...` before value checking. Banks an error for each `...` argument that does not qualify. NOTE: Unnamed `...` arguments are not valid.
#' @export
check_chars <- function(.chars, ..., .a = TRUE) {
  valid <- function(y) {
    if (base::is.character(y)) {
      y <- base::paste0(ppp::av(y), collapse = "")
      y <- ppp::av(base::strsplit(y, "", fixed = T))
      base::all(y %in% .chars)
    } else if (base::is.data.frame(y)) {base::all(base::apply(y, 2, valid))}
    else if (base::is.list(y)) {base::all(base::sapply(y, valid))}
    else {F}
  }
  n       <- base::...length()
  labs    <- base::...names()
  okN     <- n > 0
  okA     <- ppp::.cmp_lgl_scl(.a)
  okDots  <- base::length(labs) == n
  okAnon  <- !base::any(labs == "")
  okUnq   <- base::length(labs) == base::length(base::unique(labs))
  okChars <- ppp::.cmp_str(.chars)
  errs    <- NULL
  if (!okN    ) {errs <- base::c(errs, "[...] is empty.")}
  if (!okA    ) {errs <- base::c(errs, "[.a] must be TRUE or FALSE.")}
  if (!okChars) {errs <- base::c(errs, "[.chars] must be complete and of mode string (?cmp_str).")}
  if (!okDots ) {errs <- base::c(errs, "all [...] args must be named.")}
  if (!okAnon ) {errs <- base::c(errs, "[...] arg names may not be blank strings (\"\").")}
  if (!okUnq  ) {errs <- base::c(errs, "[...] arg names must be unique.")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "ppp")}
  dots <- base::list(...)
  chr  <- base::sapply(dots, ppp::.POP) & base::sapply(dots, ppp::.CHR)
  if (.a | !base::all(chr)) {ppp::stopperr("When [.a = TRUE], all [...] args must be populated and of mode character (?pop_chr).", pkg = "ppp")}
  if (!.a) {
    vls <- base::sapply(dots, chr_vls)
    dtf <- base::sapply(dots, chr_dtf)
    if (!base::all(chr | vls | dtf)) {ppp::stopperr("All [...] args must be populated and of mode character (?pop_chr), character vlists (?chr_vls), or character data.frames (?chr_dtf).", pkg = "ppp")}
  }
  for (i in 1:n) {if (!valid(dots[[i]])) {ppp::bankerr("[", labs[i], "] contains 1 or more characters not in [.chars].", gens = 1, .d = "")}}
}

#' @describeIn check_xxx Checks each `...` argument for a match to at least one value of the property spec on character argument `.spec`. Optionally checks whether all `...` arguments are named (when `.named = TRUE`). Banks an error for missing `...` argument names (if there are an missing names and `.named = TRUE`). Also banks an error for any non-qualifying `...` argument. for whether it contains only the characters contained in the character argument `.chars`. When `.a = TRUE`, \link[=av]{atomizes} `...` before value checking. Banks an error for each `...` argument that does not qualify.
#' @export
check_dots <- function(.spec, ..., .named = FALSE) {
  n   <- base::...length()
  labs  <- base::...names()
  okDots <- ppp::.cmp_lgl_scl(.named)
  okSpec  <- ppp::.cmp_chr_vec(.spec)
  if (okSpec) {
    combos <- ppp::av(base::strsplit(.spec, "|", fixed = TRUE))
    for (combo in combos) {
      props <- ppp::av(base::strsplit(combo, "_", fixed = TRUE))
      if (!base::all(base::tolower(props) %in% ppp::all_props())) {okSpec <- FALSE}
      else if (base::length(props) != base::length(base::unique(props))) {okSpec <- FALSE}
    }}
  okN    <- n > 0
  okDots <- !base::isTRUE(.named) | base::length(labs) == n
  okAnon <- !base::any(labs == "")
  okUnq  <- base::length(labs) == base::length(base::unique(labs))
  errs <- NULL
  if (!okDots) {errs <- base::c(errs, "[.named] must be TRUE or FALSE.")}
  if (!okN   ) {errs <- base::c(errs, "[...] is empty.")}
  if (!okSpec) {errs <- base::c(errs, "[.spec] must be complete and character (?cmp_chr).")}
  if (!okDots) {errs <- base::c(errs, "all [...] args must be named when [.named = TRUE].")}
  if (!okAnon) {errs <- base::c(errs, "[...] arg names may not be blank strings (\"\").")}
  if (!okUnq ) {errs <- base::c(errs, "[...] arg names must be unique.")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "ppp")}
  if (!base::all(base::sapply(base::list(...), ppp::PPP, spec = .spec))) {ppp::stopperr(base::paste0("All [...] args must be ", ppp::spec_concise(.spec), "."))}
}

#' @describeIn check_xxx Assumes two named, atomic, scalar `...` arguments. Checks that the value of the second named `...` argument is appropriate given the value of the first `...` argument by identifying which element of `.whens` is equal to the value of the first `...` argument, and checking the corresponding element of `.vals` against the second `...` argument. Banks an error if the value of the first and the value of the second `...` arguments do not pass the check. NOTE: There must be only two named `...` arguments and unnamed `...` arguments are not valid.
#' @export
check_when <- function(.whens, .vals, ...) {
  labs     <- base::...names()
  n        <- base::...length()
  okN      <- n == 2
  okDots   <- base::length(labs) == n
  okAnon   <- !base::any(labs == "")
  okUnq    <- base::length(labs) == base::length(base::unique(labs))
  okWhens  <- ppp::.pop_atm(.whens)
  okVals   <- ppp::.pop_atm(.vals)
  okNeq    <- base::length(.whens) == length(.vals)
  okScl    <- ppp::.atm_scl(..1) & ppp::.atm_scl(..2)
  okWhens2 <- base::ifelse(okWhens & okVals, uj:::.compat(.whens, ..1), T)
  okVals2  <- base::ifelse(okWhens & okVals, uj:::.compat(.vals , ..2), T)
  errs     <- NULL
  if (!okN     ) {errs <- base::c(errs, "There must be two [...] args")}
  if (!okDots  ) {errs <- base::c(errs, "all [...] args must be named.")}
  if (!okAnon  ) {errs <- base::c(errs, "[...] arg names may not be blank.")}
  if (!okUnq   ) {errs <- base::c(errs, "[...] arg names must be unique.")}
  if (!okVals  ) {errs <- base::c(errs, "[.vals] must be non-empty and atomic.")}
  if (!okWhens ) {errs <- base::c(errs, "[.whens] must be non-empty and atomic.")}
  if (!okNeq   ) {errs <- base::c(errs, "[.whens] and [.vals] must be of the same length.")}
  if (!okScl   ) {errs <- base::c(errs, "Both args in [...] must be atomic and scalar (?atm_scl).")}
  if (!okWhens2) {errs <- base::c(errs, "[.vals] and [..2] are of incompatible (?compatible) modes.")}
  if (!okVals2 ) {errs <- base::c(errs, "[.whens] and [..1] are of incompatible (?compatible) modes.")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "ppp")}
  labs1 <- base::paste0("[", labs[1], "]")
  labs2 <- base::paste0("[", labs[2], "]")
  when  <- ..1
  val   <- ..2
  i     <- base::which(base::sapply(.whens, uj::is_eq0, y = when))
  if (base::length(i) > 0) {
    match <- .vals[i[1]]
    okEq  <- base::length(val) == 1 & base::length(match) == 1
    if ( okEq) {okEq <- val == match}
    if (!okEq) {
      if (base::is.character(match)) {match <- base::paste0("'", match, "'")}
      ppp::bankerr("When ", labs1, " is ", when, ", ", labs2, " must be ", match, ".", gens = 1, .d = "")
    }}
}

#' @describeIn check_xxx Checks each named `...` argument for validity (i.e., evaluating an argument does not produce an error). Banks an error for each `...` argument that does not qualify. NOTE: Unnamed `...` arguments are not valid.
#' @export
check_fail <- function(...) {
  n      <- base::...length()
  labs   <- base::...names()
  okN    <- n > 0
  okDots <- base::length(labs) == n
  okAnon <- !base::any(labs == "")
  okUnq  <- base::length(labs) == base::length(base::unique(labs))
  errs   <- NULL
  if (!okN   ) {errs <- base::c(errs, "[...] is empty.")}
  if (!okDots) {errs <- base::c(errs, "all [...] args must be named with.")}
  if (!okAnon) {errs <- base::c(errs, "[...] arg names may not be blank strings (\"\").")}
  if (!okUnq ) {errs <- base::c(errs, "[...] arg names must be unique.")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, pkg = "ppp")}
  for (i in 1:n) {if (uj::is_err(base::...elt(i))) {ppp::bankerr("evaluating arg [", labs[i], "] produced an error: ", ppp::av(base::...elt(i)), gens = 1, .d = "")}}
}
