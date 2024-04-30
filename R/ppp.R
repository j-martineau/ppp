#' @encoding UTF-8
#' @title All purpose property checking
#' @description This family of functions brings together a variety of families defined in this and gives users the ability to manage object properties in a granular and concise manner.
#' \cr\cr **Property families defined by this package**
#' \tabular{ll}{  \code{\link{bbb}}   \tab basic properties                       \cr
#'                \code{\link{ccc}}   \tab xclass (extended class)                \cr
#'                \code{\link{ddd}}   \tab defined.D (defined dimensionality)     \cr
#'                \code{\link{eee}}   \tab effective.D (effective dimensionality) \cr
#'                \code{\link{iii}}   \tab integrity (completeness, uniqueness)   \cr
#'                \code{\link{mmm}}   \tab xmode (extended modes)                 \cr
#'                \code{\link{sss}}   \tab shape (geometric shape)                  }
#' This family uses concise and flexible *property specs* as defined in the following table.
#' \tabular{ll}{  *single*   \tab \link[=CH3]{3-char} scalars (i.e., `all(nchar(.) == 3)` with all values in `all_props()`. For example, `'ord', 'dtf', 'd2d',` and `'rct'` are *single* property specs.                                                                                                                                                                 \cr   \tab   \cr
#'                *combo*    \tab Character scalars containing multiple underscore-delimited *single* properties, indicating that those *single* properties must co-occur. For example, the *combo* property spec `'ord_dtf_d2d_rct'` (or an equivalent underscore-delimited permutation) indicates that all four *single* properties must co-occur to satisfy the spec. \cr   \tab   \cr
#'                *option*   \tab Character scalars containing multiple pipe-delimited *combo* and/or *single* property specs. For example, the *option* property spec `'ord|dtf_d2d|dtf_rct'` would be satisfied by an object with *single* property `'ord'`, *combo* property `'dtf_d2d'`, **or** *combo* property `'dtf_rct'`.                                        \cr   \tab   \cr
#'                *flex*     \tab A *single*, *combo*, or *option* property spec as defined above.                                                                                                                                                                                                                                                                                      }
#' @details **Functions for property spec decomposition**
#' \tabular{ll}{  `props_from_combo`   \tab What are the constituent *single* properties of a *combo* property spec? \cr   \tab   \cr
#'                `combo_from_spec`    \tab What are the constituent *combo* properties in a *flex* property spec?   \cr   \tab   \cr
#'                `props_from_spec`    \tab What are the *unique* constituent *single* properties in a *flex* property spec?        }
#' **Functions to check whether a value is a valid property spec**
#' \tabular{ll}{  `is_prop_combo`   \tab Is `Combo` a valid *combo* property spec?                    \cr   \tab   \cr
#'                `is_prop_spec`    \tab Is `Spec` a valid *flex* property spec?                      \cr   \tab   \cr
#'                `is_prop_fun`     \tab Is `Fun` the name of a dedicated property checking function? \cr   \tab   \cr
#'                `is_prop`         \tab Is `Prop` a valid *single* property?                                        }
#' **Functions to define properties and property specs**
#' \tabular{ll}{  `combo_concise`   \tab How is a *combo* property spec (concisely) defined?                       \cr   \tab   \cr
#'                `spec_concise`    \tab How is an *options* property spec (concisely) defined?                    \cr   \tab   \cr
#'                `prop_verbose`    \tab How is a *single* property (verbosely) defined?                           \cr   \tab   \cr
#'                `prop_defs`       \tab What are the definitions of all possible *single* properties (returned as a data.frame)? }
#' **Functions to list names of dedicated property-checking functions**
#' \tabular{ll}{  `ppp_or_funs`   \tab What dedicated functions check `x` for either special values or a match to a *flex* property spec? \cr   \tab   \cr
#'                `prop_funs`     \tab What dedicated functions check `x` for a match to a specific *single* or *combo* property?         \cr   \tab   \cr
#'                `all_props`     \tab What is the complete set of all possible *single* properties?                                                     }
#' **Functions to check object against arbitrary property specs**
#' \tabular{ll}{  `nas_or`   \tab Is `x` either `NA` or a match to the *flex* property spec in `Spec`?  \cr   \tab   \cr
#'                `nll_or`   \tab Is `x` either `NULL` or a match to the *flex* property spec in `Spec` \cr   \tab   \cr
#'                `ppp`      \tab Is `x` a match to the *flex* property spec in `Spec`?                                }
#' **Function to list all of an object's** single **properties across property families**
#' \tabular{ll}{  `ppp`   \tab What are all of `x`'s *single* properties compiled from all property families? }
#' For convenience, property functions from other function families are also described below.
#' \cr\cr **Functions to list all** single **properties in a property family**
#' \tabular{ll}{  `bbb_props`   \tab \link[=bbb]{basic}       \cr
#'                `ccc_props`   \tab \link[=ccc]{xclass}      \cr
#'                `ddd_props`   \tab \link[=ddd]{defined.D}   \cr
#'                `eee_props`   \tab \link[=eee]{effective.D} \cr
#'                `iii_props`   \tab \link[=iii]{integrity}   \cr
#'                `mmm_props`   \tab \link[=mmm]{xmode}       \cr
#'                `sss_props`   \tab \link[=sss]{shape}         }
#' **Functions to list names of dedicated property-checking functions**
#' \tabular{ll}{  `cmp_mmm_ccc_funs`   \tab integrity = `'cmp'` + xmode + xclass \cr
#'                `unq_mmm_ccc_funs`   \tab integrity = `'unq'` + xmode + xclass \cr   \tab  }
#' \tabular{ll}{  `cmp_ccc_funs`   \tab integrity = `'cmp'` + xclass \cr
#'                `cmp_mmm_funs`   \tab integrity = `'cmp'` + xmode  \cr
#'                `unq_ccc_funs`   \tab integrity = `'unq'` + xclass \cr
#'                `unq_mmm_funs`   \tab integrity = `'unq'` + xmode  \cr
#'                `bbb_ccc_funs`   \tab basic + xclass               \cr
#'                `bbb_mmm_funs`   \tab basic + xmode                \cr
#'                `mmm_ccc_funs`   \tab xmode + xclass               \cr   \tab  }
#' \tabular{ll}{  `bbb_funs`   \tab \link[=bbb]{basic}       \cr
#'                `ccc_funs`   \tab \link[=ccc]{xclass}      \cr
#'                `ddd_funs`   \tab \link[=ddd]{defined.D}   \cr
#'                `eee_funs`   \tab \link[=eee]{effective.D} \cr
#'                `iii_funs`   \tab \link[=iii]{integrity}   \cr
#'                `mmm_funs`   \tab \link[=mmm]{xmode}       \cr
#'                `sss_funs`   \tab \link[=sss]{shape}         }
#' **Dedicated functions to check an object for a specific** combo **or** single **property**
#' \cr\cr For these functions, `{bbb}`, `{ccc}`, `{ddd}`, `{eee}`, `{iii}`, `{mmm}`, and `{sss}` are placeholders for any given basic, xclass, defined.D, effective.D, integrity, xmode, and shape properties, respectively.
#' \tabular{ll}{  `{bbb}`   \tab \link[=bbb]{basic} = `'{bbb}'`       \cr
#'                `{ccc}`   \tab \link[=ccc]{xclass} = `'{ccc}'`      \cr
#'                `{ddd}`   \tab \link[=ddd]{defined.D} = `'{ddd}'`   \cr
#'                `{eee}`   \tab \link[=eee]{effective.D} = `'{eee}'` \cr
#'                `{iii}`   \tab \link[=iii]{integrity} = `'{iii}'`   \cr
#'                `{mmm}`   \tab \link[=mmm]{xmode} = `'{mmm}'`       \cr
#'                `{sss}`   \tab \link[=sss]{shape} = `'{sss}'`       \cr   \tab  }
#' \tabular{ll}{  `cmp_{ccc}`   \tab integrity = `'cmp'` + xclass = `'{ccc}'` \cr
#'                `cmp_{mmm}`   \tab integrity = `'cmp'` + xmode = `'{mmm}'`  \cr
#'                `unq_{ccc}`   \tab integrity = `'unq'` + xclass = `'{ccc}'` \cr
#'                `unq_{mmm}`   \tab integrity = `'unq'` + xmode = `'{mmm}'`  \cr   \tab  }
#' \tabular{ll}{  `{bbb}_{ccc}`   \tab basic = `'{bbb}'` + xclass = `'{ccc}'` \cr
#'                `{bbb}_{mmm}`   \tab basic = `'{bbb}'` + xmode = `'{mmm}'`  \cr
#'                `{mmm}_{ccc}`   \tab xmode = `'{mmm}'` + xclass = `'{ccc}'` \cr
#'                `{sss}_{ccc}`   \tab shape = `'{sss}'` + xclass = `'{ccc}'` \cr   \tab  }
#' \tabular{ll}{  `cmp_{mmm}_{ccc}`   \tab integrity = `'cmp'` + xmode = `'{mmm}'` + xclass = `'{ccc}'` \cr
#'                `unq_{mmm}_{ccc}`   \tab integrity = `'unq'` + xmode = `'{mmm}'` + xclass = `'{ccc}'`   }
#' **Functions to check an object against an arbitrary* combo *property spec**
#' \cr\cr For these functions, an uppercase letter repeated three times is a placeholder for the value of an arbitrary single property from the associated property family.
#' \tabular{ll}{  `bbb_ccc`   \tab basic property in arg `BBB` + xclass property in arg `CCC` \cr
#'                `mmm_ccc`   \tab xmode property in arg `MMM` + xclass property in arg `CCC` \cr
#'                `cmp_ccc`   \tab integrity = `'cmp'` + xclass property in arg `CCC`         \cr
#'                `sss_ccc`   \tab shape property in arg `SSS` + xclass property in arg `CCC` \cr
#'                `unq_ccc`   \tab integrity = `'unq'` + xclass property in arg `CCC`         \cr
#'                `bbb_mmm`   \tab basic property in arg `BBB` + xmode property in arg `MMM`  \cr
#'                `cmp_mmm`   \tab integrity = `'cmp'` + xmode property in arg `MMM`          \cr
#'                `unq_mmm`   \tab integrity = `'unq'` + xmode property in arg `MMM`          \cr   \tab  }
#' \tabular{ll}{  `cmp_mmm_ccc`   \tab integrity = `'cmp'` + xmode property in arg `MMM` + xclass property in arg `CCC` \cr   \tab   \cr
#'                `unq_mmm_ccc`   \tab integrity = `'unq'` + xmode property in arg `MMM` + xclass property in arg `CCC` }
#' **Functions to check objects against* flex *property specs in a single family**
#' \tabular{ll}{  `BBB`   \tab \link[=bbb]{basic}       \cr
#'                `CCC`   \tab \link[=ccc]{xclass}      \cr
#'                `DDD`   \tab \link[=ddd]{defined.D}   \cr
#'                `EEE`   \tab \link[=eee]{effective.D} \cr
#'                `III`   \tab \link[=iii]{integrity}   \cr
#'                `MMM`   \tab \link[=mmm]{xmode}       \cr
#'                `SSS`   \tab \link[=sss]{shape}         }
#' **Functions to retrieve all of an object's* single *properties of a specific family**
#' \tabular{ll}{  `bbb`   \tab \link[=bbb]{basic}       \cr
#'                `ccc`   \tab \link[=ccc]{xclass}      \cr
#'                `ddd`   \tab \link[=ddd]{defined.D}   \cr
#'                `eee`   \tab \link[=eee]{effective.D} \cr
#'                `iii`   \tab \link[=iii]{integrity}   \cr
#'                `mmm`   \tab \link[=mmm]{xmode}       \cr
#'                `sss`   \tab \link[=sss]{shape}         }
#' @param x An R object.
#' @param spec A \link[=cmp_chr_scl]{complete character scalar} containing one or more values from `ppp_vals()` separated by pipes and/or underscores. Combinations of properties can be specified by separating them with underscores. Separating properties or combinations of properties with pipes will result in a value of `TRUE` if any of them applies to `x`.
#' @param as.dtf `TRUE` or `FALSE` indicating whether to return the result as a data.frame with column `1` containing property values and column `2` containing the property families.
#' @param .valid A \link[=cmp_chr_vec]{complete character vec} containing all properties considered valid.
#' @param .print `TRUE` or `FALSE` indicating whether to print the property definition to the console.
#' @inheritDotParams meets
#' @inheritSection meets Specifying count and value restrictions
#' @examples
#' na0_or("5", "ch1")
#' na0_or(NA, "ch1")
#'
#' nll_or(NULL, "ch1")
#' nll_or("1", "ch1")
#' nll_or(7, "ch1")
#'
#' all_props()
#' prop_funs()
#'
#' spec_concise("nll|cmp_psw_vls|ch1_scl")
#' combo_concise("cmp_psw_vls")
#' prop_verbose("srt")
#' prop_verbose("nnw")
#'
#' combos_from_spec("nll|cmp_psw_vls|ch1_scl")
#' props_from_spec("nll|cmp_psw_vls|ch1_scl")
#' props_from_combo("cmp_psw_vls")
#'
#' PPP("7", "nll|cmp_psw_vls|ch1_scl")
#' PPP(NULL, "nll|cmp_psw_vls|ch1_scl")
#' PPP("35", "nll|cmp_psw_vls|ch1_scl")
#'
#' is_prop_combo("letter")
#' is_prop_combo("cmp_psw_vls")
#' is_prop_spec("nll|cmp_psw_vls|ch1_scl")
#'
#' is_prop("vls")
#' is_prop("18")
#'
#' is_prop_fun("cmp_psw_vls")
#' is_prop_fun("18")
#'
#' ppp(letters)
#' prop_defs()
#' @export
PROPS <- function() {utils::help("PROPS", package = "ppp")}

#' @describeIn PROPS Lists all properties of `x`. Returns a sorted, lowercase, character vector.
#' @export
ppp <- function(x) {base::sort(base::c(ppp::bbb(x), ppp::ccc(x), ppp::ddd(x), ppp::eee(x), ppp::iii(x), ppp::mmm(x), ppp::sss(x)))}

#' @describeIn PROPS Lists all property-or functions. Returns a sorted, lowercase, two-element, character vector.
#' @export
ppp_or_funs <- function() {base::c("na0_or", "nll_or")}

#' @describeIn PROPS Lists all possible properties. Returns a sorted, lowercase, character vector.
#' @export
all_props <- function(as.dtf = F) {
  if (!ppp:::.cmp_lgl_scl(as.dtf)) {ppp::stopperr("[as.dtf] must be TRUE or FALSE.", .pkg = "ppp")}
  bval <- ppp::bbb_props(); bfam <- base::rep("bbb", base::length(bval)); blab <- base::paste0("b_", bval)
  cval <- ppp::ccc_props(); cfam <- base::rep("ccc", base::length(cval)); clab <- base::paste0("c_", cval)
  dval <- ppp::ddd_props(); dfam <- base::rep("ddd", base::length(dval)); dlab <- base::paste0("d_", dval)
  eval <- ppp::eee_props(); efam <- base::rep("eee", base::length(eval)); elab <- base::paste0("e_", eval)
  ival <- ppp::iii_props(); ifam <- base::rep("iii", base::length(ival)); ilab <- base::paste0("i_", ival)
  mval <- ppp::mmm_props(); mfam <- base::rep("mmm", base::length(mval)); mlab <- base::paste0("m_", mval)
  sval <- ppp::sss_props(); sfam <- base::rep("sss", base::length(sval)); slab <- base::paste0("s_", sval)
  val <- base::c(bval, cval, dval, eval, ival, mval, sval)
  fam <- base::c(bfam, cfam, dfam, efam, ifam, mfam, sfam)
  ord <- base::order(base::c(blab, clab, dlab, elab, ilab, mlab, slab))
  if (!as.dtf) {base::unique(val[ord])} else {tibble::tibble(family = fam[ord], ppp = val[ord])}
}

#' @describeIn PROPS Lists all possible property checking functions. Includes both single-property and combination property checking functions. Returns a sorted, lowercase, character vector.
#' @export
prop_funs <- function(as.dtf = F) {
  if (!ppp:::.cmp_lgl_scl(as.dtf)) {ppp::stopperr("[as.dtf] must be TRUE or FALSE.", .pkg = "ppp")}
    bFun <-         ppp::bbb_funs();   bFam <- base::rep(        "bbb", base::length(  bFun));   bLab <- base::paste0("1_",   bFam, "_",   bFun)
    cFun <-         ppp::ccc_funs();   cFam <- base::rep(        "ccc", base::length(  cFun));   cLab <- base::paste0("1_",   cFam, "_",   cFun)
    dFun <-         ppp::ddd_funs();   dFam <- base::rep(        "ddd", base::length(  dFun));   dLab <- base::paste0("1_",   dFam, "_",   dFun)
    eFun <-         ppp::eee_funs();   eFam <- base::rep(        "eee", base::length(  eFun));   eLab <- base::paste0("1_",   eFam, "_",   eFun)
    iFun <-         ppp::iii_funs();   iFam <- base::rep(        "iii", base::length(  iFun));   iLab <- base::paste0("1_",   iFam, "_",   iFun)
    mFun <-         ppp::mmm_funs();   mFam <- base::rep(        "mmm", base::length(  mFun));   mLab <- base::paste0("1_",   mFam, "_",   mFun)
    sFun <-         ppp::sss_funs();   sFam <- base::rep(        "sss", base::length(  sFun));   sLab <- base::paste0("1_",   sFam, "_",   sFun)
   orFun <-      ppp::ppp_or_funs();  orFam <- base::rep(     "ppp_or", base::length( orFun));  orLab <- base::paste0("2_",  orFam, "_",  orFun)
   bcFun <-     ppp::bbb_ccc_funs();  bcFam <- base::rep(    "bbb_ccc", base::length( bcFun));  bcLab <- base::paste0("3_",  bcFam, "_",  bcFun)
   bmFun <-     ppp::bbb_mmm_funs();  bmFam <- base::rep(    "bbb_mmm", base::length( bmFun));  bmLab <- base::paste0("3_",  bmFam, "_",  bmFun)
   mcFun <-     ppp::mmm_ccc_funs();  mcFam <- base::rep(    "mmm_ccc", base::length( mcFun));  mcLab <- base::paste0("3_",  mcFam, "_",  mcFun)
   scFun <-     ppp::sss_ccc_funs();  scFam <- base::rep(    "sss_ccc", base::length( scFun));  scLab <- base::paste0("3_",  scFam, "_",  scFun)
   ccFun <-     ppp::cmp_ccc_funs();  ccFam <- base::rep(    "cmp_ccc", base::length( ccFun));  ccLab <- base::paste0("4_",  ccFam, "_",  ccFun)
   cmFun <-     ppp::cmp_mmm_funs();  cmFam <- base::rep(    "cmp_mmm", base::length( cmFun));  cmLab <- base::paste0("4_",  cmFam, "_",  cmFun)
   ucFun <-     ppp::unq_mmm_funs();  ucFam <- base::rep(    "unq_ccc", base::length( ucFun));  ucLab <- base::paste0("4_",  ucFam, "_",  ucFun)
   umFun <-     ppp::unq_mmm_funs();  umFam <- base::rep(    "unq_mmm", base::length( umFun));  umLab <- base::paste0("4_",  umFam, "_",  umFun)
  cmcFun <- ppp::cmp_mmm_ccc_funs(); cmcFam <- base::rep("cmp_mmm_ccc", base::length(cmcFun)); cmcLab <- base::paste0("5_", cmcFam, "_", cmcFun)
  umcFun <- ppp::unq_mmm_ccc_funs(); umcFam <- base::rep("unq_mmm_ccc", base::length(umcFun)); umcLab <- base::paste0("5_", umcFam, "_", umcFun)
  Fun <- base::c(bFun, cFun, dFun, eFun, iFun, mFun, sFun, orFun, bcFun, bmFun, mcFun, scFun, ccFun, cmFun, ucFun, umFun, cmcFun, umcFun)
  Fam <- base::c(bFam, cFam, dFam, eFam, iFam, mFam, sFam, orFam, bcFam, bmFam, mcFam, scFam, ccFam, cmFam, ucFam, umFam, cmcFam, umcFam)
  Ord <- base::c(bLab, cLab, dLab, eLab, iLab, mLab, sLab, orLab, bcLab, bmLab, mcLab, scLab, ccLab, cmLab, ucLab, umLab, cmcLab, umcLab)
  Ord <- base::order(Ord)
  Fun <- Fun[Ord]
  Fam <- Fam[Ord]
  if (!as.dtf) {base::unique(Fun[Ord])} else {base::unique(tibble::tibble(Family = Fam[Ord], Fun = Fun[Ord]))}
}

#' @describeIn PROPS Checks whether `x` is the name of a property checking function. Returns a logical scalar.
#' @export
is_prop_fun <- function(x) {
  if (!ppp:::.cmp_chr_scl(x)) {ppp::stopperr("[x] must be a complete character scalar (?cmp_chr_scl).", .pkg = "ppp")}
  x %in% ppp::prop_funs()
}

#' @describeIn PROPS Checks whether `x` is a single property. Returns a logical scalar.
#' @export
is_prop <- function(x) {if (ppp:::.cmp_ch3_scl(x)) {base::tolower(x) %in% ppp::all_props()} else {F}}

#' @describeIn PROPS Checks whether `x` is a combination property spec. Returns a logical scalar. A combination property spec is two or more unique, valid, single properties separated by underscores.
#' @export
is_prop_combo <- function(x) {
  if (ppp:::.cmp_chr_scl(x)) {
    x <- ppp::av(base::strsplit(x, "|", fixed = T))
    if (base::length(x) == 1) {
      Props <- ppp::uv(base::strsplit(x, "_", fixed = T))
      if (base::length(base::unique(Props)) == base::length(Props)) {base::all(Props %in% ppp::all_props())}
      else {F}
    } else {F}
  } else {F}
}

#' @describeIn PROPS Checks whether `x` is a valid property spec. Returns a logical scalar. A valid property spec must be a character scalar containing either a single property from `all_props()`, a combination property spec (two or more unique, valid, single properties separated by underscores), or a flexible property spec (two or more single properties or combination property specs separated by pipes).
#' @export
is_prop_spec <- function(x) {
  if (ppp:::.cmp_chr_scl(x)) {
    Combos <- ppp::av(base::strsplit(x, "|", fixed = T))
    if (base::length(base::unique(Combos)) == base::length(Combos)) {
      base::all(base::sapply(Combos, ppp::is_prop_combo) | base::sapply(Combos, ppp::is_prop))
    } else {F}
  } else {F}
}

#' @describeIn PROPS Converts a property spec to a unique list of its constituent single properties by splitting `x` along pipes and underscores, removing any blank values, sorting the remaining values, and returning the unique set of remaining values.
#' @export
props_from_spec <- function(x, .valid = all_props()) {
  if (ppp::.cmp_chr_vec(.valid)) {okValid <- base::all(.valid %in% ppp::all_props())} else {okValid <- F}
  okSpec  <- ppp:::.cmp_chr_scl(x)
  errs  <- NULL
  if (!okSpec) {errs <- base::c(errs, "[x] must be a complete character scalar (?cmp_chr_scl).")}
  if (!okValid) {errs <- base::c(errs, "[.valid] must be a complete character vector (?cmp_chr_vec) containing only values from all_props().")}
  if (!base::is.null(errs)) {ppp::stopperr(errs, .pkg = "ppp")}
  singles <- ppp::spec2props(x)
  if (base::length(singles) == 0) {ppp::stopperr("[x] is empty.", .pkg = "ppp")}
  if (!base::all(singles %in% .valid)) {ppp::stopperr("The property spec [x] contains a property not in [.valid].", .pkg = "ppp")}
  base::sort(base::unique(singles))
}

#' @describeIn PROPS Converts a property spec into a vector of combination properties. Returns a character vector.
#' @export
combos_from_spec <- function(x) {base::sort(base::unique(ppp::spec2combos(x)))}

#' @describeIn PROPS Converts a combined property spec into a vector of single properties. Returns a character vector.
#' @export
props_from_combo <- function(x) {if (base::length(ppp::spec2props(x)) == 1) {base::sort(base::unique(ppp::combo2props(x)))} else {ppp::stopperr("[x] contains more than one combination property.", .pkg = "ppp")}}

#' @describeIn PROPS Checks `x` against the property spec `spec` subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
PPP <- function(x, spec, ...) {
  if (!ppp::is_prop_spec(spec)) {ppp::stopperr("[spec] specifies a property not in all_props().", .pkg = "ppp")}
  if (ppp::meets(x, ...)) {
    AllProps <- ppp::all_props()
    Combos   <- ppp::spec2combos(spec)
    for (Combo in Combos) {
      if (base::length(Combo) == 1) {IsOne <- Combo %in% AllProps} else {IsOne <- F}
      IsFun <- ppp::is_prop_fun(Combo)
      if (!IsOne & !IsFun) {
        Singles <- ppp::combo2props(Combo)
        Meets <- T
        for (Prop in Singles) {if (Meets) {Meets <- Meets & base::eval(base::parse(text = base::paste0("ppp::.", base::toupper(Prop), "(x)")))}}
      } else if (IsOne) {Meets <- base::eval(base::parse(text = base::paste0("ppp::.", base::toupper(Combo), "(x)")))}
      else {Meets <- base::eval(base::parse(text = base::paste0("ppp::", Combo, "(x)")))}
      if (Meets) {return(T)}
  }}
  F
}

#' @describeIn PROPS Checks `x` against null-ness or the property spec `spec` subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
nll_or <- function(x, spec, ...) {if (base::is.null(x)) {T} else {ppp::PPP(x, spec, ...)}}

#' @describeIn PROPS Checks `x` against scalar missingness-ness or the property spec `spec` subject to any count or value restrictions in `...`. Returns a logical scalar.
#' @export
na0_or <- function(x, spec, ...) {if (ppp:::.NA0(x)) {T} else {ppp::PPP(x, spec, ...)}}

#' @describeIn PROPS Produces a data frame containing all properties with columns indicating property family, the value of the property, a short description of the property, and a long description of the property. Returns a tibble/data frame.
#' @export
prop_defs <- function() {
  tibble::tribble(
    ~Family, ~Value  , ~Short                              , ~Long,
    "bbb"  , "atm"   , "atomic"                            , "An atomic object",
    "bbb"  , "def"   , "defined"                           , "A defined object (not NULL)",
    "bbb"  , "fun"   , "function or function name"         , "A function object or a character scalar containing a function name",
    "bbb"  , "nil"   , "nil"                               , "A nil object (of length 0, including NULL)",
    "bbb"  , "nll"   , "NULL"                              , "The NULL object",
    "bbb"  , "pop"   , "populated"                         , "A populated object (not of length 0)",
    "bbb"  , "rcr"   , "recursive"                         , "A recursive object (a data.frame or vlist)",
    "ccc"  , "arr"   , "arrary+"                           , "An array or vector",
    "ccc"  , "dtf"   , "data.frame"                        , "A data.frame",
    "ccc"  , "gen"   , "generic"                           , "A generic (vector/array/vlist)",
    "ccc"  , "mat"   , "matrix"                            , "A atomic matrix",
    "ccc"  , "mvc"   , "multivec"                          , "A vector/vlist list of length 2+ or array of length 2+ with multiple index positions in exactly 1 dimension",
    "ccc"  , "scl"   , "scalar"                            , "A vector/vlist/array of length 1",
    "ccc"  , "vec"   , "vector+"                           , "A vector/1D array/multidimensional array with multiple index positions in 0 or 1 dimension",
    "ccc"  , "vls"   , "vlist (non-data.frame list)"       , "A vector-list (not a data.frame list)",
    "ddd"  , "d0D"   , "defined as 0-dimensional"          , "A 0D object (NULL)",
    "ddd"  , "d1D"   , "defined as 1-dimensional"          , "A 1D object (vector, non-data.frame list, or 1-dimensional array)",
    "ddd"  , "d2D"   , "defined as 2-dimensional"          , "A 2D object (matrix or data.frame)",
    "ddd"  , "dHD"   , "defined as hyper-dimensional"      , "A 3D+ object (array with 3+ dimensions)",
    "eee"  , "e0D"   , "effectively 0-dimensional"         , "An effectively 0D object (vector/vlist/array of length 1 or data.frame with 1 row and 1 column",
    "eee"  , "e1D"   , "effectively 1-dimensional"         , "An effectively 1D object (vector/vlist with 2+ elements, row/column matrix/data.frame, or populated array with more than one index position in exactly 1 dimension)",
    "eee"  , "e2D"   , "effectively 2-dimensional"         , "An effectively 2D object (matrix/data.frame with 2+ rows and 2+ columns or populated array with multiple index positions in exactly 2 dimensions)",
    "eee"  , "eHD"   , "effectively hyper-dimensional"     , "An effectively 3D+ object (populated array with multiple index positions in 3+ dimensions)",
    "eee"  , "eUD"   , "effectively non-dimensional"       , "An effectively non-dimensional object (of length 0)",
    "iii"  , "cmp"   , "atomic and complete"               , "An complete atomic object (containing no NA values)",
    "iii"  , "dup"   , "atomic, complete, with duplicates" , "A complete dup atomic object (containing no NA values, containing duplicate values)",
    "iii"  , "mss"   , "atomic and missing"                , "A missing atomic object (containing only non-NA values)",
    "iii"  , "na0"   , "atomic NA scalar"                  , "An atomic NA scalar object",
    "iii"  , "ok0"   , "atomic non-NA scalar"              , "An atomic non-NA scalar object",
    "iii"  , "prt"   , "atomic and partial"                , "A partial atomic object (containing both NA and non-NA values)",
    "iii"  , "unq"   , "atomic, complete, and unique"      , "A unique, complete atomic object (containing no NA or duplicate values)",
    "mmm"  , "ch1"   , "onechar"                           , "Any non-NA values are character scalars containing exactly 1 character",
    "mmm"  , "ch3"   , "threechar"                         , "Any non-NA values are character scalars containing exactly 3 characters",
    "mmm"  , "chr"   , "character"                         , "A character object",
    "mmm"  , "clr"   , "character color value"             , "A character object containing valid color values",
    "mmm"  , "evn"   , "even whole-number"                 , "An even whole-number object",
    "mmm"  , "fac"   , "factor"                            , "An ordered-factor or unordered-factor object",
    "mmm"  , "frc"   , "fractional numeric"                , "A fractional numeric object (having 1+ non-whole-number values)",
    "mmm"  , "ind"   , "indexing"                          , "A indexing object (logical or positive whole number)",
    "mmm"  , "lgl"   , "logical"                           , "A logical object",
    "mmm"  , "neg"   , "negative numeric"                  , "A negative numeric object",
    "mmm"  , "ngw"   , "negative whole number"             , "A negative whole-number object",
    "mmm"  , "nng"   , "nonnegative numeric"               , "A non-negative numeric object",
    "mmm"  , "nnw"   , "nonnegative whole-number"          , "A non-negative whole-number object",
    "mmm"  , "nps"   , "nonpositive numeric"               , "A non-positive numeric object",
    "mmm"  , "npw"   , "nonpositive whole-number"          , "A non-positive whole-number object",
    "mmm"  , "nst"   , "non-sortable"                      , "A non-sortable object (atomic but not character, logical, numeric, or ordered factor)",
    "mmm"  , "num"   , "numeric"                           , "A numeric object",
    "mmm"  , "odd"   , "odd whole-number"                  , "An odd whole-number object",
    "mmm"  , "ord"   , "ordered-factor"                    , "An ordered-factor object",
    "mmm"  , "pct"   , "percentage numeric"                , "A numeric, percentage object (values in the interval [0, 100])",
    "mmm"  , "pos"   , "positive numeric"                  , "A positive numeric object",
    "mmm"  , "ppn"   , "proportion  numeric"               , "A numeric proportion object (values in the interval [0, 1])",
    "mmm"  , "psw"   , "positive whole-number"             , "A positive whole-number object",
    "mmm"  , "str"   , "string"                            , "A string object (character without any blanks)",
    "mmm"  , "srt"   , "sortable"                          , "A sortable object (character, logical, numeric, or ordered factor)",
    "mmm"  , "uno"   , "unordered-factor"                  , "A unordered-factor object",
    "mmm"  , "whl"   , "whole-number"                      , "A whole-number object",
    "sss"  , "col"   , "column"                            , "A column object (2+ x 1 data.frame/matrix)",
    "sss"  , "emp"   , "empty"                             , "An empty object (of length 0, but not NULL)",
    "sss"  , "lin"   , "linear"                            , "A linear object (vector/vlist/1D array of length 2+, row/column matrix/data.frame, or populated array with multiple indexing positions in exactly 1 dimension)",
    "sss"  , "pnt"   , "point"                             , "A point object (length-1 vector/array/vlist or 1 x 1 data.frame/matrix)",
    "sss"  , "rct"   , "rectangular"                       , "A rectangular object (2+ x 2+ data.frame/matrix)",
    "sss"  , "row"   , "row"                               , "A row object (1 x 2+ data.frame/matrix)",
    "sss"  , "sld"   , "solid"                             , "A solid object (array with multiple index positions in 3+ dimensions)",
    "sss"  , "sqr"   , "square"                            , "A square atomic matrix"
  )
}

#' @describeIn PROPS Converts the property spec `x` into a verbose description of each constituent (combo) property, identifying each constituent (combo) property as an alternative to the others. Returns a character scalar.
#' @export
prop_verbose <- function(x, .print = TRUE) {
  Errors <- NULL
  if (!ppp::is_prop(x)) {Errors <- base::c(Errors, "[x] must be a character scalar containing a single property spec.")}
  x <- base::tolower(x)
  if (!ppp:::.cmp_lgl_scl(.print)) {Errors <- base::c(Errors, "[.print] must be TRUE or FALSE.")}
  if (!base::is.null(Errors)) {ppp::stopperr(Errors, .pkg = "ppp")}
  y <- ppp::prop_defs()
  if (!base::is.null(x)) {y <- base::paste0("\n Family: '", ppp::av(y$Family[y$Value == x]), "'"  ,
                                            "\n Value:  '", x, "'"                                ,
                                            "\n Short:   ", ppp::av(y$Short[y$Value == x])        ,
                                            "\n Long:    ", ppp::av(y$Long[y$Value == x]), ".\n\n")}
  if (!.print) {y} else if (base::is.data.frame(y)) {base::print(y, n = base::nrow(y))} else {base::cat(y)}
}

#' @describeIn PROPS Converts the (combo) property `x` to a concise description.
#' @export
combo_concise <- function(x) {
  if (!ppp::is_prop_combo(x)) {ppp::stopperr("[x] does not contain a valid property combo spec.", .pkg = "ppp")}
  x      <- ppp::combo2props(x)
  Defs   <- ppp::prop_defs()
  Family <- ppp::av(Defs$Family)
  Value  <- ppp::av(Defs$Value)
  Short  <- ppp::av(Defs$Short)
  bbb    <- Short[Value %in% x & Family == "bbb"]
  ccc    <- Short[Value %in% x & Family == "ccc"]
  ddd    <- Short[Value %in% x & Family == "ddd"]
  eee    <- Short[Value %in% x & Family == "eee"]
  iii    <- Short[Value %in% x & Family == "iii"]
  mmm    <- Short[Value %in% x & Family == "mmm"]
  sss    <- Short[Value %in% x & Family == "sss"]
  Object <- base::length(ccc) == 0
  y      <- base::paste0(base::c(bbb, ccc, ddd, eee, iii, mmm, sss), collapse = ", ")
  y      <- ppp::av(base::strsplit(y, ", ", fixed = T))
  y      <- y[!base::duplicated(y)]
  y      <- base::paste0(y, collapse = ", ")
  if (Object) {y <- base::paste0(y, " object")}
  if (base::substr(y, 1, 1) %in% base::c("a", "e", "i", "o", "u")) {Prefix <- "an "} else {Prefix <- "a "}
  base::paste0(Prefix, y)
}

#' @describeIn PROPS Converts the property spec `x` to a concise description.
#' @export
spec_concise <- function(x) {
  if (!ppp::is_prop_spec(x)) {ppp::stopperr("[x] is not a valid property spec.", .pkg = "ppp")}
  Combos <- ppp::spec2combos(x)
  for (i in 1:base::length(Combos)) {Combos[i] <- ppp::combo_concise(Combos[i])}
  base::paste0(Combos, collapse = " OR ")
}
