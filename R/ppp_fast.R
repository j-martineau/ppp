#' @title Fast Property Checking for Common Checks with Optional valid Values
#' @description Fast property checking for \code{\link{ppp}} functions without the dot prefix (e.g., `.ATM(x)` is a faster version of `ATM(x)` because it uses only base functions to conduct property checking).
#' @details When `valid` is supplied, both `x` and `valid` are \link[ppp:av]{atomized} before checking whether all atomic elements of `x` are contained in the atomic elements of `valid`.
#' @param x Any object. See *details*.
#' @param valid An optional object containing valid atomic values to check against atomic values in `x`. See *details*.
#' @return A logical scalar.
#' @export
ppp_fast <- function() {utils::help("ppp_fast", "ppp")}

# .xxx ####

#' @rdname ppp_fast
#' @export
.ATM <- function(x, valid = NULL) {ppp::check_valid(x, base::is.atomic(x) & !base::is.null(x), valid)}

#' @rdname ppp_fast
#' @export
.DEF <- function(x, valid = NULL) {ppp::check_valid(x, !base::is.null(x), valid)}

#' @rdname ppp_fast
#' @export
.FUN <- function(x, valid = NULL) {
  if (!base::is.function(x)) {
    if (base::length(x) == 1 & base::is.character(x)) {
      newX <- tryCatch(base::match.fun(x), error = function(x) e, finally = NULL)
      ppp::check_valid(x, !rlang::is_error(newX), valid)
    } else {F}
  } else {
    for (valid in valid) {if (base::is.function(valid)) {if (base::identical(x, valid)) {return(T)}}}
    F
  }
}

## .bbb ####

#' @rdname ppp_fast
#' @export
.NIL <- function(x) {base::length(x) == 0}

#' @rdname ppp_fast
#' @export
.NLL <- function(x) {base::is.null(x)}

#' @rdname ppp_fast
#' @export
.POP <- function(x, valid = NULL) {ppp::check_valid(x, base::length(x) > 0, valid)}

#' @rdname ppp_fast
#' @export
.RCR <- function(x, valid = NULL) {ppp::check_valid(x, base::is.recursive(x), valid)}

## .ccc ####

#' @rdname ppp_fast
#' @export
.ARR <- function(x, valid = NULL) {ppp::check_valid(x, base::is.array(x), valid)}

#' @rdname ppp_fast
#' @export
.DTF <- function(x, valid = NULL) {ppp::check_valid(x, base::is.data.frame(x), valid)}

#' @rdname ppp_fast
#' @export
.GEN <- function(x, valid = NULL) {ppp::check_valid(x, base::is.array(x) | base::is.vector(x), valid)}

#' @rdname ppp_fast
#' @export
.MAT <- function(x, valid = NULL) {ppp::check_valid(x, base::is.matrix(x), valid)}

#' @rdname ppp_fast
#' @export
.MVC <- function(x, valid = NULL) {
  if (base::length(x) < 2) {F}
  else if (base::is.vector(x)) {ppp::check_valid(x, T, valid)}
  else if (!base::is.array(x)) {F}
  else {ppp::check_valid(x, base::length(base::which(base::dim(x) > 1)) == 1, valid)}
}

#' @rdname ppp_fast
#' @export
.SCL <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else {ppp::check_valid(x, base::is.vector(x) | base::is.array(x), valid)}
}

#' @rdname ppp_fast
#' @export
.VLS <- function(x, valid = NULL) {
  if (base::is.data.frame(x)) {F}
  else {ppp::check_valid(x, base::is.list(x), valid)}
}

#' @rdname ppp_fast
#' @export
.VEC <- function(x, valid = NULL) {
  if (base::length(x) == 0) {F}
  else if (base::is.vector(x)) {ppp::check_valid(x, T, valid)}
  else if (!base::is.array(x)) {F}
  else {ppp::check_valid(x, base::length(base::which(base::dim(x) > 1)) < 2, valid)}
}

## .ddd ####

#' @rdname ppp_fast
#' @export
.D0D <- function(x) {base::is.null(x)}

#' @rdname ppp_fast
#' @export
.D1D <- function(x, valid = NULL) {ppp::check_valid(x, base::is.vector(x), valid)}

#' @rdname ppp_fast
#' @export
.D2D <- function(x, valid = NULL) {ppp::check_valid(x, base::is.matrix(x) | base::is.data.frame(x), valid)}

#' @rdname ppp_fast
#' @export
.DHD <- function(x, valid = NULL) {
  if (is.array(x)) {ppp::check_valid(x, base::length(base::dim(x)) > 2, valid)}
  else {F}
}

## .eee ####

#' @rdname ppp_fast
#' @export
.EUD <- function(x) {base::length(x) == 0}

#' @rdname ppp_fast
#' @export
.E2D <- function(x, valid = NULL) {ppp::check_valid(x, base::length(base::which(base::dim(x) > 1)) == 2, valid)}

#' @rdname ppp_fast
#' @export
.EHD <- function(x, valid = NULL) {ppp::check_valid(x, base::length(base::which(base::dim(x) > 1)) > 2, valid)}

#' @rdname ppp_fast
#' @export
.E0D <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {ppp::check_valid(x, base::length(x) == 1, valid)}
  else if (base::is.data.frame(x)) {ppp::check_valid(x, base::NROW(x) * base::NCOL(x) == 1, valid)}
  else if (base::is.list(x)) {ppp::check_valid(x, base::length(x) == 1, valid)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.E1D <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::is.vector(x)) {ppp::check_valid(x, base::length(x) > 1, valid)}
    else if (base::is.array(x)) {ppp::check_valid(x, base::length(base::which(base::dim(x) > 1)) == 1, valid)}
    else {F}
  } else if (base::is.data.frame(x)) {
    if (base::NROW(x) == 1) {ppp::check_valid(x, base::NCOL(x) > 1, valid)}
    else if (base::NCOL(x) == 1) {ppp::check_valid(x, base::NROW(x) > 1, valid)}
    else {F}
  } else if (base::is.list(x)) {ppp::check_valid(x, base::length(x) > 1, valid)}
  else {F}
}

## .iii ####

#' @rdname ppp_fast
#' @export
.NA0 <- function(x) {
  if (!base::is.atomic(x) | base::length(x) != 1) {F}
  else {base::is.na(x)}
}

#' @rdname ppp_fast
#' @export
.OK0 <- function(x, valid = NULL) {
  if (!base::is.atomic(x) | base::length(x) != 1) {F}
  else {ppp::check_valid(x, !base::is.na(x), valid)}
}

#' @rdname ppp_fast
#' @export
.DUP <- function(x, valid = NULL) {
  if (!base::is.atomic(x) | base::length(x) == 0) {F}
  else {ppp::check_valid(x, base::length(x) != base::length(base::unique(x)), valid)}
}

#' @rdname ppp_fast
#' @export
.UNQ <- function(x, valid = NULL) {
  if (!base::is.atomic(x) | base::length(x) == 0) {F}
  else {ppp::check_valid(x, base::length(x) == base::length(base::unique(x)), valid)}
}

#' @rdname ppp_fast
#' @export
.CMP <- function(x, valid = NULL) {
  if (!base::is.atomic(x) | base::length(x) == 0) {F}
  else {ppp::check_valid(x, !base::any(base::is.na(x)), valid)}
}

#' @rdname ppp_fast
#' @export
.MSS <- function(x) {
  if (!base::is.atomic(x) | base::length(x) == 0) {F}
  else {base::all(base::is.na(x))}
}

#' @rdname ppp_fast
#' @export
.PRT <- function(x, valid = NULL) {
  if (!base::is.atomic(x) | base::length(x) <= 1) {F}
  else if (!any(base::is.na(x))) {F}
  else if (all(base::is.na(x))) {F}
  else {ppp::check_valid(x, x[!base::is.na(x)], valid)}
}

## .mmm ####

#' @rdname ppp_fast
#' @export
.CH1 <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.character(x)) {
          x <- x[!base::is.na(x)]
          if (base::length(x) == 0) {ppp::check_valid(x, T, valid)}
          else {ppp::check_valid(x, base::all(base::nchar(x) == 1), valid)}
        } else {F}
      } else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.CH3 <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.character(x)) {
          x <- x[!base::is.na(x)]
          if (base::length(x) == 0) {ppp::check_valid(x, T, valid)}
          else {ppp::check_valid(x, base::all(base::nchar(x) == 3), valid)}
        } else {F}
      } else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.CHR <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.character(x)) {ppp::check_valid(x, T, valid)}
      else {ppp::check_valid(x, base::all(base::is.na(x)), valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.STR <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.character(x)) {
          x <- x[!base::is.na(x)]
          if (base::length(x) == 0) {ppp::check_valid(x, T, valid)}
          else {ppp::check_valid(x, base::all(base::nchar(x) > 0), valid)}
        } else {F}
      } else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.CLR <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        x <- x[!base::is.na(x)]
        if (base::length(x) > 0) {ppp::check_valid(x, !uj::is_err(grDevices::col2rgb(x)), valid)}
        else {F}
      } else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.FAC <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.factor(x)) {ppp::check_valid(x, T, valid)}
      else {ppp::check_valid(x, base::all(base::is.na(x)), valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.ORD <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.ordered(x)) {ppp::check_valid(x, T, valid)}
      else {ppp::check_valid(x, base::all(base::is.na(x)), valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.UNO <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.factor(x) & !base::is.ordered(x)) {ppp::check_valid(x, T, valid)}
      else {ppp::check_valid(x, base::all(base::is.na(x)), valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.IND <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        x <- x[!base::is.na(x)]
        if (base::is.numeric(x)) {ppp::check_valid(x, base::all(x == base::round(x)), valid)}
        else {ppp::check_valid(x, base::is.logical(x), valid)}
      } else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NST <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {ppp::check_valid(x, !base::is.character(x) & !base::is.numeric(x) & !base::is.ordered(x), valid)}
      else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.SRT <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {ppp::check_valid(x, base::is.character(x) | base::is.numeric(x) | base::is.ordered(x), valid)}
      else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.LGL <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.logical(x)) {ppp::check_valid(x, T, valid)}
      else {ppp::check_valid(x, base::all(base::is.na(x)), valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NUM <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (base::is.numeric(x)) {ppp::check_valid(x, T, valid)}
      else {ppp::check_valid(x, base::all(base::is.na(x)), valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.WHL <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          ppp::check_valid(x, base::all(x == base::round(x)), valid)
        } else {F}
      } else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.ODD <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          if (base::all(x == base::round(x))) {
            x <- x / 2
            ppp::check_valid(x, base::any(x != base::round(x)), valid)
          } else {F}
        } else {F}
      } else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.EVN <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          if (base::all(x == base::round(x))) {
            x <- x / 2
            ppp::check_valid(x, base::all(x == base::round(x)), valid)
          } else {F}
        } else {F}
      } else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NGW <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          if (base::all(x == base::round(x))) {ppp::check_valid(x, base::all(x < 0), valid)}
          else {F}
        } else {F}
      } else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NPW <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          if (base::all(x == base::round(x))) {ppp::check_valid(x, base::all(x <= 0), valid)}
          else {F}
        } else {F}
      } else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.POS <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          if (base::all(x == base::round(x))) {ppp::check_valid(x, base::all(x > 0), valid)}
          else {F}
        } else {F}
      } else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.PSW <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          if (base::all(x == base::round(x))) {ppp::check_valid(x, base::all(x > 0), valid)}
          else {F}
        } else {F}
      } else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.FRC <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          ppp::check_valid(x, base::any(x != base::round(x)), valid)
        } else {F}
      } else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NEG <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          ppp::check_valid(x, base::all(x < 0), valid)
        } else {F}
      } else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.POS <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          ppp::check_valid(x, base::all(x > 0), valid)
        } else {F}
      } else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NNG <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          ppp::check_valid(x, base::all(x >= 0), valid)
        } else {F}
      } else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NNW <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          if (base::all(x == base::round(x))) {ppp::check_valid(x, base::all(x >= 0), valid)}
          else {F}
        } else {F}
      } else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.NPS <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          ppp::check_valid(x, base::all(x <= 0), valid)
        } else {F}
      } else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.PCT <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          ppp::check_valid(x, base::all(x >= 0 | x <= 100), valid)
        } else {F}
      } else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.PPN <- function(x, valid = NULL) {
  if (base::is.atomic(x)) {
    if (base::length(x) > 0) {
      if (!base::all(base::is.na(x))) {
        if (base::is.numeric(x)) {
          x <- x[!base::is.na(x)]
          ppp::check_valid(x, base::all(x >= 0 | x <= 1), valid)
        } else {F}
      } else {ppp::check_valid(x, T, valid)}
    } else {ppp::check_valid(x, T, valid)}
  } else {F}
}

## .sss ####

#' @rdname ppp_fast
#' @export
.COL <- function(x, valid = NULL) {
  if (base::is.data.frame(x) | base::is.matrix(x)) {ppp::check_valid(x, base::NROW(x) > 1 & base::NCOL(x) == 1, valid)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.ROW <- function(x, valid = NULL) {
  if (base::is.data.frame(x) | base::is.matrix(x)) {ppp::check_valid(x, base::NROW(x) == 1 & base::NCOL(x) > 1, valid)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.PNT <- function(x, valid = NULL) {
  if (base::is.data.frame(x)) {ppp::check_valid(x, base::NROW(x) * base::NCOL(x) == 1, valid)}
  else {ppp::check_valid(x, base::length(x) == 1, valid)}
}

#' @rdname ppp_fast
#' @export
.SLD <- function(x, valid = NULL) {
  if (base::is.array(x)) {ppp::check_valid(x, base::length(base::which(base::dim(x) > 1)) > 2, valid)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.SQR <- function(x, valid = NULL) {
  if (base::is.matrix(x)) {ppp::check_valid(x, base::NROW(x) > 1 & base::NCOL(x) > 1 & base::NROW(x) == base::NCOL(x), valid)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.EMP <- function(x, valid = NULL) {
  if (base::is.data.frame(x)) {ppp::check_valid(x, base::NROW(x) * base::NCOL(x) == 0, valid)}
  else if (base::length(x) == 0) {ppp::check_valid(x, !base::is.null(x), valid)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.RCT <- function(x, valid = NULL) {
  if (base::length(x) > 1) {
    if (base::is.data.frame(x) | is.matrix(x)) {ppp::check_valid(x, base::NROW(x) > 1 & base::NCOL(x) > 1, valid)}
    else {F}
  } else {F}
}

#' @rdname ppp_fast
#' @export
.LIN <- function(x, valid = NULL) {
  if (base::length(x) > 1) {
    if (base::is.data.frame(x)) {
      if (base::NROW(x) == 1 & base::NCOL(x) > 1) {ppp::check_valid(x, T, valid)}
      else {ppp::check_valid(x, base::NROW(x) > 1 & base::NCOL(x) == 1, valid)}
    } else if (base::is.array(x)) {ppp::check_valid(x, base::length(base::which(base::dim(x) > 1)) == 2, valid)}
    else if (base::is.list(x)) {ppp::check_valid(x, T, valid)}
    else if (base::is.vector(x)) {ppp::check_valid(x, T, valid)}
  } else {F}
}

# .xxx_yyy ####

## .mmm_ccc ####

#' @rdname ppp_fast
#' @export
.atm_dtf <- function(x, valid = NULL) {
  NC <- base::NCOL(x)
  if (base::is.data.frame(x) & base::NROW(x) > 0 & NC > 0) {
    for (i in 1:NC) {
      if (!base::is.atomic(x[[i]])) {return(F)}
    }
    ppp::check_valid(x, T, valid)
  } else {F}
}

#' @rdname ppp_fast
#' @export
.atm_mat <- function(x, valid = NULL) {ppp::check_valid(x, base::is.atomic(x) & base::is.matrix(x), valid)}

#' @rdname ppp_fast
#' @export
.atm_scl <- function(x, valid = NULL) {ppp::check_valid(x, base::is.atomic(x) & base::length(x) == 1, valid)}

#' @rdname ppp_fast
#' @export
.atm_vec <- function(x, valid = NULL) {ppp::check_valid(x, base::is.atomic(x) & ppp::.VEC(x), valid)}

#' @rdname ppp_fast
#' @export
.atm_vls <- function(x, valid = NULL) {
  if (base::is.list(x) & !base::is.data.frame(x) & base::length(x) > 0) {ppp::check_valid(x, base::all(base::sapply(x, base::is.atomic)), valid)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.chr_arr <- function(x, valid = NULL) {ppp::check_valid(x, base::is.array(x) & base::is.character(x), valid)}

#' @rdname ppp_fast
#' @export
.chr_dtf <- function(x, valid = NULL) {
  NC <- base::NCOL(x)
  if (base::is.data.frame(x) & base::NROW(x) > 0 & NC > 0) {
    for (i in 1:NC) {
      if (!base::is.character(x[[i]])) {return(F)}
    }
    ppp::check_valid(x, T, valid)
  } else {F}
}

#' @rdname ppp_fast
#' @export
.chr_vec <- function(x, valid = NULL) {ppp::check_valid(x, ppp::.VEC(x) & base::is.character(x), valid)}

#' @rdname ppp_fast
#' @export
.chr_vls <- function(x, valid = NULL) {
  if (base::is.list(x) & !is.data.frame(x)) {ppp::check_valid(x, base::all(base::sapply(x, base::is.character)), valid)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.cmp_atm <- function(x, valid = NULL) {
  if (base::is.atomic(x) & base::length(x) > 0) {ppp::check_valid(x, !base::any(base::is.na(x)), valid)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.cmp_chr <- function(x, valid = NULL) {
  if (base::is.character(x) & base::length(x) > 0) {ppp::check_valid(x, !base::any(base::is.na(x)), valid)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.cmp_num <- function(x, valid = NULL) {
  if (base::is.numeric(x) & base::length(x) > 0) {ppp::check_valid(x, !base::any(base::is.na(x)), valid)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.cmp_scl <- function(x, valid = NULL) {
  if (base::is.atomic(x) & base::length(x) == 1) {ppp::check_valid(x, !base::is.na(x), valid)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.cmp_vec <- function(x, valid = NULL) {
  if (!base::is.atomic(x) | base::length(x) == 0) {F}
  else if (!ppp::.VEC(x)) {F}
  else {ppp::check_valid(x, !base::any(base::is.na(x)), valid)}
}

#' @rdname ppp_fast
#' @export
.lgl_scl <- function(x, valid = NULL) {ppp::check_valid(x, base::logical(x) & base::length(x) == 1, valid)}

#' @rdname ppp_fast
#' @export
.nnw_vec <- function(x, valid = NULL) {
  if (ppp::.VEC(x)) {
    if (base::is.numeric(x)) {
      x <- x[!base::is.na(x)]
      if (base::length(x) > 0) {
        if (base::all(x >= 0)) {ppp::check_valid(x, base::all(x == base::round(x)), valid)}
        else {F}
      } else {ppp::check_valid(x, T, valid)}
    } else {F}
  } else {F}
}

## .bbb_mmm ####

#' @rdname ppp_fast
#' @export
.pop_atm <- function(x, valid = NULL) {ppp::check_valid(x, base::is.atomic(x) & base::length(x) > 0, valid)}

#' @rdname ppp_fast
#' @export
.pop_chr <- function(x, valid = NULL) {ppp::check_valid(x, base::length(x) > 0 & base::is.character(x), valid)}

#' @rdname ppp_fast
#' @export
.pop_dtf <- function(x, valid = NULL) {ppp::check_valid(x, base::is.data.frame(x) & base::NROW(x) > 0 & base::NCOL(x) > 0, valid)}

#' @rdname ppp_fast
#' @export
.pop_mat <- function(x, valid = NULL) {ppp::check_valid(x, base::is.matrix(x) & base::length(x) > 0, valid)}

#' @rdname ppp_fast
#' @export
.pop_srt <- function(x, valid = NULL) {ppp::check_valid(x, base::length(x) > 0 & (base::is.character(x) | base::is.numeric(x) | base::is.ordered(x)), valid)}

#' @rdname ppp_fast
#' @export
.pop_vec <- function(x, valid = NULL) {ppp::check_valid(x, ppp::.VEC(x) & base::length(x) > 0, valid)}

#' @rdname ppp_fast
#' @export
.pop_vls <- function(x, valid = NULL) {ppp::check_valid(x, ppp::.VLS(x) & base::length(x) > 0, valid)}

# .xxx_yyy_zzz ####

## .iii_mmm_ccc ####

#' @rdname ppp_fast
#' @export
.cmp_atm_vec <- function(x, valid = NULL) {
  if (!ppp::.VEC(x)) {F}
  else if (!base::is.atomic(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {ppp::check_valid(x, T, valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_ch1_scl <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.character(x)) {F}
  else if (base::is.na(x)) {F}
  else {ppp::check_valid(x, base::nchar(x) == 1, valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_ch1_vec <- function(x, valid = NULL) {
  if (!ppp::.VEC(x)) {F}
  else if (!base::is.character(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {ppp::check_valid(x, base::all(base::nchar(x) == 1), valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_ch3_scl <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.character(x)) {F}
  else if (base::is.na(x)) {F}
  else {ppp::check_valid(x, base::nchar(x) == 3, valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_ch3_vec <- function(x, valid = NULL) {
  if (!ppp::.VEC(x)) {F}
  else if (!base::is.character(x)) {F}
  else if (base::is.na(x)) {F}
  else {ppp::check_valid(x, base::all(base::nchar(x) == 3), valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_chr_arr <- function(x, valid = NULL) {
  if      (base::length(x) == 0     ) {F}
  else if (!ppp::.ARR(x)            ) {F}
  else if (!base::is.character(x)   ) {F}
  else if (base::any(base::is.na(x))) {F}
  else {ppp::check_valid(x, T, valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_chr_vec <- function(x, valid = NULL) {
  if (!ppp::.VEC(x)) {F}
  else if (!base::is.character(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {ppp::check_valid(x, T, valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_chr_vls <- function(x, valid = NULL) {
  if (!ppp::.chr_vls(x)) {F}
  else if (base::any(base::is.na(ppp::av(x)))) {F}
  else {ppp::check_valid(x, T, valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_chr_gen <- function(x, valid = NULL) {
  if (ppp::.cmp_chr_vec(x, valid = valid)) {T}
  else if (ppp::.cmp_chr_arr(x, valid = valid)) {T}
  else {ppp::.cmp_chr_vls(x, valid = valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_chr_scl <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.character(x)) {F}
  else if (base::is.na(x)) {F}
  else {ppp::check_valid(x, T, valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_clr_vec <- function(x, valid = NULL) {ppp::check_valid(x, ppp::.cmp_vec(x) & ppp::.CLR(x), valid)}

#' @rdname ppp_fast
#' @export
.cmp_lgl_scl <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.logical(x)) {F}
  else {ppp::check_valid(x, !base::is.na(x), valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_lgl_vec <- function(x, valid = NULL) {
  if (!ppp::.VEC(x)) {F}
  else if (!base::is.logical(x)) {F}
  else {ppp::check_valid(x, !base::any(base::is.na(x)), valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_nng_scl <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::is.na(x)) {F}
  else {ppp::check_valid(x, x < 0, valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_nnw_scl <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::is.na(x)) {F}
  else if (x < 0) {F}
  else {ppp::check_valid(x, x == round(x), valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_nnw_vec <- function(x, valid = NULL) {
  if (!ppp::.VEC(x)) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else if (base::any(x < 0)) {F}
  else {ppp::check_valid(x, base::all(x == round(x)), valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_num_scl <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::is.na(x)) {F}
  else {ppp::check_valid(x, T, valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_num_vec <- function(x, valid = NULL) {
  if (!ppp::.VEC(x)) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {ppp::check_valid(x, T, valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_pos_vec <- function(x, valid = NULL) {
  if (base::length(x) == 0) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {ppp::check_valid(x, base::all(x >= 0 & x <= 1), valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_ppn_vec <- function(x, valid = NULL) {
  if (base::length(x) == 0) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {ppp::check_valid(x, base::all(x >= 0 & x <= 1), valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_psw_scl <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::is.na(x)) {F}
  else if (x <= 0) {F}
  else {ppp::check_valid(x, x == round(x), valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_psw_vec <- function(x, valid = NULL) {
  if (!ppp::.VEC(x)) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else if (base::any(x <= 0)) {F}
  else {ppp::check_valid(x, base::all(x == round(x)), valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_srt_vec <- function(x, valid = NULL) {
  if (!ppp::.VEC(x)) {F}
  else if (!base::is.numeric(x) & !base::is.character(x) & !base::is.ordered(x)) {F}
  else {ppp::check_valid(x, base::any(base::is.na(x)), valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_str_scl <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.character(x)) {F}
  else if (base::is.na(x)) {F}
  else {ppp::check_valid(x, !base::any(x == ""), valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_str_vec <- function(x, valid = NULL) {
  if (base::length(x) == 0) {F}
  else if (!base::is.character(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {ppp::check_valid(x, !base::any(x == ""), valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_whl_scl <- function(x, valid = NULL) {
  if (base::length(x) != 1) {F}
  else if (!base::is.numeric(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {ppp::check_valid(x, x == base::round(x), valid)}
}

#' @rdname ppp_fast
#' @export
.cmp_atm_mvc <- function(x, valid = NULL) {
  if (!ppp::.MVC(x)) {F}
  else if (!base::is.atomic(x)) {F}
  else if (base::any(base::is.na(x))) {F}
  else {ppp::check_valid(x, T, valid)}
}

#' @rdname ppp_fast
#' @export
.unq_atm_mvc <- function(x, valid = NULL) {
  if (ppp::.cmp_atm_mvc(x)) {ppp::check_valid(x, base::length(x) == base::length(base::unique(x)), valid)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.unq_atm_vec <- function(x, valid = NULL) {
  if (ppp::.cmp_atm_vec(x)) {ppp::check_valid(x, base::length(x) == base::length(base::unique(x)), valid)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.unq_ch1_vec <- function(x, valid = NULL) {
  if (ppp::.cmp_ch1_vec(x)) {ppp::check_valid(x, base::length(x) == base::length(base::unique(x)), valid)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.unq_ch3_vec <- function(x, valid = NULL) {
  if (ppp::.cmp_ch3_vec(x)) {ppp::check_valid(x, base::length(x) == base::length(base::unique(x)), valid)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.unq_chr_vec <- function(x, valid = NULL) {
  if (ppp::.cmp_chr_vec(x)) {ppp::check_valid(x, base::length(x) == base::length(base::unique(x)), valid)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.unq_nnw_vec <- function(x, valid = NULL) {
  if (ppp::.cmp_nnw_vec(x)) {ppp::check_valid(x, base::length(x) == base::length(base::unique(x)), valid)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.unq_num_vec <- function(x, valid = NULL) {
  if (ppp::.cmp_num_vec(x)) {ppp::check_valid(x, base::length(x) == base::length(base::unique(x)), valid)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.unq_psw_vec <- function(x, valid = NULL) {
  if (ppp::.cmp_psw_vec(x)) {ppp::check_valid(x, base::length(x) == base::length(base::unique(x)), valid)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.unq_str_vec <- function(x, valid = NULL) {
  if (ppp::.cmp_str_vec(x)) {ppp::check_valid(x, base::length(x) == base::length(base::unique(x)), valid)}
  else {F}
}

#' @rdname ppp_fast
#' @export
.atm_rct_dtf <- function(x, valid = NULL) {
  if (!ppp::.DTF(x)) {return(F)}
  if (base::nrow(x) < 2 | base::ncol(x) < 2) {return(F)}
  for (i in 1:base::ncol(x)) {if (!ppp::.ATM(x[[i]])) {return(F)}}
  ppp::check_valid(x, T, valid)
}
