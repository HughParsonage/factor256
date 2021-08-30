#' Factors of fewer than 256 elements
#' @importFrom utils packageName
#' @useDynLib factor256, .registration=TRUE
#'
#' @description Whereas base R's factors as based on 32-bit integer vectors,
#' \code{factor256} uses 8-bit raw vectors to minimize memory footprints
#'
#' @param x An atomic vector with fewer than 256 elements.
#' @param levels An optional vector of the unique values of \code{x}.
#'
#' @param f A raw vector of class \code{factor256}.
#' @param tbl The table of values to lookup in \code{f}.
#'
#' @return A raw vector of class \code{factor256}.
#'
#' Values in \code{x} absent from \code{levels} are mapped to \code{00}.
#'
#' \code{recompose256} is the inverse operation.
#'
#' @export

factor256 <- function(x, levels = NULL) {
  if (inherits(x, "factor256")) {
    return(x)
  }
  stopifnot(is.atomic(x))
  if (is.null(levels)) {
    levels <- unique(x)
    if (length(levels) >= 256L) {
      stop("`length(unique(x)) = ", length(levels), "` but must be less than 256.")
    }
  } else {
    if (length(levels) >= 256L) {
      stop("length(levels) = ", length(levels), " but must be less than 256.")
    }
    if (typeof(levels) != typeof(x)) {
      stop("`levels` was type ", typeof(levels), " yet `x` was type ", typeof(x), ". Not supported. ",
           "Ensure `x` and `levels` have matching types.")
    }
    if (anyDuplicated(levels)) {
      dup_ele <- levels[anyDuplicated(levels)]
      stop("`levels` had duplicated elements: ", dup_ele,
           # don't worry abouy efficiency of levels == dup_ele since length() < 256
           "appears at positions ", which(levels == dup_ele), ". ",
           "All elements of `levels` must be unique.")
    }

  }
  ans <-
    switch(typeof(x),
           "logical" = logical2factor256(x, levels),
           "integer" = integer2factor256(x, levels),
           "character" = character2factor256(x, levels))
  class(ans) <- "factor256"
  attr(ans, "factor256_levels") <- levels
  ans
}

#' @rdname factor256
#' @export
recompose256 <- function(f) {
  if (!is.factor256(f)) {
    stop("`f` was of class ", class(f), " but must be a 'factor256'.")
  }
  if (!is.raw(f)) {
    stop("Internal error: f was type ", typeof(f), " but type raw was expected.") # nocov
  }
  lf <- levels(f)
  lf[as.integer0(f)]
}

logical2factor256 <- function(x, levels = NULL) {
  ans <- .Call("Clogical2factor256", x, PACKAGE = packageName())
}

integer2factor256 <- function(x, levels = NULL) {
  if (is.null(levels)) {
    levels <- unique(x)
    if (length(levels) >= 256L) {
      stop("`length(unique(x)) = ", length(levels), "` but must be less than 256.")
    }
  }
  mx <- match(x, levels, nomatch = 0L)
  .Call("Cint2factor256", mx, PACKAGE = packageName())
}

character2factor256 <- function(x, ux, anyNAx) {
  mx <- match(x, ux, nomatch = 0L)
  ans <- .Call("Cint2factor256", mx, PACKAGE = packageName())
}

as.integer0 <- function(x) {
  .Call("C_asInteger0", x, PACKAGE = packageName())
}


StackMatch <- function(x, ux = NULL) {
  # as.raw(match(x, ux, nomatch = 0L))
  # but only when
  x <- as.integer(x)
  if (!is.integer(ux)) {
    ux <- unique(x)
  }
  ans <- .Call("CStackMatch", x, as.integer(ux), PACKAGE = packageName())
  if (is.null(ans)) {
    return(as.raw(match(x, ux, nomatch = 0L)))
  }
  ans
}

Bsearch <- function(a, x) {
  .Call("BSearch", as.integer(a), as.integer(x), PACKAGE = packageName())
}

#' @rdname factor256
#' @export
levels.factor256 <- function(x) {
  attr(x, "factor256_levels")
}

#' @rdname factor256
#' @export
is.factor256 <- function(x) {
  inherits(x, "factor256")
}

#' @rdname factor256
#' @export
factor256_in <- function(x, tbl) {
  x <- factor256(x)
  tbl <- factor256(tbl, levels(x))
  .Call("Cfactor256_in", x, tbl, FALSE, PACKAGE = packageName())
}

#' @rdname factor256
#' @export
factor256_notin <- function(x, tbl) {
  x <- factor256(x)
  tbl <- factor256(tbl, levels(x))
  .Call("Cfactor256_in", x, tbl, TRUE, PACKAGE = packageName())
}




