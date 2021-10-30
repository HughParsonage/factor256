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
#' @param tbl The table of values to lookup in \code{f}. May be a \code{factor256}
#' class but will be implicitly converted based on the levels of \code{f}.
#'
#' @return
#' \code{factor256} is a class based on raw vectors.
#' Values in \code{x} absent from \code{levels} are mapped to \code{00}.
#'
#' In the following list, \code{o} is the result.
#'
#' \describe{
#' \item{\code{factor256}}{A raw vector of class \code{factor256}.}
#' \item{\code{recompose256}}{is the inverse operation.}
#' \item{\code{factor256_e?(not)?in}}{A logical vector the same length of \code{f}, \code{o[i] = TRUE} if
#' \code{f[i]} is among the values of \code{tbl} when converted to \code{factor256}.
#' \code{_notin} is the negation. The \code{factor256_e} variants will error if
#' none of the values of \code{tbl} are present in \code{f}.}
#' \item{\code{tabulate256}}{Takes a raw vector and counts the number of times
#' each element occurs within it. It is always length-256; if an element is absent
#' it will have value zero in the output.}
#' \item{\code{as_factor}}{Converts from \code{factor256} to \code{factor}.}
#'
#' }
#'
#' @examples
#' f10 <- factor256(1:10)
#'
#' fletters <- factor256(rep(letters, 1:26))
#' head(factor256_in(fletters, "g"))
#' head(tabulate256(fletters))
#' head(recompose256(fletters))
#'
#' gletters <- factor256(rep(letters, 1:26), levels = letters[1:25])
#' tail(tabulate256(gletters))

#'
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
  if (is.logical(lf)) {
    return(.Call("Cfactor2562logical", f, PACKAGE = packageName()))
  }
  lf[as.integer0(f)]
}

logical2factor256 <- function(x, levels = NULL) {
  ans <- .Call("Clogical2factor256", x, PACKAGE = packageName())
}

integer2factor256 <- function(x, levels) {
  mx <- match(x, levels, nomatch = 0L)
  .Call("Cint2factor256", mx, PACKAGE = packageName())
}

character2factor256 <- function(x, ux, anyNAx) {
  mx <- match(x, ux, nomatch = 0L)
  ans <- .Call("Cint2factor256", mx, PACKAGE = packageName())
}

as.integer0 <- function(x) {
  # same as integer but converts 0 to NA
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
    message("Falling back in StackMatch")
    return(as.raw(match(x, ux, nomatch = 0L)))
  }
  ans
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


#' @export
"[.factor256" <- function(x, i, ...) {
  lvls <- levels(x)
  o <- as.raw(x)[i]
  attr(o, "factor256_levels") <- lvls
  class(o) <- oldClass(x)
  o
}

## #' @method is.unsorted factor256
## #' @export
## is.unsorted.factor256 <- function(x, na.rm = FALSE, strictly = FALSE) {
##   o <- .Call("CisntSorted256", x, strictly, PACKAGE = packageName())
##   as.logical(o)
## }


#' @export
anyNA.factor256 <- function(x, ...) {
  FALSE
}

#' @rdname factor256
#' @export
isntSorted256 <- function(x, strictly = FALSE) {
  .Call("CisntSorted256", x, strictly, PACKAGE = packageName())
}

#' @rdname factor256
#' @export
as_factor <- function(x) {
  if (is.factor(x)) {
    return(x)
  }
  lvls <- levels(x)
  f <- as.integer0(x)
  class(f) <- "factor"
  levels(f) <- lvls
  f
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

#' @rdname factor256
#' @export
factor256_ein <- function(x, tbl) {
  x <- factor256(x)
  if (anyDuplicated(tbl)) {
    stop("`anyDuplicated(tbl) = ", anyDuplicated(tbl), "`. Remove duplicate elements from tbl.")
  }

  tbl256 <- factor256(tbl, levels(x))
  ux <- unique_raw(x)
  for (j in seq_along(tbl)) {
    if (!(as.raw(tbl256[j]) %in% ux)) {
      stop("`tbl` contained ", tbl[j], ", but this value was not found in x. ",
           "All values of `tbl` must be in `x`. Ensure you have specified `tbl` correctly.")
    }
  }
  .Call("Cfactor256_in", x, tbl256, FALSE, PACKAGE = packageName())
}

#' @rdname factor256
#' @export
factor256_enotin <- function(x, tbl) {
  x <- factor256(x)
  tbl256 <- factor256(tbl, levels(x))
  ux <- unique_raw(x)
  for (j in seq_along(tbl)) {
    if (!(as.raw(tbl256[j]) %in% ux)) {
      stop("`tbl` contained ", tbl[j], ", but this value was not found in x. ",
           "All values of `tbl` must be in `x`. Ensure you have specified `tbl` correctly.")
    }
  }
  .Call("Cfactor256_in", x, tbl256, TRUE, PACKAGE = packageName())
}

#' @rdname factor256
#' @export
tabulate256 <- function(f) {
  stopifnot(is.raw(f))
  .Call("Ctabulate256", f, PACKAGE = packageName())
}

rank256 <- function(x) {
  if (!is.raw(x)) {
    return(order(x))
  }
  .Call("C_rank256", x, FALSE, PACKAGE = packageName())
}

order256 <- function(x) {
  if (!is.raw(x)) {
    return(order(x))
  }
  .Call("C_rank256", x, TRUE, PACKAGE = packageName())
}

unique_raw <- function(x) {
  stopifnot(is.raw(x))
  tx <- tabulate256(x)
  as.raw(0:255)[tx > 0]
}



interlace256 <- function(x, y, z, w) {
  stopifnot(is.raw(x), is.raw(y), length(x) == length(y))
  if (!is.raw(z)) {
    z <- raw(length(x))
  }
  if (!is.raw(w)) {
    w <- raw(length(w))
  }
  .Call("C_interlace256", x, y, z, w, PACKAGE = packageName())
}

deinterlace256 <- function(r, new_names = paste0("r", 0:3)) {
  .Call("C_deinterlace256", r, PACKAGE = packageName())
}

isntSortedFw16 <- function(x, d = 0L) {
  .Call("CisSorted_d", x, d, PACKAGE = packageName())
}

SortFw16 <- function(x, d = 0L) {
  # c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 10L, 11L, 12L, 13L, 16L, 17L,
  #   18L, 21L, 22L, 23L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L,
  #   34L, 35L, 36L)

  for (.d in d) {
    x <- .Call("CSortFw16", x, .d, PACKAGE = packageName())
  }
  x
}



