#' Factors of fewer than 256 elements
#' @importFrom utils packageName
#' @importFrom utils head
#' @importFrom utils tail
#' @useDynLib factor256, .registration=TRUE
#'
#' @description Whereas base R's factors as based on 32-bit integer vectors,
#' \code{factor256} uses 8-bit raw vectors to minimize memory footprints.
#'
#' @param x An atomic vector with fewer than 256 unique elements.
#' @param levels An optional character vector of or representing the unique values of \code{x}.
#'
#'
#' @param f A raw vector of class \code{factor256}.
#' @param tbl The table of values to lookup in \code{f}. May be a \code{factor256}
#' class but will be implicitly converted based on the levels of \code{f}.
#'
#' @param strictly If \code{TRUE} then if \code{x[i] == x[j]} and \code{i != j}
#' then \code{x} is not sorted.
#'
#' @param nmax,dotInterval (\code{tabulate256_levels} only). Every \code{dotInterval}
#' iterations through \code{x} check number of unique elements detected so far. If any count exceeds
#' \code{nmax} the rest of the vector is ignored.
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
#' \item{\code{tabulate256_levels}}{Similar to \code{tabulate256} but with optional arguments \code{nmax},
#' \code{dotInterval}.}
#' \item{\code{as_factor}}{Converts from \code{factor256} to \code{factor}.}
#' \item{\code{order256}}{Same as \code{order} but supports raw vectors. \code{order256(x)}}
#' \item{\code{rank256}}{Same as \code{rank} with \code{ties.method = "first"} but supports raw vectors.}
#' \item{\code{unique256}}{Unique elements of.}
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
#' tabulate256_levels(gletters, nmax = 5L, dotInterval = 1L)

#'
#'
#' @export

factor256 <- function(x, levels = NULL) {
  if (inherits(x, "factor256")) {
    if (is.null(levels) || identical(levels(x), levels)) {
      return(x)
    }
    attr(x, "factor256_levels") <- union(levels(x), levels)
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
    if (anyDuplicated(levels)) {
      dup_ele <- levels[anyDuplicated(levels)]
      stop("`levels` had duplicated elements: ", dup_ele,
           # don't worry abouy efficiency of levels == dup_ele since length() < 256
           " appeared at positions ", toString(head(which(levels == dup_ele))), ". ",
           "All elements of `levels` must be unique.")
    }
    if (is.character(levels) && !is.character(x) && !is.logical(x)) {
      ux <- unique(x)
      ans <- integer2factor256(x, ux)
      class(ans) <- "factor256"
      attr(ans, "factor256_levels") <- as.character(levels)
      return(ans)
    }

  }

  ans <-
    switch(typeof(x),
           "raw" = raw2factor256(x, levels),
           "logical" = logical2factor256(x, levels),
           "integer" = integer2factor256(x, levels),
           "character" = character2factor256(x, levels),
           "double" = integer2factor256(x, levels))
  class(ans) <- "factor256"
  attr(ans, "factor256_levels") <- as.character(levels)
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
  switch(as.character(attr(f, "orig_type")),
         "logical" = .Call("Cfactor2562logical", f, PACKAGE = packageName()),
         "integer" = as.integer(lf[as.integer0(f)]),
         "double" = as.double(lf[as.integer0(f)]),
         "raw" = as.raw(lf)[as.integer0(f)],
         # including character
         lf[as.integer0(f)])
}

.orig_lgl <- function(x) {
  attr(x, "orig_type") == "logical"
}

logical2factor256 <- function(x, levels = NULL) {
  ans <- .Call("Clogical2factor256", x, PACKAGE = packageName())
  attr(ans, "orig_type") <- "logical"
  ans
}

raw2factor256 <- function(x, levels) {
  ans <- .Call("C_raw2factor256", x, levels, PACKAGE = packageName())
  attr(ans, "orig_type") <- "raw"
  ans
}

integer2factor256 <- function(x, levels) {
  mx <- match(x, levels, nomatch = 0L)
  ans <- .Call("Cint2factor256", mx, PACKAGE = packageName())
  attr(ans, "orig_type") <- typeof(x)
  ans
}

character2factor256 <- function(x, ux, anyNAx) {
  mx <- match(x, ux, nomatch = 0L)
  ans <- .Call("Cint2factor256", mx, PACKAGE = packageName())
  attr(ans, "orig_type") <- "character"
  ans
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
relevel256 <- function(x, levels) {
  lx <- levels(x)
  if (length(levels) == 1) {
    if (levels %notin% lx) {
      stop("`levels = ", levels, "` but must be an existing level.")
    }
    nl <- c(levels, setdiff(lx, levels))
    mnl <- match(nl, lx)
    ans <- raw2factor256(x, as.raw(mnl))
    levels(ans) <- nl
    return(ans)
  }
  if (length(levels) != length(levels(x))) {
    stop("length(levels) = ", length(levels), ", yet length(levels(x)) = ", length(levels(x)), ". ",
         "levels must have the same length as the levels of `x`, or length 1.")
  }
  nl <- levels
  mnl <- match(nl, lx)
  ans <- raw2factor256(x, as.raw(mnl))
  levels(ans) <- nl
  return(ans)
  x

}

#' @rdname factor256
#' @export
levels.factor256 <- function(x) {
  attr(x, "factor256_levels")
}

#' @export
"levels<-.factor256" <- function(x, value) {
  attr(x, "factor256_levels") <- value
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
  attr(o, "orig_type") <- attr(x, "orig_type")
  o
}

#' @export
print.factor256 <- function(x, ...) {
  cat("factor256 ")
  print(levels(x)[as.integer0(x)], max = 20)
  cat("Levels[", length(levels(x)), "]: ", sep = "")
  cat(head(levels(x)))
}

#' @export
format.factor256 <- function(x, ...) {
  format(levels(x)[as.integer(x)], ...)
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


no_intertwine <- function(x, y) {
  # x and y are levels, we don't want the order to be different
  # and can only be different if no overlap
  if (identical(x, y)) {
    return(TRUE)
  }
  i <- 1L
  mx <- match(x, y)
  if (anyNA(mx)) {
    if (all(is.na(mx))) {
      return(TRUE)
    }

  }
}

ensure_common256 <- function(x, to) {
  stopifnot(is.factor256(x), is.factor256(to))
  if (identical(levels(x), levels(to))) {
    return(x)
  }
  factor256(x, levels = union(levels(x), levels(to)))
}


#' @rdname factor256
#' @export
factor256_in <- function(x, tbl) {
  x <- factor256(x)
  tbl <- factor256(as.character(tbl), levels(x))
  .Call("Cfactor256_in", x, tbl, FALSE, PACKAGE = packageName())
}

#' @rdname factor256
#' @export
factor256_notin <- function(x, tbl) {
  x <- factor256(x)
  tbl <- factor256(as.character(tbl), levels(x))
  .Call("Cfactor256_in", x, tbl, TRUE, PACKAGE = packageName())
}

#' @rdname factor256
#' @export
factor256_ein <- function(x, tbl) {
  x <- factor256(x)
  if (anyDuplicated(tbl)) {
    stop("`anyDuplicated(tbl) = ", anyDuplicated(tbl), "`. Remove duplicate elements from tbl.")
  }

  tbl256 <- factor256(as.character(tbl), levels(x))
  ux <- unique256(x)
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
  if (anyDuplicated(tbl)) {
    stop("`anyDuplicated(tbl) = ", anyDuplicated(tbl), "`. Remove duplicate elements from tbl.")
  }
  tbl256 <- factor256(tbl, levels(x))
  ux <- unique256(x)
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

#' @rdname factor256
#' @export
rank256 <- function(x) {
  if (!is.raw(x)) {
    return(rank(x, ties.method = "first"))
  }
  .Call("C_rank256", x, FALSE, PACKAGE = packageName())
}

#' @rdname factor256
#' @export
order256 <- function(x) {
  if (!is.raw(x)) {
    return(order(x))
  }
  .Call("C_rank256", x, TRUE, PACKAGE = packageName())
}

#' @rdname factor256
#' @export
unique256 <- function(x) {
  stopifnot(is.raw(x))
  tx <- tabulate256(x)
  as.raw(0:255)[tx > 0]
}

#' @rdname factor256
#' @export
tabulate256_levels <- function(x, nmax = NULL, dotInterval = 65535L) {
  if (!is.integer(nmax)) {
    nmax <- 256L
  }
  .Call("Ctabulate256_levels", x, nmax = nmax, dotInterval = dotInterval, PACKAGE = packageName())
}


"%notin%" <- function(x, tbl) is.na(match(x, tbl))

.order <- function(...) {
  .Call("Corder2", NULL, ..1, ..2, PACKAGE = packageName())
}






