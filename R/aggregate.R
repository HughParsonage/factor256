#' Aggregating helpers
#' @param DT A \code{data.table}.
#' @param by (string) A column of \code{DT}, the count of which is desired.
#' @param count_col (string) The name of the column in the result containing
#' the counts.
#' @return
#' For:
#' \describe{
#' \item{\code{count_by256}}{A tally of \code{by}.}
#' }
#'
count_by256 <- function(DT, by = NULL, count_col = "N") {
  stopifnot(is.data.frame(DT), hasName(DT, by))
  if (is.raw(v <- .subset2(DT, by))) {
    ans1 <- .Call("Ctabulate256", v, PACKAGE = packageName())
    ans <- list(x = levels(v), N = ans1[ans1 > 0])
    names(ans) <- c(by, count_col)
    if (requireNamespace("data.table", quietly = TRUE)) {
      data.table::setDT(ans)
    }
    return(ans)
  }
  byy <- by
  if (requireNamespace("data.table", quietly = TRUE)) {
    .N <- NULL
    return(data.table::setnames(DT[, .N, keyby = c(byy)], "N", count_col))
  }
  message("data.table not installed and `by` is not raw so ignoring.")
}

sum_by256 <- function(DT, by, sum_col, sum_by = paste0("tot", by)) {
  stopifnot(is.data.frame(DT), hasName(DT, by), hasName(DT, sum_col))
  if (is.raw(v <- .subset2(DT, by))) {
    ans1 <- .Call("C_sum_by256", .subset2(DT, sum_col), v, PACKAGE = packageName())
    if (anyNA(levels(v))) {
      ans <- list(x = levels(v), N = ans1[seq_along(levels(v))])
    } else {
      ans <- list(x = levels(v), N = ans1[seq_along(levels(v)) + 1L])
    }
    names(ans) <- c(by, sum_by)
    if (requireNamespace("data.table", quietly = TRUE)) {
      data.table::setDT(ans)
    }
    return(ans)
  }
}


