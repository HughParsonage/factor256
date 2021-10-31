#' Interlace raw vectors
#' @description Some processes do not accept raw vectors so it can be
#' necessary to convert our vectors to integers.
#'
#' @param w,x,y,z Raw vectors. A vector may be \code{NULL} if fewer than four
#' vectors need to be compressed.
#' @param u An integer vector.
#' @return \code{interlace256} Return an integer vector, compressing raw vectors.
#' \code{deinterlace256} is the inverse operation, returning a list of four raw vectors.
#'
#' @param DT A \code{data.frame} containing raw vectors to be interlaced.
#' @param new_colnames A mechanism for producing the new columns. Currently only
#' \code{1L} is implemented, the default mechanism.
#'
#' @export
interlace256 <- function(w, x, y = NULL, z = NULL) {
  .Call("C_interlace256", w, x, y, z, PACKAGE = packageName())
}

#' @rdname interlace256
#' @export
deinterlace256 <- function(u) {
  .Call("C_deinterlace256", u, PACKAGE = packageName())
}

#' @rdname interlace256
#' @export
interlace256_columns <- function(DT, new_colnames = 1L) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package data.table is required for interlace256_column")
  }
  stopifnot(is.data.frame(DT))
  if (!is.data.table(DT)) {
    DT <- data.table::as.data.table(DT)
  }
  raw_cols <- names(DT)[vapply(DT, is.raw, NA)]
  if (length(raw_cols) <= 1L) {
    if (length(raw_cols) == 1L) {
      data.table::set(DT, j = raw_cols, value = as.integer(.subset2(DT, raw_cols)))
      setnames(DT, raw_cols, paste0("I256-", raw_cols))
    }
    return(DT)
  }

  v1 <- .subset2(DT, raw_cols[1])
  v2 <- .subset2(DT, raw_cols[2])

  v3 <- if (length(raw_cols) >= 3L) .subset2(DT, raw_cols[3])
  v4 <- if (length(raw_cols) >= 4L) .subset2(DT, raw_cols[4])

  new_colname2 <- paste0("I256-", paste0(head(raw_cols, 4), collapse = "-"))
  data.table::set(DT, j = new_colname2, value = interlace256(v1, v2, v3, v4))
  data.table::set(DT, j = head(raw_cols, 4), value = NULL)

  # recurse
  interlace256_columns(DT, new_colnames = new_colnames)
}

#' @rdname interlace256
#' @export
deinterlace256_columns <- function(DT, new_colnames = 1L) {
  likely_rawcols <- (copy(names(DT)[startsWith(names(DT), "I256-")]))
  for (j in seq_along(likely_rawcols)) {
    nom_j <- likely_rawcols[j]
    noms_j <- rev(tail(strsplit(nom_j, split = "-", fixed = TRUE)[[1L]], -1L))
    List <- deinterlace256(.subset2(DT, nom_j))
    for (k in rev(seq_along(noms_j))) {
      data.table::set(DT, j = noms_j[k], value = List[[k]])
    }
  }
  data.table::set(DT, j = likely_rawcols, value = NULL)
  DT
}

