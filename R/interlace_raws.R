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
#' @export
interlace256 <- function(w, x, y = NULL, z = NULL) {
  .Call("C_interlace256", w, x, y, z, PACKAGE = packageName())
}

#' @rdname interlace256
#' @export
deinterlace256 <- function(u) {
  .Call("C_deinterlace256", u, PACKAGE = packageName())
}
