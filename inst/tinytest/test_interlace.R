library(tinytest)
library(factor256)

expect_error(interlace256(1L, 1L), "integer")
expect_error(interlace256(as.raw(1), 1), "double")
expect_error(interlace256(as.raw(1), as.raw(1), "f"), "character")
expect_error(interlace256(as.raw(1), as.raw(1), as.raw(1), 1 + 1i), "complex")
expect_equal(deinterlace256(interlace256(as.raw(1:5), raw(5))), list(as.raw(1:5), raw(5), raw(5), raw(5)))


DF <- lapply(1:7, function(x) sample.int(8, size = 8, replace = TRUE))

DF <- setNames(as.data.frame(DF), paste0("Z", 1:7))
ans <- DF
if (requireNamespace("data.table", quietly = TRUE)) {
  ans <- deinterlace256_columns(interlace256_columns(DF))
  ans <- as.data.frame(ans)
}
expect_equal(ans, DF)

DF <- lapply(1:7, function(x) as.raw(sample.int(8, size = 8, replace = TRUE)))

DF <- setNames(as.data.frame(DF), paste0("Z", 1:7))
ans <- DF
if (requireNamespace("data.table", quietly = TRUE)) {
  ans <- deinterlace256_columns(interlace256_columns(DF))
  ans <- as.data.frame(ans)
}
expect_equal(ans, DF)

DF <- lapply(1:9, function(x) as.raw(sample.int(8, size = 8, replace = TRUE)))

DF <- setNames(as.data.frame(DF), paste0("Z", 1:9))
ans <- DF
if (requireNamespace("data.table", quietly = TRUE)) {
  ans <- deinterlace256_columns(interlace256_columns(DF))
  ans <- as.data.frame(ans)
  data.table::setcolorder(ans, paste0("Z", 1:9))
}
expect_equal(ans, DF)





