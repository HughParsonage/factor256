library(tinytest)
library(factor256)

if (requireNamespace("data.table", quietly = TRUE)) {
  library(data.table)

  for (nc in 1:13) {
    DT <- lapply(1:nc, function(z) {
      if (z == (nc %/% 2L)) {
        return(1:10)
      }
      as.raw(rep_len(rpois(11, z), 10))
    })
    setDT(DT)
    DT1_orig <- copy(DT)
    orig_noms <- copy(names(DT))
    ans <- deinterlace256_columns(interlace256_columns(DT))

    setcolorder(ans, orig_noms)
    expect_equal(ans,
                 DT1_orig)

  }
}

DF <- lapply(1:7, function(x) sample.int(8, size = 8, replace = TRUE))

DF <- setNames(as.data.frame(DF), paste0("Z", 1:7))
ans <- deinterlace256_columns(interlace256_columns(DF))
ans <- as.data.frame(ans)
expect_equal(ans, DF)



