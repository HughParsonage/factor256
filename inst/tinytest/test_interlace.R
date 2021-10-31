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

