library(tinytest)
library(factor256)
# Placeholder with simple test
expect_equal(1 + 1, 2)

State1 <- factor256(c("NSW", "VIC", "VIC", "NSW"),
                    levels = c("NSW", "VIC", "QLD", "SA", "WA", "TAS", "NT", "ACT", "OT"))


expect_true(is.raw(State1))
expect_equal(factor256_in(State1, "NSW"), c(TRUE, FALSE, FALSE, TRUE))
expect_equal(factor256_notin(State1, "NSW"), !c(TRUE, FALSE, FALSE, TRUE))

State_f <- factor256(c(NA, "NSW", "VIC", "NSW"), level = c("NSW", "VIC"))
expect_equal(recompose256(State_f), c(NA, "NSW", "VIC", "NSW"))

stackx <- c(-5L, 60e3L, -10:10)
expect_identical(factor256:::StackMatch(stackx), as.raw(match(stackx, unique(stackx))))
stackx <- c(-5L, 67e3L, -10:10)
expect_identical(factor256:::StackMatch(stackx), as.raw(match(stackx, unique(stackx))))
stackx <- c(1L, 5L, -.Machine$integer.max, .Machine$integer.max)
expect_identical(factor256:::StackMatch(stackx), as.raw(match(stackx, unique(stackx))))


# expect_equal(sum_equal(State1, "NSW"), 2)




