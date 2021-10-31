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

stackx <- c(-5L, 20e3L, -10:10)
expect_identical(factor256:::StackMatch(stackx), as.raw(match(stackx, unique(stackx))))
stackx <- c(5L, 47e3L, 5:15)
expect_identical(factor256:::StackMatch(stackx), as.raw(match(stackx, unique(stackx))))
stackx <- c(1L, 5L, -.Machine$integer.max, .Machine$integer.max)
expect_identical(factor256:::StackMatch(stackx), as.raw(match(stackx, unique(stackx))))

lglx <- c(runif(10) > 0.8, NA, runif(3) > 0.2, NA, TRUE, FALSE)
lglf <- factor256(lglx)
expect_true(is.raw(lglf))
expect_equal(recompose256(lglf), lglx)

x <- factor256(1:255)
tx <- tabulate256(x)
expect_identical(tx, c(0L, rep(1L, 255)))

mtcars_rownames <-
  c("Mazda RX4", "Mazda RX4 Wag", "Datsun 710", "Hornet 4 Drive",
    "Hornet Sportabout", "Valiant", "Duster 360", "Merc 240D", "Merc 230",
    "Merc 280", "Merc 280C", "Merc 450SE", "Merc 450SL", "Merc 450SLC",
    "Cadillac Fleetwood", "Lincoln Continental", "Chrysler Imperial",
    "Fiat 128", "Honda Civic", "Toyota Corolla", "Toyota Corona",
    "Dodge Challenger", "AMC Javelin", "Camaro Z28", "Pontiac Firebird",
    "Fiat X1-9", "Porsche 914-2", "Lotus Europa", "Ford Pantera L",
    "Ferrari Dino", "Maserati Bora", "Volvo 142E")

cc <- c(sample(mtcars_rownames), sample(mtcars_rownames, size = 99, replace = TRUE))
cc_f256 <- factor256(cc)
expect_equal(cc %in% c("foo", "Ferrari Dino", "Volvo 142E"),
             factor256_in(cc_f256, c("foo", "Ferrari Dino", "Volvo 142E")))
expect_equal(!(cc %in% c("foo", "Ferrari Dino", "Volvo 142E")),
             factor256_notin(cc_f256, c("foo", "Ferrari Dino", "Volvo 142E")))
expect_error(factor256_ein(cc_f256, c("foo", "Ferrari Dino", "Volvo 142E")))
expect_error(factor256_enotin(cc_f256, c("foo", "Ferrari Dino", "Volvo 142E")))

expect_equal(cc %in% c("Ferrari Dino", "Volvo 142E"),
             factor256_ein(cc_f256, c("Ferrari Dino", "Volvo 142E")))
expect_equal(!(cc %in% c("foo", "Ferrari Dino", "Volvo 142E")),
             factor256_enotin(cc_f256, c("Ferrari Dino", "Volvo 142E")))

library(utils)
expect_equal(head(cc), recompose256(head(cc_f256)))
expect_equal(tail(cc), recompose256(tail(cc_f256)))
#
ffaabc <- factor256(c("A", "A", "B", "C"), levels = LETTERS)
expect_true(as.logical(isntSorted256(factor256(c("A", "A", "B", "A"), levels = LETTERS))))
expect_equal(isntSorted256(factor256(c("A", "A", "B")), strictly = TRUE), 2L)
expect_equal(isntSorted256(factor256(c("A", "A", "B")), strictly = FALSE), 0L)
expect_equal(isntSorted256(factor256(c("A"))), 0L)





