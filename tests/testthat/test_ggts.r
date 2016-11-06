## c:/Dropbox/RpackagesCreate/ggts/tests/testthat/test_ggts.r

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-11-01

context("ggts plots")

library(AER);
library(xts)

data(AirPassengers)
data("USMacroSW")
data(AAPL.data)
data(economics)

##A random time column named date
USMacroSW2 <- as_ts_df(USMacroSW)[, c("ffrate", "tbill", "time", "tbond")]
names(USMacroSW2)[3] <- "date"
##A second random time column
USMacroSW3 <- USMacroSW2
USMacroSW3$date2 <- USMacroSW2$date

test_that("ggts returns type plot for different objects", {
    expect_is(ggts(AAPL.data), "gg")
    expect_is(ggts(AAPL.data, linetype = FALSE), "gg")
    expect_is(ggts(AAPL.data, linetype = FALSE, color = TRUE), "gg")
    expect_is(ggts(AAPL.data, dplyr::matches("Adjusted")), "gg")
    expect_is(ggts(AirPassengers), "gg")
    expect_is(ggts(USMacroSW), "gg")
    expect_is(ggts(USMacroSW2), "gg")
    expect_is(ggts(USMacroSW3), "gg")
    expect_is(ggts(economics, psavert, uempmed), "gg")
})

test_that("ggts_facet returns type plot for different objects", {
    expect_is(ggts_facet(economics), "gg")
    expect_is(ggts_facet(economics, psavert, uempmed), "gg")
    expect_is(ggts_facet(AAPL.data), "gg")
    expect_is(ggts_facet(AAPL.data, -dplyr::matches("Volume")), "gg")
    expect_is(ggts_facet(USMacroSW), "gg")
})




test_that("ggts (or ggts_facet()) + geom_cycle() returns type plot for differnt object", {
    expect_is(ggts(AirPassengers) + geom_cycle(), "gg")
    expect_is(ggts(AAPL.data, -dplyr::matches("Volume")) + geom_cycle(), "gg")
    expect_is(ggts(AAPL.data, -dplyr::matches("Volume")) + geom_cycle(dates = bear_dates), "gg")
    expect_is(ggts(AAPL.data, -dplyr::matches("Volume")) +
              geom_cycle(dates=recession_dates) +
              geom_cycle(dates=bear_dates, fill="gray50"), "gg")
    expect_is(ggts(economics, psavert, uempmed) + geom_cycle(), "gg")
    expect_is(ggts_facet(USMacroSW) + geom_cycle(), "gg")
    expect_is(ggts_facet(USMacroSW) + geom_cycle(dates=bear_dates), "gg")
    ggts_facet(USMacroSW) + geom_cycle() + geom_cycle(dates=bear_dates, fill="grey50")
})


