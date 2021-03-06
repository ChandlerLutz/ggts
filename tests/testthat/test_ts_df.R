## c:/Dropbox/RpackagesCreate/ggts/tests/testthat/test_ts_df.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-11-01

context("ts and zoo to dataframe")

library(AER)
data(AirPassengers)
data("USMacroSW")
data(AAPL.data)
##With spaces in the names of variables
USMacroSW2 <- as_ts_df(USMacroSW)
names(USMacroSW2)[4] <- "fed funds"
names(USMacroSW2)[5] <- "T Bill"


df.data <- data.frame(x = 1:10, y = 11:20)

##Test as_ts_df
test_that("as_ts_df returns a dataframe with time as the first column", {
    expect_equal(as_ts_df(AirPassengers) %>% class, "data.frame")
    expect_equal(as_ts_df(USMacroSW) %>% class, "data.frame")
    expect_equal(as_ts_df(AirPassengers)[, 1] %>% class, "Date")
    expect_equal(as_ts_df(USMacroSW)[, 1] %>% class, "Date")
    expect_error(as_ts_df(AAPL.data))
    expect_error(as_ts_df(df.data))
})

##Test as_zoo_df
test_that("as_zoo_df returns a data frame with time as the first column", {
    expect_equal(as_zoo_df(AAPL.data) %>% class, "data.frame")
    expect_equal(as_zoo_df(AAPL.data)[, 1] %>% class, "Date")
    expect_error(as_zoo_df(USMacroSW))
    expect_error(as_zoo_df(df.data))
})


##Test tidy_ts
test_that("tidy_ts returns a data frame where the first column is time", {
    expect_is(tidy_ts(AirPassengers, ts.name = "AirPassengers"), "data.frame")
    expect_is(tidy_ts(AirPassengers, ts.name = "AirPassengers")[, 1], "Date")
    expect_is(tidy_ts(AAPL.data), "data.frame")
    expect_is(tidy_ts(AAPL.data)[, 1], "Date")
    expect_is(tidy_ts(USMacroSW), "data.frame")
    expect_is(tidy_ts(USMacroSW, cpi, gdpjp), "data.frame")
    expect_is(tidy_ts(USMacroSW)[, 1], "Date")
})


##Test that tidy_ts works with dataframes that have spaces
test_that("tidy_ts works with dataframes that have spaces in variable names", {
    expect_is(tidy_ts(USMacroSW2), "data.frame")
})
