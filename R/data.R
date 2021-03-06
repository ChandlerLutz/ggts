## c:/Dropbox/RpackagesCreate/ggts/R/data.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-11-03

## To document the datasets

#' Apple (AAPL) stock prices and volumes from Yahoo Finance
#'
#' @format An xts object with 1437 rows and 6 columns
#' \describe{
#'    \item{AAPL.Open}{Daily Opening Price}
#'    \item{AAPL.High}{Daily High}
#'    \item{AAPL.Low}{Daily Low}
#'    \item{AAPL.Volume}{Daily Volume}
#'    \item{AAPL.Adjusted}{Closing Price adjusted for splits and dividends}
#' }
#' @source \url{http://finance.yahoo.com/}
"AAPL.data"

#' Bear market dates
#'
#' Bear market dates defined as a 20 percent or more drop in the
#' S&P500 over a period of 2 or more months
#'
#' @format A data frame with 2 columns:
#' \describe{
#'    \item{start}{Start of bear market cycle}
#'    \item{end}{End of bear market cycle}
#' }
#' @source Chandler Lutz's calculations
"bear_dates"

#' NBER Recession dates
#'
#' Recession dates from the NBER
#'
#' @format A data frame with 2 columns:
#' \describe{
#'    \item{start}{Start of recession}
#'    \item{end}{End of recession}
#' }
#'
#' @source Chandler Lutz's calculations
"recession_dates"
