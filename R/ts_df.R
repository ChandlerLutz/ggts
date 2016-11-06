## c:/Dropbox/RpackagesCreate/ggts/R/ts_df.r

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2016-10-31

##A set of functions to conver ts objects to dataframes

#' Converts a ts or mts object to a dataframe
#'
#' \code{as.ts.df} transfroms a ts or mts object to a dataframe
#'
#' The first column of the new dataframe holds the time/date in
#' in a column labeled "time"
#'
#' Note: If the data are quarterly, the first quarter will be translated
#' to "YYYY-01-01", if monthly the first month will be translated to
#' "YYYY-01-01" and so on
#'
#' @param ts_obj the ts or mts object to be converted into a dataframe
#' @param date_col Flag variable.  If TRUE, then the time variable in the new dataframe
#' will be a date object
#' @return dataframe where a column of class "Date" where the first column is labeled "time"
#'
#' @seealso \code{\link{as_zoo_df}}
#'
#' @examples
#' #USMacrsoSW form the AER package is an "mts" object
#' library(AER)
#' data("USMacroSW")
#' as_ts_df(USMacroSW)
#' #Using the AirPassengers data
#' data(AirPassengers)
#' as_ts_df(AirPassengers)
#'
#' @export
as_ts_df <- function(ts_obj, date_col=TRUE) {

    #Check to make sure we have a ts object
    if (is.ts(ts_obj)) {

        time <- time(ts_obj)
        if (date_col) {
            #Call the as.Date.ts() from the zoo package
            time <- zoo::as.Date.ts(time)
        }

        ##Get the temporary data for the dataframe and make
        ##sure everything is numeric
        temp.data <- data.frame(ts_obj)
        for (i in 1:ncol(temp.data)) {
            temp.data[, i] <- as.numeric(temp.data[, i])
        }

        #we have a time series object
        dataframe <- data.frame(time=time, temp.data)

        if (ncol(dataframe) == 2) {
            #two columns in the dataframe, name the second after the
            #orginal ts object
            #Use the deparse-substitute trick
            names(dataframe)[2] <- deparse(substitute(ts_obj))
        }

        return(dataframe)
    } else {
        #Print an error message
        stop("ts_obj is not a time series object in as_ts_df()")
    }
}


#' Convert a zoo or xts object into a dataframe
#'
#' @param data.zoo zoo or xts object
#' @return a data frame with the first column as a date object labeled "time"
#'
#' @seealso \code{\link{as_ts_df}}
#' @examples
#' library(xts)
#' #The apple stock data is a xts object
#' data(AAPL.data)
#' class(AAPL.data)   # "xts" "zoo"
#' head(as_zoo_df(AAPL.data))
#'
#' @export
as_zoo_df <- function(data.zoo) {

    if (!("zoo" %in% class(data.zoo))) {
        stop("object not of class zoo")
    }

    df <- data.frame(time=time(data.zoo), data.frame(data.zoo))
    row.names(df) <- NULL
    return(df)
}