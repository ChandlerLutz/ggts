##' Converts a ts or mts object to a dataframe
##'
##' The first column of the new dataframe holds the time/date in
##' in a column labeled "time"
##'
##' Note: If the data are quarterly, the first quarter will be translated
##' to "YYYY-01-01" and so on
##'
##' @param ts_obj the ts or mts object to be converted into a dataframe
##' @param date_col Flag variable.  If TRUE< then the time variable in the new dataframe will be a date object
##' @return dataframe
##' @export
##' @seealso \code{\link{as.zoo.df}}
##' @examples
##' #USMacrsoSW form the AER package is an "mts" object
##' data("USMacroSW")
##' as.ts.df(USMacroSW)
##' #Using the AirPassengers data
##' data(AirPassengers)
##' as.ts.df(AirPassengers)
##' @author Chandler Lutz \email{cl.eco@@cbs.dk}
as.ts.df <- function(ts_obj,date_col=TRUE) {

    #Check to make sure we have a ts object
    if (is.ts(ts_obj)) {

        time <- time(ts_obj)
        if (date_col) {
            #Call the as.Date.ts() from the zoo package
            time <- as.Date.ts(time)
        }

        #we have a time series object
        dataframe <- data.frame(time=time,data.frame(ts_obj))

        if (ncol(dataframe) == 2) {
            #two columns in the dataframe, name the second after the
            #orginal ts object
            #Use the deparse-substitute trick
            names(dataframe)[2] <- deparse(substitute(ts_obj))
        }

        return(dataframe)
    } else {
        #Print an error message
        stop("Warning: ts_obj is not a time series object in as.ts.df()")
    }
}
