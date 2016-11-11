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


#' Tidy up time series data
#'
#' @param .data the data (ts, mts, xts, zoo) which will be tidied
#' @param ... variables to be passed to the dplyr select statement
#' @param ts.name (string) the name of the dataset. Necessary if .data
#'     is a ts object
#'
#' @return (dataframe) a tidy dataframe with the time series data. The
#'     first column will be the time data labeled time
#'
#' @export
tidy_ts <- function(.data, ..., ts.name = NULL) {

    if (!is.data.frame(.data)) {

        if (!(class(.data)[1] %in% c("ts","mts","zoo","xts"))) {
            stop("Data needs to be a dataframe, ts, mts, zoo, or xts object")
        }

        ##get the data with as a dataframe
        if (class(.data) == "ts" || class(.data) == "mts") {

            ##For ts objects, grab the name of the object
            if (class(.data)[1] == "ts" && is.null(ts.name)) {
                stop("for ts objects, need a name")
            }
            .data <- as_ts_df(.data)

            ##For ts objects, update the names
            if (ncol(.data) == 2) names(.data)[2] <- ts.name

        } else if ("zoo" %in% class(.data)) {
            .data <- as_zoo_df(.data)
        }

        ##check to make sure the data is a data frame
        stopifnot(is.data.frame(.data))

        time.col <- 1

    } else {
        ##data.frame
        col.classes <- vapply(.data, class, character(1))
        ##determine the location of the time columns
        time.col <- which(col.classes %in% c("Date", "POSIXlt", "POSIXct"))

        if (length(time.col) > 1) {
            message("More than 1 time or date column found, using the first one and other time col will be dropped")
            time.col <- time.col[1]
        }
    }

    ##select the time column and all of the data from select
    time.data <- .data[, time.col, drop = FALSE]

    ##if the dots, use dplyr to select
    if (!missing(...)) {
        ##use dplyr select
        data.out <- .data %>% dplyr::select(...)
    } else {
        data.out <- .data[, -time.col, drop = FALSE]
    }

    ##make sure we only have numeric columns
    data.out <- data.out[, vapply(data.out, is.numeric, logical(1)), drop = FALSE]
    ##the number of variables
    num.vars <- ncol(data.out)
    ##gather -- use standard evaluation to comply
    ##with CRAN checks
    data.out <- data.out %>%
        tidyr::gather_("variable", "value", names(data.out))

    ##repeat the time data num.vars times
    time.data <- time.data[rep(seq_len(nrow(time.data)), num.vars),, drop = FALSE]

    ##Bind all of the data together
    data.out <- dplyr::bind_cols(time.data, data.out)

    names(data.out)[1] <- "time"

    return(data.out)

}
