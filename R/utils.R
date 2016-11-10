#' Tidy up time series data for use with ggts() and ggts_facet()
#'
#' @param .data the data (ts, mts, xts, zoo) which will be tidied
#' @param ... variables to be passed to the dplyr select statement
#' @param ts.name (string) the name of the dataset. Necessary if .data
#'     is a ts object
#'
#' @return (dataframe) a tidy dataframe with the time series data
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
