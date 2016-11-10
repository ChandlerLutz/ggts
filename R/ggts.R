#' Plot time series using ggts
#'
#' @param .data the data to be plotted. Either a dataframe with a time
#'     column (e.g. a "Date", "POSIXlt", or "POSIXct" object), a ts or
#'     mts object, zoo object, or xts object.
#' @param ... arguments to be passed to dplyr select. If a dplyr
#'     helper frunction is used, e.g. \code{matches}, dplyr must be
#'     loaded or the function must be pasted via
#'     \code{dplyr::matches("x")}
#' @param linetype (logical) if TRUE, linetypes will be used
#' @param color (logical) if TRUE, color will be used
#' @param standardize (logical) If set to TRUE, all time series will
#'     be standardized to have zero mean and unit variance before
#'     plotting. Default is FALSE
#' @param index (logical) If set to TRUE, all time series will be
#'     tranformed into an index using \code{x / x[1] - 1}
#'
#' @return a \code{ggplot} of the time series
#'
#' @import ggplot2
#' @importFrom dplyr "%>%"
#' @importFrom dplyr starts_with
#' @importFrom dplyr ends_with
#' @importFrom dplyr contains
#' @importFrom dplyr matches
#' @importFrom dplyr num_range
#' @importFrom dplyr one_of
#' @importFrom dplyr everything
#' @importFrom stats sd
#' @importFrom stats is.ts
#' @importFrom stats time
#'
#' @examples
#' data(AirPassengers) ##A ts object
#' ggts(AirPassengers)
#'
#' ## Interest rates from the Stock and Watson dataset
#' library(AER)
#' data(USMacroSW)  ##a mts object
#' ggts(USMacroSW, ffrate, tbill, tbond)
#' ##Interest rates with colors from the Stock and Watson dataset
#' ggts(USMacroSW, ffrate, tbill, tbond, color = TRUE)
#' ##Index values for CPI and Japanese GDP from SW datasets
#' ggts(USMacroSW, cpi, gdpjp, index = TRUE)
#' ##Standardized unemployment rate and Pound/Dollar exchange rate
#' ggts(USMacroSW, cpi, gdpjp, standardize = TRUE)
#'
#' library(xts)
#' data(AAPL.data) ##xts object
#' ##Plot everything but volume using dplyr matches helper function.
#' ##Note that all dplyr select helper functions can be used, but
#' ##either dplyr must be attached or the package explicitly referenced
#' ##(below example)
#' ggts(AAPL.data, -dplyr::matches("Volume"))
#'
#' ##If we have a randomly placed time column
#' USMacroSW2 <- as_ts_df(USMacroSW)[, c("ffrate", "tbill", "time", "tbond")]
#' names(USMacroSW2)[3] <- "date"
#' ggts(USMacroSW2)
#'
#' ##If more than 1 time column, ggts() will output a message
#' ##saying that only the first time column will be used
#' USMacroSW3 <- USMacroSW2
#' USMacroSW3$date2 <- USMacroSW2$date
#' ggts(USMacroSW3)
#'
#' ##Using a normal dataframe
#' data(economics)
#' ggts(economics, psavert, uempmed)
#'
#' @export
ggts <- function(.data, ..., linetype = TRUE, color = FALSE, standardize = FALSE, index = FALSE) {

    ##if a ts object, get the name and process
    if (class(.data)[1] == "ts") {
        ts.name <- deparse(substitute(.data))
        data.out <- tidy_ts(.data, ..., ts.name = ts.name)
    } else {
        data.out <- tidy_ts(.data, ..., ts.name = NULL)
    }


    ##For SE with mutate_ see
    ##https://www.r-bloggers.com/using-mutate-from-dplyr-inside-a-function-getting-around-non-standard-evaluation/
    ##Set the formula first and then geive it a name
    if (standardize) {
        ##Standardize the data
        data.out <- data.out %>%
            dplyr::group_by_("variable") %>%
            dplyr::mutate_(.dots = stats::setNames(list( ~ (value - mean(value)) / sd(value)), "value")) %>%
            dplyr::ungroup()
    } else if (index) {
        ##use a data index so that the variables start at 0
        data.out <- data.out %>%
            dplyr::group_by_(.dots = "variable") %>%
            dplyr::mutate_(.dots = stats::setNames(list( ~ value/value[1] - 1), "value")) %>%
            dplyr::ungroup()
    }

    p.out <- ggplot2::ggplot(data.out, ggplot2::aes_string(x = "time", y = "value", group = "variable"))

    if (linetype && color) {
        p.out <- p.out + ggplot2::geom_line(ggplot2::aes_string(linetype = "variable", color = "variable"))
    } else if (linetype) {
        p.out <- p.out + ggplot2::geom_line(ggplot2::aes_string(linetype = "variable"))
    } else if (color) {
        p.out <- p.out + ggplot2::geom_line(ggplot2::aes_string(color = "variable"))
    } else {
        p.out <- p.out + ggplot2::geom_line()
    }

    ##Aesthetics
    p.out <- p.out +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.title.x = ggplot2::element_blank(),
              legend.title = ggplot2::element_blank())


    return(p.out)

}

#' Plot a faceted time series -- multiple time series panels
#'
#' @param .data the data to be plotted. Either a dataframe with a time
#'     column (e.g. a "Date", "POSIXlt", or "POSIXct" object), a ts or
#'     mts object, zoo object, or xts object.
#' @param ... arguments to be passed to dplyr select. If a dplyr
#'     helper frunction is used, e.g. \code{matches}, dplyr must be
#'     loaded or the function must be pasted via
#'     \code{dplyr::matches("x")}
#'
#' @return a \code{ggplot} with the faceted time series
#'
#' @examples
#' data(economis)
#' ggts_facet(economics)
#' ggts_facet(economics, psavert, uempmed)
#'
#' library(xts)
#' data(AAPL.data) ##xts object
#' ggts_facet(AAPL.data)
#' ggts_facet(AAPL.data, -dplyr::matches("Volume"))
#'
#' library(AER)
#' data(USMacroSW) ##mts object
#' ggts_facet(USMacroSW)
#'
#' @export
ggts_facet <- function(.data, ...) {

    ##if a ts object, get the name and process
    if (class(.data)[1] == "ts") {
        ts.name <- deparse(substitute(.data))
        data.out <- tidy_ts(.data, ..., ts.name)
    } else {
        data.out <- tidy_ts(.data, ...)
    }

    p.out <- ggplot2::ggplot(data.out, ggplot2::aes_string(x = "time", y = "value", group = "variable")) +
        ggplot2::geom_line() +
        ggplot2::facet_grid(variable ~ ., scales = "free_y") +
        ggplot2::theme_bw()

    return(p.out)

}


