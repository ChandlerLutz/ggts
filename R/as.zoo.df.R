##' Convert a zoo or xts object into a dataframe
##'
##' @param data.zoo zoo or xts object
##' @return a data frame with the first column as a date object labeled "time"
##' @export
##' @seealso \code{\link{as.ts.df}}
##' @examples
##' #The apple stock data is a xts object
##' data(AAPL.data)
##' class(AAPL.data)   # "xts" "zoo"
##' as.zoo.df(AAPL.data)
##' @author Chandler Lutz \email{cl.eco@@cbs.dk}
as.zoo.df <- function(data.zoo) {
    df <- data.frame(time=time(data.zoo),data.frame(data.zoo))
    row.names(df) <- NULL
    return(df)
}
