##' Standardize a vector to have mean zero and unit variance
##'
##' @param x  a vector to be standardized
##' @return a standardized vector
##' @export
##' @author Chandler Lutz \email{cl.eco@@cbs.dk}
std <- function(x) {
    y <- (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
    return(y)
}
