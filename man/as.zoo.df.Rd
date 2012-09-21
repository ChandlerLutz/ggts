\name{as.zoo.df}
\alias{as.zoo.df}
\title{Convert a zoo or xts object into a dataframe}
\usage{
  as.zoo.df(data.zoo)
}
\arguments{
  \item{data.zoo}{zoo or xts object}
}
\value{
  a data frame with the first column as a date object
  labeled "time"
}
\description{
  Convert a zoo or xts object into a dataframe
}
\examples{
#The apple stock data is a xts object
data(AAPL.data)
class(AAPL.data)   # "xts" "zoo"
as.zoo.df(AAPL.data)
}
\author{
  Chandler Lutz \email{cl.eco@cbs.dk}
}
\seealso{
  \code{\link{as.ts.df}}
}

