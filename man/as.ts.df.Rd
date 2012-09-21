\name{as.ts.df}
\alias{as.ts.df}
\title{Converts a ts or mts object to a dataframe}
\usage{
  as.ts.df(ts_obj, date_col = TRUE)
}
\arguments{
  \item{ts_obj}{the ts or mts object to be converted into a
  dataframe}

  \item{date_col}{Flag variable.  If TRUE< then the time
  variable in the new dataframe will be a date object}
}
\value{
  dataframe
}
\description{
  The first column of the new dataframe holds the time/date
  in in a column labeled "time"
}
\details{
  Note: If the data are quarterly, the first quarter will
  be translated to "YYYY-01-01" and so on
}
\examples{
#USMacrsoSW form the AER package is an "mts" object
data("USMacroSW")
as.ts.df(USMacroSW)
#Using the AirPassengers data
data(AirPassengers)
as.ts.df(AirPassengers)
}
\author{
  Chandler Lutz \email{cl.eco@cbs.dk}
}
\seealso{
  \code{\link{as.zoo.df}}
}

