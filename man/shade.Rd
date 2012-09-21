\name{shade}
\alias{shade}
\title{Shade a time series plotted via ggts or ggts_facet}
\usage{
  shade(dates, color, xrng = NULL, yrng = c(-Inf, Inf))
}
\arguments{
  \item{dates}{the dataframe containing the dates to be
  shaded. The first column is the start; the second column
  is the end}

  \item{color}{the color for the shading}

  \item{xrng}{a vector of length two for the range for the
  x-axis. This needs to be set if the data begins in the
  middle of the shading region. The first element of xrng
  should be the start of the data; the second element
  should be the end of the data}

  \item{yrng}{The range for the y-axis. Default is -Inf to
  Inf}
}
\value{
  ggplot2 object for shaded vertical bars
}
\description{
  Shade a time series plotted via ggts or ggts_facet
}
\examples{
data(djia.data)
#The following are equivalent
ggts(djia.data,bear=TRUE)
#Note that djia.data is a dataframe
ggts(djia.data) + shade(bear_dates,color="#003F87",xrng=range(djia.data$time))
ggts(djia.data) + shade(bear_dates,color="red",xrng=range(djia.data$time))
#Using ggts_facet and data from the AER package
data("USMacroSW")
ggts_facet(USMacroSW,recession=TRUE)
}
\author{
  Chandler Lutz \email{cl.eco@cbs.dk}
}

