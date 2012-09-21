\name{ggts}
\alias{ggts}
\title{Plot time series using ggts}
\usage{
  ggts(data, variables = NULL, all = FALSE,
    standardize = FALSE, dates = TRUE, names = NULL,
    bear = FALSE, recession = FALSE, color = FALSE,
    palette = NULL, theme = theme_bw(), filename = NULL,
    dpi = 600)
}
\arguments{
  \item{data}{the data to be plotted. Either a dataframe
  with first column labeled time (a date object), a ts or
  mts object, zoo object, or xts object. Intraday data is
  not currently supported}

  \item{variables}{a character of the variables to be
  graphed. Will be void if 'all' is set to TRUE. If set to
  NULL, the second element of the dataframe will be
  graphed.}

  \item{all}{flag variable. If set to TRUE, all variables
  will be graphed}

  \item{standardize}{if set to TRUE, all variables will be
  standardized to have zero mean and unit variance}

  \item{dates}{set to TRUE if the objects has dates.
  Default is TRUE}

  \item{names}{A character vector with the names that will
  be written in the legend}

  \item{bear}{set to TRUE if bear markets are to be shaded}

  \item{recession}{set to TRUE if recessions are to be
  shaded}

  \item{color}{Set to TRUE if the lines are to be of
  different colors}

  \item{palette}{A character vector xfor a color palette of
  the different lines. color must be set TRUE for palette
  to be relevent}

  \item{theme}{the theme for the graph.  Default is
  theme_bw()}

  \item{filename}{the location to save the graph}

  \item{dpi}{the dpi to use for raster graphics. 600 for
  publication quality graphics.}
}
\value{
  a ggplot2 object
}
\description{
  Plot time series using ggts
}
\examples{
#plot the Dow Jones shading bear markets. DJIA is a dataframe
data(djia.data)
ggts(djia.data,bear=TRUE)
#plot the Dow Jones shading bear markets and recessions
#when both bear markets and recessions are shaded, the
#bear markets will be blue and the recessions will be grey
ggts(djia.data,bear=TRUE,recession=TRUE)
#USMacroSW from the AER package is a "mts" object
data("USMacroSW")
ggts(USMacroSW,variables=c("ffrate","tbill","tbond"),color=TRUE,recession=TRUE,names=c("Fed Funds","T-Bill","T-Bond"))
}
\author{
  Chandler Lutz \email{cl.eco@cbs.dk}
}
\seealso{
  \code{\link{ggts_facet}}
}

