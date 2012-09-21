\name{ggts_facet}
\alias{ggts_facet}
\title{Faceted time series plots}
\usage{
  ggts_facet(data, variables = NULL, bear = FALSE,
    recession = FALSE, dates = TRUE, theme = theme_bw(),
    filename = NULL, dpi = 600)
}
\arguments{
  \item{data}{the data to be plotted. Either a dataframe
  with first column labeled time (a date object), a ts or
  mts object, zoo object, or xts object. Intraday data is
  not currently supported}

  \item{variables}{a character of the variables to be
  graphed. If set to NULL, all variables will be graphed}

  \item{dates}{set to TRUE if the objects has dates.
  Default is TRUE}

  \item{bear}{set to TRUE if bear markets are to be shaded}

  \item{recession}{set to TRUE if recessions are to be
  shaded}

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
  Faceted time series plots
}
\examples{
#USMacroSW from the AER package is an "mts" object
data(USMacroSW)
ggts_facet(USMacroSW)
ggts_facet(USMacroSW,bear=TRUE)
ggts_facet(USMacroSW,recession=TRUE)
#If both bear and recession are set to TRUE, bear markets
#will be blue and recessions will be grey.
ggts_facet(USMacroSW,bear=TRUE,recession=TRUE)
}
\author{
  Chandler Lutz \email{cl.eco@cbs.dk}
}
\seealso{
  \code{\link{ggts}}
}

