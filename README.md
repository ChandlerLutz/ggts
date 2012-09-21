The ggts package allows for easy time series visualization via ggplot2 using dataframes, ts, zoo, and xts objects.  The main functions, `ggts` and `ggts_facet` allow for easy variable selection, bear markets or recessions to shaded, different colored lines, and easy saving.  Users can easily add their own shading via the `shade()` function

Instillation and Usages:

	     #install.packages("devtools")
	     library(devtools)
	     install_github("ggts",username="ChandlerLutz")
	     
	     data(djia.data)
	     ggts(djia.data,bear=TRUE)
	     data("USMacroSW")
	     ggts(USMacroSW,variables=c("ffrate","tbill","tbond"),color=TRUE,
	     recession=TRUE,names=c("Fed Funds","T-Bill","T-Bond"))
	     ggts_facet(USMacroSW)
	     ggts_facet(USMacroSW,recession=TRUE)
	     

# Main Functions

* `ggts` to plot one or more time series on a single graph

* `ggts_facet` to plot multiple time series using faceting

* `shade` to add customized shading to a ggplot2 time series

# Notes

* Intraday data is currently not supported

* If a dataframe is passed to `ggts` or `ggts_facet`, then the 
  first column must be a date object labeled "time"

Chandler Lutz
cl.eco@cbs.dk