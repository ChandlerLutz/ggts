The ggts package allows for easy time series visualization via ggplot2 using dataframes, ts, zoo, and xts objects.  The main functions, `ggts` and `ggts_facet`, allow for easy variable selection, bear market or recession shading, different colored lines, and easy saving.  Users can easily add customized shading via the `shade()` function.

Instillation and Usage:

	     #install.packages("devtools")
	     library(devtools)
	     install_github("ggts",username="ChandlerLutz")
	     library(ggts)
	     
	     data(djia.data)
	     ggts(djia.data,bear=TRUE)
	     data("USMacroSW")
	     ggts(USMacroSW,variables=c("ffrate","tbill","tbond"),color=TRUE,recession=TRUE,names=c("Fed Funds","T-Bill","T-Bond"))
	     ggts_facet(USMacroSW)
	     ggts_facet(USMacroSW,recession=TRUE)
	     

A comparison to Hadley Whickam's ggplot2 book (P. 164-166):
  	     
	     data(economics)
  	     ## From Hadley's book ##
	     require(ggplot2)
	     require(plyr)
	     require(reshape)
	     emp <- melt(economics, id = "date",measure = c("unemploy", "uempmed"))
	     
	     range01 <- function(x) {
	     	     rng <- range(x, na.rm = TRUE)
		     (x - rng[1]) / diff(rng)
	     }
	     emp2 <- ddply(emp, .(variable), transform, value = range01(value))
	     #Regular plot
	     qplot(date, value, data = emp2, geom = "line",colour = variable, linetype = variable)
	     #faceted plot
	     qplot(date, value, data = emp, geom = "line") + facet_grid(variable ~ ., scales = "free_y")
	     
	     ## Using the ggts package ##
	     require(ggts)
	     #Note: we only need a slightly modified version of the original `economics` dataset
	     names(economics)[1] <- "time"
	     #Regular Plot
	     ggts(economics,variables=c("uempmed","unemploy"),color=TRUE,standardize=TRUE)
	     #Faceted plot
	     ggts_facet(economics,variables=c("uempmed","unemploy"))
	     

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