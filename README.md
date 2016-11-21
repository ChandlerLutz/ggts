The ggts package allows for easy time series visualization of
dataframes, ts, zoo, and xts objects via ggplot2.  The main functions,
`ggts`, `ggts_facet`, and `geom_cycle` allow for easy variable
selection, different linetypes and colors, standardizing or indexing,
and shading of cycles such as rececessions or bear markets.

See below and `?ggts` `?ggts_facet`, and `?geom_cycle` for more
examples.

# Main Functions

* `ggts` to plot one or more time series on a single graph

* `ggts_facet` to plot multiple time series using faceting

* `geom_cycle` to add customized (e.g. NBER recession) shading to a ggplot2 plot


# Installation and Usage:

	     #install.packages("devtools")
	     library(devtools)
	     install_github("ChandlerLutz/ggts")
	     library(ggts)

	     data(AirPassengers) ##A ts object
	     ggts(djia.data,bear=TRUE)
	     ggts(AirPassengers)

		 library(AER)
		 data(USMacroSW)  ##a mts object
		 ggts(USMacroSW, ffrate, tbill, tbond)
		 ##Index values for CPI and Japanese GDP from SW datasets
		 ggts(USMacroSW, cpi, gdpjp, index = TRUE)
		 ##Standardized unemployment rate and Pound/Dollar exchange rate
		 ggts(USMacroSW, cpi, gdpjp, standardize = TRUE)

		 ##Can also easily plot a faceted graph
		 ##This is handy for plotting several time series
		 ggts_facet(USMacroSW)

		 library(xts)
		 data(AAPL.data) ##xts object
		 ##Plot everything but volume using dplyr matches helper function.
         ##Note that all dplyr select helper functions can be used, but
		 ##either dplyr must be attached or the package explicitly
		 referenced
		 ##(below example)
		 ggts(AAPL.data, -dplyr::matches("Volume"))

		 ##If we have a randomly placed time column in a dataframe
		 ##ggts will find it
		 USMacroSW2 <- as_ts_df(USMacroSW)[, c("ffrate",
		                                        "tbill",
												"time",
												"tbond")]
	     names(USMacroSW2)[3] <- "date"
		 ggts(USMacroSW2)

	     ##geom_cycle() will automatically plot NBER recessions
		 ##This is the default cycle
		 ggts_facet(USMacroSW) + geom_cycle()
		 ##Can also plot bear market dates
		 data(bear_dates)
		 ggts_facet(USMacroSW) + geom_cycle(dates = bear_dates)
	     ##Or both recessions and bear markets
		 ggts_facet(USMacroSW) + geom_cycle() +
		     geom_cycle(dates=bear_dates, fill="grey50")


	     #Using the quantmod package
	     library(quantmod)
		 data(bear_dates)
	     getSymbols("^VIX")
	     class(VIX)  # "xts" "zoo"
	     ggts(VIX, VIX.Close) + geom_cycle(dates = bear_dates)


# A comparison to Hadley Whickam's ggplot2 book  (first ed, p. 164-166):

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
	     qplot(date, value, data = emp2, geom = "line",colour =
	     	     variable,
				 linetype = variable)
	     #faceted plot
	     qplot(date, value, data = emp, geom = "line") +
		    facet_grid(variable ~ ., scales = "free_y")

	     ## Using the ggts package ##
	     require(ggts)
	     #Regular Plot
	     ggts(economics, uempmed, unemploy)
	     #Faceted plot
	     ggts_facet(economics)
		 #Add nber recessions cycles
		 ggts_facet(economics) + geom_cycle()



Chandler Lutz
http://chandlerlutz.com/
