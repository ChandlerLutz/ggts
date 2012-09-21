##' Faceted time series plots
##'
##' @param data the data to be plotted. Either a dataframe with first column labeled time (a date object), a ts or mts object, zoo object, or xts object. Intraday data is not currently supported
##' @param variables a character of the variables to be graphed. If set to NULL, all variables will be graphed
##' @param dates set to TRUE if the objects has dates. Default is TRUE
##' @param bear set to TRUE if bear markets are to be shaded
##' @param recession set to TRUE if recessions are to be shaded
##' @param theme the theme for the graph.  Default is theme_bw()
##' @param filename the location to save the graph
##' @param dpi the dpi to use for raster graphics. 600 for publication quality graphics.
##' @return a ggplot2 object
##' @export
##' @seealso \code{\link{ggts}}
##' @examples
##' #USMacroSW from the AER package is an "mts" object
##' data(USMacroSW)
##' ggts_facet(USMacroSW)
##' ggts_facet(USMacroSW,bear=TRUE)
##' ggts_facet(USMacroSW,recession=TRUE)
##' #If both bear and recession are set to TRUE, bear markets
##' #will be blue and recessions will be grey.
##' ggts_facet(USMacroSW,bear=TRUE,recession=TRUE)
##' @author Chandler Lutz \email{cl.eco@@cbs.dk}
ggts_facet <- function(data,variables=NULL,bear=FALSE,recession=FALSE,
                       dates=TRUE,theme=theme_bw(),filename=NULL,
                       dpi=600) {

    #load the data for the bear markets and recessions
    data(bear_dates)
    data(recession_dates)

    shade_color <- c("#003F87","grey50")

    #Make sure data is a dataframe
    if (!is.data.frame(data)) {

        if (!(class(data)[1] %in% c("ts","mts","zoo","xts"))) {
            stop("Data needs to be a dataframe, ts, mts, zoo, or xts object")
        }

        #Not a dataframe
        #turn into a dataframe
        if (dates) {
            if (class(data) == "ts" || class(data) == "mts")
                data <- as.ts.df(data)
            else
                data <- as.zoo.df(data)
        } else {
            data <- data.frame(time=time(data),data.frame(data))
        }

    }

    #Re-name the first column -- this should hold the dates
    names(data)[1] <- "time"

    if (!is.null(variables)) {
        data <- data[,c("time",variables)]
    }

    if (ncol(data) == 2) {
        return (ggts(data))
    }

    #require ggplot2, etc.
    require(ggplot2); require(reshape2);


    vars <- c(names(data)[2:length(names(data))])
    num.vars <- length(vars)+1
    data.melt <- melt(data,id="time",measure=vars)
    plot.facet <- qplot(time,value,data=data.melt,geom="line") +
        facet_grid(variable ~., scales="free_y") + theme

    x <- c(data$time[1],data$time[length(data$time)])
    x_scale <- scale_x_date(lim=x)
    if (bear && recession) {
        plot.facet <- plot.facet + x_scale +
            shade(bear_dates,shade_color[1],xrng=x) +
            shade(recession_dates,shade_color[2],xrng=x)
    } else if (bear) {
        plot.facet <- plot.facet + x_scale +
            shade(bear_dates,color=shade_color[1],xrng=x)
    } else if (recession) {
        plot.facet <- plot.facet + x_scale +
            shade(recession_dates,color=shade_color[1],xrng=x)
    }

    if (!is.null(filename)) {
        #save the file
        ggsave(file=filename,plot=plot.facet,dpi=dpi,width=7.82,
               height=4.66)
    }

    return(plot.facet)

}
