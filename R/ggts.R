##' Plot time series using ggts
##'
##' @param data the data to be plotted. Either a dataframe with first column labeled time (a date object), a ts or mts object, zoo object, or xts object. Intraday data is not currently supported
##' @param variables a character of the variables to be graphed. Will be void if 'all' is set to TRUE. If set to NULL, the second element of the dataframe will be graphed.
##' @param all flag variable. If set to TRUE, all variables will be graphed
##' @param standardize if set to TRUE, all variables will be standardized to have zero mean and unit variance
##' @param dates set to TRUE if the objects has dates. Default is TRUE
##' @param names A character vector with the names that will be written in the legend
##' @param bear set to TRUE if bear markets are to be shaded
##' @param recession set to TRUE if recessions are to be shaded
##' @param color Set to TRUE if the lines are to be of different colors
##' @param palette A character vector xfor a color palette of the different lines. color must be set TRUE for palette to be relevent
##' @param theme the theme for the graph.  Default is theme_bw()
##' @param filename the location to save the graph
##' @param dpi the dpi to use for raster graphics. 600 for publication quality graphics.
##' @return a ggplot2 object
##' @export
##' @seealso \code{\link{ggts_facet}}
##' @examples
##' #plot the Dow Jones shading bear markets. DJIA is a dataframe
##' data(djia.data)
##' ggts(djia.data,bear=TRUE)
##' #plot the Dow Jones shading bear markets and recessions
##' #when both bear markets and recessions are shaded, the
##' #bear markets will be blue and the recessions will be grey
##' ggts(djia.data,bear=TRUE,recession=TRUE)
##' #USMacroSW from the AER package is a "mts" object
##' data("USMacroSW")
##' ggts(USMacroSW,variables=c("ffrate","tbill","tbond"),color=TRUE,recession=TRUE,names=c("Fed Funds","T-Bill","T-Bond"))
##' @author Chandler Lutz \email{cl.eco@@cbs.dk}
ggts <- function(data,variables=NULL,all=FALSE,standardize=FALSE,
                 dates=TRUE,names=NULL,bear=FALSE,recession=FALSE,
                 color=FALSE,palette=NULL,theme=theme_bw(),
                 filename=NULL,dpi=600) {

    #load the data for the bear markets and recessions
    data(bear_dates)
    data(recession_dates)

    shade_color <- c("#003F87","grey50")

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

    #standardize if necessary
    if (standardize) {
        if (ncol(data) == 2) {
            data[,2] <- std(data[,2])
        } else {
            data[,2:ncol(data)] <- sapply(data[,2:ncol(data)],std)
        }
    }



    if (all) {
        #All variables will be graphed.
        #We have to remove the first column which is for the time
        variables <- names(data)[-1]
    } else if (is.null(variables)) {
        #If no variable is listed, grab the second element of the
        #data frame
        variables <- names(data)[2]
    }
    #The data should now be in a time series with a time column
    plot <- ggplot(data,aes(x=time))
    len <- length(variables)
    for (i in 1:len) {
        if (!is.null(names)) {
            #names is defined! Use it
            string <- paste('"',names[i],'"',sep="")
        } else {
            #names is not defined. use the variable
            #name for the linetype
            string <- paste('"',variables[[i]],'"',sep="")
        }

        if (color==TRUE) {
            plot <- plot +
                geom_line(aes_string(y=variables[[i]],
                                     colour=string,
                                     linetype=string))
            if (!is.null(palette)) {
                plot + scale_colour_manual(values=palette)
            }
        } else {
            plot <- plot +
                geom_line(aes_string(y=variables[[i]],
                                     linetype=string))
        }

    }
    plot <- plot + theme
    #Limit the x scale
    x_scale <- scale_x_date(lim=c(data$time[1],data$time[length(data$time)]))
    plot <- plot + x_scale + theme(axis.title.y=element_blank(),
                            legend.title=element_blank())
    rng <- c(min(data$time),max(data$time))
    if (bear && recession && dates) {
        plot <- plot + shade(bear_dates,shade_color[1],xrng=rng)
        plot <- plot + shade(recession_dates,shade_color[2],xrng=rng)
    } else if (bear && dates) {
        plot <- plot + shade(bear_dates,shade_color[1],xrng=rng)
    } else if (recession && dates) {
        plot <- plot + shade(recession_dates,shade_color[1],xrng=rng)
    }

    if (!is.null(filename)) {
        #save the file
        ggsave(file=filename,plot=plot,dpi=dpi,width=7.82,
               height=4.66)
    }

    return(plot)
}
