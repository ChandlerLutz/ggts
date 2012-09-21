##' Shade a time series plotted via ggts or ggts_facet
##'
##' @param dates the dataframe containing the dates to be shaded. The first column is the start; the second column is the end
##' @param color the color for the shading
##' @param xrng a vector of length two for the range for the x-axis. This needs to be set if the data begins in the middle of the shading region. The first element of xrng should be the start of the data; the second element should be the end of the data
##' @param yrng The range for the y-axis. Default is -Inf to Inf
##' @return ggplot2 object for shaded vertical bars
##' @export
##' @examples
##' data(djia.data)
##' #The following are equivalent
##' ggts(djia.data,bear=TRUE)
##' #Note that djia.data is a dataframe
##' ggts(djia.data) + shade(bear_dates,color="#003F87",xrng=range(djia.data$time))
##' ggts(djia.data) + shade(bear_dates,color="red",xrng=range(djia.data$time))
##' #Using ggts_facet and data from the AER package
##' data("USMacroSW")
##' ggts_facet(USMacroSW,recession=TRUE)
##' @author Chandler Lutz \email{cl.eco@@cbs.dk}
shade <- function(dates,color,xrng=NULL,yrng=c(-Inf,Inf)) {
    # shade <- shade(dates,color,xrng=NULL,yrng=c(-Inf,Inf))
    #
    # User-specified inputs:
    #   dates -- the dataframe containg the dates to be shaded. The
    #            first column is the start; the second column is the
    #            end
    #   color -- the color for the shading
    #   xrng -- the range for the x-axis. This needs to be set if
    #           the data begins or ends in the middle of a shading region.
    #           The first element of xrng should be the start of the data
    #           The second element of xrng should be the end of the data
    #   yrng -- The range for the y-axis. Default is -Inf to Inf
    #
    #
    # User-requested output:
    #   shade -- the shaded vertical bars


    #require the scales package
    require(scales);

    start <- dates[,1]
    end <- dates[,2]

    #If the user submits xrng, delete all bear markets outside of xrng
    if (!is.null(xrng)) {
        #elminate all extra the bear markets at the beginning of the sample
        start <- start[start > xrng[1]]
        end <- end[end > xrng[1]]
        if (length(start) != length(end)) {
            #the sample started in the middle of a bear market.
            #set the first date of the sample equal to the start date
            #of the first bear market
            start <- c(xrng[1],start)
        }
        #eliminate all extra bear markets at the end of the sample
        end <- end[end < xrng[2]]
        start <- start[start < xrng[2]]
        if (length(start) != length(end)) {
            #the sample ended in the middle of a bear market
            #set the last date of the sample equal to the end date
            #of the last bear market
            end <- c(end,xrng[2])
        }



    }

    shade_data <- data.frame(obs=1:length(start),start=start,end=end)


    shade <- geom_rect(aes(NULL,NULL,xmin=start,xmax=end),
                       ymin=yrng[1], ymax=yrng[2],data=dates,
                       fill=color,alpha=0.2)
}
