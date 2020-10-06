#'Clean theme for PieDonut plot
#'@param base_size An integer, default 12.
#'@importFrom ggplot2 theme_grey %+replace%
#'@importFrom grid unit
#'@export
theme_clean=function(base_size=12){
        theme_grey(base_size) %+replace%
                theme(
                        axis.title=element_blank(),
                        axis.text=element_blank(),
                        panel.background=element_blank(),
                        panel.grid=element_blank(),
                        axis.ticks.length=unit(0,"cm"),
                        # axis.text=element_text(margin=margin(0,0,0,0,"pt")),
                        panel.spacing=unit(0,"lines"),
                        plot.margin=unit(c(0,0,0,0),"lines"),
                        complete=TRUE
                )
}
#'Computing breaks for make a histogram of a continuous variable
#'
#'@param x A continuous variables
#'@export
#'@return A list contains a factor and a numeric vector
#'
num2cut=function(x){
        breaks=list()
        breaks <- c(list(x = x), breaks)
        breaks <- list(x = x)
        breaks$plot <- FALSE
        breaks <- do.call("hist", breaks)$breaks
        x1 <- cut(x, breaks = breaks, include.lowest = TRUE)
        result=list(x1=x1,breaks=breaks)
        result
}


#' Add value labels to the data.frame
#'@param data A data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@importFrom sjmisc to_label
#'@export
addLabelDf=function(data,mapping=NULL){

        if(!is.null(mapping)) {
                (mapnames=names(mapping))
                cols=c()
                for(i in 1:length(mapnames)) {
                        temp=getMapping(mapping,mapnames[i])
                        # if(length(temp)>1) temp=temp[-1]
                        cols=c(cols,temp)
                }
                cols=unique(cols)
                data[cols]=lapply(data[cols],function(x) to_label(x,add.non.labelled=TRUE))
                # for(i in 1:length(cols)){
                #
                #         data[[cols[[i]]]]=to_label(data[[cols[i]]],add.non.labelled=TRUE)
                # }
        } else{
                # cols=colnames(data)
                # for(i in 1:length(cols)){
                #         data[[cols[[i]]]]=to_label(data[[cols[i]]],add.non.labelled=TRUE)
                # }
                data=lapply(data,function(x) to_label(x,add.non.labelled=TRUE))
        }
        data
}
