#' Make an interactive Heatmap
#'
#'@param data A data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param stat The statistical transformation to use on the data for this layer, as a string
#'            c("count","identity")
#'@param palette A palette name used for discrete fill var, Default value is "Blues"
#'@param reverse If true, reverse palette colors
#'@param addlabel A logical value. If TRUE, label will be added to the plot
#'@param polar A logical value. If TRUE, coord_polar() function will be added
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@param yangle A integer. The value will be used adjust the angle of axis.text.y
#'@param color  Color argument passed on to geom_rect_interactive.
#'@param size Size argument passed on to geom_rect_interactive.
#'@param ... other arguments passed on to geom_rect_interactive.
#'@export
#'@return An interactive barplot
#'
#'@examples
#'require(moonBook)
#'require(ggplot2)
#'require(ggiraph)
#'ggHeatmap(acs,aes(x=Dx,y=smoking),addlabel=TRUE)
#'ggHeatmap(rose,aes(x=Month,y=group,fill=value),stat="identity")
#'ggHeatmap(taco,aes(x=AgeGroup,y=Filling,fill=Rating,facet=ShellType),color="grey50",stat="identity")
ggHeatmap=function(data,mapping,
                   #xvar,yvar,fillvar=NULL,facetvar=NULL,
                   stat="count",palette="Blues",reverse=FALSE,
                   addlabel=FALSE,polar=FALSE,interactive=FALSE,yangle=0,color="grey50",size=0.1,...){

        xvar<-fillvar<-facetvar<-yvar<-NULL
        if("x" %in% names(mapping)) xvar<-paste(mapping[["x"]])
        if("y" %in% names(mapping)) yvar<-paste(mapping[["y"]])
        if("fill" %in% names(mapping)) fillvar<-paste(mapping[["fill"]])
        if("facet" %in% names(mapping)) facetvar<-paste(mapping[["facet"]])


    if(stat=="count") {
        df=plyr::ddply(data,c(xvar,yvar,facetvar),"nrow")
        fillvar="nrow"
    } else {
        df=data[c(xvar,yvar,fillvar,facetvar)]
    }

    width=1
    df$xno=as.numeric(factor(df[[1]]))
    df$yno=as.numeric(factor(df[[2]]))

    df$xmin=df$xno-width/2
    df$xmax=df$xno+width/2
    df$ymin=df$yno-width/2
    df$ymax=df$yno+width/2
    df$tooltip=paste0(df[[xvar]],"<br>",df[[yvar]],"<br>",df[[fillvar]])
    df$data_id=as.character(1:nrow(df))
    #print(str(df))
    # write.csv(df,"df.csv",row.names=FALSE)

    # df=read.csv("df.csv",stringsAsFactors = FALSE)
    # head(df)
    # gradient_colors=c("white","steelblue");fillvar="value";facetvar=NULL
    # addlabel=FALSE;polar=FALSE;interactive=FALSE;yangle=0;color="black";size=0.1

    xlabels=levels(factor(df[[1]]))
    ylabels=levels(factor(df[[2]]))

    xtotal=length(xlabels)
    x=1:xtotal
    ytotal=length(ylabels)
    y=1:ytotal


    mycolors=palette2colors(palette,reverse=reverse)
    p<-ggplot(df,aes_string(xmin="xmin",xmax="xmax",ymin="ymin",ymax="ymax",
                            data_id="data_id",tooltip="tooltip"))+
        geom_rect_interactive(aes_string(fill=fillvar),color=color,size=size,...)+
        #geom_rect_interactive(aes_string(fill=fillvar),color="black",size=0.1);p
        xlab(xvar)+ylab(yvar)
    p<-p+scale_x_continuous(breaks=x,labels=xlabels,limits = c(0.5,xtotal+0.5))
    p<-p+scale_y_continuous(breaks=y,labels=ylabels,limits = c(0.5,ytotal+0.5))
    if(yangle!=0) p<-p+theme(axis.text.y=element_text(angle=90,hjust = 0.5))
    p<- p+scale_fill_gradientn(colours=mycolors)

    if(addlabel)
        p<-p+geom_text(aes_string(x="xno",y="yno",label=fillvar))+guides(fill=FALSE)
    if(polar) p<-p+coord_polar()
    if(!is.null(facetvar)) p<-p+facet_wrap(facetvar)
    p<-p+theme_bw()
    if(interactive) p<-ggiraph(code=print(p),zoom_max = 10)
    p
}

