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
#'@param use.label Logical. Whether or not use column label in case of labelled data
#'@param use.labels Logical. Whether or not use value labels in case of labelled data
#'@param ... other arguments passed on to geom_rect_interactive.
#'@export
#'@return An interactive barplot
#'
#'@examples
#'require(moonBook)
#'require(ggplot2)
#'require(ggiraph)
#'require(sjmisc)
#'ggHeatmap(acs,aes(x=Dx,y=smoking),addlabel=TRUE,interactive=TRUE)
#'ggHeatmap(acs,aes(x=sex,y=Dx,fill=age),addlabel=TRUE,interactive=TRUE)
#'ggHeatmap(rose,aes(x=Month,y=group,fill=value),stat="identity",addlabel=TRUE)
#'ggHeatmap(rose,aes(x=Month,y=group,fill=value),addlabel=TRUE)
#'ggHeatmap(taco,aes(x=AgeGroup,y=Filling,fill=Rating,facet=ShellType),color="grey50",stat="identity")
ggHeatmap=function(data,mapping,
                   #xvar,yvar,fillvar=NULL,facetvar=NULL,
                   stat="count",palette="Blues",reverse=FALSE,
                   addlabel=FALSE,polar=FALSE,interactive=FALSE,yangle=0,color="grey50",size=0.1,
                   use.label=TRUE,use.labels=TRUE,...){

        # data=acs;mapping=aes(x=sex,y=Dx,fill=age)
        # stat="count";palette="Blues";reverse=FALSE
        # addlabel=FALSE;polar=FALSE;interactive=FALSE;yangle=0;color="grey50";size=0.1
        # use.label=TRUE;use.labels=TRUE

        xvar<-fillvar<-facetvar<-yvar<-NULL
        name=names(mapping)
        xlabels<-ylabels<-filllabels<-colourlabels<-xlab<-ylab<-colourlab<-filllab<-NULL
        for(i in 1:length(name)){
                (varname=paste0(name[i],"var"))
                labname=paste0(name[i],"lab")
                labelsname=paste0(name[i],"labels")
                assign(varname,getMapping(mapping,name[i]))
                x=eval(parse(text=paste0("data$",eval(parse(text=varname)))))
                assign(labname,attr(x,"label"))
                assign(labelsname,get_labels(x))
        }

        mycount=length(unique(data[[xvar]]))*length(unique(data[[yvar]]))
        if(!is.null(facetvar)) mycount=mycount*length(unique(data[[facetvar]]))
        if(nrow(data)<=mycount) stat="identity"
        if(stat=="count") {
            if(is.null(fillvar)){
                df=plyr::ddply(data,c(xvar,yvar,facetvar),"nrow")
                fillvar="nrow"
            } else {
                df=summarySE(data,fillvar,c(xvar,yvar))
                df[[fillvar]]=round(df[[fillvar]],1)
            }
    } else {
        df=data[c(xvar,yvar,fillvar,facetvar)]
    }
    df
    width=1
    (df$xno=as.numeric(factor(df[[1]])))
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
        #geom_rect_interactive(aes_string(fill=fillvar),color=color,size=size,...)+
        geom_rect_interactive(aes_string(fill=fillvar),color="black",size=0.1);p
        p<-p+xlab(xvar)+ylab(yvar)
    p
    p<-p+scale_x_continuous(breaks=x,labels=xlabels,limits = c(0.5,xtotal+0.5))
    p<-p+scale_y_continuous(breaks=y,labels=ylabels,limits = c(0.5,ytotal+0.5))
    if(yangle!=0) p<-p+theme(axis.text.y=element_text(angle=90,hjust = 0.5))
    p<- p+scale_fill_gradientn(colours=mycolors)

    if(addlabel)
        p<-p+geom_text(aes_string(x="xno",y="yno",label=fillvar))
    if(polar) p<-p+coord_polar()
    if(!is.null(facetvar)) p<-p+facet_wrap(facetvar)
    #p<-p+theme_bw()
    if(use.labels) {
            if(!is.null(xlabels)) p<-p+scale_x_continuous(breaks=1:length(xlabels),labels=xlabels)
            if(!is.null(ylabels))  p<-p+scale_y_continuous(breaks=1:length(ylabels),labels=ylabels)
            #if(!is.null(filllabels)) p=p+scale_fill_continuous(breaks=1:length(filllabels),labels=filllabels)
            if(!is.null(colourlabels)) p=p+scale_color_discrete(labels=colourlabels)
            #p+scale_color_continuous(labels=colourlabels)
    }
    if(use.label){
            if(!is.null(xlab)) p<-p+labs(x=xlab)
            if(!is.null(ylab)) p<-p+labs(y=ylab)
            if(!is.null(colourlab)) p<-p+labs(colour=colourlab)
            if(!is.null(filllab)) p<-p+labs(fill=filllab)
    }

    if(interactive) {
        tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
        p<-girafe(ggobj=p)
        p<-girafe_options(p,
                          opts_tooltip(css=tooltip_css,opacity=.75),
                          opts_zoom(min=1,max=10))
    }
    p
}

