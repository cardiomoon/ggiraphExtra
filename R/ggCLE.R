#' Draw a cleveland dot plot
#'@param data a data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param reorderByX If true, the data is reordered by x variable
#'@param no Number of data be drawn in plot
#'@param start start point of x axis as ratio to minimum x variable
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@param decreasing Should the sort order be increasing or decreasing?
#'@param use.label Logical. Whether or not use column label in case of labelled data
#'@param use.labels Logical. Whether or not use value labels in case of labelled data
#'@param ... other arguments passed on to geom_point_interactive
#'@importFrom ggplot2 theme theme_bw facet_grid
#'@importFrom ggiraph geom_segment_interactive geom_point_interactive
#'@export
#'@examples
#' require(ggplot2)
#' require(ggiraph)
#' ggCLE(data=mtcars,aes(x=mpg),decreasing=FALSE,interactive=TRUE)
#' ggCLE(data=mtcars,aes(x=mpg,color=am,facet=am),interactive=TRUE)
#' if(requireNamespace("gcookbook",quietly=TRUE)){
#'    require(gcookbook)
#'    ggCLE(data=tophitters2001,aes(x=avg,y=name,color=lg,facet=lg),no=30,interactive=TRUE)
#' }
ggCLE=function(data,mapping,
               reorderByX=TRUE,no=NULL,start=0.99,interactive=FALSE,decreasing=TRUE,
               use.label=TRUE,use.labels=TRUE,...){


    # (xvar=paste(mapping[["x"]]))
    # yvar=NULL
    # if("y" %in% names(mapping)) yvar=paste(mapping[["y"]])
    # facetvar=NULL
    # if("facet" %in% names(mapping)) facetvar<-paste(mapping[["facet"]])
    # colorvar=NULL
    # if("colour" %in% names(mapping)) colorvar<-paste(mapping[["colour"]])
        # reorderByX=TRUE;no=30;start=0.99;interactive=FALSE;decreasing=TRUE
        # use.label=TRUE;use.labels=TRUE
    xvar<-yvar<-colourvar<-facetvar<-NULL
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

    if(!is.null(no)) data<-data[order(data[[xvar]],decreasing=decreasing)[1:no],]
    data$id=rownames(data)
    if(is.null(yvar)) yvar="id"
    if(reorderByX) {
            nameorder=data[[yvar]][order(data[[xvar]],decreasing=(!decreasing))]
            data$yvar1=factor(data[[yvar]],levels=nameorder)
            #str(data)
            # data$yvar1=reorder(data[[yvar]],data[[xvar]])
            # if(decreasing==FALSE) data$yvar1=factor(data$yvar1,levels=rev(data$yvar1))
    } else {
            data$yvar1=data[[yvar]]
    }

    data$tooltip=paste0(data$id,"<br>",yvar,"=",data[[yvar]],"<br>",xvar,"=",data[[xvar]])
    xend<-min(data[[xvar]])*start
    xend<-as.character(xend)

    data$tooltip=gsub("'","&#39",data$tooltip,fixed=TRUE)

    data
    if(!is.null(colourvar)){
    if(is.numeric(data[[colourvar]])){
            data[[colourvar]]=factor(data[[colourvar]])
    }
    }
    p<-ggplot(data,aes_string(x=xvar,y="yvar1",colour=colourvar,
                              data_id="id",tooltip="tooltip"))

    yvar
    p<-p+geom_segment_interactive(aes_string(xend=xend,yend="yvar1"))
    p<-p+  geom_point_interactive(size=3,...)+
            #geom_point_interactive(size=3)+
            ylab("")+
            theme_bw()+theme(legend.position="none")

    p
    if(!is.null(facetvar)) {
            myformula=as.formula(paste0(facetvar,"~."))
            p<-p+facet_grid(myformula,scales="free_y",space="free_y")
    }
    if(use.labels) {
            if(!is.null(xlabels)) p<-p+scale_x_continuous(breaks=1:length(xlabels),labels=xlabels)
            if(!is.null(ylabels)) {
                    if(is.numeric(data[[yvar]])) p<-p+scale_y_continuous(breaks=1:length(ylabels),labels=ylabels)
                    else p<-p+scale_y_discrete(labels=ylabels)
            }
            if(!is.null(filllabels)) p=p+scale_fill_discrete(labels=filllabels)
            if(!is.null(colourlabels)) p=p+scale_color_discrete(labels=colourlabels)
            #p+scale_color_continuous(labels=colourlabels)
    }
    if(use.label){
            if(!is.null(xlab)) p<-p+labs(x=xlab)
            if(!is.null(ylab)) p<-p+labs(y=ylab)
            if(!is.null(colourlab)) p<-p+labs(colour=colourlab)
            if(!is.null(filllab)) p<-p+labs(fill=filllab)
    }
    p



    if(interactive){
            tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
            #hover_css="fill-opacity=.3;cursor:pointer;stroke:gold;"
            hover_css="r:4px;cursor:pointer;stroke-width:6px;"
            p<-ggiraph(code=print(p),tooltip_extra_css=tooltip_css,tooltip_opacity=.75,
                       zoom_max=10,hover_css=hover_css)
    }
    p

}

