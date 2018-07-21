#' Draw boxplots of a data.frame
#'
#'@param data a data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param rescale if true, rescale the data.frame
#'@param horizontal if true, horizontal boxplots will be made
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@param addMean Whether add mean point on the plot
#'@param position An integer. Uses as argument of position_dodge()
#'@param use.label Logical. Whether or not use column label in case of labelled data
#'@param use.labels Logical. Whether or not use value labels in case of labelled data
#'@param ... other arguments passed on to geom_boxplot_interactive
#'@importFrom ggplot2 coord_flip element_blank
#'@importFrom ggiraph geom_boxplot_interactive
#'@export
#'@examples
#'require(ggplot2)
#'require(ggiraph)
#'require(reshape2)
#'ggBoxplot(mtcars,rescale=TRUE)
#'ggBoxplot(mtcars,aes(x=c(mpg,cyl,disp,hp,drat),color=am),rescale=TRUE)
#'ggBoxplot(mtcars,aes(x=c(mpg,cyl,disp,hp,drat)),rescale=TRUE)
#'ggBoxplot(mtcars,rescale=TRUE,interactive=TRUE)
#'ggBoxplot(mtcars,horizontal=TRUE,interactive=TRUE)
ggBoxplot=function(data,mapping=NULL,rescale=FALSE,horizontal=FALSE,interactive=FALSE,addMean=TRUE,position=0.9,
                   use.label=TRUE,use.labels=TRUE,...){
#    data=acs;rescale=FALSE;horizontal=FALSE;interactive=FALSE

        # data=mtcars;mapping=NULL;rescale=FALSE;horizontal=FALSE;polar=FALSE;interactive=FALSE;
        # addMean=TRUE;position=0.9;use.label=TRUE;use.labels=TRUE
        data=as.data.frame(data)
        (groupname=setdiff(names(mapping),c("x","y")))
        if(length(groupname)==0) {
                groupvar<-NULL
        } else {
                (groupvar=getMapping(mapping,groupname))
        }
        if(length(groupvar)>1) warning("Only one grouping variable is allowed")

        if(!is.null(mapping)){
                name=names(mapping)
                xvar<-xlabels<-ylabels<-filllabels<-colourlabels<-xlab<-ylab<-filllab<-colourlab<-NULL
                for(i in 1:length(name)){
                        (varname=paste0(name[i],"var"))
                        (labname=paste0(name[i],"lab"))
                        (labelsname=paste0(name[i],"labels"))
                        temp=getMapping(mapping,name[i])
                        # if(length(temp)>1) temp=temp[-1]
                        assign(varname,temp)
                        x=eval(parse(text=paste0("data$",eval(parse(text=varname)))))
                        assign(labname,attr(x,"label"))
                        assign(labelsname,get_labels(x))
                }
                if(is.null(xvar)) xvar=setdiff(colnames(data),groupvar)
        } else{
                xvar=colnames(data)
                xlabels<-colourlabels<-colourlab<-NULL
        }

        data=num2factorDf(data,groupvar)

        (select=sapply(data,is.numeric))

        if(length(paste0(mapping[["x"]]))==0) {
                xvars=colnames(data)[select]
        } else {
                xvars=getMapping(mapping,"x")
                # if(length(xvars)>1) xvars<-xvars[-1]
                if(length(xvars)<3) warning("At least three variables are required")
        }

        (xvars=setdiff(xvars,groupvar))

    df=data[c(xvars)]
    if(rescale) df<-data.frame(apply(df,2,scale))
    if(!is.null(groupvar)) df[[groupvar]]=data[[groupvar]]
    df$id=1:nrow(df)

    longdf=reshape2::melt(df,id=c("id",groupvar))
    #str(longdf)
    if(is.null(groupvar)) colorvar="variable"
    else colorvar=groupvar
    p<-ggplot(longdf,aes_string(y="value",x="variable",color=colorvar,fill=colorvar,
                                data_id="id",tooltip="variable"))+

        geom_boxplot_interactive(position=position_dodge(position),alpha=0.1,...)
        #geom_boxplot_interactive(position=position_dodge(position),alpha=0.1)
    if(is.null(groupvar)){
            if(addMean) p<-p+ stat_summary(fill="white",geom='point',fun.y=mean,shape=23,size=3)
        p<-p+theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.position="none")
    } else{
            if(addMean) p<-p+ stat_summary(fill="white",geom='point',fun.y=mean,shape=23,size=3,position=position_dodge(position))
            p<-p+theme(axis.title.x = element_blank(),
                       axis.title.y = element_blank(),
                       legend.position="top")
    }
    if(horizontal) p<-p+coord_flip()
    if(use.labels) {
            if(!is.null(xlabels)) p<-p+scale_y_continuous(breaks=1:length(xlabels),labels=xlabels)
            if(!is.null(colourlabels)) {
                    p<-p+scale_color_discrete(labels=colourlabels)+
                            scale_fill_discrete(labels=colourlabels)
            }
    }
    if(use.label){
            labels=c()
            for(i in 1:length(xvar)){
                    labels=c(labels,get_label(data[[xvar[i]]]))
            }
            if(!is.null(labels)) p<-p+scale_x_discrete(labels=labels)
            if(!is.null(colourlab)) p<-p+labs(colour=colourlab,fill=colourlab)
    }
    p

    if(interactive){
        tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
        #hover_css="fill-opacity=.3;cursor:pointer;stroke:gold;"
        hover_css="r:4px;cursor:pointer;stroke-width:6px;"
        selected_css = "fill:#FF3333;stroke:black;"
        p<-ggiraph(code=print(p),tooltip_extra_css=tooltip_css,tooltip_opacity=.75,
                   zoom_max=10,hover_css=hover_css,selected_css=selected_css)
    }
    p
}
