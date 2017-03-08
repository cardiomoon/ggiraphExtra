#' Draw boxplots of a data.frame
#'
#'@param data a data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param rescale if true, rescale the data.frame
#'@param horizontal if true, horizontal boxplots will be made
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@param addMean Whether add mean point on the plot
#'@param position An integer. Uses as argumant of position_dodge()
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
ggBoxplot=function(data,mapping=NULL,rescale=FALSE,horizontal=FALSE,interactive=FALSE,addMean=TRUE,position=0.9,...){
#    data=acs;rescale=FALSE;horizontal=FALSE;interactive=FALSE

        #data=mtcars;mapping=NULL;rescale=FALSE;horizontal=FALSE;polar=FALSE;interactive=FALSE
        data=as.data.frame(data)
        (groupname=setdiff(names(mapping),c("x","y")))
        if(length(groupname)==0) {
                groupvar<-NULL
        } else {
                (groupvar=paste(mapping[groupname]))
        }
        if(length(groupvar)>1) warning("Only one grouping variable is allowed")
        data=num2factorDf(data,groupvar)

        (select=sapply(data,is.numeric))

        if(length(paste0(mapping[["x"]]))==0) {
                xvars=colnames(data)[select]
        } else {
                xvars=paste0(mapping[["x"]])
                if(length(xvars)>1) xvars<-xvars[-1]
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
        #geom_boxplot()
        geom_boxplot_interactive(position=position_dodge(position),alpha=0.1,...)
    p<-p+theme_bw()

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
