#' Draw violin plots of a data.frame
#'
#'@param data a data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param rescale if true, rescale the data.frame
#'@param horizontal if true, horizontal boxplots will be made
#'@param alpha An integer. Default value is 0.1.
#'@param addBoxplot Whether add boxplots on the plot
#'@param addMean Whether add mean point on the plot
#'@param use.label Logical. Whether or not use column label in case of labelled data
#'@param use.labels Logical. Whether or not use value labels in case of labelled data
#'@param ... other arguments passed on to geom_boxplot_interactive
#'@importFrom ggplot2 coord_flip element_blank stat_summary geom_violin
#'@importFrom ggiraph geom_boxplot_interactive
#'@export
#'@examples
#'require(ggplot2)
#'require(ggiraph)
#'require(reshape2)
#'ggViolin(iris)
#'ggViolin(iris,aes(fill=Species),rescale=TRUE)
#'ggViolin(mtcars,aes(x=c(mpg,cyl,disp,hp,drat),color=am),rescale=TRUE)
#'ggViolin(mtcars,aes(x=c(mpg,cyl,disp,hp,drat)),rescale=TRUE)
ggViolin=function(data,mapping=NULL,rescale=FALSE,horizontal=FALSE,alpha=0.1,addBoxplot=TRUE,addMean=TRUE,
                  use.label=TRUE,use.labels=TRUE,...){

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

        if(is.null(getMapping(mapping,"x"))) {
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
        #geom_boxplot()
        geom_violin(alpha=alpha,...)


    if(is.null(groupvar)){
            if(addBoxplot) p<-p+ geom_boxplot(fill="darkred",width=0.1,alpha=0.3)
            if(addMean) p<-p+ stat_summary(fill="white",geom='point',fun.y=mean,shape=23,size=3)
        p<-p+theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              legend.position="none")
    } else{
            if(addBoxplot) p<-p+ geom_boxplot(fill="darkred",width=0.1,alpha=0.3,position=position_dodge(0.9))
            if(addMean) p<-p+ stat_summary(fill="white",geom='point',fun.y=mean,shape=23,size=3,position=position_dodge(0.9))
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
}
