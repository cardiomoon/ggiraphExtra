#' Draw violin plots of a data.frame
#'
#'@param data a data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param rescale if true, rescale the data.frame
#'@param horizontal if true, horizontal boxplots will be made
#'@param alpha An integer. Default value is 0.1.
#'@param addBoxplot Whether add boxplots on the plot
#'@param addMean Whether add mean point on the plot
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
ggViolin=function(data,mapping=NULL,rescale=FALSE,horizontal=FALSE,alpha=0.1,addBoxplot=TRUE,addMean=TRUE,...){

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
        geom_violin(alpha=alpha,...)+theme_bw()


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

    p
}
