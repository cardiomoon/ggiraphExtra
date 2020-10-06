#' Rescale a vector with which minimum value 0 and maximum value 1
#' @param x A numeric vector
#' @export
myscale=function(x){
        if(is.character(x)) x=as.numeric(factor(x))
        if(is.factor(x)) x=as.numeric(x)
        x=round((x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)),2)
        x
}

#' Rescale a vector with which minimum value 0 and maximum value 1
#' @param x A numeric vector
#' @param minx The minimum value
#' @param maxx The maximum value
#' @export
myscale2=function(x,minx=0,maxx=1){
        if(is.factor(x)) {
                x=as.numeric(x)
                x=round((x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T)),2)
                x=x*(maxx-minx)+minx
        }
        x
}
#' Make an interactive scatter and line plot
#'
#' @param data a data.frame
#' @param mapping Set of aesthetic mappings created by aes or aes_.
#' @param rescale if true, rescale the data.frame
#' @param idcolor Logical. If TRUE, row numbers uses as a color variable
#' @param horizontal Logical. If TRUE, coord_flip() function is used to make a horizontal plot
#' @param use.label Logical. Whether or not use column label in case of labelled data
#' @param use.labels Logical. Whether or not use value labels in case of labelled data
#' @param includeFactor Logical. Whether or not include factor variables
#' @param includeAll Logical. Whether or not include all variables
#' @param interactive Logical. If TRUE, an interactive plot using girafe() function will be returned
#' @importFrom ggplot2 guides coord_flip geom_boxplot
#' @export
#' @examples
#' require(ggplot2)
#' require(ggiraph)
#' require(sjmisc)
#' require(moonBook)
#' ggPair(iris,rescale=TRUE,horizontal=TRUE)
#' ggPair(acs,aes(colour=smoking),horizontal=TRUE,rescale=TRUE)
#' ggPair(radial,aes(color=male),horizontal=TRUE,rescale=TRUE)
#' ggPair(mtcars,horizontal=TRUE,rescale=TRUE)
#' ggPair(iris,rescale=TRUE,horizontal=TRUE,interactive=TRUE)
#' ggPair(iris,aes(color=Species),rescale=TRUE,interactive=TRUE)
#' ggPair(iris,aes(x=c(Sepal.Length,Sepal.Width),color=Species),horizontal=TRUE,interactive=TRUE)
ggPair=function(data,mapping=NULL,rescale=FALSE,idcolor=TRUE,horizontal=FALSE,use.label=FALSE,
                use.labels=TRUE,includeFactor=TRUE,includeAll=FALSE,interactive=FALSE) {
#
#                 data=iris;mapping=aes(color=Species);
# # #             # # data=iris;mapping=NULL
#                 rescale=TRUE;idcolor=TRUE;horizontal=TRUE;use.label=TRUE;
#                use.labels=TRUE;interactive=FALSE;includeFactor=FALSE;includeAll=FALSE

        df=as.data.frame(data)
        mapping
        (colorvar<-getMapping(mapping,"colour"))
        if(length(colorvar)==0) colorvar<-NULL
        if(!is.null(colorvar)){
                if(is.numeric(df[[colorvar]])) df[[colorvar]]=factor(df[[colorvar]])
        }
        select=sapply(df,is.numeric)
        minx=min(df[select],na.rm=T)
        maxx=max(df[select],na.rm=T)

        (xvars=getMapping(mapping,"x"))

        if(is.null(xvars)) {

                if(includeAll){
                        xvars=colnames(df)
                } else {
                        xvars=union(colnames(df)[select],colorvar)
                }
        } else {
                xvars=getMapping(mapping,"x")
                # if(length(xvars)>1) xvars<-xvars[-1]
                if(length(xvars)<2) warning("At least two variables are required")
        }
        xvars
        xvarlength=length(xvars)

        if(includeFactor){
                select1=sapply(df,is.factor)
                (select=select|select1)
        }

        if(horizontal) (xvars=union(xvars,colorvar))
        #if(!is.null(colorvar)) df1[[colorvar]]=df[[colorvar]]
        df1<-df[union(xvars,colorvar)]
        (cols=colnames(df[xvars]))
        varcount=length(xvars)

        temp=rownames(df1)
        count=(length(df1)-length(colorvar)+(xvarlength>0))
        if(is.null(colorvar)&(xvarlength>0)) count=count-1
        for(i in 1:count) {
                temp=paste0(temp,"<br>",names(df1)[i],":",df1[[i]])
        }
        df
        xvars
        if(rescale) {
                df1<-data.frame(lapply(df[c(xvars,colorvar)],myscale))
        } else {

                df1<-data.frame(lapply(df[c(xvars,colorvar)],myscale2,minx=minx,maxx=maxx))
        }
        summary(df1)
        df1[["id"]]=rownames(df1)
        df1$tooltip=temp
        #df1$tooltip=paste0(df1$id,"<br>",df1[[1]],"<br>",df[[2]])
        #str(df1)
        addboxplot=TRUE

        longdf=reshape2::melt(df1,id=c("id","tooltip",colorvar))
        colorvar
        summary(longdf)
        if(!is.null(colorvar)) {
                colorlabels=sjlabelled::get_labels(data[[colorvar]])
                colorlabels
                if(!is.null(colorlabels)) longdf[[colorvar]]=factor(longdf[[colorvar]],labels=colorlabels)
        }
        if(is.null(colorvar) & idcolor) {
                colorvar="id"
                addboxplot=FALSE
        }
        summary(longdf)
        p<-ggplot(data=longdf,
                  aes_string(x="variable",y="value",group="id",colour=colorvar))+
                geom_point_interactive(aes_string(data_id="id",tooltip="tooltip"))+
                geom_path_interactive(aes_string(data_id="id",tooltip="tooltip"))+xlab("")+ylab("")

        if(horizontal) p<-p+coord_flip()
        p
        if(!is.null(colorvar)) {
                if(colorvar=="id") p <- p+guides(colour=FALSE)
        }
        if((varcount==2)){
                longdf1=longdf[longdf[["variable"]]==xvars[1],]
                #longdf1$toolitip=paste0(longdf1$id,"<br>",vars[1])
                if(addboxplot)
                p<-p+geom_boxplot(data=longdf1,
                                  aes(x=as.numeric(longdf1[["variable"]])-0.2,group=NULL),width=0.2)
                else
                        p<-p+geom_boxplot(data=longdf1,
                                          aes(x=as.numeric(longdf1[["variable"]])-0.2,colour=NULL,group=NULL),width=0.2)
                longdf2=longdf[longdf[["variable"]]==xvars[2],]
                #longdf2$toolitip=paste0(longdf2$id,"<br>",vars[2])
                if(addboxplot)
                p<-p+geom_boxplot(data=longdf2,
                                  aes(x=as.numeric(longdf2[["variable"]])+0.2,group=NULL),width=0.2)
                else
                        p<-p+geom_boxplot(data=longdf2,
                                          aes(x=as.numeric(longdf2[["variable"]])+0.2,colour=NULL,group=NULL),width=0.2)
        }
        p


                labels=c()
                xvars
                colorvar
                if(!is.null(colorvar) &(colorvar!="id")) xvars=c(setdiff(xvars,colorvar),colorvar)
                cols=colnames(df[xvars])
                for(i in 1:length(xvars)){
                        temp=NULL
                        if(use.label) temp=sjlabelled::get_label(data[[xvars[i]]])
                        labels=c(labels,ifelse(is.null(temp),cols[i],temp))
                }
                p<-p+scale_x_discrete(labels=labels)
       if(use.label){
                colorlab=sjlabelled::get_label(data[[colorvar]])
                if(!is.null(colorlab)) p<-p+labs(colour=colorlab)
        }
        # if(use.labels){
        #         if(use.labels) {
        #                 colorlabels=get_labels(data[[colorvar]])
        #                 colorlabels
        #                 if(!is.null(colorlabels)) {
        #                         # if(rescale) {
        #                         #         p<-p+scale_color_discrete(labels=colorlabels)
        #                         # } else {
        #                                 p<-p+scale_color_continuous(breaks=sort(unique(longdf[[colorvar]])),
        #                                                                     labels=colorlabels)
        #                         #}
        #                 }
        #         }
        #
        # }
        if(rescale) p<-p+scale_y_continuous(breaks=c(0,1),labels=c("Min","Max"))
        p<-p+theme_bw()
        p

        if(interactive) {
                tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
                hover_css="fill-opacity=.3;cursor:pointer;stroke:gold;"
                p<-girafe(ggobj=p)
                p<-girafe_options(p,
                                  opts_hover(css=hover_css),
                                  opts_tooltip(css=tooltip_css,opacity=.75),
                                  opts_zoom(min=1,max=10))

        }
        p
}

