#' Make an interactive scatter and line plot
#'
#' @param data a data.frame
#' @param mapping Set of aesthetic mappings created by aes or aes_.
#' @param idcolor Logical cvalue. If TRUE, row numbers uses as a color variable
#' @param horizontal Logical cvalue. If TRUE, coord_flip() function is used to make a horizontal plot
#' @param use.label Logical. Whether or not use column label in case of labelled data
#' @param use.labels Logical. Whether or not use value labels in case of labelled data
#' @param interactive Logical cvalue. If TRUE, an interactive plot using ggiraph() function will be returned
#' @importFrom ggplot2 guides coord_flip geom_boxplot
#' @export
#' @examples
#' require(ggplot2)
#' require(ggiraph)
#' require(sjmisc)
#' ggPair(iris,interactive=TRUE,use.label=FALSE)
#' ggPair(iris[3:5],interactive=TRUE)
#' ggPair(iris,aes(color=Species),interactive=TRUE)
#' ggPair(iris,aes(color=Species),horizontal=TRUE, interactive=TRUE)
#' ggPair(iris,aes(x=c(Sepal.Length,Sepal.Width)),interactive=TRUE)
#' ggPair(iris,aes(x=c(Sepal.Length,Sepal.Width),color=Species),interactive=TRUE)
ggPair=function(data,mapping=NULL,idcolor=TRUE,horizontal=FALSE,use.label=TRUE,
                use.labels=TRUE,interactive=FALSE) {


        df=as.data.frame(data)

        (colorvar<-paste0(mapping[["colour"]]))
        if(length(colorvar)==0) colorvar<-NULL
        if(!is.null(colorvar)){
                if(is.numeric(df[[colorvar]])) df[[colorvar]]=factor(df[[colorvar]])
        }
        (xvars=paste0(mapping[["x"]]))

        (select=sapply(df,is.numeric))

        if(length(paste0(mapping[["x"]]))<3) {
                xvars=colnames(df)[select]
        } else {
                xvars=paste0(mapping[["x"]])
                if(length(xvars)>1) xvars<-xvars[-1]
                if(length(xvars)<2) warning("At least two variables are required")
        }


        df1=df[c(xvars,colorvar)]
        df1
        cols=colnames(df[xvars])
        varcount=length(xvars)

        df1[["id"]]=1:nrow(df1)
        temp=df1[["id"]]
        for(i in 1:(length(df1)-1)) {
                temp=paste0(temp,"<br>",names(df1)[i],":",df1[[i]])
        }

        df1$tooltip=temp
        #df1$tooltip=paste0(df1$id,"<br>",df1[[1]],"<br>",df[[2]])
        #str(df1)
        longdf=reshape2::melt(df1,id=c("id","tooltip",colorvar))
        if(is.null(colorvar) & idcolor) colorvar="id"


        #str(longdf)
        p<-ggplot(data=longdf,
                  aes_string(x="variable",y="value",group="id",colour=colorvar))+
                geom_point_interactive(aes_string(data_id="id",tooltip="tooltip"))+
                geom_path_interactive(aes_string(data_id="id",tooltip="tooltip"))+xlab("")+ylab("")
        p
        if(horizontal) p<-p+coord_flip()
        if(!is.null(colorvar)) {
                if(colorvar=="id") p <- p+guides(colour=FALSE)
        }
        if(varcount==2) {
                longdf1=longdf[longdf[["variable"]]==xvars[1],]
                #longdf1$toolitip=paste0(longdf1$id,"<br>",vars[1])
                p<-p+geom_boxplot(data=longdf1,
                                  aes(x=as.numeric(longdf1[["variable"]])-0.2,group=NULL),width=0.2)
                longdf2=longdf[longdf[["variable"]]==xvars[2],]
                #longdf2$toolitip=paste0(longdf2$id,"<br>",vars[2])
                p<-p+geom_boxplot(data=longdf2,
                                  aes(x=as.numeric(longdf2[["variable"]])+0.2,group=NULL),width=0.2)
        }
        if(use.label){
                labels=c()
                for(i in 1:length(xvars)){
                        temp=get_label(data[[xvars[i]]])
                        labels=c(labels,ifelse(is.null(temp),cols[i],temp))
                }
                p<-p+scale_x_discrete(labels=labels)
                colorlab=get_label(data[[colorvar]])
                if(!is.null(colorlab)) p<-p+labs(colour=colorlab)
        }
        if(use.labels){
                if(use.labels) {
                        colorlabels=get_labels(data[[colorvar]])
                        if(!is.null(colorlabels)) {
                                p<-p+scale_color_discrete(labels=colorlabels)
                        }
                }

        }
        tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
        hover_css="fill-opacity=.3;cursor:pointer;stroke:gold;"
        if(interactive) p<-ggiraph(code=print(p),tooltip_extra_css=tooltip_css,tooltip_opacity=.75,
                                   zoom_max=10,hover_css=hover_css)
        p
}

