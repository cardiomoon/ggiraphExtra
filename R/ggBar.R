#'Draw an interactive barplot
#'
#'@param data A data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param stat The statistical transformation to use on the data for this layer, as a string
#'            c("count","identity")
#'@param position Position adjustment. One of the c("fill","stack","dodge")
#'@param palette A character string indicating the color palette
#'@param horizontal A logical value. If TRUE,a horizontal bar plot will be returned
#'@param yangle An integer. The value will be used adjust the angle of axis.text.y
#'@param xangle An integer. The value will be used adjust the angle of axis.text.x
#'@param maxylev integer indicating threshold of unique value to be treated as a categorical variable
#'@param addlabel A logical value. If TRUE, label will be added to the plot
#'@param labelsize label size
#'@param polar A logical value. If TRUE, coord_polar() function will be added
#'@param reverse If true, reverse palette colors
#'@param use.label Logical. Whether or not use column label in case of labelled data
#'@param use.labels Logical. Whether or not use value labels in case of labelled data
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@param ... other arguments passed on to geom_bar_interactive.
#'@importFrom ggplot2 position_dodge position_stack position_fill
#'@importFrom plyr ddply mutate
#'@export
#'@return An interactive barplot
#'
#'@examples
#'require(moonBook)
#'require(ggplot2)
#'require(ggiraph)
#'require(plyr)
#'ggBar(acs,aes(x=Dx,fill=smoking),interactive=TRUE,width=1,colour="white",size=0.2,polar=TRUE)
#'ggBar(acs,aes(x=Dx,fill=smoking),position="fill",addlabel=TRUE,horizontal=TRUE,width=0.5)
#'ggBar(acs,aes(x=Dx,fill=smoking),position="dodge",interactive=TRUE,addlabel=TRUE)
#'ggBar(acs,aes(x=Dx,fill=smoking),position="fill",addlabel=TRUE)
#'ggBar(rose,aes(x=Month,fill=group,y=value),stat="identity",polar=TRUE,palette="Reds",width=1,
#'       color="black",size=0.1,reverse=TRUE,interactive=TRUE)
ggBar=function(data,mapping,
               stat="count",position="stack",palette=NULL,
               horizontal=FALSE,yangle=0,xangle=0,maxylev=6,
               addlabel=FALSE,labelsize=5,polar=FALSE,reverse=FALSE,
               use.label=TRUE,use.labels=TRUE,
               interactive=FALSE,...){
        # data=acs;mapping=aes(x=Dx,fill=smoking)
        # palette=NULL;position="stack";stat="count"
        # horizontal=FALSE;yangle=0;xangle=0;maxylev=6;use.label=TRUE;use.labels=TRUE
        # addlabel=TRUE;polar=FALSE;interactive=FALSE;reverse=FALSE

        xvar <- fillvar <- facetvar <- yvar <- NULL
        if ("x" %in% names(mapping))
                xvar <- getMapping(mapping,"x")
        if ("y" %in% names(mapping))
                yvar <- getMapping(mapping,"y")
        if ("fill" %in% names(mapping))
                (fillvar <- getMapping(mapping,"fill"))

        (xlab=attr(data[[xvar]],"label"))
        if(is.null(xlab)) xlab=xvar
        if(!use.label) xlab=xvar
        #(xvars=get_labels(data[[xvar]]))
        (filllab=attr(data[[fillvar]],"label"))
        if(is.null(filllab)) filllab=fillvar
        if(!use.label) filllab=fillvar
        #(fillvars=get_labels(data[[fillvar]]))
        if(use.labels) data=addLabelDf(data,mapping)

        if(is.numeric(data[[xvar]]) &(length(unique(data[[xvar]]))<=maxylev))
                data[[xvar]]=factor(data[[xvar]])
        if(is.numeric(data[[fillvar]]) &(length(unique(data[[fillvar]]))<=maxylev))
                data[[fillvar]]=factor(data[[fillvar]])
        if(stat=="count") {
                if(fillvar==xvar) {
                        res1=table(data[[fillvar]])
                        res1
                        res3=as.data.frame(res1)
                        res3
                        colnames(res3)[1]=fillvar
                        colnames(res3)[2]="n"
                        res3$ratio=1
                        res=res3
                        yvar="n"
                        res
                } else {
                        res1=table(data[[fillvar]],data[[xvar]])
                        res3=reshape2::melt(res1)
                        colnames(res3)[3]="n"
                        res2=apply(res1,2,function(x) x/sum(x,na.rm=TRUE))
                        res=reshape2::melt(res2)
                        colnames(res)=c(fillvar,xvar,"ratio")
                        res=cbind(res,n=res3$n)
                        yvar="n"
                        res
                }


        } else {
                res=eval(parse(text=paste0("ddply(data,'",xvar,"',mutate,ratio=",yvar,"/sum(",yvar,"))")))
                res
        }
        res[["percent"]]=scales::percent(res[["ratio"]])
        res$tooltip=paste0(xvar,"=",res[[xvar]],"\n",fillvar,"=",res[[fillvar]],
                           "\n","n=",res[[yvar]],"\n",res[["percent"]])

        #str(res)
        if(is.factor(data[[xvar]])) res[[xvar]]=factor(res[[xvar]],levels=levels(data[[xvar]]))

        if(is.numeric(res[[xvar]]) &(length(unique(res[[xvar]]))<=maxylev))
                res[[xvar]]=factor(res[[xvar]])
        if(is.numeric(res[[fillvar]]) &(length(unique(res[[fillvar]]))<=maxylev))
                res[[fillvar]]=factor(res[[fillvar]])

        p<-ggplot(res,aes_string(x=xvar,fill=fillvar,y=yvar,tooltip="tooltip"))+
                geom_bar_interactive(stat="identity",position=position,...)
        # p<-ggplot(res,aes_string(x=xvar,fill=fillvar,y=yvar,tooltip="tooltip"))+
        #          geom_bar_interactive(stat="identity",position=position)



        if(addlabel) {
                if(position=="stack")
                        p=p+geom_text(aes_string(label=yvar),
                                      position=position_stack(vjust=0.5),size=labelsize)
                else if(position=="dodge"){
                        if(horizontal){
                                p=p+geom_text(aes_string(label=yvar),
                                              position=position_dodge(0.9),hjust=-0.5,size=labelsize)
                        } else{
                                p=p+geom_text(aes_string(label=yvar),
                                              position=position_dodge(0.9),vjust=-0.5,size=labelsize)
                        }
                } else{
                        p=p+geom_text(data=res,aes_string(label="percent",y="ratio"),
                                      position=position_fill(vjust=0.5),size=labelsize)

                }
        }
        if(fillvar==xvar) p<-p+guides(fill=FALSE)

        p<-p+labs(x=xlab,fill=filllab,y="count")
        if(yangle!=0) p<-p+theme(axis.text.y=element_text(angle=yangle,hjust = 0.5))
        if(xangle!=0) p<-p+theme(axis.text.x=element_text(angle=xangle,vjust = 0.5))
        if(polar==TRUE) p<-p+ coord_polar()
        if(horizontal==TRUE) p<-p+ coord_flip()
        direction=ifelse(reverse,-1,1)
        if(!is.null(palette)) p<-p+scale_fill_brewer(palette=palette,direction=direction)

        if(interactive){

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

