#'Make an interactive bar plot with error bar
#'
#'@param data A data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@param digits An integer indicating the number of decimal places
#'@param mode if 2, two-sided error bar will be displayed, if 1 one-sided errorbar will be displayed
#'@param errorbar which value is displayed with errorbar :"se" or "sd"
#'@param use.label Logical. Whether or not use column label in case of labelled data
#'@param use.labels Logical. Whether or not use value labels in case of labelled data
#'@importFrom ggiraph geom_bar_interactive
#'@export
#'@return An interactive catepillar plot
#'@examples
#'require(ggplot2)
#'require(ggiraph)
#'ggErrorBar(mpg,aes(x=drv,y=cty))
#'ggErrorBar(mpg,aes(x=drv,y=hwy,color=cyl),mode=1,interactive=TRUE,errorbar="sd")
ggErrorBar=function(data,mapping,interactive=FALSE,digits=1,mode=2,errorbar="se",
                    use.label=TRUE,use.labels=TRUE){

    # data=mpg;mapping=aes(x=drv,y=cty);interactive=FALSE;digits=1;mode=2;errorbar="se"
    # use.label=TRUE;use.labels=TRUE
    df<-data

    yvar=getMapping(mapping,"y")
    xvar=getMapping(mapping,"x")
    if(is.numeric(data[[xvar]])) data[[xvar]]=factor(data[[xvar]])
    groupvar<-NULL
    (groupname=setdiff(names(mapping),c("x","y")))
    length(groupname)
    if(length(groupname)>0){
         groupvar=getMapping(mapping,groupname)
    }


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

    A=yvar
   (B<-groupvar)

    (C=xvar)



    if(is.null(B)){
        dat=summarySE(df,A,C)
        dat$tooltip=""
        dat$label=paste0(C,"=",dat[[C]],"<br>mean:",round(dat[[A]],digits),
                         "<br>se:",round(dat$se,digits),"<br>sd:",round(dat$sd,digits))
    } else {
        dat=summarySE(df,A,c(B,C))
        dat[[B]]=factor(dat[[B]])
        dat$tooltip=dat[[B]]
        dat$label=paste0(B,"=",dat[[B]],"<br>",C,"=",dat[[C]],"<br>mean:",round(dat[[A]],digits),
                         "<br>se:",round(dat$se,digits),"<br>sd:",round(dat$sd,digits))
    }

    dat$id=as.character(1:nrow(dat))
    dat
    if(is.null(B)) {
        p<-ggplot(dat,aes_string(x=xvar,fill=xvar,y=yvar,tooltip="label",data_id="id"))+guides(fill=FALSE)
    } else {
        p<-ggplot(dat,aes_string(x=xvar,fill=groupvar,y=yvar,tooltip="label",data_id="id"))
    }
    if(mode==2) p<-p+geom_bar_interactive(position="dodge",stat="identity")
    p<-p+eval(parse(text=paste0("geom_errorbar(aes(ymin=",A,"-",errorbar,",ymax=",
                                 A,"+",errorbar,"),position=position_dodge(0.9),width=0.2)")))

    if(mode!=2) p<-p+geom_bar_interactive(position="dodge",stat="identity")
    p
    if(use.labels) {
            if(!is.null(xlabels)) p<-p+scale_x_discrete(labels=xlabels)
            if(!is.null(ylabels))  p<-p+scale_y_continuous(breaks=1:length(ylabels),labels=ylabels)
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

   # if(interactive) p<-ggiraph(code=print(p),zoom_max = 10)
    if(interactive){
            tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
            hover_css="r:4px;cursor:pointer;stroke-width:6px;"
            p<-ggiraph(code=print(p),tooltip_extra_css=tooltip_css,tooltip_opacity=.75,
                       zoom_max=10,hover_css=hover_css)
    }
    p
}
