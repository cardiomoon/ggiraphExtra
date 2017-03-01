#'Make an interactive bar plot with error bar
#'
#'@param data A data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@param digits An integer indicating the number of decimal places
#'@param mode if 2, two-sided error bar will be displayed, if 1 one-sided errorbar will be displayed
#'@param errorbar which value is displayed with errorbar :"se" or "sd"
#'@importFrom ggiraph geom_bar_interactive
#'@export
#'@return An interactive catepillar plot
#'@examples
#'require(ggplot2)
#'require(ggiraph)
#'ggErrorBar(mpg,aes(x=drv,y=cty),interactive=TRUE)
#'ggErrorBar(mpg,aes(x=drv,y=hwy,color=cyl),mode=1,interactive=TRUE,errorbar="sd")
ggErrorBar=function(data,mapping,interactive=FALSE,digits=1,mode=2,errorbar="se"){

    df<-data

    yvar=paste(mapping[["y"]])
    (groupname=setdiff(names(mapping),c("x","y")))
    (groupvar=paste(mapping[groupname]))
    if(length(groupvar)==0) groupvar<-NULL

    A=yvar
   (B<-groupvar)

    xvar=paste0(mapping[["x"]])
    if(length(xvar)>1) xvar<-xvar[-1]

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

    if(is.null(B)) {
        p<-ggplot(dat,aes_string(x=xvar,fill=xvar,y=yvar,tooltip="label",data_id="id"))+guides(fill=FALSE)
    } else {
        p<-ggplot(dat,aes_string(x=xvar,fill=groupvar,y=yvar,tooltip="label",data_id="id"))
    }
    if(mode==2) p<-p+geom_bar_interactive(position="dodge",stat="identity")
    p<-p+eval(parse(text=paste0("geom_errorbar(aes(ymin=",A,"-",errorbar,",ymax=",
                                 A,"+",errorbar,"),position=position_dodge(0.9),width=0.2)")))

    if(mode!=2) p<-p+geom_bar_interactive(position="dodge",stat="identity")

   # if(interactive) p<-ggiraph(code=print(p),zoom_max = 10)
    if(interactive){
            tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
            hover_css="r:4px;cursor:pointer;stroke-width:6px;"
            p<-ggiraph(code=print(p),tooltip_extra_css=tooltip_css,tooltip_opacity=.75,
                       zoom_max=10,hover_css=hover_css)
    }
    p
}
