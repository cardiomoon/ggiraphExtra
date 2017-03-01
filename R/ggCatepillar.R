#' Paste character vectors separated by colon
#'
#'@param ... Arguments passed on to paste()
#'@export
pastecolon=function(...){
        paste(...,sep=":")
}

#' Make an interactive catepillar plot
#'
#'@param data a data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@param digits An integer indicating the number of decimal places
#'@param errorbar which value is displayed with errorbar :"se" or "sd"
#'@param flip Logical cvalue. If TRUE, coord_flip() function is used to make a horizontal plot
#'@importFrom ggplot2 geom_errorbar
#'@export
#'@return An interactive catepillar plot
#'@examples
#'require(moonBook)
#'require(ggiraph)
#'require(ggplot2)
#'ggCatepillar(acs,aes(Dx,age,color=HBP),interactive=TRUE)
#'ggCatepillar(acs,aes(c(Dx,sex),age,color=HBP),interactive=TRUE,flip=TRUE)
#'ggCatepillar(acs,aes(age,height,color=sex),errorbar=FALSE,interactive=TRUE)
ggCatepillar=function(data,mapping,errorbar="se",interactive=FALSE,digits=1,flip=FALSE){

        #data=acs;mapping=aes(c(Dx,sex),height,color=HBP);interactive=FALSE;digits=1;errorbar="se";flip=TRUE
        xvar<-yvar<-groupvar<-NULL
        (xvar=paste(mapping[["x"]]))
        yvar=paste(mapping[["y"]])

        if(is.null(xvar)|is.null(yvar)) warning("Both x and y aesthetics are should be mapped")
        (groupname=setdiff(names(mapping),c("x","y")))
        if(length(groupname)>0) (groupvar=paste(mapping[groupname]))


        if(length(xvar)>1) xvar<-xvar[-1]

        df<-data
        A=yvar
        B=groupvar
        C=xvar

        if(is.null(B)){
                dat=summarySE(df,A,C)
                dat$tooltip="all"
                if(length(C)==1) dat$label=paste0(C,": ",dat[[C]],"<br>",A,": ",round(dat[[A]],digits),
                                 "<br>sd: ",round(dat$sd,digits),"<br>se: ",round(dat$se,digits))
                else dat$label=paste0(A,": ",round(dat[[A]],digits),
                                 "<br>sd: ",round(dat$sd,digits),"<br>se: ",round(dat$se,digits))

        } else {
                dat=summarySE(df,A,c(B,C))
                dat[[B]]=factor(dat[[B]])
                dat$tooltip=dat[[B]]
                if(length(C)==1) dat$label=paste0(B,": ",dat[[B]],"<br>",C,":",dat[[C]],"<br>",A,": ",round(dat[[A]],digits),
                                  "<br>sd: ",round(dat$sd,digits),"<br>se: ",round(dat$se,digits))
                else dat$label=paste0(B,": ",dat[[B]],"<br>",A,": ",round(dat[[A]],digits),
                                      "<br>sd: ",round(dat$sd,digits),"<br>se: ",round(dat$se,digits))

        }
        if(length(C)>1){
                temp=Reduce(paste0,C)
                dat[[temp]]=Reduce(pastecolon,dat[C])
                C=temp
                dat[[C]]=factor(dat[[C]])
        }

        dat

        #dat$tooltip=dat[[B]]
        #dat$label=paste0(dat[[B]],"<br>",dat[[C]],"<br>",round(dat[[A]],digits))
        dat$id=1:nrow(dat)

        #print(dat)

        if(class(dat[[C]])%in% c("numeric","integer")) {
                mywidth=max(dat[[C]])/80
        } else mywidth=0.2
        #mywidth

         if(is.null(B)) {
                 p<-ggplot(data=dat,aes_string(x=C,y=A,group=1,colour=C))+xlab(Reduce(pastecolon,C))

         } else p<-ggplot(data=dat,aes_string(x=C,y=A,group=B,colour=B))


        p<-p+ geom_path_interactive(aes_string(tooltip="tooltip",data_id="id"),position=position_dodge(width=mywidth))+
                geom_point_interactive(aes_string(tooltip="label",data_id="id"),size=4,position=position_dodge(width=mywidth))
        p
        if(errorbar=="se"|errorbar=="sd") p<-p+eval(parse(text=paste0("geom_errorbar(aes(ymin=",A,"-",errorbar,",ymax=",
                                    A,"+",errorbar,"),width=",mywidth,",
                                    position=position_dodge(width=mywidth))")))
        if(flip) p<-p+coord_flip()
        #p<-my_theme(p)
        #p<-p+theme(legend.position="none")
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
