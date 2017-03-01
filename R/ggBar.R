#'Draw an interactive barplot
#'
#'@param data A data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param stat The statistical transformation to use on the data for this layer, as a string
#'            c("count","identity")
#'@param position Position adjustment. One of the c("fill","stack","dodge")
#'@param palette A character string indicating the color palette
#'@param width Bar width
#'@param digits integer indicating the number of decimal places
#'@param horizontal A logical value. If TRUE,a horizontal bar plot will be returned
#'@param yangle An integer. The value will be used adjust the angle of axis.text.y
#'@param addlabel A logical value. If TRUE, label will be added to the plot
#'@param polar A logical value. If TRUE, coord_polar() function will be added
#'@param reverse If true, reverse palette colors
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@param ... other arguments passed on to geom_bar_interactive.
#'@importFrom ggplot2 position_dodge position_stack position_fill
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
#'ggBar(acs,aes(x=Dx,fill=smoking),position="dodge",interactive=TRUE)
#'ggBar(rose,aes(x=Month,fill=group,y=value),stat="identity",polar=TRUE,palette="Reds",width=1,
#'       color="black",size=0.1,interactive=TRUE)
ggBar=function(data,mapping,
               stat="count",position="stack",palette=NULL,
               width=NULL,digits=1,horizontal=FALSE,yangle=0,
               addlabel=FALSE,polar=FALSE,interactive=FALSE,reverse=FALSE,...){

        xvar<-fillvar<-facetvar<-yvar<-NULL
        if("x" %in% names(mapping)) xvar<-paste(mapping[["x"]])
        if("y" %in% names(mapping)) yvar<-paste(mapping[["y"]])
        if("fill" %in% names(mapping)) fillvar<-paste(mapping[["fill"]])
        if("facet" %in% names(mapping)) facetvar<-paste(mapping[["facet"]])
        (fillvar<-paste0(mapping[["fill"]]))

    contmode=0

    if(is.numeric(data[[xvar]])){
        if(is.null(width)) width=1
        result=num2cut(data[[xvar]])
        b=result$x1
        breaks=result$breaks
        a=table(data[[fillvar]],b)
        a
        df=reshape2::melt(a)
        df=df[c(2,1,3)]
        colnames(df)=c(xvar,fillvar,"nrow")
        df
        contmode=1

    } else if((stat=="identity") &(!is.null(yvar))){
        df=data[c(xvar,fillvar,yvar)]
        colnames(df)[3]="nrow"
        myformula=as.formula(paste(fillvar,"~",xvar))
        myformula
        b=reshape2::dcast(df,myformula,value=nrow)
        a=b[,-1]
        rownames(a)=b[[1]]
        #str(a)
        a=as.matrix(a)

    } else {
        df=ddply(data,c(xvar,fillvar),"nrow")
        df
        a=table(data[[fillvar]],data[[xvar]])
        a

    }
    if(is.null(width)) width=ifelse(contmode,1,0.9)
    barwidth=width

    df
    a
    if(is.null(width)) width=0.9
    df$xno=as.numeric(factor(df[[1]]))
    df$yno=as.numeric(factor(df[[2]]))

    total=sum(a,na.rm=TRUE)
    (csum=colSums(a,na.rm=TRUE))
    (csum/total)<0.05
    (rsum=rowSums(a,na.rm=TRUE))
    (xmax=cumsum(csum))
    (xmin=cumsum(csum)-csum)
    (x=(xmax+xmin)/2)
    (width=csum*width)
    (xmax=x+width/2)
    (xmin=x-width/2)
    df$xmin=df$xmax=df$x=df$csum=df$width=0
    for(i in 1:max(df$xno)){
        df[df$xno==i,]$csum=csum[i]
        df[df$xno==i,]$xmin=xmin[i]
        df[df$xno==i,]$xmax=xmax[i]
        df[df$xno==i,]$x=x[i]
        df[df$xno==i,]$width=width[i]
    }

    count=max(df$xno)

    if(position=="dodge"){
        df$ymax=df$nrow
        df$ymin=0
        df$y=(df$ymax+df$ymin)/2
        ycount=max(df$yno)
        df$xmin2=df$xmin+(df$yno-1)*(df$width/ycount)
        df$xmax2=df$xmin2+(df$width/ycount)
        df$xmin=df$xmin2
        df$xmax=df$xmax2
        df$x=(df$xmax+df$xmin)/2
        df2=df
    } else{
        for(i in 1:count){
            dfsub=df[df$xno==i,]
            dfsub<-dfsub[nrow(dfsub):1,]
            dfsub$ratio=round(dfsub$nrow*100/csum[i],digits)
            dfsub$ymax=cumsum(dfsub$nrow)
            dfsub$ymin=dfsub$ymax-dfsub$nrow
            if(position=="fill"){
                dfsub$ymax=dfsub$ymax*100/csum[i]
                dfsub$ymin=dfsub$ymin*100/csum[i]
            }
            dfsub$y=(dfsub$ymin+dfsub$ymax)/2
            if(i==1) df2=dfsub
            else df2=rbind(df2,dfsub)
        }

    }

    df2$data_id=as.character(1:nrow(df2))
    df2$tooltip=paste0(df2[[xvar]],"<br>",df2[[fillvar]],"<br>",df2$nrow)
    if(contmode) df2$label=ifelse((df2$csum/total)>0.04,df2$nrow,"")
    else df2$label=df2$nrow
    if(position=="fill") {
        df2$tooltip=paste0(df2$tooltip,"(",df2$ratio,"%)")
        if(contmode) df2$label=ifelse((df2$csum/total)>0.04,paste0(df2$ratio,"%"),"")
        else df2$label=paste0(df2$ratio,"%")
    }

    #print(df2)

    if(contmode) {
        xlabels=breaks[2:length(breaks)]
        xlabels
        xlabels[csum/total<0.04]=""
    } else xlabels=levels(factor(df[[1]]))

    ylabels=levels(factor(df[[2]]))
    if(contmode) {
        ycount=length(ylabels)
        (pos=1:ycount)
        y=(100/ycount)*(pos-1)+(100/ycount)/2
    } else y=df2[df2$xno==1,"y"]



    p<-ggplot(mapping=aes_string(x=xvar,fill=fillvar,y="nrow"),data=df2)+
        # geom_bar(stat="identity")
        # geom_bar_interactive(stat="identity")
        geom_bar_interactive(aes_string(tooltip="tooltip",data_id="data_id"),stat="identity",
                             position=position,width=barwidth,...)
    if(addlabel) {
            if(position=="stack") {
                    p=p+geom_text(aes(label=df2$label),position=position_stack(vjust=0.5))
            } else if(position=="fill") {
                    p=p+geom_text(aes(label=df2$label),position=position_fill(vjust=0.5))
            } else {
                    if(horizontal){
                            p=p+geom_text(aes(label=df2$label),position=position_dodge(0.9),hjust=-0.5)
                    } else{
                            p=p+geom_text(aes(label=df2$label),position=position_dodge(0.9),vjust=-0.5)
                    }
            }

    }

    # if(addlabel) {
    #     if(position=="stack") {
    #         p=p+geom_text(aes(x=df2$xno,y=df2$y,label=df2$label))
    #     } else if(position=="fill") {
    #         p=p+geom_text(aes(x=df2$xno,y=(df2$y)/100,label=df2$label))
    #     } else {
    #         if(horizontal){
    #             p=p+geom_text(aes(label=df2$label),position=position_dodge(0.9),hjust=-0.5)
    #         } else{
    #         p=p+geom_text(aes(label=df2$label),position=position_dodge(0.9),vjust=-0.5)
    #         }
    #     }
    #
    # }

    if(polar==TRUE) p<-p+ coord_polar()
    if(horizontal==TRUE) p<-p+ coord_flip()
    if(yangle!=0) p<-p+theme(axis.text.y=element_text(angle=yangle,hjust = 0.5))
    direction=ifelse(reverse,-1,1)
    if(!is.null(palette)) p<-p+scale_fill_brewer(palette=palette,direction=direction)
    tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
    hover_css="fill-opacity=.3;cursor:pointer;stroke:gold;"
    if(interactive) p<-ggiraph(code=print(p),tooltip_extra_css=tooltip_css,tooltip_opacity=.75,
                               zoom_max=10,hover_css=hover_css)
    p


}
