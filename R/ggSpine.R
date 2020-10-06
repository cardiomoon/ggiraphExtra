#'Draw an interactive spinogram
#'
#'@param data A data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param stat The statistical transformation to use on the data for this layer, as a string
#'            c("count","identity")
#'@param position Position adjustment. One of the c("fill","stack","dodge")
#'@param palette A character string indicating the color palette
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@param polar A logical value. If TRUE, coord_polar() function will be added
#'@param reverse If true, reverse palette colors
#'@param width Bar width
#'@param maxylev integer indicating threshold of unique value to be treated as a categorical variable
#'@param digits integer indicating the number of decimal places
#'@param colour Bar colour
#'@param size Bar size
#'@param addlabel A logical value. If TRUE, label will be added to the plot
#'@param labelsize label size
#'@param minlabelgroup minimal threshold of label group. Default is 0.04
#'@param minlabel minimal threshold of label. Default is 2
#'@param hide.legend A logical value. If TRUE, the legend is removed and y labels are recreated
#'@param ylabelMean Logical. If TRUE, y axis labels are positioned at mean value.
#'@param sec.y.axis Logical. If TRUE, secondary y axis is shown at the right side.
#'@param use.label Logical. Whether or not use column label in case of labelled data
#'@param use.labels Logical. Whether or not use value labels in case of labelled data
#'@param labeller A function that takes one data frame of labels and returns a list or data frame of character vectors.
#'@param facetbycol Logical. If TRUE, facet by column.
#'@param xangle  angle of axis label
#'@param yangle angle of axis label
#'@param xreverse Logical. Whether or not reverse x-axis
#'@param yreverse Logical. Whether or not reverse y-axis
#'@param xlab Label for x-axis
#'@param filllab Label for fill aes
#'@param family font family
#'@param ... other arguments passed on to geom_rect_interactive.
#'@importFrom ggplot2 coord_polar scale_y_continuous guide_legend sec_axis scale_x_reverse scale_y_reverse
#'@importFrom ggiraph geom_rect_interactive
#'@importFrom dplyr lag group_by n
#'@importFrom magrittr '%>%'
#'@importFrom scales percent
#'@importFrom tidyr spread complete
#'@importFrom dplyr select arrange
#'@importFrom plyr '.'
#'@importFrom purrr map_df
#'@export
#'@return An interactive spinogram
#'@examples
#'require(moonBook)
#'require(ggplot2)
#'acs$Dx=factor(acs$Dx,levels=c("Unstable Angina","NSTEMI","STEMI"))
#'ggSpine(data=acs,aes(x=age,fill=Dx,facet=sex),palette="Reds")
#'ggSpine(data=acs,aes(x=age,fill=Dx,facet=sex),facetbycol=FALSE,minlabelgroup=0.02)
#'ggSpine(data=acs,aes(x=age,fill=Dx),palette="Reds")
#'ggSpine(data=acs,aes(x=smoking,fill=Dx),palette="Reds")
#'ggSpine(data=acs,aes(x=DM,fill=Dx,facet=sex),palette="Reds")
#'ggSpine(data=acs,aes(x=Dx,fill=smoking,facet=sex),palette="Reds")
#'ggSpine(data=acs,aes(x=DM,facet=smoking,fill=Dx),sec.y.axis=TRUE)
#'ggSpine(data=acs,aes(x=DM,facet=smoking,fill=Dx),facetbycol=FALSE)
#'ggSpine(mtcars,aes(x=gear,fill=carb),interactive=TRUE)
#'ggSpine(mtcars,aes(x=gear,fill=carb,facet=am))
#'ggSpine(data=acs,aes(x=Dx,fill=smoking),position="dodge")
#'ggSpine(data=acs,aes(x=Dx,fill=smoking),position="stack")
ggSpine=function (data, mapping, stat = "count", position = "fill", palette = "Blues",
                  interactive = FALSE, polar = FALSE, reverse = FALSE, width = NULL,maxylev=6,
                  digits = 1, colour = "black", size = 0.2, addlabel = TRUE, labelsize=5,
                  minlabelgroup=0.04,minlabel=2,
                  hide.legend=TRUE,ylabelMean=FALSE,sec.y.axis=FALSE,
                  use.label=TRUE,use.labels=TRUE,labeller=NULL,facetbycol=TRUE,
                  xangle=NULL,yangle=NULL, xreverse=FALSE, yreverse=FALSE,
                  xlab=NULL,filllab=NULL,family=NULL,...)
{

    # data=mtcars;mapping=aes(x=gear,fill=carb,facet=am)
    # stat = "count"; position = "fill"; palette = "Blues"
    # interactive = FALSE; polar = FALSE; reverse = FALSE; width = NULL;maxylev=6
    # digits = 1; colour = "black"; size = 0.2; addlabel = TRUE; labelsize=5
    # minlabelgroup=0.04;minlabel=2
    # hide.legend=TRUE;ylabelMean=FALSE;sec.y.axis=FALSE
    # use.label=TRUE;use.labels=TRUE;labeller=NULL;facetbycol=TRUE
    # xangle=NULL;yangle=NULL
    #
    # data=acs;mapping=aes(x=DM,fill=Dx,facet=sex)
    # palette="Reds";addlabel=TRUE
    #   stat = "count"; position = "fill"; palette = "Blues";
    #  interactive = FALSE; polar = FALSE; reverse = FALSE; width = NULL;maxylev=6
    #  digits = 1; colour = "black"; size = 0.2; addlabel = FALSE; hide.legend=TRUE
    #  use.label=TRUE;use.labels=TRUE;labeller=NULL;facetbycol=TRUE;sec.y.axis=FALSE
    #  xangle=NULL;yangle=NULL;minlabelgroup=0.04;minlabel=2;labelsize=5;ylabelMean=FALSE
    #  df=mtcars %>% group_by(gear,carb,am) %>% summarize(n=n())
    #  ggSpine(df,aes(x=gear,fill=carb,y=n,facet=am),stat="identity")
    #  data=df;mapping=aes(x=name,fill=group,y=rate);stat="identity"
    # xreverse=FALSE;yreverse=FALSE
    # data
    # require(scales)
    # require(purrr)
    # require(tidyverse)


    xvar <- fillvar <- facetvar <- yvar <- NULL
    if ("x" %in% names(mapping))
        xvar <- getMapping(mapping,"x")
    if ("y" %in% names(mapping))
        yvar <- getMapping(mapping,"y")
    if ("fill" %in% names(mapping))
        fillvar <- getMapping(mapping,"fill")
    if ("facet" %in% names(mapping))
        facetvar <- getMapping(mapping,"facet")
    contmode = 0

    if(is.null(xlab)){
    (xlab=attr(data[[xvar]],"label"))
    if(is.null(xlab)) xlab=xvar
    if(!use.label) xlab=xvar
    }
    if(is.null(filllab)){
    (filllab=attr(data[[fillvar]],"label"))
    if(is.null(filllab)) filllab=fillvar
    if(!use.label) filllab=fillvar
    }
    if(use.labels) data=addLabelDf(data,mapping=mapping)



    if (is.numeric(data[[xvar]])&(length(unique(data[[xvar]]))>maxylev)) {
        if (is.null(width)) width = 1
        width
        contmode = 1
        if(is.null(facetvar)){

            df2<-num2cutData(data,xvar,fillvar,width,position,digits,facetbycol,
                             minlabelgroup=minlabelgroup,minlabel=minlabel)

        } else{
            df1<-data %>% split(.[[`facetvar`]]) %>%
                lapply(num2cutData,xvar,fillvar,width,position,digits,facetbycol,
                       minlabelgroup,minlabel)
            for(i in 1:length(df1)) {
                df1[[i]][[facetvar]]=names(df1)[[i]]
            }
            df2<-map_df(df1,rbind)
        }

        if(is.factor(data[[fillvar]])){
            df2[[fillvar]]=factor(df2[[fillvar]],levels=levels(data[[fillvar]]))
        }


    } else if ((stat == "identity") & (!is.null(yvar))) {
        if(is.null(width)) width=0.9

        # data<-mtcars%>% group_by(gear,carb,am) %>% summarize(n=n())
        # mapping=aes(facet=am,fill=gear,y=n,x=carb)
        # position="fill";digits=1;facetbycol=TRUE;facetvar="am";width=0.9

        data=data.frame(data)
        data

        if(!is.null(facetvar)){
            data<-data %>% complete(!!mapping$x,!!mapping$fill,!!mapping$facet)
            data[[yvar]][is.na(data[[yvar]])]=0
            data=data.frame(data)
        }

        my_summarize_n2=function(data,mapping,width,position,digits,facetbycol,minlabelgroup,minlabel){

            # df=acs %>% group_by(Dx,sex) %>% summarize(n=n())
            # data=df;mapping=aes(x=Dx,fill=sex,y=n);stat="identity"
            # position="fill";digits=1;facetbycol=TRUE;width=0.9;minlabelgroup=0.04;minlabel=2

            df = data %>%
                select(!!mapping$x,!!mapping$fill,!!mapping$y) %>%
                arrange(!!mapping$x,!!mapping$fill)

            df
            colnames(df)[ncol(df)] = "n"
            df=data.frame(df)
            df <- df %>% complete(!!mapping$x,!!mapping$fill,fill=list(n=0))
            df


            df1 <- df %>% tidyr::spread(!!mapping$x,n)

            df1

            # df =df[order(df[[xvar]],df[[fillvar]]),]
            # colnames(df)[3] = "n"
            #
            # df1<-df %>% spread(!!mapping$x,n)
            df1=data.frame(df1)
            rownames(df1)=df1[[1]]
            df1<-df1[-1]
            a=as.matrix(df1)
            a

            my_sumSub(df,a,width=width,position=position,digits=digits,facetbycol,
                      minlabelgroup=minlabelgroup,minlabel=minlabel)


        }

        if(is.null(facetvar)){


            df2=my_summarize_n2(data,mapping,width,position,digits,facetbycol,
                                minlabelgroup=minlabelgroup,minlabel=minlabel)

        } else{

            df1<-data %>% split(.[[`facetvar`]]) %>%
                lapply(my_summarize_n2,mapping,width,position,digits,facetbycol,
                       minlabelgroup=minlabelgroup,minlabel=minlabel)
            for(i in 1:length(df1)) {
                df1[[i]][[facetvar]]=names(df1)[[i]]
            }
            df2<-map_df(df1,rbind)
            df2
        }

    } else {

        if(is.null(width)) width=0.9
        if(is.null(facetvar)){
            df2=my_summarize_n(data,mapping,
                               width=width,position=position,digits=digits,facetbycol=facetbycol,
                               minlabelgroup=minlabelgroup,minlabel=minlabel)
        } else{
            if(!is.factor(data[[fillvar]])) data[[fillvar]]=factor(data[[fillvar]])
            # str(data)

            df1<-data %>% split(.[[`facetvar`]]) %>%
                lapply(my_summarize_n,mapping,
                       width=width,position=position,digits=digits,facetbycol=facetbycol,
                       minlabelgroup=minlabelgroup,minlabel=minlabel)
            df1
            for(i in 1:length(df1)) {
                df1[[i]][[facetvar]]=names(df1)[[i]]
            }
            df2<-map_df(df1,rbind)
            df3<-df2 %>%complete(!!mapping$fill,fill=list(n=0,ratio=0,label=""))
            df3<-data.frame(df3)
            df3
        }
        df2

    }

    xlabels = levels(factor(df2[[1]]))
    xlabels
    ylabels = levels(factor(data[[fillvar]]))
    ylabels

    if (contmode) {
        total=nrow(data)
        ycount = length(ylabels)
        (pos = 1:ycount)
        y = (100/ycount) * (pos - 1) + (100/ycount)/2
        if(facetbycol==FALSE) y=y*(total/100)
    } else {


        if(ylabelMean){
            y=rowMeans(matrix(df2$y,nrow=length(ylabels)))
        } else{
            df2
            if(is.null(facetvar)){

                y=df2$y[df2$xno==1]
                y[which(df2$ratio[df2$xno==1]==0)]=NA
                yend=df2$y[df2$xno==max(df2$xno)]
                yend[which(df2$ratio[df2$xno==max(df2$xno)]==0)]=NA

                if(xreverse){
                   ytemp=y
                   y=yend
                   yend=ytemp
                }
                if(any(is.na(y))) sec.y.axis=TRUE

            } else{
                condition=(df2$xno==1)&(df2[[facetvar]]==unique(df2[[facetvar]])[1])
                y=df2$y[condition]
                y[which(df2$ratio[condition]==0)]=NA
                condition1=(df2$xno==max(df2$xno))&
                    (df2[[facetvar]]==unique(df2[[facetvar]])[length(unique(df2[[facetvar]]))])
                yend=df2$y[condition1]
                yend[which(df2$ratio[condition1]==0)]=NA
                if(xreverse){
                    ytemp=y
                    y=yend
                    yend=ytemp
                }
                if(any(is.na(y))) sec.y.axis=TRUE
            }

        }
    }

    if(!is.null(facetvar)){
        if(facetbycol==FALSE) hide.legend=FALSE
    }
    if (is.numeric(df2[[fillvar]]))
        df2[[fillvar]] = factor(df2[[fillvar]])
    df2
    p <- ggplot(mapping = aes_string(xmin = "xmin", xmax = "xmax",
                                     ymin = "ymin", ymax = "ymax", fill = fillvar), data = df2)
    p <- p +geom_rect_interactive(aes_string(tooltip = "tooltip",
                                             data_id = "data_id"), size = size, colour = colour,...)
      # p <- p +geom_rect_interactive(aes_string(tooltip = "tooltip",
      #                                          data_id = "data_id"), size = size, colour = colour)
     # p
    if(!is.null(facetvar)) {

        addNumber<-function(string){
            if ((stat == "identity") & (!is.null(yvar))){
                result=sapply(string,function(x){paste0(facetvar,":",x,"\nN=",
                                                        sum(data[data[[facetvar]]==x,"n"]) )})
            } else{
                result=sapply(string,function(x){paste0(facetvar,":",x,"\nN=",nrow(data[data[[facetvar]]==x,]))})
            }
            result
        }
        if(is.null(labeller)){
            if(facetbycol){
                p<-p+eval(parse(text=paste0("facet_grid(.~",facetvar,",scales='free_x',space='free',labeller=labeller(.default=addNumber))")))
            } else{
                p<-p+eval(parse(text=paste0("facet_grid(",facetvar,"~.,scales='free_y',space='free_y',switch='y',labeller=labeller(.default=addNumber))")))

            }
        } else{

            if(facetbycol){
                p<-p+eval(parse(text=paste0("facet_grid(.~",facetvar,",scales='free_x',space='free',labeller=",labeller,")")))
            } else{
                p<-p+eval(parse(text=paste0("facet_grid(",facetvar,"~.,scales='free_y',space='free_y',switch='y',labeller=",labeller,")")))

            }
        }
    }

    p
    # x=rowMeans(matrix(unique(df2$x),nrow=length(xlabels)))

    # if (contmode) {
    #         p <- p + scale_x_continuous(breaks = xmax, labels = xlabels,name=xlab,
    #                                     limits = c(0, total))
    # }else {
    #         #if(length(x)!=length(xlabels)) xlabels=c(xlabels,NA)
    #         p<-p + scale_x_continuous(breaks = NULL, labels = NULL, name=xlab)
    # }
    if(!is.null(facetvar)) p<-p + scale_x_continuous(breaks = NULL, labels = NULL, name=xlab)

    direction = ifelse(reverse, -1, 1)


    if ((position != "dodge") & hide.legend ){

        if(sec.y.axis){

            if(yreverse){
                p <- p + scale_y_reverse(breaks = y, labels = ylabels,name=filllab,
                                            sec.axis=sec_axis(trans=~.,breaks = yend, labels = ylabels,name=filllab))
            } else{
                  p <- p + scale_y_continuous(breaks = y, labels = ylabels,name=filllab,
                                        sec.axis=sec_axis(trans=~.,breaks = yend, labels = ylabels,name=filllab))
            }

        } else{

            if(yreverse){
                p<-p + scale_y_reverse(breaks = y, labels = ylabels,name=filllab)
            } else{
               p<-p + scale_y_continuous(breaks = y, labels = ylabels,name=filllab)
            }
        }

        p<- p+ scale_fill_brewer(palette = palette, direction = direction,
                                 guide = FALSE) + ylab("")
    } else{
        p <- p + ylab("count")+guides(fill=guide_legend(reverse=TRUE))

        p <- p + scale_fill_brewer(palette = palette,
                                   direction = direction)
    }
    p
    if (addlabel)
        p = p + geom_text(aes_string(x = "x", y = "y", label = "label"),size=labelsize)

    if(is.null(xangle)){
        if(max(nchar(colnames(df2)))>10) xangle=20
        else xangle=0
    }


    p<-p+theme(axis.text.x=element_text(angle=xangle,vjust = 0.5))
    p
    if(is.null(yangle)) yangle=90
    p <- p  + theme(axis.text.y = element_text(angle = yangle),
                    axis.text.y.right = element_text(angle = -yangle),
                    axis.ticks.y = element_blank())


    df3=df2[df2$yno==1,]
    vjust=ifelse(facetbycol,1.8,1)
    if(yreverse) vjust=ifelse(facetbycol,-1,-0.1)


    if(contmode){
        total=nrow(data)
        df3
        df3$ratio1=df3$width/total
        df3$label=stringr::str_extract(substr(df3[[xvar]],2,nchar(df3[[xvar]])),"^[^,]+")
        if(mean(as.numeric(df3$label))>1e+9) {
            df3$label=paste0(as.numeric(df3$label)/1e+9,"T")
        } else if(mean(as.numeric(df3$label))>1e+6) {
            df3$label=paste0(as.numeric(df3$label)/1e+6,"M")
        } else if(mean(as.numeric(df3$label))>1e+3) {
            df3$label=paste0(as.numeric(df3$label)/1e+3,"K")
        }
        df3$ratio2=lag(df3$ratio1)
        df3$ratio2[1]=df3$ratio1[1]
        df3$label[(df3$ratio1<minlabelgroup)&(df3$ratio2<minlabelgroup)]=""
        p
        if(is.null(facetvar)){
            if(xreverse){
                p<-p + scale_x_reverse(breaks = df3$xmin,labels = df3$label,name=xlab)
            } else{
                 p<-p + scale_x_continuous(breaks = df3$xmin,labels = df3$label,name=xlab)
            }
        } else{
            p<-p + geom_text(aes_string(x = "xmin", y = "0", label = "label"),data=df3,vjust=vjust)
        }
    } else{
        if(is.null(facetvar)){
            if(xreverse){
                p<-p + scale_x_reverse(breaks = df3$x,labels = df3[[xvar]],name=xlab)
            } else{
                p<-p + scale_x_continuous(breaks = df3$x,labels = df3[[xvar]],name=xlab)
            }
        } else{

            if(!is.factor(df3[[xvar]])) df3[[xvar]]=factor(df3[[xvar]])
            df3$label2=ifelse(df3$ratio>=0,levels(df3[[xvar]]),"")
            if(is.null(family)){
            p<-p + geom_text(aes_string(x = "x", y = ifelse(yreverse,"100","0"), label = "label2"),
                             data=df3,vjust=vjust)
            } else{
              p<-p + geom_text(aes_string(x = "x", y = ifelse(yreverse,"100","0"), label = "label2"),
                               data=df3,vjust=vjust,family=family)
            }

        }
    }
    if(!is.null(family)) p<-p+theme(text= element_text(family=family))

    if(facetbycol==FALSE){
        if(yreverse){
            p<-p + scale_y_reverse(breaks = NULL, labels = NULL,name=NULL)
        } else{
            p<-p + scale_y_continuous(breaks = NULL, labels = NULL,name=NULL)
        }
        p <- p + theme(legend.position="bottom")
    }
    p

    if(!is.null(facetvar)) p<-p+theme(strip.placement = "outside")
    if (polar == TRUE)
        p <- p + coord_polar()



    if (interactive){
        tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
        hover_css = "fill-opacity=.3;cursor:pointer;stroke:gold;"

        p<-girafe(ggobj=p)
        p<-girafe_options(p,
                          opts_hover(css=hover_css),
                          opts_tooltip(css=tooltip_css,opacity=.75),
                          opts_zoom(min=1,max=10))
    }
    p

}


num2cutData=function(data,xvar,fillvar,width=0.9,position="fill",digits=1,facetbycol=TRUE,
                     minlabelgroup=0.04,minlabel=2){
    result = num2cut(data[[xvar]])
    result
    b = result$x1
    breaks = result$breaks
    a = table(data[[fillvar]], b)
    a
    df=as.data.frame(a,stringsAsFactors = FALSE)
    # df = reshape2::melt(a)
    df = df[c(2, 1, 3)]
    colnames(df) = c(xvar, fillvar, "n")
    df
    a
    my_sumSub(df,a,width=width,position=position,digits=digits,facetbycol=facetbycol,
              minlabelgroup=minlabelgroup,minlabel=minlabel)

}


my_sumSub=function(df,a,width=0.9,position="fill",digits=1,facetbycol=TRUE,minlabelgroup=0.04,minlabel=2){
        # acs$Dx=factor(acs$Dx,levels=c("Unstable Angina","NSTEMI","STEMI"))
        # df=acs %>% group_by(Dx,smoking) %>%summarize(n=n())
        # a=table(acs$smoking,acs$Dx)
        # width=0.9;position="fill";digits=1;facetbycol=TRUE;
        # minlabelgroup=0.04,minlabel=2
        #
        # str(df)
        # str(a)
        (total = sum(a))
        (csum = colSums(a))
        (rsum = rowSums(a))
        (xmax = cumsum(csum))
        (xmin = cumsum(csum) - csum)
        (x = (xmax + xmin)/2)
        (width = csum * width)
        (xmax = x + width/2)
        (xmin = x - width/2)

        df$xno=rep(1:ncol(a),each=nrow(a))
        df$yno=rep(1:nrow(a),ncol(a))
        df=as.data.frame(df)
        df
        df$csum = rep(csum,each=nrow(a))
        df$xmin = rep(xmin,each=nrow(a))
        df$xmax = rep(xmax,each=nrow(a))
        df$x = rep(x,each=nrow(a))
        df$width = rep(width,each=nrow(a))
        count = max(df$xno,na.rm=TRUE)
        df
        count
        if (position == "dodge") {
                df$ymax = df$n
                df$ymin = 0
                df
                df$y = (df$ymax + df$ymin)/2
                ycount = max(df$yno,na.rm=TRUE)
                df$xmin2 = df$xmin + (df$yno - 1) * (df$width/ycount)
                df$xmax2 = df$xmin2 + (df$width/ycount)
                df$xmin = df$xmin2
                df$xmax = df$xmax2
                df$x = (df$xmax + df$xmin)/2
                df2 = df
        } else {
                for (i in 1:count) {
                        dfsub = df[df$xno == i, ]
                        dfsub$ratio = round(dfsub$n * 100/csum[i], digits)
                        dfsub$ymax = cumsum(dfsub$n)
                        dfsub$ymin = dfsub$ymax - dfsub$n
                        if (position == "fill") {
                                dfsub$ymax = dfsub$ymax * 100/csum[i]
                                dfsub$ymin = dfsub$ymin * 100/csum[i]
                        }
                        dfsub$y = (dfsub$ymin + dfsub$ymax)/2
                        if (i == 1) {
                                df2 = dfsub
                        } else {
                            df2 = rbind(df2, dfsub)
                        }
                        df2
                }
        }
        df2$data_id = as.character(1:nrow(df2))

        df2$tooltip = paste0(df2[[2]], "<br>", df2[[1]],
                             "<br>", df2$nrow)
        df2$label = ifelse((df2$csum/total) >= minlabelgroup, df2$n, "")
        df2$tooltip = paste0(df2$tooltip, "(", df2$ratio, "%)")
        if (position == "fill") {
                df2$label = ifelse((df2$csum/total) >= minlabelgroup,
                                   ifelse(df2$ratio >= minlabel,percent(df2$ratio/100),""),"")
        }
        df2
        if(facetbycol==FALSE){
            df2$xmin=df2$xmin/total
            df2$xmax=df2$xmax/total
            df2$x=df2$x/total
            df2$ymin=df2$ymin*(total/100)
            df2$ymax=df2$ymax*(total/100)
            df2$y=df2$y*(total/100)
        }
        df2
}



my_summarize_n=function(data,mapping,width=0.9,position="fill",digits=1,facetbycol=TRUE,
                        minlabelgroup=0.04,minlabel=2){

        # data=mtcars;mapping=aes(x=carb,fill=gear)
        # width=0.9;position="fill";digits=1;facetbycol=TRUE
        # minlabelgroup=0.04;minlabel=2
        #
        # data[[mapping$fill]]=factor(data[[mapping$fill]])
        #
        # df<-data %>%
        #         group_by( !!mapping$x,!!mapping$fill) %>%
        #         dplyr::summarize(n=n()) %>%
        #         tidyr::complete(!!mapping$fill,fill=list(n=0))
        # df=data.frame(df)
        # df

        xvar=getMapping(mapping,"x")
        fillvar=getMapping(mapping,"fill")
        a=table(data[[xvar]],data[[fillvar]])

        df=data.frame(a)

        colnames(df)=c(xvar,fillvar,"n")
        df<-df %>% arrange(!!mapping$x,!!mapping$fill)

        my_sumSub(df,t(a),width=width,position=position,digits=digits,facetbycol=facetbycol,
                  minlabelgroup=minlabelgroup,minlabel=minlabel)

}



