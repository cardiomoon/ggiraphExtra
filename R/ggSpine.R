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
#'@param use.label Logical. Whether or not use column label in case of labelled data
#'@param use.labels Logical. Whether or not use value labels in case of labelled data
#'@param labeller A function that takes one data frame of labels and returns a list or data frame of character vectors.
#'@param facetbycol Logical. If TRUE, facet by column.
#'@param xangle  angle of axis label
#'@param yangle angle of axis label
#'@param ... other arguments passed on to geom_rect_interactive.
#'@importFrom ggplot2 coord_polar scale_y_continuous guide_legend
#'@importFrom ggiraph geom_rect_interactive
#'@importFrom dplyr lag group_by n
#'@importFrom magrittr '%>%'
#'@importFrom scales percent
#'@importFrom tidyr spread
#'@importFrom dplyr select arrange
#'@importFrom plyr '.'
#'@importFrom purrr map_df
#'@export
#'@return An interactive spinogram
#'@examples
#'require(moonBook)
#'require(ggplot2)
#'require(ggiraph)
#'acs$Dx=factor(acs$Dx,levels=c("Unstable Angina","NSTEMI","STEMI"))
#'ggSpine(data=acs,aes(x=age,fill=Dx,facet=sex),palette="Reds")
#'ggSpine(data=acs,aes(x=age,fill=Dx,facet=sex),facetbycol=FALSE,minlabelgroup=0.02)
#'ggSpine(data=acs,aes(x=age,fill=Dx),palette="Reds")
#'ggSpine(data=acs,aes(x=smoking,fill=Dx),palette="Reds")
#'ggSpine(data=acs,aes(x=DM,fill=Dx,facet=sex),palette="Reds")
#'ggSpine(data=acs,aes(x=DM,facet=smoking,fill=Dx))
#'ggSpine(data=acs,aes(x=DM,facet=smoking,fill=Dx),facetbycol=FALSE)
#'ggSpine(data=acs,aes(x=Dx,fill=smoking))
#'ggSpine(data=rose,aes(x=Month,fill=group,y=value),stat="identity")
#'ggSpine(data=acs,aes(x=age,fill=Dx,facet=DM))
#'ggSpine(data=acs,aes(x=Dx,fill=smoking),position="dodge")
#'ggSpine(data=acs,aes(x=Dx,fill=smoking),position="stack")
ggSpine=function (data, mapping, stat = "count", position = "fill", palette = "Blues",
                  interactive = FALSE, polar = FALSE, reverse = FALSE, width = NULL,maxylev=6,
                  digits = 1, colour = "black", size = 0.2, addlabel = TRUE, labelsize=5,
                  minlabelgroup=0.04,minlabel=2,
                  hide.legend=TRUE,
                  use.label=TRUE,use.labels=TRUE,labeller=NULL,facetbycol=TRUE,
                  xangle=NULL,yangle=NULL,...)
{

       # data=acs;mapping=aes(x=smoking,fill=Dx,facet=sex)
       # palette="Reds";addlabel=TRUE
       # stat = "count"; position = "fill"; palette = "Blues";
       # interactive = FALSE; polar = FALSE; reverse = FALSE; width = NULL;maxylev=6
       # digits = 1; colour = "black"; size = 0.2; addlabel = FALSE; hide.legend=FALSE
       # use.label=TRUE;use.labels=TRUE;labeller=NULL;facetbycol=FALSE
       # xangle=NULL;yangle=NULL
       # df=acs %>% group_by(sex,Dx,smoking) %>% summarize(n=n())
       # data=acs
       # mapping=aes(x=age,fill=Dx,facet=sex)

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


        (xlab=attr(data[[xvar]],"label"))
        if(is.null(xlab)) xlab=xvar
        if(!use.label) xlab=xvar
        (filllab=attr(data[[fillvar]],"label"))
        if(is.null(filllab)) filllab=fillvar
        if(!use.label) filllab=fillvar
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

                # data<-acs %>% group_by(sex,Dx,HBP) %>% summarize(n=n())
                # mapping=aes(facet=sex,fill=Dx,y=n,x=HBP)
                # position="fill";digits=1;facetbycol=TRUE;facetvar="sex";width=0.9

                data=data.frame(data)
                # data
                my_summarize_n2=function(data,mapping,width,position,digits,facetbycol,minlabelgroup,minlabel){
                   df = data %>%
                     select(!!mapping$x,!!mapping$fill,!!mapping$y) %>%
                     arrange(!!mapping$x,!!mapping$fill)

                   colnames(df)[ncol(df)] = "n"
                   df1 <- df %>% tidyr::spread(!!mapping$x,n)

                   # df =df[order(df[[xvar]],df[[fillvar]]),]
                   # colnames(df)[3] = "n"
                   #
                   # df1<-df %>% spread(!!mapping$x,n)
                   df1=data.frame(df1)
                   rownames(df1)=df1[[1]]
                   df1<-df1[-1]
                   a=as.matrix(df1)

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
                df1<-data %>% split(.[[`facetvar`]]) %>%
                    lapply(my_summarize_n,mapping,
                           width=width,position=position,digits=digits,facetbycol=facetbycol,
                           minlabelgroup=minlabelgroup,minlabel=minlabel)
                for(i in 1:length(df1)) {
                    df1[[i]][[facetvar]]=names(df1)[[i]]
                }
                df2<-map_df(df1,rbind)
                }
                df2

        }

        xlabels = levels(factor(df2[[1]]))
        xlabels
        ylabels = levels(factor(data[[fillvar]]))

        if (contmode) {
                total=nrow(data)
                ycount = length(ylabels)
                (pos = 1:ycount)
                y = (100/ycount) * (pos - 1) + (100/ycount)/2
                if(facetbycol==FALSE) y=y*(total/100)
        } else {


                y=rowMeans(matrix(unique(df2$y),nrow=length(ylabels)))
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
                p <- p + scale_y_continuous(breaks = y, labels = ylabels,name=filllab)

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
        if(is.null(yangle)) yangle=90
        p <- p  + theme(axis.text.y = element_text(angle = yangle),
                        axis.ticks.y = element_blank())


        df3=df2[df2$yno==1,]
        vjust=ifelse(facetbycol,1.8,1)
        if(contmode){
        total=nrow(data)
        df3$ratio1=df3$width/total
        df3$label=stringr::str_extract(substr(df3$age,2,nchar(df3$age)),"^[^,]+")

        df3$ratio2=lag(df3$ratio1)
        df3$ratio2[1]=df3$ratio1[1]
        df3$label[(df3$ratio1<minlabelgroup)&(df3$ratio2<minlabelgroup)]=""
        if(is.null(facetvar)){
                p<-p + scale_x_continuous(breaks = df3$xmin,labels = df3$label,name=xlab)
        } else{
            p<-p + geom_text(aes_string(x = "xmin", y = "0", label = "label"),data=df3,vjust=vjust)
        }
        } else{
                if(is.null(facetvar)){
                        p<-p + scale_x_continuous(breaks = df3$x,labels = df3[[xvar]],name=xlab)
                } else{
                        p<-p + geom_text(aes_string(x = "x", y = "0", label = xvar),
                                         data=df3,vjust=vjust)
                }
        }

        if(facetbycol==FALSE){
           p<-p + scale_y_continuous(breaks = NULL, labels = NULL,name=NULL) +
               theme(legend.position="bottom")
        }

        if(!is.null(facetvar)) p<-p+theme(strip.placement = "outside")
        if (polar == TRUE)
                p <- p + coord_polar()
        tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
        hover_css = "fill-opacity=.3;cursor:pointer;stroke:gold;"


        if (interactive)
                p <- ggiraph(code = print(p), tooltip_extra_css = tooltip_css,
                             tooltip_opacity = 0.75, zoom_max = 10, hover_css = hover_css)
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

        df<-data %>%
                group_by( !!mapping$x,!!mapping$fill) %>%
                dplyr::summarize(n=n())

        df=data.frame(df)
        a=table(data[[colnames(df)[2]]],data[[colnames(df)[1]]])
        my_sumSub(df,a,width=width,position=position,digits=digits,facetbycol=facetbycol,
                  minlabelgroup=minlabelgroup,minlabel=minlabel)

}



