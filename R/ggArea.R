#'Draw an interactive area plot
#'
#'@param data A data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param position Either "stack" or "fill"
#'@param palette A character string indicating the color palette
#'@param reverse If true, reverse palette colors
#'@param alpha Transparency
#'@param size Line size
#'@param use.label Logical. Whether or not use column label in case of labelled data
#'@param use.labels Logical. Whether or not use value labels in case of labelled data
#'@return An area plot
#'@importFrom ggplot2 geom_area
#'@export
#'@examples
#'require(gcookbook)
#'require(ggplot2)
#'ggArea(uspopage,aes(x=Year,y=Thousands,fill=AgeGroup))
#'ggArea(uspopage,aes(x=Year,y=Thousands,fill=AgeGroup),position="fill")
ggArea=function(data,mapping,position="stack",palette="Blues",reverse=TRUE,alpha=0.4,size=0.3,use.label=TRUE,use.labels=TRUE){

         # data=uspopage;mapping=aes(x=Year,y=Thousands,fill=AgeGroup)
        # palette="Blues";reverse=TRUE;alpha=0.4;size=0.3;use.label=TRUE;use.labels=TRUE
        # df=uspopage
        # df$Year[df$Year==1900]="0-1900"
        # data=df;mapping=aes(x=Year,y=Thousands,fill=AgeGroup)
        # position="stack";palette="Blues";
        # reverse=TRUE;alpha=0.4;size=0.3;use.label=TRUE;use.labels=TRUE

        fillvar<-xvar<-yvar<-NULL
        name=names(mapping)
        xlabels<-ylabels<-filllabels<-colourlabels<-xlab<-ylab<-colourlab<-filllab<-NULL
        for(i in 1:length(name)){
                (varname=paste0(name[i],"var"))
                labname=paste0(name[i],"lab")
                labelsname=paste0(name[i],"labels")
                assign(varname,getMapping(mapping,name[i]))
                x=eval(parse(text=paste0("data$",eval(parse(text=varname)))))
                assign(labname,attr(x,"label",exact = TRUE))
                assign(labelsname,sjlabelled::get_labels(x))
        }

        direction=ifelse(reverse,-1,1)
        if(is.factor(data[[xvar]])){
                temp=as.numeric(as.character(data[[xvar]]))
                if(sum(is.na(temp))==0) {
                        data[[xvar]]=temp
                        xlabels<-NULL
                }
        }
        data
        # str(data)
        # p<-ggplot(data,aes(x=Year,y=Thousands,fill=AgeGroup,group=AgeGroup))+
        #         geom_area(alpha=alpha)+
        #         geom_line(position="stack",size=size)+
        #         scale_fill_brewer(palette=palette,direction=direction,labels=filllabels)
        # p

        if(position=="stack"){
        p<-ggplot(data,aes_string(x=xvar,y=yvar,fill=fillvar,group=fillvar))+
                geom_area(alpha=alpha)+
                geom_line(position="stack",size=size)+
                scale_fill_brewer(palette=palette,direction=direction,labels=filllabels)
        p
        } else if(position=="fill"){

           # require(dplyr)
                 data<-data %>% group_by(!!mapping$x) %>% dplyr::mutate(ratio=!!mapping$y/sum(!!mapping$y))

                #  ggplot(df,aes(x=Year,y=ratio,fill=AgeGroup,color=AgeGroup))+geom_point()
                # ggplot(df,aes(x=Year,y=ratio,fill=AgeGroup,color=AgeGroup))+
                #         geom_area()+
                #         geom_line()
                #
                p<-ggplot(data,aes_string(x=xvar,y="ratio",fill=fillvar,group=fillvar))+
                        geom_area(alpha=alpha)+
                        geom_line(position="stack",size=size)+
                        scale_fill_brewer(palette=palette,direction=direction,labels=filllabels)
                p
        }
        if(use.labels) {
                if(is.numeric(data[[xvar]])){
                if(!is.null(xlabels)) p<-p+scale_x_continuous(breaks=1:length(xlabels),labels=xlabels)
                }
                if(!is.null(ylabels))  p<-p+scale_y_continuous(breaks=1:length(ylabels),labels=ylabels)
                # if(!is.null(filllabels)) p=p+scale_fill_brewer(palette=palette,direction=direction,
                #                                                labels=filllabels)
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

}

