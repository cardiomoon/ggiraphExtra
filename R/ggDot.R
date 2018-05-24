#' Draw a Wilkinson dot plot
#'@param data a data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param stackdir which direction to stack the dots. "up" (default), "down", "center", "centerwhole" (centered, but with dots aligned)
#'@param binaxis The axis to bin along, "x" (default) or "y"
#'@param binwidth When method is "dotdensity", this specifies maximum bin width. When method is "histodot", this specifies bin width. Defaults to 1/30 of the range of the data
#'@param method	"dotdensity" (default) for dot-density binning, or "histodot" for fixed bin widths (like stat_bin)
#'@param position Position adjustment. If 0, no adjustment.
#'@param boxwidth The width of boxplot
#'@param boxfill Fill color of boxplot
#'@param use.label Logical. Whether or not use column label in case of labelled data
#'@param use.labels Logical. Whether or not use value labels in case of labelled data
#'@param ... other arguments passed on to geom_dotplot
#'@importFrom ggplot2 geom_dotplot scale_x_continuous
#'@export
#'@examples
#'require(ggplot2)
#'if(requireNamespace("gcookbook",quietly=TRUE)){ # for data heightweight
#'  require(gcookbook)
#'  ggDot(heightweight,aes(sex,heightIn,fill=sex),boxfill="white",binwidth=0.4)
#'  ggDot(heightweight,aes(heightIn))
#'  ggDot(heightweight,aes(x=heightIn,fill=sex))
#'}
#'require(moonBook) #for use data radial
#'ggDot(radial,aes(x=sex,y=height,fill=sex),boxfill="white",position=0,binwidth=1,boxwidth=1)
#'ggDot(radial,aes(x=height,fill=sex),binwidth=1)
#'ggDot(acs,aes(x=sex,y=age,color=sex))
#'ggDot(acs,aes(x=Dx,y=age,color=Dx))
ggDot=function(data,mapping,
                stackdir="center",binaxis="y",binwidth=0.5,method="dotdensity",
                position=0.2,boxwidth=0.25,
                boxfill=NULL,use.label=TRUE,use.labels=TRUE,
               ...){


         #   data=spssdata;mapping=aes(x=sexw1,y=q33a01w1,fill=sexw1)
        #   data=acs;mapping=aes(x=smoking,y=age,fill=smoking)
        # data=radial;mapping=aes(x=sex,y=height,fill=sex)
        #   stackdir="center";binaxis="y";binwidth=0.5;method="dotdensity"
        # #  position=0.2;boxwidth=0.25
        # #  boxfill=NULL;use.label=TRUE;use.labels=TRUE;
        # boxfill="white";position=0;binwidth=1;boxwidth=1

        name=names(mapping)
        name
        xvar<-yvar<-NULL
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
         xlabels
         ylabels
         yvar
        (groupname=setdiff(names(mapping),c("x","y")))
        (groupvar=paste(mapping[groupname]))
        if(length(groupvar)==0) groupvar<-NULL


        if(is.null(yvar)){
                binaxis="x"

                # if(is.null(fillvar)) p<-ggplot(data,aes_string(x=xvar))
                # else p<-ggplot(data,aes_string(x=xvar,fill=fillvar))
                p<-ggplot(data,mapping)
                p<-p+geom_dotplot(method=method,stackdir=stackdir,binaxis=binaxis,binwidth=binwidth,...)
        } else{

                # if(is.null(fillvar)) {
                #     p<-ggplot(data,aes_string(x=xvar,y=yvar))
                # } else {
                #     p<-ggplot(data,aes_string(x=xvar,y=yvar,fill=fillvar))
                # }

                if(!is.factor(data[[xvar]])) data[[xvar]]=factor(data[[xvar]])
                p<-ggplot(data,mapping)
                if(position==0){
                        if(is.null(boxfill)) p<- p + geom_boxplot(width=boxwidth)
                        else p<- p + geom_boxplot(fill=boxfill,width=boxwidth)
                        p<-p+geom_dotplot(method=method,stackdir=stackdir,binaxis=binaxis,binwidth=binwidth)
                } else{
                        if(is.null(boxfill)) p<- p + geom_boxplot(aes_string(x=paste0("as.numeric(",xvar, ")+ ",position),group=xvar),
                                                                  width=boxwidth)
                        else p<- p + geom_boxplot(aes_string(x=paste0("as.numeric(",xvar, ")+ ",position),group=xvar),
                                                  fill=boxfill,width=boxwidth)
                        p
                        p<-p+geom_dotplot(aes_string(x=paste0("as.numeric(",xvar, ")- ",position),group=xvar),
                                          method=method,stackdir=stackdir,binaxis=binaxis,binwidth=binwidth)
                        p<-p+scale_x_continuous(breaks=1:nlevels(data[[xvar]]),labels=levels(data[[xvar]]))
                }
                p<-p+theme(legend.position='none')
        }
        p
        #str(data[[xvar]])
       # str(data)
        # is.numeric(data[[xvar]])

        if(use.labels) {
                if(!is.null(xlabels)) {
                        if(position!=0)
                                p<-p+scale_x_continuous(breaks=1:length(xlabels),labels=xlabels)
                         else
                                 p<-p+scale_x_discrete(labels=xlabels)
                }
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

}

