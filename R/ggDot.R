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
ggDot=function(data,mapping,
                stackdir="center",binaxis="y",binwidth=0.5,method="dotdensity",
                position=0.2,boxwidth=0.25,
                boxfill=NULL,...){

        yvar=paste(mapping[["y"]])
        if(length(yvar)==0) yvar<-NULL
        (groupname=setdiff(names(mapping),c("x","y")))
        (groupvar=paste(mapping[groupname]))
        if(length(groupvar)==0) groupvar<-NULL

        (xvar=paste0(mapping[["x"]]))
        yvar

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
}

