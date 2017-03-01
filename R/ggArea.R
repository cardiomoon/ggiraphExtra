#'Draw an interactive area plot
#'
#'@param data A data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param palette A character string indicating the color palette
#'@param reverse If true, reverse palette colors
#'@param alpha Transparency
#'@param size Line size
#'@return An area plot
#'@importFrom ggplot2 geom_area
#'@export
#'@examples
#'require(gcookbook)
#'require(ggplot2)
#'ggArea(uspopage,aes(x=Year,y=Thousands,fill=AgeGroup))
ggArea=function(data,mapping,palette="Blues",reverse=TRUE,alpha=0.4,size=0.3){
        fillvar<-xvar<-yvar<-NULL
        if("fill" %in% names(mapping)) fillvar<-paste(mapping[["fill"]])
        if("y" %in% names(mapping)) yvar<-paste(mapping[["y"]])
        if("x" %in% names(mapping)) xvar<-paste(mapping[["x"]])

        direction=ifelse(reverse,-1,1)
        ggplot(data,aes_string(x=xvar,y=yvar,fill=fillvar))+
                geom_area(alpha=alpha)+
                geom_line(position="stack",size=size)+
                scale_fill_brewer(palette=palette,direction=direction)

}

