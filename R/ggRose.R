#'Draw an interactive Rose plot
#'
#'@param data A data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param palette A character string indicating the color palette
#'@param color Bar colour
#'@param size Bar size
#'@param ... other arguments passed on to geom_bar_interactive.
#'@return An interactive Rose plot
#'@export
#'@examples
#'require(moonBook)
#'require(ggplot2)
#'require(ggiraph)
#'require(plyr)
#'ggRose(rose,aes(x=Month,fill=group,y=value),stat="identity",interactive=TRUE)
#'ggRose(acs,aes(x=Dx,fill=smoking),interactive=TRUE)
ggRose=function(data,mapping,palette="Reds",color="black",size=0.1,...){

     p<-ggBar(data,mapping,width=1,color=color,size=size,
             palette=palette,polar=TRUE,...)
    p
}
