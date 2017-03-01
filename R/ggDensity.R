#' Add comma to vectors
#' @param ... Argument passed to paste0
#' @export
pastecomma=function(...){
        paste(...,sep=",")
}


#'Make a density plot with histogram
#' @param data a data.frame
#' @param mapping Set of aesthetic mappings created by aes or aes_.
#' @param linecolor Color of density curve
#' @param addhist Whether add histogram or not
#' @importFrom ggplot2 aes geom_line geom_density
#' @export
#' @examples
#'require(ggplot2)
#'require(moonBook)
#'ggDensity(acs,aes(x=age))
#'ggDensity(acs,aes(x=age,color=sex,fill=sex),addhist=FALSE)
#'ggDensity(acs,aes(x=age,color=sex,fill=sex))
#'ggDensity(acs,aes(x=age,fill=sex),addhist=FALSE)
#'ggDensity(acs,aes(x=age,color=sex))
ggDensity=function(data,mapping,linecolor="red",addhist=TRUE){

         # data<-acs; mapping<-aes(x=age);linecolor="red";addhist=TRUE

        colorvar<-fillvar<-facetvar<-yvar<-groupvar<-NULL
        if("fill" %in% names(mapping)) fillvar<-paste(mapping[["fill"]])
        if("colour" %in% names(mapping)) colorvar<-paste(mapping[["colour"]])
        if("facet" %in% names(mapping)) facetvar<-paste(mapping[["facet"]])
        if("y" %in% names(mapping)) yvar<-paste(mapping[["y"]])
        (groupvar=unique(c(colorvar,fillvar)))

        if(!is.null(groupvar)){
        if(length(groupvar)==1) {
                if(is.numeric(data[[groupvar]])) data[[groupvar]]=factor(data[[groupvar]])
        } else{
                for(i in 1:length(groupvar)){
                        if(is.numeric(data[[groupvar[i]]])) data[[groupvar[i]]]=factor(data[[groupvar[i]]])
                }
        }
        }
        assigntemp<-settemp<-c()
        if(is.null(fillvar)) settemp<-c(settemp,paste0("fill='cornsilk'"))
        if(is.null(colorvar)) settemp<-c(settemp,paste0("color='grey50'"))
        if(is.null(yvar)) assigntemp="y=..density.."
        p<-ggplot(data,mapping)
        if(addhist){
                temp="geom_histogram("
                if(length(assigntemp)>0) temp=paste0(temp,"aes(",assigntemp,"),")
                if(length(settemp)>0) temp=paste0(temp,Reduce(pastecomma,settemp))
                temp=paste0(temp,",position='dodge')")
                p<-p+eval(parse(text=temp))
        }

        if(is.null(groupvar)){
                settemp<-c()
                if(is.null(fillvar)) settemp<-c(settemp,paste0("fill=linecolor"))
                if(is.null(colorvar)) settemp<-c(settemp,paste0("color=NA"))
                temp="geom_density("
                if(length(settemp)>0) temp=paste0(temp,Reduce(pastecomma,settemp),",alpha=0.2")
                temp=paste0(temp,")")

                p<-p+eval(parse(text=temp))
                if(is.null(groupvar)) {
                        p<-p+geom_line(color=linecolor,stat='density')
                } else {
                        p<-p+geom_line(stat='density')
                }


        } else{
                p<-p+geom_density(alpha=0.2)
                p<-p+geom_line(stat='density')
         }
        p
}



