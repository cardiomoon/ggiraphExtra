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
#' @param use.label Logical. Whether or not use column label in case of labelled data
#' @param use.labels Logical. Whether or not use value labels in case of labelled data
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
ggDensity=function(data,mapping,linecolor="red",addhist=TRUE,use.label=TRUE,use.labels=TRUE){

         # data<-acs; mapping<-aes(x=age);linecolor="red";addhist=TRUE
         # linecolor="red";addhist=TRUE;use.label=TRUE;use.labels=TRUE

        colourvar<-fillvar<-facetvar<-yvar<-groupvar<-NULL
        # if("fill" %in% names(mapping)) fillvar<-paste(mapping[["fill"]])
        # if("colour" %in% names(mapping)) colourvar<-paste(mapping[["colour"]])
        # if("facet" %in% names(mapping)) facetvar<-paste(mapping[["facet"]])
        # if("y" %in% names(mapping)) yvar<-paste(mapping[["y"]])
        name=names(mapping)
        xlabels<-ylabels<-filllabels<-colourlabels<-xlab<-ylab<-filllab<-colourlab<-NULL
        for(i in 1:length(name)){
                (varname=paste0(name[i],"var"))
                (labname=paste0(name[i],"lab"))
                (labelsname=paste0(name[i],"labels"))
                temp=getMapping(mapping,name[i])
                # if(length(temp)>1) temp=temp[-1]
                assign(varname,temp)
                x=eval(parse(text=paste0("data$",eval(parse(text=varname)))))
                assign(labname,attr(x,"label"))
                assign(labelsname,get_labels(x))
        }
        (groupvar=unique(c(colourvar,fillvar)))

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
        if(is.null(colourvar)) settemp<-c(settemp,paste0("colour='grey50'"))
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
                if(is.null(colourvar)) settemp<-c(settemp,paste0("colour=NA"))
                temp="geom_density("
                if(length(settemp)>0) temp=paste0(temp,Reduce(pastecomma,settemp),",alpha=0.2,size=0.1")
                temp=paste0(temp,")")

                p<-p+eval(parse(text=temp))
                if(is.null(groupvar)) {
                        p<-p+geom_line(color=linecolor,stat='density',size=0.2)
                } else {
                        p<-p+geom_line(stat='density',size=0.2)
                }


        } else{
                p<-p+geom_density(alpha=0.2,size=0.2)
                p<-p+geom_line(stat='density',size=0.2)
        }
        p
        if(use.labels) {
                if(!is.null(xlabels)) p<-p+scale_x_continuous(breaks=1:length(xlabels),labels=xlabels)
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



