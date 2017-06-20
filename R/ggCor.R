#' Draw a heatmap of correlation test
#'
#' @param data A data.frame
#' @param what if 1, correlation, if 2, partial correlation, if 3, semi-partial correlation
#' @param label if 0, no label(default), if 1, use r value as label, if 2, use r value with significant mark as label
#' @param colors colors for low, mid and high correlation values
#' @param title if true, add title to the heatmap
#' @param mode 1 or 2
#' @param digits The number of decimal place
#' @param interactive A logical value. If TRUE, an interactive plot will be returned
#' @param yreverse If true, reverse y axis
#' @param xangle x-axis text angle
#' @param yangle y-axis text angle
#' @param use.label Logical whether or not use label in case of labelled data
#' @param ... further arguments to be passed to cor.test
#' @importFrom mycor mycor
#' @importFrom ggplot2 scale_fill_gradient2 coord_equal geom_text scale_x_discrete scale_y_discrete
#' @importFrom ggiraph geom_tile_interactive
#' @importFrom  stats na.omit
#' @importFrom ppcor pcor spcor
#' @importFrom sjlabelled get_label
#' @export
#' @examples
#' require(mycor)
#' require(ggplot2)
#' require(ggiraph)
#' require(ppcor)
#' ggCor(iris,digits=3,label=3)
#' ggCor(iris,what=3,digits=3,label=3)
#' ggCor(iris,label=3,interactive=TRUE)
#' ggCor(mtcars,interactive=TRUE)
#' ggCor(mtcars,mode=2,interactive=TRUE)
#' ggCor(iris,method="pearson",interactive=TRUE)
ggCor=function(data,what=1,label=0,colors=NULL,title=TRUE,mode=2,digits=2,interactive=FALSE,yreverse=TRUE,
               xangle=45,yangle=0,use.label=FALSE,...){

    # data=acs;what=1;label=0;colors=NULL;title=FALSE;mode=2;interactive=TRUE;yreverse=TRUE
    # digits=2;xangle=45;yangle=0;use.label=FALSE
    data=as.data.frame(data)
    select=sapply(data,is.numeric)
    data=data[select]
    if(what>1){

       data=na.omit(data)
    }
    data
    if(use.label){
            colnames(data)=get_label(data)
            data
    }
    if(what==1)  {
            result=mycor(data,digits=digits,...)
            method=result$out$method
            Lab=paste("Correlation Coeffients by",method)
    } else if(what==2) {
            result<-pcor(data,...)
            result$r=result$estimate
            result$p<-result$p.value
            method=result$method
            Lab=paste("Partial Correlation Coeffients by",method)
    } else if(what==3) {
            result<-spcor(data,...)
            result$r=result$estimate
            result$p<-result$p.value
            method=result$method
            Lab=paste("Semi-Partial Correlation Coeffients by",method)
    }
    # result=mycor(data)

    if(is.null(colors)) colors=c("#6D9EC1","white","#E46726")
    cor_mat<-result$r
    p_mat<-result$p
    diag( cor_mat ) <- NA
    diag( p_mat ) <- NA
    if(mode==2) cor_mat[upper.tri(cor_mat)]=NA
    var1 <- rep( row.names(cor_mat), ncol(cor_mat) )
    var2 <- rep( colnames(cor_mat), each = nrow(cor_mat) )
    cor <- as.numeric(cor_mat)
    cor_mat <- data.frame( var1 = var1, var2 = var2,
                           cor = cor, stringsAsFactors = FALSE )
    pval=as.numeric(p_mat)
    cor_mat$label=ifelse(is.na(cor_mat$cor),"",sprintf(paste0("%0.",digits,"f"),cor_mat$cor))

    if(label==2) cor_mat$label=paste0(cor_mat$label,ifelse(is.na(pval),"",ifelse(pval<0.001,"***",ifelse(pval<0.01,"**",ifelse(pval<0.05,"*","")))))
    else if(label==3) cor_mat$label=paste0(cor_mat$label,"\n",p2chr(pval))
    cor_mat$p=ifelse(is.na(pval),"",ifelse(pval<0.001,"< 0.001",sprintf(" = %0.3f",pval)))
    cor_mat[["tooltip"]] <-
        sprintf("<i>%s</i> vs <i>%s</i>:</br><i>r</i> = %s</br><i>p</i> %s",
                var1, var2, cor_mat$label,cor_mat$p)
    if(mode==2) cor_mat=na.omit(cor_mat)
    # ggplot creation and ggiraph printing ----
    if(mode==1) p <- ggplot(data = cor_mat, aes_string(x = "var1", y = "var2",tooltip="tooltip") )
    else if(mode==2)
            p <- ggplot(data = cor_mat, aes_string(x = "var2", y = "var1",tooltip="tooltip") )
    p<-p+geom_tile_interactive(aes(fill = cor), colour = "grey50") +
        scale_fill_gradient2(low = colors[1], mid = colors[2], high = colors[3], limits = c(-1, 1)) +
        coord_equal()+
        xlab("")+ylab("")

    if(title) {
        #title=paste0(result$out$method,",",result$out$alternative)
        p<-p+ggtitle(Lab)
    }
    if(label>0) p<-p+geom_text(aes(label=label))

    if(mode==2) {
            mynames=rownames(result$r)
    p<-p+scale_x_discrete(limits=mynames[-length(mynames)])
    if(yreverse) p<-p+scale_y_discrete(limits=rev(mynames[-1]))
    else p<-p+scale_y_discrete(limits=mynames[-1])
    p<-p+theme_clean2(xangle=xangle,yangle=yangle)
    p<-p+theme(legend.position=c(0.8,0.8))+labs(fill="r value")
    }

    if(interactive){
            tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
            hover_css="r:4px;cursor:pointer;stroke-width:6px;"
            selected_css = "fill:#FF3333;stroke:black;"
            p<-ggiraph(code=print(p),tooltip_extra_css=tooltip_css,tooltip_opacity=.75,
                       zoom_max=10,hover_css=hover_css,selected_css=selected_css)
    }

    p
}

#' Clean theme for ggCor
#'@param base_size  base font size
#'@param xangle x-axis text angle
#'@param yangle y-axis text angle
#'@importFrom ggplot2 element_text
#'@export
theme_clean2=function(base_size=12, xangle=45,yangle=0){
        theme_grey(base_size) %+replace%
                theme(
                        panel.background=element_blank(),
                        panel.grid=element_blank(),
                        axis.title=element_blank(),
                        axis.text.x=element_text(angle=xangle),
                        axis.text.y=element_text(angle=yangle),
                        axis.ticks.length=unit(0,"cm"),
                        #axis.ticks.margin=unit(0,"cm"),
                        #panel.margin=unit(0,"lines"),
                        #plot.margin=unit(c(0,0,0,0),"lines"),
                        complete=TRUE
                )
}

#' Convert p values to character
#' @param x A vector
#' @export
p2chr=function(x){
        ifelse(is.na(x),"",ifelse(x<0.001,"(<.001)",paste0("(",substr(sprintf("%.3f",x),2,5),")")))
}
