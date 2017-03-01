#' Draw Tukey Honest Significant Differences plot
#'@param tukey A object of class "TukeyHSD", the result of TukeyHSD()
#'@param no An integer specify the order of list
#'@param digits integer indicating the number of decimal places
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@importFrom ggplot2 geom_errorbar geom_hline geom_text ggtitle element_text rel
#'@export
#'@return A (interactive) ggplot
#'@examples
#'require(ggplot2)
#'fm1 <- aov(breaks ~ wool + tension, data = warpbreaks)
#'result=TukeyHSD(fm1, "tension", ordered = TRUE)
#'str(result)
#'ggHSD(result)
ggHSD=function(tukey,no=1,digits=2,interactive=FALSE){
    #result=TukeyHSD(fit)
    result=tukey[[no]]
    name=names(tukey)[no]
    df=data.frame(result)
    df[["sig"]]=ifelse(df$p.adj>0.05,"",ifelse(df$p.adj>0.01,"*","**"))
    df[["Comparison"]]<-rownames(df)
    df[["data_id"]]=1:nrow(df)
    df[["tooltip"]]=paste0(round(df$diff,digits),"(",round(df$lwr,digits),"-",
                      round(df$upr,digits),")<br>p = ",round(df$p.adj,4))
    df
    df2=df[c(2)]
    colnames(df2)[1]="y"
    df2$Comparison=rownames(df2)
    df3=df[c(3)]
    colnames(df3)[1]="y"
    df3[["Comparison"]]=rownames(df3)
    df4=rbind(df2,df3)
    df4[["data_id"]]=1:nrow(df4)
    df4
    lab=paste("Differences in mean levels of",name)
    p<-ggplot(df,aes_string(x="Comparison",y="diff",colour="Comparison",
                     data_id="data_id",tooltip="tooltip"))+
        geom_hline(yintercept=0,linetype=2)+
        geom_errorbar(aes_string(ymin="lwr",ymax="upr"),size=1,width=0.2)+
        geom_text(aes_string(label="sig"),size=7,vjust=0.2)+
        geom_path_interactive(data=df4,aes_string(x="Comparison",y="y",colour="Comparison",
                                                  group="Comparison",
                               data_id="data_id",tooltip="Comparison"))+
        geom_point_interactive(size=3)+
        coord_flip()+
        ylab(lab)+xlab("")+
        guides(colour=FALSE,label=FALSE)+
        ggtitle("95% family-wise confidence level")+
        theme(plot.title=element_text(size=rel(2)),
              axis.text.y=element_text(angle=90,size=rel(1.5),hjust=0.5),
              axis.text.x=element_text(size=rel(1.5)),
              axis.title.x=element_text(size=rel(1.5)))+
        theme(legend.position="none")
    if(interactive) p<-ggiraph(code=print(p),hover_css="r:7px;cursor:pointer;stroke-width:6px;",
                               zoom_max=10)
    p
}

