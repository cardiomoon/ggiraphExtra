#'Visualize the effect of interaction between two continuous independent variables on a response variable
#'
#'@param x Object to ggEffect
#'@param ... additional arguments passed to the generic function
#'@export
#'
#'@examples
#'require(ggplot2)
#'require(ggiraph)
#'ggEffect(mtcars,aes(x=wt,y=mpg,color=hp),use.rownames=TRUE,interactive=TRUE)
#'require(moonBook)
#'ggEffect(acs,aes(x=height,y=weight,color=smoking))
#'ggEffect(NTAV~age*smoking,data=radial,interactive=TRUE)
#'fit=lm(age~sex*smoking,data=acs)
#'ggEffect(fit,interactive=TRUE)
#'ggEffect(radial,aes(x=age,y=NTAV,group=smoking))
ggEffect <- function(x,...) UseMethod("ggEffect")


#'@describeIn ggEffect Visualize the effect of interaction between two continuous independent variables on a response variable
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@export
#'@return An interactive plot showing interaction
ggEffect.default <-function(x,mapping,...) {

        xvar<-yvar<-groupvar<-NULL
        (xvar=paste(mapping[["x"]]))
        yvar=paste(mapping[["y"]])
        if(is.null(xvar)|is.null(yvar)) warning("Both x and y aesthetics are should be mapped")
        (groupname=setdiff(names(mapping),c("x","y")))
        if(length(groupname)==1) (groupvar=paste(mapping[groupname]))
        else warning("Only one grouping variable is required")

    formula=as.formula(paste(yvar,"~",xvar,"*",groupvar))
    ggEffect.formula(formula,x,...)
}


#'@describeIn ggEffect Visualize the effect of interaction between two continuous independent variables on a response variable
#'
#'@param data A data.frame
#'@export
ggEffect.formula <-function(x,data,...){

    # print(df)
    formula<-x
    df=data
    fit=lm(formula,data=data)
    if(length(names(fit$model))!=3) {
        print("two independent variables are allowed")
        return
    }
    ggEffect.lm(fit,...)

}

#'@describeIn ggEffect Visualize the effect of interaction between two continuous independent variables on a response variable
#'
#'@param no an integer
#'@param probs A vector of probability weights for obtaining the elements of the vector being sampled.Default value is c(0.10,0.5,0.90)
#'@param point A logical value. If TRUE, draw points
#'@param xvalue A numeric vector
#'@param digits An integer indicating the number of decimal places
#'@param use.rownames If TRUE, use rownames in label
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'
#'@importFrom stats quantile
#'@importFrom utils str
#'@export
ggEffect.lm=function(x,
                     no=1,
                     probs=c(0.10,0.5,0.90),
                     point=TRUE,
                     xvalue=NULL,
                     digits=2,
                     use.rownames=FALSE,
                     interactive=FALSE,...)
{
        fit<-x
        df=fit$model
        coef=fit$coef
        name=colnames(df)
        count=0
        if(is.numeric(df[[2]])) count=count+1
        if(is.numeric(df[[3]])) count=count+2
        name
        count
        if(count==0){
                p<-ggCatepillar(df,aes_string(x=name[1+no],y=name[1],color=name[4-no]))
        } else if(count<3){
                if(use.rownames) {
                        df$label=rownames(df)
                } else {
                        df$id=rownames(df)
                        df$label=paste0(df$id,"<br>",name[4-count],"=",df[[name[4-count]]],"<br>",
                                        name[1+no],"=",round(df[[name[1+no]]],digits),"<br>",
                                        name[1],"=",round(df[[name[1]]],digits))
                }
                df$data_id=1:nrow(df)
                # str(df)
                # coef
                # summary(fit)

                name
                (xvar=name[1+count])
                (color=name[4-count])

                if(is.factor(df[[color]])) names=levels(df[[color]])
                else names=unique(df[[color]])
                # df[[xvar]]
                xmin=min(df[[xvar]],na.rm=TRUE)
                #xmin
                xmin=rep(xmin,length(names))
                xmax=max(df[[xvar]],na.rm=TRUE)
                xmax=rep(xmax,length(names))
                #length(names)
                (intercept=coef[1])
                (slope=coef[2])
                for(i in 2:length(names)){
                        slope=c(slope,coef[2]+coef[2+length(names)+(i-2)])
                        intercept=c(intercept,coef[1]+coef[3+(i-2)])
                }
                ymin=slope*xmin+intercept
                ymax=slope*xmax+intercept
                # slope
                # intercept
                # xmin
                # ymin
                # xmax
                # ymax
                df1=data.frame(names,slope,intercept,xmin,ymin,xmax,ymax)
                # df1
                name2=rep(df1$names,2)
                x2=c(df1$xmin,df1$xmax)
                y2=c(df1$ymin,df1$ymax)
                slope2=rep(df1$slope,2)
                intercept2=rep(df1$intercept,2)
                df2=data.frame(name2,x2,y2,slope2,intercept2)
                colnames(df2)=c(color,"x","y","slope","intercept")
                df2$tooltip=paste0("for ",color,"=",df2[[color]],"<br>y=",round(df2$slope,digits),"*x +",round(df2$intercept,digits))
                df2$data_id=1:nrow(df2)
                # str(df)
                # str(df2)
                p<-ggplot(data=df,aes_string(x=name[1+count],y=name[1],colour=color,tooltip="tooltip",data_id="data_id"))+
                        #stat_smooth(method="lm",se=se,fullrange=TRUE)+
                        geom_path_interactive(data=df2,
                                              aes_string(x="x",y="y"),size=1)
                if(point) p<-p+ geom_point_interactive(aes_string(tooltip="label"))

                # p1<-ggplot(data=df,aes_string(x=name[1+count],y=name[1],colour=color))+
                #     stat_smooth(method="lm",se=se,fullrange=TRUE)+
                #   #  geom_path_interactive(data=df2,
                #    #                       aes_string(x="x",y="y",tooltip="tooltip",data_id="data_id"))+
                #     geom_point_interactive(aes(tooltip=label,data_id=data_id))
                # p1
        } else {
                (z=name[4-no])
                color=name[4-count]
                df$data_id=1:nrow(df)
                if(use.rownames){
                        df$label=rownames(df)
                } else {
                        df$label=paste0(df$data_id,"<br>",z,"=",df[[z]],"<br>",name[1+no],"=",df[[name[1+no]]],"<br>",name[1],"=",df[[name[1]]])
                }
                #str(df)
                if(is.null(xvalue)) {
                        A=quantile(df[[4-no]],probs,na.rm=TRUE)
                } else A=xvalue
                count=length(A)
                labels=as.character(A)
                intercept=coef[1]+coef[4-no]*A
                slope=coef[1+no]+coef[4]*A
                xvar=df[[name[1+no]]]

                xmin=rep(min(xvar,na.rm=TRUE),count)
                xmax=rep(max(xvar,na.rm=TRUE),count)
                ymin=xmin*slope+intercept
                ymax=xmax*slope+intercept
                df1=data.frame(A,intercept,slope,xmin,xmax,ymin,ymax)
                # print(df1)
                name2=rep(df1$A,2)
                x2=c(df1$xmin,df1$xmax)
                y2=c(df1$ymin,df1$ymax)
                slope2=rep(df1$slope,2)
                intercept2=rep(df1$intercept,2)
                df2=data.frame(name2,x2,y2,slope2,intercept2)
                colnames(df2)=c(z,"x","y","slope","intercept")
                df2[[z]]=factor(df2[[z]])
                df2$tooltip=paste0("for ",z,"=",df2[[z]],"<br>y=",round(df2$slope,digits),"*x +",round(df2$intercept,digits))
                df2$data_id=1:nrow(df2)
                # print(df2)
                # str(df)
                #str(df2)
                # name
                #df
                #str(df)

                if(length(unique(df[[z]]))<6) {
                        df[[z]]=factor(df[[z]])
                        p<-ggplot(data=df,aes_string(x=name[1+no],y=name[1],tooltip="label",
                                                     data_id="data_id",colour=z))
                } else {
                        p<-ggplot(data=df,aes_string(x=name[1+no],y=name[1],tooltip="label",
                                                     data_id="data_id"))
                }

                p<-p+ geom_path_interactive(data=df2,
                                            aes_string(x="x",y="y",tooltip="tooltip",color=z),size=1)

                if(point) p<-p + geom_point_interactive()

        }
        if(interactive){
                tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
                #hover_css="fill-opacity=.3;cursor:pointer;stroke:gold;"
                hover_css="r:4px;cursor:pointer;stroke-width:6px;"
                if(interactive) p<-ggiraph(code=print(p),tooltip_extra_css=tooltip_css,tooltip_opacity=.75,
                                           zoom_max=10,hover_css=hover_css)
        }
        p
}
