#'Make an interactive plot for an ANCOVA model
#'
#'@param x an object
#'@param ... additional arguments passed to the generic function
#'@export
#'
#'@examples
#'require(moonBook)
#'require(ggplot2)
#'require(ggiraph)
#'ggAncova(radial,aes(age,NTAV,color=sex),interactive=TRUE)
#'fit=lm(NTAV~age+HBP,data=radial)
#'ggAncova(fit,interactive=TRUE)
#'ggAncova(NTAV~age+DM,data=radial)
ggAncova=function(x,...) UseMethod("ggAncova")

#'@describeIn ggAncova Make an interactive plot for an ANCOVA model
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param use.label Logical. Whether or not use column label in case of labelled data
#'@param use.labels Logical. Whether or not use value labels in case of labelled data
#'@export
ggAncova.default=function(x,mapping,use.label=TRUE,use.labels=TRUE,...){
    # x=radial
    # mapping=aes(age,NTAV,color=sex)
    # interactive=TRUE

    data<-x

    xvar<-yvar<-groupvar<-NULL
    name=names(mapping)
    name
    xlabels<-ylabels<-filllabels<-colourlabels<-xlab<-ylab<-colourlab<-filllab<-NULL
    for(i in 1:length(name)){
            (varname=paste0(name[i],"var"))
            (labname=paste0(name[i],"lab"))
            (labelsname=paste0(name[i],"labels"))
            temp=getMapping(mapping,name[i])
            if(length(temp)>1) temp=temp[-1]
            assign(varname,temp)
            tempx=eval(parse(text=paste0("x$",eval(parse(text=varname)))))
            assign(labname,attr(tempx,"label"))
            assign(labelsname,get_labels(tempx))
    }

    if(is.null(xvar)|is.null(yvar)) warning("Both x and y aesthetics are should be mapped")
    (groupname=setdiff(names(mapping),c("x","y")))
    if(length(groupname)>0) (groupvar=getMapping(mapping,groupname))

    A=groupvar[1]
    formula=as.formula(paste(yvar,"~",xvar,"+",A))
    if(use.labels) data[[A]]=factor(data[[A]],labels=colourlabels)
    p<-ggAncova.formula(formula,data,...)
    if(use.labels) {
            if(!is.null(xlabels)) p<-p+scale_x_continuous(breaks=1:length(xlabels),labels=xlabels)
            if(!is.null(ylabels))  p<-p+scale_y_continuous(breaks=1:length(ylabels),labels=ylabels)
            if(!is.null(filllabels)) p=p+scale_fill_discrete(labels=filllabels)
            #if(!is.null(colourlabels)) p=p+scale_color_discrete(labels=colourlabels)
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


#'@describeIn ggAncova Make an interactive plot for an ANCOVA model
#'@importFrom stats lm
#'@param data a data.frame
#'@export
ggAncova.formula=function(x,data,...){

    # print(df)
    formula <- x
    df=data
    fit=lm(formula,data=df)
    #summary(fit)
    if(length(names(fit$model))!=3) {
        print("only one independent variable and one covariate are allowed")
        return()
    }
    (y=names(fit$model)[1])
    (x=names(fit$model)[2])
    (A=names(fit$model)[3])
    if((!is.numeric(df[[x]])) &(is.numeric(df[[A]]))){
        temp=A
        A=x
        x=temp
    } else if((is.numeric(df[[x]])) &(is.numeric(df[[A]]))){
        df[[A]]=factor(df[[A]])
        return(ggAncova.formula(formula,df,...))
    } else if((!is.numeric(df[[x]])) &(!is.numeric(df[[A]]))){
        print("only one independent variable and one covariate are allowed")
        return()
    }
    ggAncova.lm(fit,...)

}


#'@describeIn ggAncova Make an interactive plot for an ANCOVA model
#'
#'@param label A character string of column name be assigned to the label
#'@param digits An integer indicating the number of decimal places
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@export
ggAncova.lm=function(x,label=NULL,digits=1,interactive=FALSE,...){
    # print(df)
    fit<-x
    df=fit$model

    #summary(fit)
    if(length(names(fit$model))!=3) {
        print("only one independent variable and one covariate are allowed")
        return()
    }
    (y=names(fit$model)[1])
    (x=names(fit$model)[2])
    (A=names(fit$model)[3])
    if((!is.numeric(df[[x]])) &(is.numeric(df[[A]]))){
        temp=A
        A=x
        x=temp
    } else if((is.numeric(df[[x]])) &(is.numeric(df[[A]]))){
        df[[A]]=factor(df[[A]])
        formula=as.formula(paste(y,"~",x,"+",A))
        fit=lm(formula,df)
        return(ggAncova.lm(fit,label=label,digits=digits,interactive=interactive))
    } else if((!is.numeric(df[[x]])) &(!is.numeric(df[[A]]))){
        print("only one independent variable and one covariate are allowed")
        return()
    }
    df$all=rep("all",nrow(df))
    df$colour=factor(df[[A]])
    if(is.null(label)) {
        df$label=paste0(df[[A]],"<br>",x,"=",round(df[[x]],1),"<br>",y,"=",round(df[[y]],digits))
    } else df$label=df[[label]]
    df$data_id=rownames(df)
    coef=fit$coef
    slope=rep(coef[2],length(coef)-1)
    intercept=coef[1]
    for(i in 3:length(coef)) intercept=c(intercept,coef[1]+coef[i])
    name=levels(df[[A]])
    xmin=min(df[[x]])
    xmin=rep(xmin,length(coef)-1)
    xmax=max(df[[x]])
    xmax=rep(xmax,length(coef)-1)
    ymin=xmin*slope+intercept
    ymax=xmax*slope+intercept
    df1=data.frame(name,slope,intercept,xmin,ymin,xmax,ymax)
    colnames(df1)[1]=A
    df1$colour=df1[[A]]
    # print(df1)
    name2=rep(name,2)
    x2=c(df1$xmin,df1$xmax)
    y2=c(df1$ymin,df1$ymax)
    slope2=rep(df1$slope,2)
    intercept2=rep(df1$intercept,2)
    df2=data.frame(name2,x2,y2,slope2,intercept2)
    colnames(df2)=c(A,x,y,"slope","intercept")
    df2$color=df2[[A]]
    df2$tooltip=paste0(A,"=",df2[[A]],"<br>y=",round(df2$slope,1),"*x +",round(df2$intercept,1))
    df2$data_id=1:nrow(df2)
    # print(df2)

    p<-ggplot(data=df,aes_string(x=x,y=y,colour="colour",fill=A))+
        geom_point_interactive(aes_string(tooltip="label",data_id="data_id"))+
        facet_grid(as.formula(paste(".~",A)),margins=TRUE)+
        guides(colour=FALSE,fill=FALSE,linetype=FALSE)+
        #geom_abline(data=df1,aes_string(slope="slope",intercept="intercept",
        #                                colour="colour",linetype="colour"))
        geom_path_interactive(data=df2,aes_string(colour="color",tooltip="tooltip",data_id="data_id",
                                                  linetype="color"))
    if(interactive){
            tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"

            hover_css="r:4px;cursor:pointer;stroke-width:6px;"
            selected_css = "fill:#FF3333;stroke:black;"

            p<-girafe(ggobj=p)
            p<-girafe_options(p,
                              opts_hover(css=hover_css),
                              opts_tooltip(css=tooltip_css),
                              opts_selection(css=selected_css),
                              opts_zoom(min=1,max=10))
    }
    p

}


