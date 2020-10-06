#' Make numeric column of a data.frame to factor
#'
#' @param data a data.frame
#' @param colnames Column names to be converted
#' @param maxfactorno maximum unique value of column
#' @export
num2factorDf=function(data,colnames,maxfactorno=6){
    if(!is.null(colnames)){
        for(i in 1 :length(colnames)){
            name<-colnames[i]
            if(is.numeric(data[[name]])&(length(unique(data[[name]]))<=maxfactorno)){
                data[[name]]<-factor(data[[name]])
            }

        }
    }
    data
}

#' Unselect numeric column of a data.frame
#'
#' @param data a data.frame
#' @param colnames Column names to be converted
#' @param maxfactorno maximum unique value of column
#' @export
unselectNumeric=function(data,colnames,maxfactorno=6){
        result=c()
        if(!is.null(colnames)){
                for(i in 1 :length(colnames)){
                        name<-colnames[i]
                        if(is.numeric(data[[name]])&(length(unique(data[[name]]))>maxfactorno)){
                                result=c(result,name)
                        }

                }
        }
        result<-setdiff(colnames,result)
        result
}


#'Make a regression equation of a model
#'
#'@param model A model of class "lm" or"glm" or"loess"
#'@param digits integer indicating the number of decimal places
#'@export
makeEq=function(model,digits=2) {

       # model=loess(mpg~wt,data=mtcars);digits=2
       # str(model)
       # summary(model)


    intercept<-model$coef[1]
    slope<-model$coef[2]
    (eq=paste0("method=",class(model)[1]))
    if("glm" %in% class(model)){
        eq=paste0(eq,"<br>y=1/(1+exp(-(",round(slope,digits),"x",ifelse(intercept>=0,"+","-"),abs(round(intercept,digits)),")))")
    }
    else if("lm" %in% class(model)){
        eq=paste0(eq,"<br>y = ",round(intercept,2),ifelse(slope>=0,"+","-"),abs(round(slope,2)),"x")
        if(length(model$coef)>2){
            for(i in 3:length(model$coef))
            eq=paste0(eq,ifelse(model$coef[i]>=0,"+","-"),abs(round(model$coef[i],2)),"x^",(i-1))
        }
    }
    if((class(model)[1]=="lm")|(class(model)[1]=="glm")){
    pval=NULL
    if(nrow(model$model)>1) pval=summary(model)$coef[2,4]

    if(!is.null(pval)) {
            if(pval<0.0001) eq=paste0(eq,"<br>p < 0.0001")
            else eq=paste0(eq,"<br>p = ",sprintf("%0.4f",pval))
    }
    }
    eq

}

#'Make a data.frame of yhat with a model
#'
#'@param model A model of class "lm" or"glm" or"loess"
#'@param x A optional vector of explanatory variable
#'@param n number of observations.
model2df=function(model,x=NULL,n=100){
    #  x=NULL;n=100
    # model=loess(formula = mpg ~ poly(wt,2),data=mtcars);digits=2
    # str(model)

    (dfnames=names(attr(model$terms,"dataClasses")))
    if(is.null(x)){
        if(class(model)[1] %in% c("glm","lm","gam")) {
                x<-model$model[2]
        } else {
                x<-model$x
        }
        x
         # if(is.null(n)) n=length(x)
        (xmin=min(x,na.rm=TRUE))
        (xmax=max(x,na.rm=TRUE))
        (newx=seq(xmin,xmax,length.out=n))
         #newx=x

    } else {
        newx=x
    }
    data1=data.frame(newx)
    colnames(data1)=dfnames[2]
    data1

    if("glm" %in% class(model)){
        newy=predict(model,data1,type="response")
    } else {
        newy=predict(model,newdata=data1)
    }

    data2=data.frame(newy,newx)
    #str(data2)
    colnames(data2)=dfnames[1:2]
    data2
}

#' Make an interactive scatterplot with regression line(s)
#'@param data a data.frame
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param smooth Logical. Add regression lines to the scatter plot
#'@param se Logical. display confidence interval around linear regression? (TRUE by default)
#'@param method smoothing method (function) to use, eg. "lm", "glm", "gam", "loess", "rlm"
#'@param formula formula to use in smoothing function, eg. y ~ x, y ~ poly(x, 2), y ~ log(x)
#'@param fullrange should the fit span the full range of the plot, or just the data
#'@param level level of confidence interval to use (0.95 by default)
#'@param use.count Logical. If true use geom_count instead of geom_point_interactive
#'@param maxfactorno An integer. Maximum unique number of a numeric vector treated as a factor
#'@param digits integer indicating the number of decimal places
#'@param tooltip A character string of column name be included in tooltip. Default value is NULL
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@param use.label Logical. Whether or not use column label in case of labelled data
#'@param use.labels Logical. Whether or not use value labels in case of labelled data
#'@param title The text for plot title
#'@param subtitle The text for plot subtitle
#'@param caption The text for plot caption
#'@param ... other arguments passed on to geom_point
#'@export
#'@importFrom ggplot2 ggplot geom_point stat_smooth aes aes_string position_jitter facet_wrap labs geom_count scale_color_discrete scale_fill_discrete
#'
#'@importFrom ggiraph girafe opts_hover opts_tooltip opts_selection opts_zoom girafe_options geom_point_interactive geom_path_interactive
#'@importFrom mgcv gam
#'@importFrom plyr dlply splat
#'@importFrom stats as.formula predict
#'@importFrom sjlabelled get_labels get_label
#'@examples
#'require(ggplot2)
#'require(ggiraph)
#'require(plyr)
#'ggPoints(aes(x=wt,y=mpg,fill=am),data=mtcars)
#'ggPoints(aes(x=wt,y=mpg),data=mtcars)
#'ggPoints(aes(x=wt,y=mpg,fill=am),data=mtcars,method="lm",interactive=TRUE)
#'ggPoints(aes(x=wt,y=mpg,color=am),data=mtcars,interactive=TRUE)
ggPoints=function(data,mapping, smooth=TRUE,
                  se=TRUE,method="auto",formula=y~x, fullrange=FALSE,level=0.95,
                  use.count=FALSE,
                  maxfactorno=6,digits=2,title=NULL,subtitle=NULL,caption=NULL,
                  use.label=TRUE,use.labels=TRUE,
                  tooltip=NULL,interactive=FALSE,...) {


          # data=mtcars;mapping=aes(x=wt,y=mpg);smooth=TRUE
          # se=TRUE;method="lm";formula=y~x; fullrange=FALSE;level=0.95;
          # maxfactorno=3;digits=2;
          # tooltip=NULL;interactive=TRUE;
          # title=NULL;subtitle=NULL;caption=NULL

    #formula=y~x

    if(method=="auto"){
        if(nrow(data)<1000) {
                method<-"loess"
        } else {
            method<-"gam"
            if(identical(formula,y~x)) formula<-y~s(x,bs="cs")
        }
    }


    #str(mapping)
        xname <- fillname <- facetname <- colorname<-yname <- NULL
        if ("x" %in% names(mapping))
                xname <- getMapping(mapping,"x")
        if ("y" %in% names(mapping))
                yname <- getMapping(mapping,"y")
        if ("fill" %in% names(mapping))
                fillname <- getMapping(mapping,"fill")
        if ("colour" %in% names(mapping))
                colorname <- getMapping(mapping,"colour")
        if ("facet" %in% names(mapping))
                facetname <- getMapping(mapping,"facet")

        name=names(mapping)
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
    # xname=paste(mapping[["x"]])
    #
    # yname=paste(mapping[["y"]])
    # facetname=NULL
    # if("facet" %in% names(mapping)) facetname<-paste(mapping[["facet"]])
    # colorname=NULL
    # if("colour" %in% names(mapping)) colorname<-paste(mapping[["colour"]])
        xlevels<-NULL
        if(is.character(data[[xname]])) data[[xname]]=factor(data[[xname]])
        if(is.factor(data[[xname]])) {
                xlevels=levels(data[[xname]])
                data[[xname]]=as.numeric(data[[xname]])
        }




    (formulas=as.character(formula))
    (right=gsub("x",xname,formulas[3]))
    if(grepl("poly(",right,fixed=TRUE)==TRUE){
        (equation=unlist(strsplit(right,",")))
        number=as.numeric(unlist(strsplit(equation[2],")")))
        temp=xname
        for(i in 2:number){
            temp=paste0(temp,"+I(",xname,"^",i,")")
        }
        right=temp
    }
    myformula=paste0(yname,"~",right)
    myformula


    temp=paste0("function(",yname,",",xname,",...){",method,"(",myformula)
    if(method=="glm") temp=paste0(temp,",family=binomial")
    (temp=paste0(temp,")}"))

    (grepModel=eval(parse(text=temp)))


    groupvar<-NULL
    (groupname=setdiff(names(mapping),c("x","y")))

    if(length(groupname)>0) (groupvar=getMapping(mapping,groupname))
    if(!is.null(groupvar))  {
            data=num2factorDf(data,groupvar,maxfactorno=maxfactorno)
            groupvar=unselectNumeric(data,groupvar,maxfactorno=maxfactorno)
            if(!is.null(facetname)) groupvar=c(groupvar,facetname)
    }
    data$id=rownames(data)
    if(is.null(tooltip)) data$tooltip=data$id
    else data$tooltip=data[[tooltip]]
    data$tooltip=paste0(data$tooltip,"<br>",xname,"=",data[[xname]],"<br>",yname,"=",data[[yname]])
    data

    p<-ggplot(data,mapping)


    if(method=="glm") {
            if(smooth) p<-p+stat_smooth(method='glm',formula=formula,method.args=list(family='binomial'),se=se,fullrange=fullrange)
            if(is.null(fillname)){
                    if(nrow(data)<1000){
                         p<-p+geom_point_interactive(aes_string(data_id="id",tooltip="tooltip"),
                                        position=position_jitter(width=0.3,height=0.06),alpha=0.5,...)
                    } else{
                            p<-p+geom_point(position=position_jitter(width=0.3,height=0.06),alpha=0.5,...)

                    }
            }
            else {
                    if(nrow(data)<1000){
                    p<-p+geom_point_interactive(aes_string(data_id="id",tooltip="tooltip"),shape=21,
                                             position=position_jitter(width=0.3,height=0.06),alpha=0.5,...)
                    } else{
                            p<-p+geom_point(shape=21,position=position_jitter(width=0.3,height=0.06),alpha=0.5,...)
                    }
            }
    } else {
            if(smooth) p<-p+ stat_smooth(method=method,formula=formula,se=se,fullrange=fullrange)
            if(is.null(fillname)){
                    if(use.count){
                       p<-p+geom_count(...)
                    } else{
                            if(nrow(data)<1000){
                       p<-p+geom_point_interactive(aes_string(data_id="id",tooltip="tooltip"),...)
                            } else{
                                    p<-p+geom_point(...)
                            }

                    }
            } else {
                    if(use.count){
                            p<-p+geom_count(...)
                    } else{
                    if(nrow(data)<1000){
                            p<-p+geom_point_interactive(aes_string(data_id="id",tooltip="tooltip"),shape=21,...)
                    } else{
                            p<-p+geom_point(shape=21,...)
                    }
                    }

            }

             # p<-p+geom_point_interactive(aes_string(data_id="id",tooltip="tooltip"),shape=21)

    }



    if(smooth&(method %in% c("lm","glm"))){
    if(length(groupvar)<1){
        model=splat(grepModel)(data)

        model
        (equation=makeEq(model))

        data2=model2df(model)
        nrow(data2)
        data2$id=1
        data2$tooltip=equation



        if(is.null(colorname)){
        p<-p+geom_path_interactive(aes_string(x=xname,y=yname,data_id="id",tooltip="tooltip"),
                                   color="blue",data=data2)
        } else {
                data2[[colorname]]=min(data[[colorname]])

        p<-p+geom_path_interactive(aes_string(x=xname,y=yname,data_id="id",tooltip="tooltip"),
                                   color="blue",data=data2)
        }
    } else {

        ## exclude subset data with nrow<1
         data<-ddply(data,groupvar,function(d) {if(nrow(d)>1) d else NULL})

        data

        (models=dlply(data,groupvar,splat(grepModel)))
        strata=attr(models,"split_labels")
        for(i in 1:length(models)){


            if(fullrange){
                (xmin=min(data[[xname]],na.rm=TRUE))
                (xmax=max(data[[xname]],na.rm=TRUE))
                newx=seq(xmin,xmax,length.out=100)

                data2=model2df(models[[i]],x=newx)
            } else{
                data2=model2df(models[[i]])
            }

            data2$tooltip=makeEq(models[[i]])
            data2$id=i
            if(!is.null(strata)){
                for(j in 1:ncol(strata)){
                    data2=cbind(data2,j=strata[i,j])
                    colnames(data2)[ncol(data2)]=names(strata)[j]
                    data2$tooltip=paste0("for ",names(strata)[j],"=",strata[i,j],"<br>",data2$tooltip)
                }
            }
            if(is.null(colorname)){
                    p<-p+geom_path_interactive(aes_string(x=xname,y=yname,data_id="id",tooltip="tooltip"),
                                       color="blue",data=data2)
            } else{
                    p<-p+geom_path_interactive(aes_string(x=xname,y=yname,data_id="id",tooltip="tooltip",
                                                          color=colorname),
                                               data=data2)
            }
        }
    }
    }
     if(is.null(subtitle)) subtitle=paste0("smoothing method=",method)
     p<-p+labs(title=title,subtitle=subtitle,caption=caption)

        #str(data2)
    if(!is.null(facetname)) {
        formula1=as.formula(paste0("~",facetname))
        p<-p+facet_wrap(formula1)
    }

    if(!is.null(xlevels)) p<-p+scale_x_continuous(breaks=1:length(xlevels),labels=xlevels)
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
    if(interactive){
        tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
        #hover_css="fill-opacity=.3;cursor:pointer;stroke:gold;"
        hover_css="r:4px;cursor:pointer;stroke-width:2px;"
        selected_css = "fill:#FF3333;stroke:black;"
        p<-girafe(ggobj=p)
        p<-girafe_options(p,
                          opts_hover(css=hover_css),
                          opts_tooltip(css=tooltip_css,opacity=.75),
                          opts_selection(css=selected_css),
                          opts_zoom(min=1,max=10))
    }
    p

}



