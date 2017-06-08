#'Visualize the multiple linear/logistic regression model with or without interaction
#'
#'@param x Object to ggPredict
#'@param ... additional arguments passed to the generic function
#'@export
#'
#'@examples
#'require(ggplot2)
#'require(ggiraph)
#'require(moonBook)
#'require(TH.data)
#'ggPredict(radial,aes(y=NTAV,x=age,color=sex),method="lm",se=TRUE)
#'fit=lm(NTAV~age*DM,data=radial)
#'ggPredict(fit,point=TRUE,interactive=TRUE)
#'fit1=lm(NTAV~age*weight,data=radial)
#'ggPredict(fit1,interactive=TRUE,point=TRUE)
#'fit2=glm(cens~pnodes*horTh,data=GBSG2)
#'ggPredict(fit2,se=TRUE,interactive=TRUE)
#'fit3=glm(cens~pnodes*age,data=GBSG2)
#'ggPredict(fit3,colorn=100,interactive=TRUE)
#'fit4=glm(cens~pnodes*age*horTh,data=GBSG2)
#'ggPredict(fit4,interactive=TRUE)
ggPredict <- function(x,...) UseMethod("ggPredict")

#'@describeIn ggPredict Visualize the multiple linear/logistic regression model with or without interaction
#'
#'@export
ggPredict.lm=function(x,...){

        fit<-x

        names(fit$model)
        (count=length(names(fit$model))-1)

        interaction<-NULL
        if(length(unlist(strsplit(deparse(fit$terms),"*",fixed=TRUE)))==count) interaction<-TRUE
        else if(length(unlist(strsplit(deparse(fit$terms),"+",fixed=TRUE)))==count) interaction<-FALSE

        if(count>3) {
                warning("maximum three independent variables are allowed")
                return
        }
        if(is.null(interaction)){
                warning("only completely crossing or non-crossing models are allowed")
                return
        }

        (yname=names(fit$model)[1])
        (xname=names(fit$model)[2])
        temp=paste0("aes(y=",yname,",x=",xname)
        if(count>1) {
                (colorname=names(fit$model)[3])
                temp=paste0(temp,",color=",colorname)
        }
        if(count>2){
                (facetname=names(fit$model)[4])
                temp=paste0(temp,",facet=",facetname)
        }
        temp=paste0(temp,")")
        mapping=eval(parse(text=temp))
        ggPredict(x=fit$model,mapping=mapping,method="lm",interaction=interaction,...)
}

#'@describeIn ggPredict Visualize the multiple linear/logistic regression model with or without interaction
#'
#'@export
ggPredict.glm=function(x,...){

        fit<-x
        (count=length(names(fit$model))-1)

        interaction<-NULL
        if(length(unlist(strsplit(deparse(fit$terms),"*",fixed=TRUE)))==count) interaction<-TRUE
        else if(length(unlist(strsplit(deparse(fit$terms),"+",fixed=TRUE)))==count) interaction<-FALSE

        if(count>3) {
                warning("maximum three independent variables are allowed")
                return
        }
        if(is.null(interaction)){
                warning("only completely crossing or non-crossing models are allowed")
                return
        }

        (yname=names(fit$model)[1])
        (xname=names(fit$model)[2])
        temp=paste0("aes(y=",yname,",x=",xname)
        if(count>1) {
                (colorname=names(fit$model)[3])
                temp=paste0(temp,",color=",colorname)
        }
        if(count>2){
                (facetname=names(fit$model)[4])
                temp=paste0(temp,",facet=",facetname)
        }
        temp=paste0(temp,")")
        mapping=eval(parse(text=temp))
        ggPredict(x=fit$model,mapping=mapping,method="glm",interaction=interaction,...)
}

#'@describeIn ggPredict Visualize the effect of interaction between two continuous independent variables on a response variable
#'
#'@param mapping Set of aesthetic mappings created by aes or aes_.
#'@param colorn Integer. Number of subgroups of color variables.
#'@param method "lm" or "glm"
#'@param interaction Logical. Whether or not include interaction between independent variables
#'@param point Logical. Whether ot not draw each point
#'@param se Logical. Whether ot not draw se
#'@param colorAsFactor Logical. Whether ot not treat color variable as categorical variable
#'@param digits An integer indicating the number of decimal places
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'
#'@importFrom ggplot2 geom_jitter geom_ribbon
#'@importFrom ggiraph geom_line_interactive
#'@export
ggPredict.default=function(x,mapping,colorn=4,method="glm",interaction=TRUE,point=FALSE,se=FALSE,
                           colorAsFactor=FALSE,digits=2,interactive=FALSE,...) {

        data<-x
        xname <- facetname <- colorname<-yname <- NULL
        if ("x" %in% names(mapping))
                xname <- paste(mapping[["x"]])
        if ("y" %in% names(mapping))
                yname <- paste(mapping[["y"]])
        if ("colour" %in% names(mapping))
                colorname <- paste(mapping[["colour"]])
        if ("facet" %in% names(mapping))
                facetname <- paste(mapping[["facet"]])


        operator=ifelse(interaction,"*","+")
        temp=paste0(method,"(",yname,"~",xname)
        if(!is.null(colorname)) temp=paste0(temp,operator,colorname)
        if(!is.null(facetname)) temp=paste0(temp,operator,facetname)
        temp=paste0(temp,",data=data")
        if(method=="glm") temp=paste0(temp,",family='binomial'")
        temp=paste0(temp,")")

        model <- eval(parse(text=temp))
        #print(summary(model))
        (uniqueXNo=length(unique(data[[xname]])))
        if(uniqueXNo>colorn) {
                newx = pretty(data[[xname]],20)
                xcount=1
        } else {
                newx = unique(data[[xname]])
                if(is.numeric(data[[xname]])) xcount=1
                else xcount=length(unique(data[[xname]]))
        }

        if(!is.null(colorname)){
                uniqueColorNo=length(unique(data[[colorname]]))
                if(uniqueColorNo>colorn){
                        newcolor=pretty(data[[colorname]],colorn-1)
                        colorcount=1
                } else{
                        newcolor = unique(data[[colorname]])
                        colorcount=length(unique(data[[colorname]]))
                }
        }
        if(!is.null(facetname)){
                uniqueFacetNo=length(unique(data[[facetname]]))
                if(uniqueFacetNo>colorn){
                        newfacet=pretty(data[[facetname]],colorn-1)
                        facetcount=1
                } else{
                        newfacet = unique(data[[facetname]])
                        facetcount=length(unique(data[[facetname]]))
                }
        }
        if(is.null(colorname)&&is.null(facetname)){
                temp= newx
                newdata=data.frame(temp)
                colnames(newdata)=c(xname)

        } else if(is.null(facetname)){

                newdata <-expand.grid(
                        newx=newx,
                        newcolor=newcolor
                )
                colnames(newdata)=c(xname,colorname)
        } else{
                newdata <-expand.grid(
                        newx=newx,
                        newcolor=newcolor,
                        newfacet=newfacet
                )
                colnames(newdata)=c(xname,colorname,facetname)
        }


        result <- predict(model, newdata = newdata, type = "response",se.fit=TRUE)
        newdata[[yname]]<-result$fit
        newdata$se.fit<-result$se.fit
        newdata$ymax<-newdata[[yname]]+result$se.fit
        newdata$ymin<-newdata[[yname]]-result$se.fit

        #print(summary(model))
        xcount
        if(is.null(facetname)){
                if(is.null(colorname)){
                        newdata$intercept=model$coef[1]
                        newdata$slope=model$coef[2]
                } else if(is.numeric(newdata[[colorname]])){

                        newdata$intercept=model$coef[1]+model$coef[2+xcount]*newdata[[colorname]]
                        newdata$slope=ifelse(interaction,model$coef[2]+model$coef[3+xcount]*newdata[[colorname]],model$coef[2])
                } else{
                        if(!is.factor(newdata[[colorname]])) newdata[[colorname]]=factor(newdata[[colorname]])
                        newdata$start=as.numeric(newdata[[colorname]])-1

                        #newdata$intercept=model$coef[1]
                        newdata$intercept=ifelse(newdata$start==0,model$coef[1],model$coef[1]+model$coef[1+xcount+newdata$start])
                        newdata$slope=model$coef[2]
                        if(interaction) newdata$slope=ifelse(newdata$start==0,model$coef[2],model$coef[2]+model$coef[1+xcount+colorcount-1+newdata$start])
                }
        }
        newdata$tooltip=""
        if(method=="lm"){
                if(!is.null(colorname)) newdata$tooltip=paste0("for ",colorname,"=",newdata[[colorname]])
                if(is.null(facetname)){
                        if(xcount==1) newdata$tooltip=paste0(newdata$tooltip,"\ny=",round(newdata$slope,digits),"*x",ifelse(newdata$intercept>0,"+",""),round(newdata$intercept,digits))
                } else{
                        newdata$tooltip=paste0(newdata$tooltip,"\n",facetname,"=",newdata[[facetname]])
                }
        } else if(method=="glm"){
                if(!is.null(colorname)) newdata$tooltip=paste0("for ",colorname,"=",newdata[[colorname]])
                if(is.null(facetname)){
                        if(xcount==1) newdata$tooltip=paste0(newdata$tooltip,"\ny=1/(1+exp(-(",round(newdata$slope,digits),"x",ifelse(newdata$intercept>=0,"+","-"),abs(round(newdata$intercept,digits)),")))")
                } else{
                        newdata$tooltip=paste0(newdata$tooltip,"\n",facetname,"=",newdata[[facetname]])
                }
        }
        newdata
        newdata$id=rownames(newdata)
        #print(newdata)
        p<-ggplot(data=newdata,mapping=mapping) #+
        #stat_smooth(method="glm",method.args=list(family='binomial'),se=FALSE)

        # ggplot(data=newdata,aes(x=Temp,y=Sample,color=Age,fill=Age,group=Age))+
        #         geom_ribbon(aes(x=Temp,ymin=ymin,ymax=ymax,color=NULL),alpha=0.2)+
        #         geom_line()
        #
        if(point) {
                data$data_id=rownames(data)
                data$tooltip=paste0(data$data_id,"\n",xname,"=",data[[xname]],"\n",yname,"=",data[[yname]])
                if(method=="lm") p<-p+geom_point_interactive(data=data,aes_string(data_id="data_id",tooltip="tooltip"))
                else p<-p+geom_jitter(data=data,width=0,height=0.05)
        }
        newdata
        if(is.null(colorname)) {
                if(se) p<-p+geom_ribbon(aes_string(x=xname,ymin="ymin",ymax="ymax",group=1),alpha=0.2)
                p<-p+geom_line_interactive(aes_string(x = xname, y = yname,tooltip="tooltip",data_id="id",group=1),size=0.75)

        } else {
                if(colorAsFactor) newdata[[colorname]]=factor(newdata[[colorname]])
                if(se) p<-p+geom_ribbon(aes_string(x=xname,ymin="ymin",ymax="ymax",fill=colorname,group=colorname,color=NULL),alpha=0.2)
                p<-p+geom_line_interactive(aes_string(x = xname, y = yname,colour=colorname,group=colorname,
                                                      tooltip="tooltip",data_id="id"),size=0.75)

        }
        if(!is.null(facetname)) p<-p+eval(parse(text=paste0("facet_wrap(~",facetname,")")))

        p
        if(interactive){
                tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
                #hover_css="fill-opacity=.3;cursor:pointer;stroke:gold;"
                hover_css="r:4px;cursor:pointer;stroke:red;stroke-width:3px;"
                selected_css = "fill:#FF3333;stroke:black;"
                p<-ggiraph(code=print(p),tooltip_extra_css=tooltip_css,tooltip_opacity=.75,
                           zoom_max=10,hover_css=hover_css,selected_css=selected_css)
        }
        p


}

