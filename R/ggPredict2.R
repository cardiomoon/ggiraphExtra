#'Visualize predictions from the results of various model fitting functions.
#'@param fit a model object for which prediction is desired.
#'@param colorn Integer. Number of subgroups of color variables.
#'@param point Logical. Whether ot not draw each point
#'@param jitter Logical. Whether ot not jitter points
#'@param se Logical. Whether ot not draw se
#'@param colorAsFactor Logical. Whether ot not treat color variable as categorical variable
#'@param digits An integer indicating the number of decimal places
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@param ... additional arguments affecting the predictions produced.
#'@export
#'
#'@importFrom ggplot2 geom_ribbon geom_jitter
#'@importFrom ggiraph geom_line_interactive
#'@examples
#'require(moonBook)
#'require(ggplot2)
#'require(ggiraph)
#fit=lm(NTAV~age*weight,data=radial)
#'ggPredict(fit,point=TRUE)
#'require(TH.data)
#'fit1=glm(cens~pnodes*age*horTh,data=GBSG2,family=binomial)
#'ggPredict(fit1,interactive=TRUE)
#'ggPredict(fit1,colorn=100,interactive=TRUE)
ggPredict2=function(fit,colorn=4,point=FALSE,se=FALSE,jitter=FALSE,
                   colorAsFactor=FALSE,digits=2,interactive=FALSE,...) {

#colorn=4;point=FALSE;se=FALSE;jitter=FALSE;colorAsFactor=FALSE;digits=2;interactive=FALSE

(count=length(names(fit$model))-1)

if(count>3) {
        warning("maximum three independent variables are allowed")
        return
}
xname <- facetname <- colorname<-yname <- NULL
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

data<-fit$model

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
                colorcount=colorn


        } else{
                if(is.numeric(data[[colorname]])&(length(unique(data[[colorname]]))>6)){
                        newcolor=seq(from=min(data[[colorname]],na.rm = T),to=max(data[[colorname]],na.rm=T),
                                     length.out=colorn)
                        colorcount=colorn
                } else{

                        newcolor = unique(data[[colorname]])
                        colorcount=length(unique(data[[colorname]]))
                }

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


result <- predict(fit, newdata = newdata, type = "response",se.fit=TRUE)
newdata[[yname]]<-result$fit
newdata$se.fit<-result$se.fit
newdata$ymax<-newdata[[yname]]+result$se.fit
newdata$ymin<-newdata[[yname]]-result$se.fit

newdata

method=""
if("glm" %in% class(fit)){
        method="glm"
} else if(class(fit)=="lm"){
        method="lm"
}

if(colorcount!=colorn) {
        fit2eq=function(fit){
                slope=round(fit$coef[2],digits)
                intercept=round(fit$coef[1],digits)
                if(method=="lm"){
                        equation=paste0("y = ",round(slope,digits),"*x ",ifelse(intercept>=0,"+","-"),round(intercept,digits))
                } else if(method=="glm"){
                        equation=paste0("y =1/(1+exp(-( ",round(slope,digits),"*x ",ifelse(intercept>=0,"+","-"),round(intercept,digits),")))")
                }
        }

        temp=paste0("function(",yname,",",xname,",digits=",digits,",...) {fit=",method,"(",yname,"~",xname,");fit2eq(fit)}")

        getEquation=eval(parse(text=temp))

        long=eval(parse(text=paste0("ddply(newdata,.(",colorname,",",facetname,"),splat(getEquation))")))
        colnames(long)[3]="tooltip"

        newdata2=merge(newdata,long,by=c(colorname,facetname))

} else {
        fit2eq2=function(fit){
                intercept=fit$coef[1]+fit$coef[3]*newcolor
                slope=fit$coef[2]+fit$coef[4]*newcolor
                if(method=="lm"){
                        equation=paste0("y = ",round(slope,digits),"*x ",ifelse(intercept>=0,"+","-"),round(intercept,digits))
                } else if(method=="glm"){
                        equation=paste0("y =1/(1+exp(-( ",round(slope,digits),"*x ",ifelse(intercept>=0,"+","-"),round(intercept,digits),")))")
                }
                equation
        }

        temp=paste0("function(",yname,",",xname,",",colorname,",digits=",digits,",...) {fit=",method,"(",yname,"~",xname,"*",colorname,");fit2eq2(fit)}")

        getEquation2=eval(parse(text=temp))

        res=eval(parse(text=paste0("ddply(data,.(",facetname,"),splat(getEquation2))")))
        colnames(res)[2:ncol(res)]=newcolor
        long=reshape2::melt(res,facetname,variable.name=colorname,value.name="tooltip")

        newdata2=merge(newdata,long,by=c(colorname,facetname))
}
if(!is.null(facetname)) newdata2$tooltip=paste0("for ",facetname,"=",newdata2[[facetname]],"\n",newdata2$tooltip)
if(is.numeric(newdata2[[colorname]])) newdata2[[colorname]]=round(newdata2[[colorname]],digits)
if(!is.null(colorname)) newdata2$tooltip=paste0("for ",colorname,"=",newdata2[[colorname]],"\n",newdata2$tooltip)
newdata2$data_id=rownames(newdata2)
p<-ggplot(data=newdata2,aes_string(y=yname,x=xname,color=colorname,group=colorname))
if(point) {
        if(colorAsFactor) data[[colorname]]=factor(data[[colorname]])
        data$data_id=rownames(data)
        data$tooltip=paste0(data$data_id,"\n",xname,"=",data[[xname]],"\n",yname,"=",data[[yname]])
        if(jitter) p<-p+geom_jitter(data=data,width=0,height=0.05)
        else p<-p+geom_point_interactive(data=data,aes_string(data_id="data_id",tooltip="tooltip"))

}
p<-p+ geom_line_interactive(aes(tooltip=tooltip,data_id=data_id))
if(!is.null(facetname)) p<-p+facet_wrap(as.formula(paste0("~",facetname)))
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

# fit=lm(NTAV~age*DM*HBP,data=radial)
# ggPredict2(fit,interactive=TRUE,point=TRUE)
# require(TH.data)
# fit1=glm(cens~pnodes*age*horTh,data=GBSG2,family=binomial)
# ggPredict2(fit1,interactive=TRUE,colorn=100)
