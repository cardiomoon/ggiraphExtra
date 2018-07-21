#'Visualize predictions from the multiple regression models.
#'@param fit a model object for which prediction is desired.
#'@param colorn Integer. Number of subgroups of color variables.
#'@param point Logical. Whether or not draw each point
#'@param jitter Logical. Whether or not jitter points
#'@param se Logical. Whether or not draw se
#'@param show.summary Logical. Whether or not show summary
#'@param colorAsFactor Logical. Whether or not treat color variable as categorical variable
#'@param digits An integer indicating the number of decimal places
#'@param interactive A logical value. If TRUE, an interactive plot will be returned
#'@param ... additional arguments affecting the predictions produced.
#'@export
#'
#'@importFrom ggplot2 geom_ribbon geom_jitter label_both
#'@importFrom ggiraph geom_line_interactive
#'@examples
#'require(moonBook)
#'require(ggplot2)
#'require(ggiraph)
#'require(plyr)
#'fit=lm(NTAV~age*weight,data=radial)
#'fit=lm(NTAV~age*weight*DM,data=radial)
#'fit=lm(NTAV~age+DM,data=radial)
#'ggPredict(fit,interactive=TRUE)
#'require(TH.data)
#'fit=glm(cens~pnodes*horTh,data=GBSG2,family=binomial)
#'ggPredict(fit,se=TRUE)
#'fit1=glm(cens~pnodes*age,data=GBSG2,family=binomial)
#'ggPredict(fit1)
#'ggPredict(fit1,colorn=100,jitter=FALSE,interactive=TRUE)
#'fit2=glm(cens~pnodes*age*horTh,data=GBSG2,family=binomial)
#'ggPredict(fit2,colorn=100,jitter=FALSE,interactive=TRUE)
ggPredict=function(fit,colorn=4,point=NULL,jitter=NULL,se=FALSE,show.summary=FALSE,
                   colorAsFactor=FALSE,digits=2,interactive=FALSE,...) {

   #fit=glm(cens~pnodes*age*horTh,data=GBSG2,family=binomial)
   #colorn=4;point=TRUE;se=FALSE;jitter=FALSE;colorAsFactor=FALSE;digits=3;interactive=FALSE;show.summary=TRUE

if(show.summary) print(summary(fit))
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

xnumeric=FALSE
if(is.numeric(data[[xname]])) {
        xnumeric=TRUE
} else {
      if(count>1){
          if(is.numeric(data[[colorname]])){
                  temp=xname
                  xname=colorname
                  colorname=temp
                  xnumeric=TRUE
          }
      }
      if(!xnumeric) {
              if(count>2) {
                      if(is.numeric(data[[facetname]])){
                              temp=xname
                              xname=facetname
                              facetname=temp
                              xnumeric=TRUE
                      }
              }
      }
}

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

if(is.null(point)) {
        if(method=="lm") point=TRUE
        else point=FALSE
}
if(is.null(jitter)) {
        if(method=="glm") jitter=TRUE
        else jitter=FALSE
}


makeEq=function(slope,intercept){
        if(method=="lm"){
                equation=paste0("y = ",round(slope,digits),"*x ",ifelse(intercept>=0,"+","-"),abs(round(intercept,digits)))
        } else if(method=="glm"){
                equation=paste0("y =1/(1+exp(-( ",round(slope,digits),"*x ",ifelse(intercept>=0,"+","-"),abs(round(intercept,digits)),")))")
        }
        equation
}

fit2eq=function(fit){

        temp=""
        if(xnumeric){
                slope=round(fit$coef[2],digits)
                intercept=round(fit$coef[1],digits)
        #         if(xcount>1) temp=paste0("\n",xname,":",unique(data[[xname]])[i],"\n")
                 temp=paste0(temp,makeEq(slope,intercept))
        }
        temp
}

# colorn
# colorcount

if(is.null(colorname)){ ## if color variable is absent

        newdata2<-newdata
        newdata2$tooltip=fit2eq(fit)
        newdata2

} else if(colorcount!=colorn) {    ## if color variable is categorical

        temp=paste0("function(",yname,",",xname,",digits=",digits,",...) {fit=",method,"(",yname,"~",xname)
        if(method=="glm") temp=paste0(temp,",family=binomial")
        temp=paste0(temp,");fit2eq(fit)}")
        temp
        getEquation=eval(parse(text=temp))

        temp=paste0("ddply(newdata,.(",colorname)
        if(!is.null(facetname)) temp=paste0(temp,",",facetname)
        temp=paste0(temp,"),splat(getEquation))")
        long=eval(parse(text=temp))
        long
        colnames(long)[ncol(long)]="tooltip"
        newdata2=merge(newdata,long,by=c(colorname,facetname))

} else {       ## if color variable is numeric
        fit2eq2=function(fit){
                equation=""
                if(xnumeric){
                intercept=fit$coef[1]+fit$coef[3]*newcolor
                slope=fit$coef[2]+fit$coef[4]*newcolor
                if(method=="lm"){
                        equation=paste0("y = ",round(slope,digits),"*x ",ifelse(intercept>=0,"+","-"),abs(round(intercept,digits)))
                } else if(method=="glm"){
                        equation=paste0("y =1/(1+exp(-( ",round(slope,digits),"*x ",ifelse(intercept>=0,"+","-"),abs(round(intercept,digits)),")))")
                }
                }
                equation
        }

        temp=paste0("function(",yname,",",xname,",",colorname,",digits=",digits,",...) {fit=",method,"(",yname,"~",xname,"*",colorname)
        if(method=="glm") temp=paste0(temp,",family=binomial")
        temp=paste0(temp,");fit2eq(fit)}")
        temp
        getEquation2=eval(parse(text=temp))

        temp=paste0("ddply(newdata,.(",colorname)
        if(!is.null(facetname)) temp=paste0(temp,",",facetname)
        temp=paste0(temp,"),splat(getEquation2))")
        temp

        options(warn=-1)
        res=eval(parse(text=temp))
        options(warn=0)
        res

        if(!is.null(facetname)){
                colnames(res)[ncol(res)]="tooltip"
                #long=reshape2::melt(res,facetname,variable.name=colorname,value.name="tooltip")
                long=res
        } else{
                colnames(res)[ncol(res)]="tooltip"
                long=res
                long
        }

        newdata2=merge(newdata,long,by=c(colorname,facetname))
}

if(!is.null(facetname)) newdata2$tooltip=paste0("for ",facetname,"=",newdata2[[facetname]],"\n",newdata2$tooltip)

if(!is.null(colorname)) {
        newdata2$tooltip=paste0("for ",colorname,"=",newdata2[[colorname]],"\n",newdata2$tooltip)
        if(is.numeric(newdata2[[colorname]])) newdata2[[colorname]]=round(newdata2[[colorname]],digits)
}

newdata2$data_id=rownames(newdata2)
newdata2
if(colorAsFactor) newdata2[[colorname]]=factor(newdata2[[colorname]])

if(is.null(colorname)) {
        p<-ggplot(data=newdata2,aes_string(y=yname,x=xname,group=1))
} else{
        p<-ggplot(data=newdata2,aes_string(y=yname,x=xname,color=colorname,fill=colorname,group=colorname))
}

if(se) p<-p+geom_ribbon(aes_string(ymin="ymin",ymax="ymax",colour=NULL),alpha=0.2)
p
if(point|jitter) {
        if(!is.null(colorname)) {
                if(colorAsFactor) data[[colorname]]=factor(data[[colorname]])
        }
        data$data_id=rownames(data)
        data$tooltip=paste0(data$data_id,"\n",xname,"=",data[[xname]],"\n",yname,"=",data[[yname]])
        if(jitter) p<-p+geom_jitter(data=data,width=0,height=0.05)
        else p<-p+geom_point_interactive(data=data,aes_string(data_id="data_id",tooltip="tooltip"))

}

p<-p+ geom_line_interactive(aes_string(tooltip="tooltip",data_id="data_id"))

if(!is.null(facetname)) p<-p+facet_wrap(as.formula(paste0("~",facetname)),labeller=label_both)

if(interactive){
        tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:10px 20px 10px 20px;"
        #hover_css="fill-opacity=.3;cursor:pointer;stroke:gold;"
        hover_css="r:4px;cursor:pointer;stroke:red;stroke-width:2px;"
        selected_css = "fill:#FF3333;stroke:black;"
        p<-ggiraph(code=print(p),tooltip_extra_css=tooltip_css,tooltip_opacity=.75,
                   zoom_max=10,hover_css=hover_css,selected_css=selected_css)
}
p
}



