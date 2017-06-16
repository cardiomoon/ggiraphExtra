#' Generate regular sequences of desired length between minimum and maximal values
#' @param x a numeric vector
#' @param length desired length of the sequence
#' @export
myseq=function(x,length=20){

   seq(min(x,na.rm=T),max(x,na.rm=T),length=length)
}

#'Rank a numeric vector using proportional table and returns a new ordinal vector
#'
#'@param x A numeric vector
#'@param k An integer specifies how many groups you want to classifiy. default value is 4
#'
#'@export
rank2group2=function(x,k=4){

   temp=cumsum(prop.table(table(x)))
   res=c()
   for(i in 1:(k-1)){
      result=which.min(abs(temp-(i/k)))
      res=c(res,result)
   }
   res=as.numeric(names(res))
   res=c(min(x,na.rm=TRUE)-0.01,res,max(x,na.rm=TRUE))
   temp=cut(x,breaks=res)
   as.numeric(temp)
}

#'Rank a numeric vector using proportional table and returns character vactor of names of color using palette
#'@param x A numeric vector
#'@param palette Name of the color palette
#'@param reverse Logical. Whether or not reverse the order of the color palette
#'
#'@export
rank2colors=function(x,palette="Blues",reverse=TRUE){
   k=length(ggiraphExtra::palette2colors(palette,reverse=reverse))
   group=rank2group2(x,k=k)
   result=ggiraphExtra::palette2colors(palette,reverse=reverse)[group]
   result
}


#' Draw 3d predict plot using package `rgl`
#'
#' @param fit A model object for which prediction is desired.
#' @param colorn An integer giving the desired number of intervals. Non-integer values are rounded down.
#' @param maxylev Maximal length of unique values of y axis variable to be trreated as a categorical variable.
#' @param se Logical. Whether or not show se. Only effective when the y-axis variable is a categorical one.
#' @param show.summary Logical. Whether or not show statistical summary
#' @param overlay Logical. Whether or not overlay plots
#' @param show.legend Logical. Whether or not show legend
#' @param bg Character. Background color of plot
#' @param type For the default method, a single character indicating the type of item to plot. Supported types are: 'p' for points, 's' for spheres, 'l' for lines, 'h' for line segments from z = 0, and 'n' for nothing. For the mesh3d method, one of 'shade', 'wire', or 'dots'. Partial matching is used.
#' @param radius The size of sphere
#' @param palette Name of color palette
#' @param palette.reverse Logical. Whether or not reverse the palette order
#' @param show.plane Logical. If true, show regression plane
#' @param plane.color Name of color of regression plane
#' @param plane.alpha Transparency scale of regression plane
#' @param show.lines Logical. If true, show regression lines
#' @param ... additional parameters which will be passed to plot3d
#'
#' @importFrom rgl open3d next3d surface3d plot3d lines3d mfrow3d bg3d legend3d
#' @importFrom grDevices xyz.coords
#' @export
#' @examples
#'require(plyr)
#'require(moonBook)
#'require(rgl)
#'require(reshape2)
#'fit=lm(mpg~hp+wt,data=mtcars)
#'fit=lm(NTAV~age*weight,data=radial)
#'fit=lm(NTAV~age*smoking,data=radial)
#'fit=lm(NTAV~age*weight*smoking,data=radial)
#'ggPredict3d(fit)
#'require(TH.data)
#'fit=glm(cens~pnodes*horTh,data=GBSG2,family=binomial)
#'fit=glm(cens~pnodes*age,data=GBSG2,family=binomial)
#'ggPredict3d(fit)
#'fit=glm(cens~pnodes*age*horTh,data=GBSG2,family=binomial)
#'ggPredict3d(fit)
#'fit=glm(cens~pnodes+age+horTh,data=GBSG2,family=binomial)
#'ggPredict3d(fit,overlay=TRUE)
ggPredict3d=function (fit, colorn = 20, maxylev=6, se = FALSE,
          show.summary = FALSE, overlay=NULL,
          show.legend=FALSE,bg=NULL,type="s",radius=1,palette="Blues",palette.reverse=TRUE,
          show.plane=TRUE,plane.color="blue",plane.alpha=0.2,show.lines=TRUE,...)
{

   # fit=lm(NTAV~age*HBP,data=radial)
   # colorn = 20; maxylev=6; se = FALSE;
   # show.summary = FALSE; overlay=NULL;
   # show.legend=FALSE;bg=NULL;type="s";radius=1
   # palette="Blues";palette.reverse=TRUE
   # show.plane=TRUE;plane.color="blue";plane.alpha=0.2;show.lines=TRUE



   myradius=radius
   if (show.summary)
      print(summary(fit))
   (count = length(names(fit$model)) - 1)
   if (count > 3) {
      warning("maximum three independent variables are allowed")
      return
   }
   xname <- facetname <- colorname <- yname <- NULL
   facetcount=0
   (yname = names(fit$model)[1])
   (xname = names(fit$model)[2])
   temp = paste0("aes(y=", yname, ",x=", xname)
   if (count > 1) {
      (colorname = names(fit$model)[3])
      temp = paste0(temp, ",color=", colorname)
   }
   if (count > 2) {
      (facetname = names(fit$model)[4])
      temp = paste0(temp, ",facet=", facetname)
   }
   temp = paste0(temp, ")")
  # mapping = eval(parse(text = temp))
   data <- fit$model
   xnumeric = FALSE
   if (is.numeric(data[[xname]])) {
      xnumeric = TRUE
   } else {
      if (count > 1) {
         if (is.numeric(data[[colorname]])) {
            temp = xname
            xname = colorname
            colorname = temp
            xnumeric = TRUE
         }
      }
      if (!xnumeric) {
         if (count > 2) {
            if (is.numeric(data[[facetname]])) {
               temp = xname
               xname = facetname
               facetname = temp
               xnumeric = TRUE
            }
         }
      }
   }
   (uniqueXNo = length(unique(data[[xname]])))
   if (uniqueXNo > colorn) {
      newx = pretty(data[[xname]],colorn)
      xcount = 1
   } else {
      newx = unique(data[[xname]])
      if (is.numeric(data[[xname]]))
         xcount = 1
      else xcount = length(unique(data[[xname]]))
   }
   if (!is.null(colorname)) {
      uniqueColorNo = length(unique(data[[colorname]]))
      if (uniqueColorNo > colorn) {
         newcolor = pretty(data[[colorname]],colorn-1)
         colorcount = colorn
      }
      else {
         if (is.numeric(data[[colorname]]) & (length(unique(data[[colorname]])) >
                                              maxylev)) {
            #data[[colorname]]=factor(data[[colorname]])
            newcolor = seq(from = min(data[[colorname]],
                                      na.rm = T), to = max(data[[colorname]], na.rm = T),
                           length.out = colorn)
            colorcount = colorn
         }
         else {
            newcolor = sort(unique(data[[colorname]]))
            colorcount = length(unique(data[[colorname]]))
         }
      }
   }
   if (!is.null(facetname)) {
      uniqueFacetNo = length(unique(data[[facetname]]))
      if (uniqueFacetNo > colorn) {
         newfacet = pretty(data[[facetname]],colorn-1)
         facetcount = 1
      }
      else {
         newfacet = unique(data[[facetname]])
         facetcount = length(unique(data[[facetname]]))
      }
   }
   if (is.null(colorname) && is.null(facetname)) {
      temp = newx
      newdata = data.frame(temp)
      colnames(newdata) = c(xname)
   } else if (is.null(facetname)) {
      newdata <- expand.grid(newx = newx, newcolor = newcolor)
      colnames(newdata) = c(xname, colorname)
   } else {
      newdata <- expand.grid(newx = newx, newcolor = newcolor,
                             newfacet = newfacet)
      colnames(newdata) = c(xname, colorname, facetname)
   }
   result <- predict(fit, newdata = newdata, type = "response",
                     se.fit = TRUE)
   newdata[[yname]] <- result$fit
   newdata$se.fit <- result$se.fit
   newdata$ymax <- newdata[[yname]] + result$se.fit
   newdata$ymin <- newdata[[yname]] - result$se.fit
   newdata

   newdata2 <- newdata


   newdata2

   open3d()
   #par3d(scale=c(1,1,0.2),cex=.6)
   facetcount

   if(is.null(overlay)){
       if(facetcount==0) overlay=TRUE
       else overlay=FALSE
   }

   (facetcount>0)&&(!overlay)
   if((facetcount>0)&&(!overlay)){
       nc=2
       nr=facetcount%/%nc
       if((facetcount%%nc)>0) nr=nr+1
       mfrow3d(nr,nc,sharedMouse=TRUE)
   }

   if((facetcount==0)&&(!is.numeric(data[[colorname]]))&&(!overlay)){
      nc=2
      nr=colorcount%/%nc
      if((colorcount%%nc)>0) nr=nr+1
      mfrow3d(nr,nc,sharedMouse=TRUE)
   }



   mylim=function(x){
      if(is.character(x)) x=as.numeric(factor(x))
      else if(is.factor(x)) x=as.numeric(x)
      range(x,na.rm=TRUE)+c(-1,1)*max(min(x,na.rm=TRUE)*0.1,0.2)

   }

   myxlim=mylim(data[[xname]])
   myylim=mylim(data[[colorname]])
   myzlim=mylim(data[[yname]])

   if(!is.null(bg)) bg3d(bg)

   if(is.null(facetname)) {
         if(is.numeric(data[[colorname]]) && (colorcount>maxylev)) {
            data$color=rank2colors(data[[colorname]],palette=palette,reverse=palette.reverse)
            plot3d(data[[xname]],data[[colorname]],data[[yname]],col=data$color,
                   type=type,radius=myradius,
                   xlab=xname,ylab=colorname,zlab=yname,xlim=myxlim,ylim=myylim,zlim=myzlim,
                   sub=Reduce(paste0,deparse(fit$call)),...)

            newdata2=newdata2[order(newdata2[[colorname]],newdata2[[xname]]),]
            temp= paste0("dcast(newdata2[1:3],",xname,"~",colorname,",value.var='",yname,"')[-1]")
            newdata3=eval(parse(text=temp))
            #surface3d(newx,sort(newcolor),as.matrix(newdata3),col="blue",alpha=.5)
            if(show.plane) surface3d(newx,newcolor,as.matrix(newdata3),col=plane.color,alpha=plane.alpha)

            if(show.lines) {
            newdata2$color=rank2colors(newdata2[[colorname]],palette=palette,reverse=palette.reverse)

            for(i in 1:length(newcolor)){
                newdata4=newdata2[newdata2[[colorname]]==newcolor[i],]
                lines3d(xyz.coords(as.matrix(newdata4[1:3])),col=newdata4$color,lwd=1)
            }
            for(i in 1:length(newx)){
               newdata4=newdata2[newdata2[[xname]]==newx[i],]
               lines3d(xyz.coords(as.matrix(newdata4[1:3])),col=newdata4$color,lwd=1)
            }

            }

         } else{
            if(overlay) {
            plot3d(data[[xname]],data[[colorname]],data[[yname]],col=as.numeric(factor(data[[colorname]])),
                   type=type,radius=myradius,
                   xlab=xname,ylab=colorname,zlab=yname,xlim=myxlim,ylim=myylim,zlim=myzlim,
                   sub=Reduce(paste0,deparse(fit$call)),...)
                    #newdata2=newdata2[order(newdata2[[colorname]],newdata2[[xname]]),]
                    temp= paste0("dcast(newdata2[1:3],",xname,"~",colorname,",value.var='",yname,"')[-1]")
                    newdata3=eval(parse(text=temp))
                    if(show.plane) surface3d(newx,newcolor,as.matrix(newdata3),col=plane.color,alpha=plane.alpha)
            }
            # plot3d(data[[xname]],data[[colorname]],data[[yname]],col=as.numeric(factor(data[[colorname]])),
            #        xlab=xname,ylab=colorname,zlab=yname,xlim=myxlim,ylim=myylim,zlim=myzlim)
            #

            df=expand.grid(x=myseq(data[[xname]]),
                           y=unique(data[[colorname]]))
            colnames(df)=c(xname,colorname)
            df
            summary(fit)
            result=predict(fit,newdata=df,type="response",se.fit=TRUE)
            result
            df[[yname]]=result$fit
            df$min=result$fit-result$se.fit
            df$max=result$fit+result$se.fit

            for(i in 1:length(unique(data[[colorname]]))){
               if((i>1)&&(!overlay)){
                  next3d()
                  # par3d(scale=myscale)
                  if(!is.null(bg)) bg3d(bg)
               }
               if(!overlay) {
                  data1=data[data[[colorname]]==sort(unique(data[[colorname]]))[i],]
                  plot3d(data1[[xname]],data1[[colorname]],data1[[yname]],
                         xlab=xname,ylab=colorname,zlab=yname,
                         type=type,col=i,radius=myradius,
                         xlim=myxlim,ylim=myylim,zlim=myzlim,
                         sub=paste0(colorname,":",sort(unique(data[[colorname]]))[i]),...)
               }
               df1=df[df[[colorname]]==sort(unique(data[[colorname]]))[i],]
               if(!is.numeric(df1[[colorname]])) df1[[colorname]]=i
               if(show.lines){
               lines3d(xyz.coords(as.matrix(df1)),col=i,lwd=2)
               if(se) lines3d(xyz.coords(as.matrix(df1[c(1,2,4)])),col=i,lwd=0.5)
               if(se) lines3d(xyz.coords(as.matrix(df1[c(1,2,5)])),col=i,lwd=0.5)
               }
               newdata2=newdata2[order(newdata2[[colorname]],newdata2[[xname]]),]
               temp= paste0("dcast(newdata2[1:3],",xname,"~",colorname,",value.var='",yname,"')[-1]")
               newdata3=eval(parse(text=temp))
               #surface3d(newx,sort(newcolor),as.matrix(newdata3),col="blue",alpha=.5)
               if(!overlay){
                  if(show.plane) surface3d(newx,newcolor,as.matrix(newdata3),col=plane.color,alpha=plane.alpha)
               }
            }

         }



         if(show.legend) legend3d("bottomright",legend=sort(unique(data[[colorname]])),pch=21,pt.bg=1:colorcount)

   } else{
      if(overlay)  {
         plot3d(data[[xname]],data[[colorname]],data[[yname]],col=as.numeric(factor(data[[facetname]])),
                type=type,radius=myradius,
                xlab=xname,ylab=colorname,zlab=yname,xlim=myxlim,ylim=myylim,zlim=myzlim,
                sub=Reduce(paste0,deparse(fit$call)),...)
      }
      mysummary=function(df){
         temp=paste0("dcast(df,",xname,"~",colorname,",value.var='",yname,"')[-1]")
         temp
         eval(parse(text=temp))
      }

         result=eval(parse(text=paste0("dlply(newdata2,.(",facetname,"),mysummary)")))
         result
         i=1
         for(i in 1:facetcount){
            if((i>1)&&(!overlay)){
               next3d()
               # par3d(scale=myscale)
               if(!is.null(bg)) bg3d(bg)
            }
            if(!overlay){
            data1=data[data[[facetname]]==unique(data[[facetname]])[i],]
            plot3d(data1[[xname]],data1[[colorname]],data1[[yname]],
                   xlab=xname,ylab=colorname,zlab=yname,
                   type=type,col=i,radius=myradius,
                   xlim=myxlim,ylim=myylim,zlim=myzlim,
                   sub=paste0(facetname,":",unique(data[[facetname]])[i]),...)
            }
            z=as.matrix(result[[as.character(unique(data[[facetname]])[i])]])
            if(show.plane) surface3d(newx,newcolor,z,col=i,alpha=plane.alpha)
            newdata4=newdata2[newdata2[[facetname]]==unique(data[[facetname]])[i],]
            newdata4

            if(show.lines) {

               for(j in 1:length(newcolor)){
                  newdata5=newdata4[newdata4[[colorname]]==newcolor[j],]
                  lines3d(xyz.coords(as.matrix(newdata5[c(1,2,4)])),col=i,lwd=1,alpha=0.5)
               }
               for(j in 1:length(newx)){
                  newdata5=newdata4[newdata4[[xname]]==newx[j],]
                  lines3d(xyz.coords(as.matrix(newdata5[c(1,2,4)])),col=i,lwd=1,alpha=0.5)
               }

            }


         }

      if(show.legend) legend3d("bottomright",legend=unique(data[[facetname]]),pch=21,pt.bg=1:facetcount,cex=1)

   }

}

