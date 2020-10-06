#'Extract colors from a palette
#'@param name A palette name from the RColorBrewer package
#'@param reverse if true, reverse colors
#'@importFrom RColorBrewer brewer.pal brewer.pal.info
#'@export
palette2colors=function(name,reverse=FALSE){
        colors=brewer.pal(brewer.pal.info[rownames(brewer.pal.info)==name,"maxcolors"],name)
        if(reverse) colors=rev(colors)
        colors
}

#' Draw an interactive choropleth map
#' @param data a data.frame
#' @param mapping Set of aesthetic mappings created by aes or aes_. Passed on geom_map_interactive. Required mappings are map_id and fill. Possible mapping is facet.
#' @param map a map maybe a result of map_data()
#' @param palette A palette name used for discrete fill var, Default value is "OrRd"
#' @param reverse If true, reverse palette colors
#' @param color A name of color of polygon, Default value is "grey50"
#' @param title A title
#' @param digits An integer indicating the number of decimal places
#' @param interactive Logical. If positive an interactive map will be made
#' @param ... other arguments passed on to geom_map_interactive
#' @importFrom ggplot2 scale_fill_brewer scale_colour_gradientn facet_wrap scale_fill_gradientn coord_map
#' @importFrom ggiraph geom_map_interactive
#' @export
#'@examples
#'#crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
#'#require(ggplot2)
#'#require(ggiraph)
#'#require(maps)
#'#require(mapproj)
#'#require(reshape2)
#'#require(RColorBrewer)
#'#states_map <- map_data("state")
#'#ggChoropleth(crimes,aes(fill=Murder,map_id=state),map=states_map,interactive=TRUE)
#'#ggChoropleth(crimes,aes(fill=c(Murder,Rape),map_id=state),map=states_map,interactive=TRUE)
#'#ggChoropleth(crimes,aes(map_id=state),map=states_map,palette="OrRd",interactive=TRUE)
ggChoropleth=function(data,mapping,map,
                      palette="OrRd",reverse=FALSE,color="grey50",
                      title="",digits=1,interactive=FALSE,...){

    # crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
    # states_map <- map_data("state")
    # data=crimes;mapping=aes(fill=c(Murder,Rape),map_id=state);map=states_map;interactive=TRUE


        mapidvar<-fillvar<-facetvar<-tooltip<-NULL
        if("map_id" %in% names(mapping)) mapidvar<-getMapping(mapping,"map_id")
        if("fill" %in% names(mapping)) fillvar<-getMapping(mapping,"fill")
        if("facet" %in% names(mapping)) facetvar<-getMapping(mapping,"facet")
        if("tooltip" %in% names(mapping)) tooltip<-getMapping(mapping,"tooltip")
        #if(is.null(fillvar)) warning("Aestheic mapping to 'fill' is required.")
        if(is.null(mapidvar)) warning("Aestheic mapping to 'map_id' is required.")

        longdata=FALSE
        fillvar
        if(is.null(fillvar)) {
                (select=sapply(data,is.numeric))
                (fillvar=colnames(data)[select])
                longdata=TRUE
        } else {
                fillvar=getMapping(mapping,"fill")
                if(length(fillvar)>1) {

                        longdata=TRUE
                }
        }
        if(longdata){
                data<-data[c(mapidvar,tooltip,fillvar)]
                longdf<-melt(data,id.vars=c(mapidvar,tooltip))
                data<-longdf
        }



    data$data_id=data[[mapidvar]]
    if(is.null(tooltip)) {
            if(longdata){
                    data$tooltip=paste0(data[[mapidvar]],"<br>",
                                        data[["variable"]],"<br>",data[["value"]])
            } else{
                if(is.numeric(data[[fillvar]])) data[[fillvar]]=round(data[[fillvar]],digits)
                data$tooltip=paste0(data[[mapidvar]],"<br>",
                            fillvar,"<br>",data[[fillvar]])
            }
    } else {
            if(longdata){
                    data$tooltip=paste0(data[[tooltip]],"<br>",
                                        data[["variable"]],"<br>",data[["value"]])
            } else{
                if(is.numeric(data[[fillvar]])) data[[fillvar]]=round(data[[fillvar]],digits)
                data$tooltip=paste0(data[[tooltip]],"<br>",fillvar,"<br>",data[[fillvar]])
            }
    }
    if(!is.null(palette)) mycolors=palette2colors(palette,reverse=reverse)


    if(longdata) {
            p<-ggplot(data=data,aes_string(map_id=mapidvar,fill="value"))
            p<-p+scale_fill_gradientn(colours=mycolors)
    } else{
            p<-ggplot(data=data,mapping)
            if(is.numeric(data[[fillvar]])) {
                    p<-p+scale_fill_gradientn(colours=mycolors)
            } else {
                    if(is.null(palette)) {
                            p<-p+scale_colour_gradientn(colours=mycolors)
                    } else {
                            p<-p+scale_fill_brewer(palette)
                    }
            }
    }
    p<-p+ expand_limits(x=map$long,y=map$lat)+coord_map()
    p<-p+geom_map_interactive(aes_string(data_id="data_id",tooltip="tooltip"),
                              map=map,colour=color,size=0.1,...)
    p<-p+labs(y="",x="")

    if(longdata) p<-p+facet_wrap("variable")
    if(!is.null(facetvar)) p<-p+facet_wrap(facetvar)
    if(title!="") p<-p+ ggtitle(title)

    if(interactive) {
        tooltip_css <- "background-color:white;font-style:italic;padding:10px;border-radius:20px 20px 20px 20px;"

        p<-girafe(ggobj=p)
        p<-girafe_options(p,
                          opts_tooltip(css=tooltip_css,opacity=.75),
                          opts_zoom(min=1,max=10))
    }
    p
}


