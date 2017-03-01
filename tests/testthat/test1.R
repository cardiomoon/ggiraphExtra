require(ggplot2)
require(ggiraph)
require(plyr)
ggPoints(aes(x=wt,y=mpg,fill=cyl),data=mtcars,method="lm",interactive=TRUE,shape=21)
