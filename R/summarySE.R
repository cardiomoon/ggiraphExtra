#'Summarize a continuous variable by groups with mean, sd and SE
#'
#'@param data A data.frame
#'@param measurevar A name of variable to measure a mean and sd
#'@param groupvars Name(s) of variable used as a grouping variables
#'@param conf.interval confidence interval
#'@param na.rm A logical value indicating whether or not remove NA values
#'@param .drop should combinations of variables that do not appear in the input data be preserved (FALSE) or dropped (TRUE, default)
#'@importFrom plyr ddply
#'@importFrom stats qt sd
#'@export
#'@return A data.frame summarized a continuous variable by groups with mean, sd and SE
summarySE <- function(data=NULL, measurevar, groupvars=NULL,
                      conf.interval=.95, na.rm=TRUE, .drop=TRUE ) {

  # New version of length that can handle NAs: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # This does the summary
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col, na.rm) {
                   c( n = length2(xx[,col], na.rm=na.rm),
                      mean=mean (xx[,col],na.rm=na.rm),
                      sd = sd   (xx[,col],na.rm=na.rm)
                   )
                 },
                 measurevar,
                 na.rm
  )

  datac$se <- datac$sd / sqrt(datac$n)  # Calculate standard error of the mean
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use
  #  df=n-1, or if n==0, use df=0
  ciMult <- qt(conf.interval/2 + .5, datac$n-1)
  datac$ci <- datac$se * ciMult
  # datac$LCL <- datac$mean-datac$ci
  # datac$UCL <- datac$mean+datac$ci
  # Rename the "mean" column
  #datac
  colnames(datac)[colnames(datac)=="mean"]=measurevar
  #datac <- rename(datac, c("mean" = measurevar))
  return(datac)
}
