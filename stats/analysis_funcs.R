library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)

conversion_rate = 6 / 8 # USD to GBP
base_rate = 6

########################################################################################################
# some aux functions
########################################################################################################
#outliers

markoutliersIQR = function(x) { 
  y = ifelse(x %in% boxplot(x, plot = F)$out, T, F)
  print(paste("There was ", 100*round(mean(y), digits = 3), "percent of outliers"))
  return(y)
}

remove_brackets = function(x) {
  if (is.character(x)|| is.factor(x) ) {
    x = gsub('\\[', '',  x)
    x = gsub('\\]', '',  x)
    return(as.numeric(x))
  }
  else
    return(x)
}

sem = function(x)
  sd(x, na.rm = T) / sqrt(length(x))

reformat = function(x) {
  ifelse(!is.na(as.numeric(x)), round(as.numeric(x), 2), x)
}




########################################################################################################
# plot ratings vs delayed amount
########################################################################################################
plot_ratings = function(ratingdata) {
  myplot = ggplot(NULL) +
    geom_line(data = ratingdata, aes(x = amount_later, y = bid.high)) +
    geom_line(data = ratingdata, aes(x = amount_later, y = bid.low)) + geom_point() + facet_grid(. ~ context) +
    ggtitle(ratingdata$subjID[1]) + xlab('Delayed reward') + ylab('Offered price')
  
  print(myplot)
}


########################################################################################################
# organize data to fit model
########################################################################################################
organize_data = function(rawdata, grouping_var, long = F) {
  
  subjList <-
    unique(unlist(rawdata[grouping_var]))  # list of subjects x blocks
  numSubjs <- length(subjList)  # number of subjects
  Tsubj <-
    as.vector(rep(0, numSubjs)) # number of trials for each subject
  #View(rawdata)
  #View(subjList)
  #browser()
  for (i in 1:numSubjs)  {
    curSubj  <- subjList[i]
    Tsubj[i] <- sum(rawdata[grouping_var] == curSubj)
  }

  # Setting maxTrials
  maxTrials <- max(Tsubj)
  
  delay_later   <- array(0, c(numSubjs, maxTrials))
  amount_later  <- array(0, c(numSubjs, maxTrials))
  delay_sooner  <- array(0, c(numSubjs, maxTrials))
  amount_sooner <- array(0, c(numSubjs, maxTrials))
  choice <- array(0, c(numSubjs, maxTrials))
  RT <- array(0, c(numSubjs, maxTrials))
  iscalibration <- array(0, c(numSubjs, maxTrials))
  
  minRT <-
    with(rawdata, aggregate(rt, by = list(y = subjID), FUN = min)[["x"]])

  for (i in 1:numSubjs) {
    curSubj      <- subjList[i]
    useTrials    <- Tsubj[i]
    w = which(rawdata [, grouping_var ]  == curSubj)
    tmp          <- rawdata[w, ]
    delay_later[i, 1:useTrials]   <- tmp$delay_later
    amount_later[i, 1:useTrials]  <- tmp$amount_later
    delay_sooner[i, 1:useTrials]  <- tmp$delay_sooner
    amount_sooner[i, 1:useTrials] <- tmp$amount_sooner
    
    if ("iscalibration" %in% colnames(tmp)) 
      iscalibration[i, 1:useTrials] <- tmp$iscalibration
    
    if ("choice" %in% colnames(tmp))
      choice[i, 1:useTrials] <- tmp$choice

    if ("rt" %in% colnames(tmp))
      RT[i, 1:useTrials] <- tmp$rt
  }
  
  minRT <- apply(RT, 1, function(x)
    min(x[x > 0]))
  
  dataList <- list(
    N             = numSubjs,
    T             = maxTrials,
    Tsubj         = Tsubj,
    amount_later  = amount_later,
    delay_later   = delay_later,
    amount_sooner = amount_sooner,
    delay_sooner  = delay_sooner,
    choice        = choice,
    iscalibration = iscalibration,
    minRT         = minRT,
    RT            = RT,
    RTbound = 0.1
  )
  
  if (long){
    make_long = function(x){
      melt(x)$value
    }

    xx = melt(dataList$RT, varnames = c("instance", "trial"))
    dataList$RT = make_long(dataList$RT)
    keep = dataList$RT > 0
    dataList$instance = xx$instance[keep]
    dataList$trial = xx$trial[keep]
    dataList$amount_later = make_long(dataList$amount_later)[keep]
    dataList$delay_later = make_long(dataList$delay_later)[keep]
    dataList$amount_sooner = make_long(dataList$amount_sooner)[keep]
    dataList$delay_sooner = make_long(dataList$delay_sooner)[keep]
    dataList$choice = make_long(dataList$choice)[keep]
    dataList$iscalibration = make_long(dataList$iscalibration)[keep]
    dataList$RT = dataList$RT[keep]
    dataList$M = sum(keep)
    dataList$subjList = subjList
  }

  return(dataList)
}


summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
