library(dplyr)
library(ggplot2)
library(reshape2)

conversion_rate = 6 / 8 # USD to GBP
base_rate = 6

########################################################################################################
# some aux functions
########################################################################################################
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
# Pick sample
########################################################################################################
pick_sample = function(x) {
  # should check that results in the end do not depend on this sampling
  u = x * NA
  
  for (z in common_amounts) {
    i = which(x == z)
    print(i)
    j = sample(i, 1)
    u[j] = x[j]
  }
  return(u)
}


########################################################################################################
# compute bonus
########################################################################################################

lottery = function(x, y, z) {
  ll = runif(1, x, y)
  return(z < ll)
}

compute_bonus = function(choicedata, ratingdata, conversion_rate) {
  table(paste(choicedata$choice, choicedata$iscatch))
  missedtrials = sum(choicedata$choice[choicedata$iscatch == 1])
  missedpercent = mean(choicedata$choice[choicedata$iscatch == 1]) * 100
  
  print(paste("Percentage of catch trials missed:", missedpercent))
  #browser()
  # draw prices for each trial
  keyprice = sapply(ratingdata$amount_later, function(x)
    runif(1, 0, x))
  wantedkey = 1 * (keyprice < ratingdata$bid.low)
  wonlottery = 1 * (mapply(lottery, ratingdata$bid.low, ratingdata$bid.high, keyprice))
  
  boughtkey = (wantedkey + wonlottery) > 0
  rewards =  ratingdata$amount_later - keyprice
  if (sum(boughtkey) > 0)
    reward.ratings = sample(rewards[boughtkey], 1) * conversion_rate
  else
    reward.ratings = conversion_rate
  
  # consider only immediate choices
  rewards = choicedata$amount_sooner[choicedata$choice == 0] * conversion_rate
  
  reward.choices = sample(rewards, 1)
  
  total_reward = reward.ratings + reward.choices - 2 * conversion_rate # subtract the couple of dollars in excess
  # print(
  #   paste(
  #     "Choice bonus:",
  #     reward.choices,
  #     "Rating bonus:",
  #     reward.ratings,
  #     "Total bonus in GBP:",
  #     total_reward
  #   )
  # )
  return(c (missedtrials, total_reward))
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
# plot RTs
########################################################################################################


# pl2 = ggplot(
#   choicedata %>% filter(iscatch == 0 & context.index > 30),
#   aes(
#     x = amount_later,
#     y = rt,
#     group = context,
#     color = context
#   )
# ) +
#   geom_line() + geom_point() + theme(legend.position = "bottom")


########################################################################################################
# read data from one subject
########################################################################################################
read_data = function(data_file, NCHARS = 6) {
  rawdata <-
    read.table(data_file, header = T, sep = ",") %>% mutate(participant = substr(participant, 1, NCHARS))
  #print(colnames(rawdata))
  #View(rawdata)
  #  browser()
  choicedata = rawdata %>% filter (!is.na(response.corr))  %>% rename(
    subjID = participant,
    delay_sooner = DelaySooner,
    delay_later = DelayLaterCalibrate,
    amount_sooner = RewardSooner,
    amount_later = RewardLater,
    rt = response.rt,
    choice = response.corr,
    iscatch = IsCatch,
    iscommon = IsCommon,
    laterchoiceside = LaterChoice,
    chosenside = response.keys,
    trial = trials_subblocks.thisN
  ) %>% mutate(
    context = Context_Name,
    index = row_number(),
    amount_later.cut = cut(amount_later, breaks = 7)
  ) %>% select(
    subjID,
    date,
    delay_sooner,
    delay_later,
    amount_sooner,
    amount_later,
    rt,
    choice,
    iscatch,
    iscommon,
    context,
    trial,
    index,
    laterchoiceside,
    chosenside,
    amount_later.cut
  ) %>% group_by(context) %>% mutate(context.index = row_number()) %>% ungroup()
  
  
  ratingdata = rawdata %>% filter (!is.na(slider_resp.rt))  %>% rename(
    subjID = participant,
    bid.high = slider_rating_high.response,
    bid.low = slider_rating_low.response,
    bid.high.rt = slider_rating_high.rt,
    bid.low.rt = slider_rating_low.rt,
    delay_later = DelayLaterCalibrate,
    amount_later = RewardLater,
    key.rt = slider_resp.rt
  ) %>% mutate(
    context = Context_Name,
    bid.high = as.numeric(bid.high),
    bid.low = as.numeric(bid.low),
    amount_later.cut = cut(
      amount_later,
      breaks = c(1, 3.5, 6.5, 9),
      labels = c("low", "medium", "high")
    )
  ) %>% select(
    subjID,
    date,
    bid.high,
    bid.low,
    bid.high.rt,
    bid.low.rt,
    delay_later,
    amount_later,
    amount_later.cut,
    key.rt,
    context
  )
  
  controldata = rawdata %>% filter(resp_choices_control.keys != "") %>% rename(subjID = participant,
                                                                               keys = resp_choices_control.keys,
                                                                               rt = resp_choices_control.rt) %>% select(subjID, keys, rt) %>% mutate(rt = remove_brackets(rt))
  breakdata = rawdata %>% filter(rawdata$break_resp.keys != "") %>% select(participant, Context_Name, break_resp.rt) %>% rename(subjID = participant,
                                                                                                                                context = Context_Name,
                                                                                                                                break_duration = break_resp.rt)
  #View(ratingdata)
  #View(choicedata)
  # global variables
  bonus = compute_bonus(choicedata, ratingdata, conversion_rate)
  whichOS = rawdata$OS[1]
  howtiring = rawdata$slider_tiring.response[!is.na(rawdata$slider_tiring.response)]
  randomization = unique(rawdata$randomization[!is.na(rawdata$randomization)])
  duration = unique(rawdata$globalClockTime[!is.na(rawdata$globalClockTime)]) /
    60
  browser = unique(rawdata$browser[rawdata$browser != ""])
  base_payment = duration / 60 * base_rate # in GBP
  total_payment = 6 * conversion_rate + bonus[2] # in GBP
  prolific_bonus = total_payment - base_payment # in GBP
  
  vars = list(
    choicedata$subjID[1],
    bonus[1],
    howtiring,
    whichOS,
    choicedata$delay_later[1],
    randomization,
    browser,
    duration,
    base_payment,
    bonus[2],
    total_payment,
    prolific_bonus
  )
  
  names(vars) = c(
    "subjID",
    "missed_trials",
    "howtiring",
    "OS",
    "delay_later",
    "randomization",
    "browser",
    "duration",
    "base_payment",
    "total_bonus",
    "total_payment",
    "prolific_bonus"
  )
  
  return(
    list(
      choicedata = choicedata,
      ratingdata = ratingdata,
      rawdata = rawdata,
      breakdata = breakdata,
      controldata = controldata,
      vars = vars
    )
  )
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
    if (!is.null(tmp$choice))
      choice[i, 1:useTrials] <- tmp$choice
    if (!is.null(tmp$rt))
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
    keep = (dataList$RT > 0 ) & (dataList$amount_later  > 5)
    dataList$instance = xx$instance[keep]
    dataList$trial = xx$trial[keep]
    dataList$amount_later = make_long(dataList$amount_later)[keep]
    dataList$delay_later = make_long(dataList$delay_later)[keep]
    dataList$amount_sooner = make_long(dataList$amount_sooner)[keep]
    dataList$delay_sooner = make_long(dataList$delay_sooner)[keep]
    dataList$choice = make_long(dataList$choice)[keep]
    dataList$RT = dataList$RT[keep]
    dataList$M = sum(keep)
    dataList$subjList = subjList
  }

  return(dataList)
}
