library(mediation)

acme.mat = prop.mat = NULL
#*Group*Context
mypars.rating = c("bid.diff", "key.rt") # , "bid.rt.max")
for (mypar.rating in mypars.rating) {
  for (mypar in mypars) {
    for (co in c(1, 2)) {
      ratingdata$mediator = as.numeric(scale(ratingdata[mypar]))
      ratingdata_ = ratingdata[!is.na(ratingdata[mypar.rating]),]  %>% filter(context_order == co)
      # ratingdata_$ContextGroup = paste(ratingdata_$Context, ratingdata_$Group)
      # myformula.m = as.formula("mediator ~  Context*Group + (1| subjID)")
      myformula.m = as.formula("mediator ~  Context")
      # model.m = lmer(myformula.m, data = ratingdata_)
      model.m = lm(myformula.m, data = ratingdata_)
      if (mypar.rating == 'bid.diff')
      {
        # myformula.b = as.formula(sprintf("%s ~ mediator*amount_later_centered + Context*Group + (1| subjID)", mypar.rating, mypar))
        myformula.b = as.formula(
          sprintf(
            "%s ~ mediator*amount_later_centered + Context",
            mypar.rating,
            mypar
          )
        )
      }
      else
      {
        # myformula.b = as.formula(sprintf("%s ~ mediator + Context*Group + (1| subjID)", mypar.rating, mypar))
        myformula.b = as.formula(sprintf("%s ~ mediator + Context", mypar.rating, mypar))
        
      }
      # model.b = lmer(myformula.b, data = ratingdata_)
      model.b = lm(myformula.b, data = ratingdata_)
      med = mediate(
        model.m,
        model.b,
        treat = "Context",
        mediator = "mediator",
        boot = T,
        sims = 1000,
        control.value = "Low volatility",
        treat.value = "High volatility"
      )
      #print(paste(mypar.rating, mypar))
      #print(c(med$d0, med$d0.ci, med$d0.p ))
      #print(c(med$tau.coef, med$tau.ci, med$tau.p ))
      #print(c(med$n0, med$n0.ci, med$n0.p ))
      
      ss.acme = c(med$d0, med$d0.ci[1], med$d0.ci[2], med$d0.p)
      #ss. = c(med$tau.coef, med$tau.ci[0], med$tau.ci[1], med$tau.p)
      ss.prop = c(med$n0, med$n0.ci[1], med$n0.ci[2], med$n0.p)
      acme.mat = rbind(acme.mat, c(mypar.rating, mypar, co, round(ss.acme, 3)))
      prop.mat = rbind(prop.mat, c(mypar.rating, mypar, co, round(ss.prop, 3)))
    }
  }
}

acme.mat = as.data.frame(acme.mat)
prop.mat = as.data.frame(prop.mat)##

acme.mat = acme.mat %>% arrange(V1, V3, V2)
prop.mat = prop.mat %>% arrange(V1, V3, V2)

acme.mat = acme.mat %>% mutate(V1 = rating_par_names[as.character(V1)], V2 = choice_par_names[as.character(V2)])
prop.mat = prop.mat %>% mutate(V1 = rating_par_names[as.character(V1)], V2 = choice_par_names[as.character(V2)])

colnames(acme.mat) = c(
  "WTP parameter",
  "DDM parameter",
  "Context order",
  "ACME Estimate",
  "ACME  CI 2.5%",
  "ACME CI 97.5%",
  "p-value"
)
colnames(prop.mat) = c(
  "WTP parameter",
  "DDM parameter",
  "Context order",
  "Proportion Estimate",
  "Proportion CI 2.5%",
  "Proportion CI 97.5%",
  "p-value"
)

View(acme.mat)
View(prop.mat)

mediation.mat = cbind(acme.mat, prop.mat[, seq(4, 6)])
mediation.mat = subset(mediation.mat, `WTP parameter` == 'WTP response time')[-1]

statslist['_table_mediation_'] = tab_to_str(mediation.mat)
statslist['_prop_drift_sensitivity1_'] = as.numeric(as.character(mediation.mat[1, 7])) *
  100
statslist['_prop_drift_sensitivity2_'] = as.numeric(as.character(mediation.mat[6, 7])) *
  100
statslist['_prop_noise2_'] = as.numeric(as.character(mediation.mat[9, 7])) *
  100
statslist['_prop_nondectime2_'] = as.numeric(as.character(mediation.mat[10, 7])) *
  100

if (F) {
  detach(package:lmerTest)
  #rating_par_names = c("bid.diff" = "WTP range", "key.rt" = "WTP response time", "bid.rt.max" = "Max response time")
  # choice_par_names = c("b.drift.intercept.p" = "Drift intercept", "b.drift.amount.p" = "Drift sensitivity",
  #                         "noise.p" = "Noise", "bias.p" = "Bias",
  #                         "nondectime.p" = "Non-decision\ntime"
  #                       )
  
  acme.mat = prop.mat = NULL
  #*Group*Context
  mypars.rating = c("bid.diff", "key.rt") # , "bid.rt.max")
  for (mypar.rating in mypars.rating) {
    for (mypar in mypars) {
      ratingdata$mediator = as.numeric(scale(ratingdata[mypar]))
      ratingdata_ = ratingdata[!is.na(ratingdata[mypar.rating]),]
      myformula.m = as.formula("mediator ~ Context*Group + (1| subjID)")
      model.m = lmer(myformula.m, data = ratingdata_)
      if (mypar.rating == 'bid.diff')
      {
        myformula.b = as.formula(
          sprintf(
            "%s ~ mediator*amount_later_centered + Context*Group + (1| subjID)",
            mypar.rating,
            mypar
          )
        )
        
      }
      else
      {
        myformula.b = as.formula(sprintf(
          "%s ~ mediator + Context*Group + (1| subjID)",
          mypar.rating,
          mypar
        ))
      }
      model.b = lmer(myformula.b, data = ratingdata_)
      med = mediate(
        model.m,
        model.b,
        treat = "Context",
        mediator = "mediator",
        boot = F,
        control.value = "Low volatility",
        treat.value = "High volatility"
      )
      #print(paste(mypar.rating, mypar))
      #print(c(med$d0, med$d0.ci, med$d0.p ))
      #print(c(med$tau.coef, med$tau.ci, med$tau.p ))
      #print(c(med$n0, med$n0.ci, med$n0.p ))
      
      ss.acme = c(med$d0, med$d0.ci[1], med$d0.ci[2], med$d0.p)
      #ss. = c(med$tau.coef, med$tau.ci[0], med$tau.ci[1], med$tau.p)
      ss.prop = c(med$n0, med$n0.ci[1], med$n0.ci[2], med$n0.p)
      acme.mat = rbind(acme.mat, c(mypar.rating, mypar, round(ss.acme, 3)))
      prop.mat = rbind(prop.mat, c(mypar.rating, mypar, round(ss.prop, 3)))
    }
  }
  
  acme.mat = as.data.frame(acme.mat)
  prop.mat = as.data.frame(prop.mat)##
  acme.mat = acme.mat %>% mutate(V1 = rating_par_names[as.character(V1)], V2 = choice_par_names[as.character(V2)])
  prop.mat = prop.mat %>% mutate(V1 = rating_par_names[as.character(V1)], V2 = choice_par_names[as.character(V2)])
  
  colnames(acme.mat) = c(
    "WTP parameter",
    "DDM parameter",
    "ACME Estimate",
    "ACME  CI 2.5%",
    "ACME CI 97.5%",
    "p-value"
  )
  colnames(prop.mat) = c(
    "WTP parameter",
    "DDM parameter",
    "Proportion Estimate",
    "Proportion CI 2.5%",
    "Proportion CI 97.5%",
    "p-value"
  )
  
  acme.mat.lmer = acme.mat
  prop.mat.lmer = prop.mat
  View(acme.mat.lmer)
  View(prop.mat.lmer)
  
  library(lmerTest)
  mediation.mat = cbind(acme.mat[,-6], prop.mat[, seq(3, 6)])
  mediation.mat = subset(mediation.mat, `WTP parameter` == 'WTP response time')[-1]
  
  statslist['_table_mediation_'] = tab_to_str(mediation.mat)
  statslist['_prop_drift_intercept_'] = as.numeric(as.character(mediation.mat[1, 5])) *
    100
  statslist['_prop_nondectime_'] = as.numeric(as.character(mediation.mat[4, 5])) *
    100
  statslist['_prop_noise_'] = as.numeric(as.character(mediation.mat[5, 5])) *
    100
  
  ########
  
  acme.mat = prop.mat = NULL
  #*Group*Context
  mypars.rating = c("bid.diff", "key.rt") # , "bid.rt.max")
  for (mypar.rating in mypars.rating) {
    for (mypar in mypars) {
      ratingdata$mediator = as.numeric(scale(ratingdata[mypar]))
      ratingdata_ = ratingdata[!is.na(ratingdata[mypar.rating]),]
      # ratingdata_$ContextGroup = paste(ratingdata_$Context, ratingdata_$Group)
      # myformula.m = as.formula("mediator ~  Context*Group + (1| subjID)")
      myformula.m = as.formula("mediator ~  Context*Group")
      # model.m = lmer(myformula.m, data = ratingdata_)
      model.m = lm(myformula.m, data = ratingdata_)
      if (mypar.rating == 'bid.diff')
      {
        # myformula.b = as.formula(sprintf("%s ~ mediator*amount_later_centered + Context*Group + (1| subjID)", mypar.rating, mypar))
        myformula.b = as.formula(
          sprintf(
            "%s ~ mediator*amount_later_centered + Context*Group",
            mypar.rating,
            mypar
          )
        )
      }
      else
      {
        # myformula.b = as.formula(sprintf("%s ~ mediator + Context*Group + (1| subjID)", mypar.rating, mypar))
        myformula.b = as.formula(sprintf("%s ~ mediator + Context*Group", mypar.rating, mypar))
        myformula.b1 = as.formula(sprintf("%s ~ Context*Group", mypar.rating, mypar))
      }
      # model.b = lmer(myformula.b, data = ratingdata_)
      model.b = lm(myformula.b, data = ratingdata_)
      model.b1 = lm(myformula.b1, data = ratingdata_)
      med = mediate(
        model.m,
        model.b,
        treat = "Context",
        mediator = "mediator",
        boot = T,
        sims = 500,
        control.value = "Low volatility",
        treat.value = "High volatility"
      )
      #print(paste(mypar.rating, mypar))
      #print(c(med$d0, med$d0.ci, med$d0.p ))
      #print(c(med$tau.coef, med$tau.ci, med$tau.p ))
      #print(c(med$n0, med$n0.ci, med$n0.p ))
      
      ss.acme = c(med$d0, med$d0.ci[1], med$d0.ci[2], med$d0.p)
      #ss. = c(med$tau.coef, med$tau.ci[0], med$tau.ci[1], med$tau.p)
      ss.prop = c(med$n0, med$n0.ci[1], med$n0.ci[2], med$n0.p)
      acme.mat = rbind(acme.mat, c(mypar.rating, mypar, round(ss.acme, 3)))
      prop.mat = rbind(prop.mat, c(mypar.rating, mypar, round(ss.prop, 3)))
    }
  }
  
  acme.mat = as.data.frame(acme.mat)
  prop.mat = as.data.frame(prop.mat)##
  acme.mat = acme.mat %>% mutate(V1 = rating_par_names[as.character(V1)], V2 = choice_par_names[as.character(V2)])
  prop.mat = prop.mat %>% mutate(V1 = rating_par_names[as.character(V1)], V2 = choice_par_names[as.character(V2)])
  
  colnames(acme.mat) = c(
    "WTP parameter",
    "DDM parameter",
    "ACME Estimate",
    "ACME  CI 2.5%",
    "ACME CI 97.5%",
    "p-value"
  )
  colnames(prop.mat) = c(
    "WTP parameter",
    "DDM parameter",
    "Proportion Estimate",
    "Proportion CI 2.5%",
    "Proportion CI 97.5%",
    "p-value"
  )
  View(acme.mat)
  View(prop.mat)
  
  library(lmerTest)
  mediation.mat = cbind(acme.mat[,-6], prop.mat[, seq(3, 6)])
  mediation.mat = subset(mediation.mat, `WTP parameter` == 'WTP response time')[-1]
  
  statslist['_table_mediation_'] = tab_to_str(mediation.mat)
  statslist['_prop_drift_intercept_'] = as.numeric(as.character(mediation.mat[1, 5])) *
    100
  statslist['_prop_nondectime_'] = as.numeric(as.character(mediation.mat[4, 5])) *
    100
  statslist['_prop_noise_'] = as.numeric(as.character(mediation.mat[5, 5])) *
    100
  
}