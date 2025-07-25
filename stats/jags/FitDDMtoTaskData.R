# Fit DDM model to task data
rm(list = ls())

######################################################################
# Definitions
######################################################################
# definitions
library(runjags)
library(coda)
library(rjags)
library(MCMCvis)

source('../analysis_funcs.R')
WD = '../../data'
WDD = file.path(WD, 'experiment')

# Set sampler parameters
NTHIN = 20
NCHAINS = 4

######################################################################
# Arguments
######################################################################
if (F) {
  # tests
  INPUT_FILE = 'processed_data_censored.RData'
  #  INPUT_FILE = 'processed_data.RData'
  #MODEL_NAME = 'linear_drift_ddm'
  MODEL_NAME = 'linear_drift_no_pooling_bd_ddm'
  NSAMPLES = 500
  NBURNIN = 500
} else {
  args = commandArgs(trailingOnly = TRUE)
  INPUT_FILE = args[1]
  MODEL_NAME = args[2]
  NSAMPLES = as.numeric(args[3])
  NBURNIN = as.numeric(args[4])
}

MODEL_FILE = file.path('models', paste(MODEL_NAME, 'jags', sep = '.'))
NFINALSAMPLES = min(1000, NSAMPLES)
######################################################################
# Load and prepare data
######################################################################
load(file.path(WDD, INPUT_FILE))

grouping_var = 'subj_context'
choicedata.common$subj_context = paste(choicedata.common$subjID, choicedata.common$context)
dataList = organize_data(choicedata.common, grouping_var, long = T)

subjList <-
  unique(choicedata.common[c('subjID', 'context', 'background_col', grouping_var)])  # list of subjects x blocks
numSubjs = dataList$N
dataList$amount_diff = dataList$amount_later - dataList$amount_sooner
dataList$delay_diff = dataList$delay_later - dataList$delay_sooner
dataList$amount_later_centered = dataList$amount_later - 5 #mean center
dataList$RT.signed = dataList$RT #  rnorm(length(dataList$RT), 0, 2)
idx = dataList$choice == 0
dataList$RT.signed[idx] = -dataList$RT[idx]

######################################################################
# Generate initial values
######################################################################
# This initialization will facilitate the sampling

get_inits <- function(i){
  
  ll <- list(
    b.drift.intercept.mu = runif(numSubjs, -.1, .1),
    b.drift.amount.mu = runif(numSubjs, -.1, .1),
    nondectime.mu = runif(numSubjs, .01, .1),
    noise.mu = runif(numSubjs, .1, .5),
    bias.mu = runif(numSubjs, .4, .6),
    .RNG.name = c("base::Mersenne-Twister","base::Super-Duper", "base::Wichmann-Hill", "base::Marsaglia-Multicarry")[i],
    .RNG.seed = c(6666, 9999, 7777, 3333)[i] 
  )
  dump.format(ll)
}


monitor = c(
  "b.drift.intercept.mu",
  "b.drift.intercept.pr",
  "b.drift.intercept.p",
  "b.drift.amount.mu",
  "b.drift.amount.pr",
  "b.drift.amount.p",
  "nondectime.mu",
  "nondectime.pr",
  "nondectime.p",
  "noise.mu",
  "noise.pr",
  "noise.p",
  "bias.mu",
  "bias.kappa",
  "bias.p",
  #  "drift",
  "deviance"
)

if (MODEL_NAME %in% c('linear_drift_variability_no_pooling_ddm'))
{
  get_inits <- function(i){
    
   ll <- list(
    b.drift.intercept.p = runif(numSubjs, -.1, .1),
    b.drift.amount.p = runif(numSubjs, -.1, .1),
    sigma.p = runif(numSubjs, .1, .9),
    nondectime.p = runif(numSubjs, .01, .1),
    noise.p = runif(numSubjs, .1, .5),
    bias.p = runif(numSubjs, .4, .6),
    .RNG.name = c("base::Mersenne-Twister","base::Super-Duper", "base::Wichmann-Hill", "base::Marsaglia-Multicarry")[i],
    .RNG.seed = c(6666, 9999, 7777, 3333)[i] 
  )
   dump.format(ll)
  }

  monitor = c(
    "b.drift.intercept.p",
    "b.drift.amount.p",
    "sigma.p",
    "b.drift.pr.p",
    "nondectime.p",
    "noise.p",
    "bias.p",
    "deviance"
  )
  
}  

if (MODEL_NAME %in% c('linear_drift_variability_no_pooling_bd_ddm'))
{
  get_inits <- function(i){
    
    ll <- list(
      b.drift.intercept.p = runif(numSubjs, -.1, .1),
      b.drift.amount.p = runif(numSubjs, -.1, .1),
      sigma.p = runif(numSubjs, .1, .9),
      nondectime.p = runif(numSubjs, .01, .1),
      boundary.p = runif(numSubjs, 1.1, 2),
      bias.p = runif(numSubjs, .4, .6),
      .RNG.name = c("base::Mersenne-Twister","base::Super-Duper", "base::Wichmann-Hill", "base::Marsaglia-Multicarry")[i],
      .RNG.seed = c(6666, 9999, 7777, 3333)[i] 
    )
    dump.format(ll)
  }

  monitor = c(
    "b.drift.intercept.p",
    "b.drift.amount.p",
    "sigma.p",
    "b.drift.pr.p",
    "nondectime.p",
    "boundary.p",
    "bias.p",
    "deviance"
  )
  
}  

if (MODEL_NAME %in% c('linear_drift_no_pooling_bd_ddm'))
{
  get_inits <- function(i){
    
    ll <- list(
      b.drift.intercept.p = runif(numSubjs, -.1, .1),
      b.drift.amount.p = runif(numSubjs, -.1, .1),
      nondectime.p = runif(numSubjs, .01, .1),
      boundary.p = runif(numSubjs, 1.1, 2),
      bias.p = runif(numSubjs, .4, .6),
      .RNG.name = c("base::Mersenne-Twister","base::Super-Duper", "base::Wichmann-Hill", "base::Marsaglia-Multicarry")[i],
      .RNG.seed = c(6666, 9999, 7777, 3333)[i] 
    )
    dump.format(ll)
  }

  monitor = c(
    "b.drift.intercept.p",
    "b.drift.amount.p",
    "nondectime.p",
    "boundary.p",
    "bias.p",
    "deviance"
  )
  
}  

if (MODEL_NAME %in% c('linear_drift_no_pooling_ddm'))
{
  get_inits <- function(i){
    
    ll <- list(
      b.drift.intercept.p = runif(numSubjs, -.1, .1),
      b.drift.amount.p = runif(numSubjs, -.1, .1),
      nondectime.p = runif(numSubjs, .01, .1),
      noise.p = runif(numSubjs, 1.1, 2),
      bias.p = runif(numSubjs, .4, .6),
      .RNG.name = c("base::Mersenne-Twister","base::Super-Duper", "base::Wichmann-Hill", "base::Marsaglia-Multicarry")[i],
      .RNG.seed = c(6666, 9999, 7777, 3333)[i] 
    )
    dump.format(ll)
  }

  monitor = c(
    "b.drift.intercept.p",
    "b.drift.amount.p",
    "nondectime.p",
    "noise.p",
    "bias.p",
    "SNR.p",
    "deviance"
  )
  
}  

if (MODEL_NAME %in% c('linear_drift_no_pooling_corr_ddm'))
{
  get_inits <- function(i){
    
    ll <- list(
      b.drift.intercept.p = runif(numSubjs, -.1, .1),
      alpha = runif(1, -.1, .1),
      nondectime.p = runif(numSubjs, .01, .1),
      noise.p = runif(numSubjs, 1.1, 2),
      bias.p = runif(numSubjs, .4, .6),
      .RNG.name = c("base::Mersenne-Twister","base::Super-Duper", "base::Wichmann-Hill", "base::Marsaglia-Multicarry")[i],
      .RNG.seed = c(6666, 9999, 7777, 3333)[i] 
    )
    dump.format(ll)
  }
  
  monitor = c(
    "b.drift.intercept.p",
    "b.drift.amount.p",
    "alpha",
    "nondectime.p",
    "noise.p",
    "bias.p",
    "SNR.p",
    "deviance"
  )
  
}  

myinits = c(
  get_inits(1),
  get_inits(2),
  get_inits(3),
  get_inits(4)
)

######################################################################
# Parameter estimation
######################################################################
mycolumns = c('N',
              'M',
              'RT.signed',
              'instance',
              'amount_later_centered')
jags_data = dump.format(dataList[mycolumns])


myfit <- run.jags(
  model = file.path(MODEL_FILE),
  monitor = monitor,
  data = jags_data,
  n.chains = NCHAINS,
  inits = myinits,
  method = "parallel",
  modules = "wiener",
  burnin = NBURNIN,
  sample = NSAMPLES,
  thin = NTHIN)



######################################################################
# Parameter extraction and storage
######################################################################
datetime = format(Sys.time(), '%d-%m-%Y_%H:%M')

myfit_rjags = as.jags(myfit)
myfit_samples <- rjags::coda.samples(myfit_rjags,
                                     variable.names = monitor,
                                     n.iter = NFINALSAMPLES)
#dic.fit = dic.samples(myfit_rjags, n.iter = NFINALSAMPLES)
#dic.fit = extract(myfit, what = "dic", progres.bar = 'text', by = 100, n.iter = 10)

coda_samples = as.mcmc.list(myfit)

save(
  myfit,
  myfit_rjags,
  monitor,
  myfit_samples,
  coda_samples,
  #dic.fit,
  file = paste0(
    '../results/',
    MODEL_NAME,
    '-',
    gsub(pattern = "\\.RData$", "", INPUT_FILE),
    '-',
    datetime,
    '-',
    NSAMPLES,
    '-',
    NBURNIN,
    '-jags.RData'
  )
)

print("Saved model")
parVals = MCMCpstr(myfit_samples,
                   params = monitor,
                   type = "chains")

print("Computed chains")
myfit_summary = NULL
for (var in monitor) {
  print(var)
  myfit_summary = rbind(myfit_summary, summary(myfit, vars = var))
}
myfit_summary = unique(myfit_summary)
print("Computed summary")

######################################################################
# Generate samples for posterior predictive checks
######################################################################


######################################################################
# Save
######################################################################
save(
  parVals,
  myfit_summary,
  monitor,
  dataList,
  subjList,
  file = paste0(
    '../results/',
    MODEL_NAME,
    '-',
    gsub(pattern = "\\.RData$", "", INPUT_FILE),
    '-',
    datetime,
    '-',
    NSAMPLES,
    '-',
    NBURNIN,
    '-jags-pars.RData'
  )
)

print("Saved parameters")
