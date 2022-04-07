# Fit DDM model to task data
rm(list = ls())

######################################################################
# Definitions
######################################################################
# definitions
library(rstan)
library(RWiener)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
print(getwd())

source('../analysis_funcs.R')
WD = '../../data'
WDD = file.path(WD, 'experiment')

######################################################################
# Arguments
######################################################################

if(F){
  # for tests
  INPUT_FILE = 'processed_data_censored.RData'
  MODEL_NAME = 'linear_nobias_drift_ddm'
  NITER = 600
  NWARMUP = 400
} else {
  args = commandArgs(trailingOnly=TRUE)
  INPUT_FILE = args[1] 
  MODEL_NAME = args[2] 
  NITER = as.numeric(args[3])
  NWARMUP = NITER - 1000
}  
MODEL_FILE = file.path('models', paste(MODEL_NAME, 'stan', sep = '.'))

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
dataList$amount_later_centered = dataList$amount_later - 5
######################################################################
# Generate initial values
######################################################################
# This initialization will facilitate the sampling

if (MODEL_NAME == 'linear_drift_ddm')
  genInitList = function() {
    list(
      mu_p     =  c(-1, 0, 0, 1) + rnorm(4, 0, 0.05),
      sigma_p    = c(0.5, 2, 0.5, 0.5),
      mu_slope_p     = 0 + rnorm(1, 0, 0.05),
      sigma_slope_p    = 0.5
    )
  }

if (MODEL_NAME == 'linear_nobias_drift_ddm')
  genInitList = function() {
    list(
      mu_p     =  c(-1, 0, 1) + rnorm(3, 0, 0.05),
      sigma_p    = c(0.5, 2, 0.5),
      mu_slope_p     = 0 + rnorm(1, 0, 0.05),
      sigma_slope_p    = 0.5
    )
  }

######################################################################
# Parameter estimation
######################################################################
# Set sampler parameters
adapt_delta   = 0.96 #99
stepsize      = 1
max_treedepth = 13
nchain        = 4
nthin         = 2
niter         = NITER
nwarmup       = NWARMUP
nthin         = 1

# Estimation
myfit <- stan(
  file = MODEL_FILE,
  data   = dataList,
  warmup = nwarmup,
  init   = genInitList,
  iter   = niter,
  chains = nchain,
  thin   = nthin,
  control = list(adapt_delta   = adapt_delta,
  #               stepsize = stepsize, 
                 max_treedepth = max_treedepth)
)

######################################################################
# Parameter extraction and storage
######################################################################
datetime = format(Sys.time(), '%d-%m-%Y_%H:%M')
save(myfit, file = paste0(
  '../results/',
  MODEL_NAME,
  datetime, 
  '-', 
  NITER,
  '-stan.RData'
))

parVals <- extract(myfit, permuted = T)
myfit_summary = summary(myfit, probs = c(0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975) )$summary

######################################################################
# Generate samples for posterior predictive checks
######################################################################
NSAMPLES = 1000

save(parVals,
     myfit_summary,
     dataList,
     file = paste0(
       'results/',
       MODEL_NAME,
       datetime,
       '-', 
       NITER,
       '-stan-pars.RData'))
print("Saved parameters.")
