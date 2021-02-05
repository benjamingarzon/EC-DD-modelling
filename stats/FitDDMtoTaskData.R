# Fit DDM model to task data
rm(list = ls())

######################################################################
# Definitions
######################################################################
# definitions
library(rstan)
library(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
source('./funcs.R')
WD = '~/Software/EC-DD-Task/data'
WDD = file.path(WD, 'all')

######################################################################
# Arguments
######################################################################

if(T){
  INPUT_FILE = 'processed_data.RData'
  MODEL_NAME = 'dd_hyperbolic_exp_ddm'
  
} else {
  args = commandArgs(trailingOnly=TRUE)
  INPUT_FILE = args[1] #'processed_data.RData'
  MODEL_NAME = args[2] #'dd_hyperbolic_ddm'
}  
MODEL_FILE = file.path('stan_models', paste(MODEL_NAME, 'stan', sep = '.'))

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

######################################################################
# Generate initial values
######################################################################
# This initialization will facilitate the sampling
if (MODEL_NAME == 'dd_hyperbolic_ddm')
  genInitList = function() {
    list(
      mu_p     =  c(rep(0, 4),-3),
      sigma_p    = c(rep(0.5, 4), 3),
      mu_slope_p     = 0,
      sigma_slope_p    = 0.5
    )
  }
if (MODEL_NAME == 'dd_hyperbolic_exp_ddm')
  genInitList = function() {
    list(
      mu_p     =  c(rep(0, 4),-3, 0),
      sigma_p    = c(rep(0.5, 4), 3, 0.1),
      mu_slope_p     = 0,
      sigma_slope_p    = 0.5
    )
  }

######################################################################
# Parameter estimation
######################################################################
# Set sampler parameters
adapt_delta   = 0.99
stepsize      = 1
max_treedepth = 13
nchain        = 4
nthin         = 1
niter         = 500
nwarmup       = 400
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
                 max_treedepth = max_treedepth)
)

######################################################################
# Parameter extraction and storage
######################################################################
save(myfit, file = paste0(
  'results/',
  MODEL_NAME,
  format(Sys.time(), '%d-%m-%Y_%H:%M'),
  '.RData'
))

parVals <- extract(myfit, permuted = T)
myfit_summary = summary(myfit)$summary

save(parVals,
     myfit_summary,
     file = paste0(
       'results/',
       MODEL_NAME,
       format(Sys.time(), '%d-%m-%Y_%H:%M'),
       '-pars.RData'
     ))
