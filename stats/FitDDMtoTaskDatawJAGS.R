# Fit DDM model to task data
rm(list = ls())

######################################################################
# Definitions
######################################################################
# definitions
USER2JAGS = F
GETDIC = F 

if (USER2JAGS){
#  library(dclone)
#  library(parallel)
  library(R2jags)
} else {
  library(runjags)
  library(coda)
} 
library(rjags)
library(MCMCvis)

source('./funcs.R')
WD = '~/Software/EC-DD-Task/data'
WDD = file.path(WD, 'all')

######################################################################
# Arguments
######################################################################
if(F){
  INPUT_FILE = 'processed_data.RData'
  MODEL_NAME = 'attributewise_simple_ddm'
  NSAMPLES = 200
  NBURNIN = 100
  INCLUDED_SUB = '' #results/included_subjects.txt'
} else {
  args = commandArgs(trailingOnly=TRUE)
  INPUT_FILE = args[1] 
  MODEL_NAME = args[2] 
  NSAMPLES = as.numeric(args[3])
  NBURNIN = as.numeric(args[4])
  INCLUDED_SUB = args[5] 
}  

MODEL_FILE = file.path('jags_models', paste(MODEL_NAME, 'jags', sep = '.'))

######################################################################
# Load and prepare data
######################################################################
load(file.path(WDD, INPUT_FILE))
grouping_var = 'subj_context'
choicedata.common$subj_context = paste(choicedata.common$subjID, choicedata.common$context)
if (INCLUDED_SUB != ''){ # if we want to include some subjects only
  included_subjects = unlist(read.table(INCLUDED_SUB))
  choicedata.common = choicedata.common %>% filter (subjID %in% included_subjects)
  SUFFIX = '-subsample'
} else {
  SUFFIX = ''
} 

dataList = organize_data(choicedata.common, grouping_var, long = T)
subjList <-
  unique(choicedata.common[c('subjID', 'context', 'background_col', grouping_var)])  # list of subjects x blocks
numSubjs = dataList$N
dataList$amount_diff = dataList$amount_later - dataList$amount_sooner
dataList$delay_diff = dataList$delay_later - dataList$delay_sooner
dataList$RT.signed = dataList$RT
idx = dataList$choice == 0
dataList$RT.signed[idx] = -dataList$RT[idx]


######################################################################
# Generate initial values
######################################################################
# This initialization will facilitate the sampling
  
  # This initialization will facilitate the sampling
  inits1 <- list( noise.mu=0.5, noise.pr=0.05,  time.mu=0.5, time.pr= 0.05, nondectime.mu=0.05,
                              nondectime.pr=0.05,  b.drift.delay.mu=0.3, b.drift.delay.pr=0.65, b.drift.amount.mu=0.3, 
                              b.drift.amount.pr=3, bias.mu=0.4, bias.kappa=1, RT.signed_pred = dataList$RT.signed,  
                              gamma.mu = 1.05, gamma.pr = 4.3,
                              .RNG.name="base::Super-Duper", .RNG.seed=99999)
  
  inits2 <- list( noise.mu=0.1, noise.pr=0.05,  time.mu=-0.5, time.pr= 0.05, nondectime.mu=0.1,
                              nondectime.pr=0.05, b.drift.delay.mu=0.4, b.drift.delay.pr=0.5, b.drift.amount.mu=0.4, 
                              b.drift.amount.pr=2, bias.mu=0.5, bias.kappa=1, RT.signed_pred = dataList$RT.signed, 
                              gamma.mu = 1.1, gamma.pr = 4.2,
                              .RNG.name="base::Wichmann-Hill", .RNG.seed=1234)
  
  inits3 <- list( noise.mu=0.3, noise.pr=0.05, time.mu=0.1, time.pr= 0.05, nondectime.mu=0.12,
                              nondectime.pr=0.05,  b.drift.delay.mu=0.2, b.drift.delay.pr=0.4, b.drift.amount.mu=0.2, 
                              b.drift.amount.pr=4, bias.mu=0.3, bias.kappa=1, RT.signed_pred = dataList$RT.signed, 
                              gamma.mu = 0.9, gamma.pr = 4.2,
                              .RNG.name="base::Mersenne-Twister", .RNG.seed=77 )
  
  inits4 <- list( noise.mu=0.2, noise.pr=0.05, time.mu=-0.1, time.pr= 0.05, nondectime.mu=0.15,
                              nondectime.pr=0.05,  b.drift.delay.mu=0.1, b.drift.delay.pr=0.3, b.drift.amount.mu=0.1, 
                              b.drift.amount.pr=2.5, bias.mu=0.6, bias.kappa=1, RT.signed_pred = dataList$RT.signed, 
                              gamma.mu = 1, gamma.pr = 4.1, 
                              .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 )

    
  monitor = c("b.drift.delay.mu", "b.drift.amount.mu", 
              "b.drift.delay.pr", "b.drift.amount.pr",
              "b.drift.delay.p",  "b.drift.amount.p", 
              "drift.pr.p",
              "nondectime.mu", "nondectime.pr", "nondectime.p", 
              "noise.mu","noise.pr", "noise.p", 
              "bias.mu", "bias.kappa", "bias.p",
              "deviance")
  
if (MODEL_NAME == 'attributewise_exponential_ddm'){ 
  monitor = c(monitor, "gamma.mu", "gamma.pr", "gamma.p")
}

if (MODEL_NAME == 'attributewise_simple_ddm_2'){ 
  monitor = c(monitor, "time.mu", "time.pr", "time.p")
}
  
######################################################################
# Parameter estimation
######################################################################

# Set sampler parameters
NTHIN = 10
NCHAINS = 4
NFINALSAMPLES = min(1000, NSAMPLES)
#cl <- makePSOCKcluster(NCHAINS)
# Estimation
#if (USER2JAGS) {
  #list2env(list(NBURNIN = NBURNIN, NSAMPLES = NSAMPLES), envir = globalenv())
  #assign("NBURNIN", NBURNIN, globalenv())
  #assign("NITER", NSAMPLES, globalenv())
  # use R2jags
#  myinits =  
  # load.module("wiener")
  # parLoadModule(cl, "wiener")
  # load.module("dic")
  # parLoadModule(cl, "dic")
  # myfit <- jags.parfit(data = dataList,
  #              inits = list(inits1, inits2, inits3, inits4),
  #              params = monitor,
  #              model = file.path(MODEL_FILE),
  #              n.chains = NCHAINS,
  #              n.iter = NSAMPLES,
  #              n.burnin = NBURNIN,
  #              n.thin = NTHIN,
  #              DIC = T)
  # 
  # stopCluster(cl)
  # unload.module("wiener")
  # parUnloadModule(cl, "wiener")
  # unload.module("dic")
  # parUnloadModule(cl, "dic")

# myfit <- do.call(jags.parallel, list(data = dataList,
#                      inits = list(inits1),
#                      #inits = list(inits1, inits2, inits3, inits4),
#                      parameters.to.save = monitor,
#                      model.file = file.path(MODEL_FILE),
#                      n.chains = NCHAINS,
#                      n.iter = NSAMPLES,
#                      #n.burnin = NBURNIN,
#                      #n.thin = NTHIN,
#                      DIC = T, 
#                      jags.module = c("wiener", "dic")))

#stopCluster(cl)
#unload.module("wiener")
#parUnloadModule(cl, "wiener")
#unload.module("dic")
#parUnloadModule(cl, "dic")
#} else {
  jags_data = dump.format(dataList[c('N', 'M', 'RT.signed', 'instance', 'RT','amount_later', 'amount_sooner', 'amount_diff', 'delay_diff')])
  
# runjags    
  if (GETDIC){
    monitor = c(monitor, "dic")
    myfit <- run.jags(model=file.path(MODEL_FILE),
                      monitor = monitor,
                      data = jags_data,
                      n.chains = NCHAINS,
                      inits = c(dump.format(inits1), dump.format(inits2), dump.format(inits3), dump.format(inits4)),
                      method = "simple",
                      modules = c("wiener", "dic"),
                      burnin = NBURNIN,
                      sample = NSAMPLES,
                      thin = NTHIN)
    stophere
  } else {
    
    myfit <- run.jags(model=file.path(MODEL_FILE),
                      monitor = monitor,
                      data = jags_data,
                      n.chains = NCHAINS,
                      inits = c(dump.format(inits1), dump.format(inits2), dump.format(inits3), dump.format(inits4)),
                      method = "parallel",
                      modules = "wiener",
                      burnin = NBURNIN,
                      sample = NSAMPLES,
                      thin = NTHIN)
    
  } 

#}

#stophere

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

save(myfit, myfit_rjags, monitor, myfit_samples, coda_samples, #dic.fit, 
     file = paste0(
       'results/',
       MODEL_NAME,
       '-', 
       datetime, 
       '-', 
       NSAMPLES,
       '-',
       NBURNIN,
       SUFFIX, 
       '.RData'
     ))

print("Saved model")
parVals = MCMCpstr(
  myfit_samples,
  params = monitor,
  type = "chains"
)

print("Computed chains")
myfit_summary = NULL
for (var in monitor){
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
save(parVals,
  myfit_summary,
  monitor,
  dataList,
  subjList,
  file = paste0(
    'results/',
    MODEL_NAME,
    '-', 
    datetime,
    '-', 
    NSAMPLES,
    '-',
    NBURNIN, 
    SUFFIX, 
    '-pars.RData'))

print("Saved parameters")
