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

######################################################################
# Arguments
######################################################################
if (F) {
  # tests
  INPUT_FILE = 'processed_data_censored.RData'
  #  INPUT_FILE = 'processed_data.RData'
  MODEL_NAME = 'linear_drift_noise_ddm_test'
  NSAMPLES = 500
  NBURNIN = 1000
} else {
  args = commandArgs(trailingOnly = TRUE)
  INPUT_FILE = args[1]
  MODEL_NAME = args[2]
  NSAMPLES = as.numeric(args[3])
  NBURNIN = as.numeric(args[4])
}

MODEL_FILE = file.path('models', paste(MODEL_NAME, 'jags', sep = '.'))

######################################################################
# Load and prepare data
######################################################################
load(file.path(WDD, INPUT_FILE))

grouping_var = 'subj_context'
choicedata.common$subj_context = paste(choicedata.common$subjID, choicedata.common$context)
if (F)
  choicedata.common = subset(
    choicedata.common,
    !subjID %in% c(
      "60f534e6b1227dde7dea8273",
      "60f5a27ad908d2f80eaaa443",
      "60fabcb2529d45989426e018",
      "60fcb6fda208edc3ccf3fe6d",
      "60feff18b2baaaf388f1fa86"
    )
  )

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

inits1 <- list(
  noise.mu = 0.001,
  bias.mu = 0.5,
  b.drift.intercept.mu = 0,
  b.drift.amount.mu = 0.02,
  nondectime.mu = 0.01,
  .RNG.name = "base::Super-Duper",
  .RNG.seed = 9999
)

inits2 <- list(
  noise.mu = 0.002,
  bias.mu = 0.5,
  b.drift.intercept.mu = 0.1,
  b.drift.amount.mu = 0.01,
  nondectime.mu = 0.02,
  .RNG.name = "base::Wichmann-Hill",
  .RNG.seed = 1234
)

inits3 <- list(
  noise.mu = 0.003,
  bias.mu = 0.5,
  b.drift.intercept.mu = -0.1,
  b.drift.amount.mu = -0.01,
  nondectime.mu = 0.005,
  .RNG.name = "base::Mersenne-Twister",
  .RNG.seed = 77
)

inits4 <- list(
  noise.mu = 0.001,
  bias.mu = 0.5,
  b.drift.intercept.mu = -0.1,
  b.drift.amount.mu = -0.02,
  nondectime.mu = 0.002,
  .RNG.name = "base::Mersenne-Twister",
  .RNG.seed = 6666
)


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
  "deviance"
)

######################################################################
# Parameter estimation
######################################################################

# Set sampler parameters
NTHIN = 10
NCHAINS = 4
NFINALSAMPLES = min(1000, NSAMPLES)
mycolumns = c(
  'N',
  'M',
  'RT.signed',
  'instance',
  'RT',
  'amount_later',
  'amount_later_centered',
  'amount_sooner',
  'delay_later',
  'choice'
)

mycolumns = c(
  'N',
  'M',
  'RT.signed',
  'instance',
  'amount_later_centered'
# 'amount_later',
# 'amount_sooner',
# 'delay_later',
# 'choice'
)
jags_data = dump.format(dataList[mycolumns])


myfit <- run.jags(
  model = file.path(MODEL_FILE),
  monitor = monitor,
  data = jags_data,
  n.chains = NCHAINS,
  inits = c(dump.format(inits1), dump.format(inits2), dump.format(inits3), dump.format(inits4)),
  method = "parallel",
  modules = "wiener",
  burnin = NBURNIN,
  sample = NSAMPLES,
  thin = NTHIN
)



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
    datetime,
    '-',
    NSAMPLES,
    '-',
    NBURNIN,
    '.RData'
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
    datetime,
    '-',
    NSAMPLES,
    '-',
    NBURNIN,
    '-pars.RData'
  )
)

print("Saved parameters")
