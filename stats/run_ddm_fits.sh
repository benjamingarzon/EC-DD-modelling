#!/bin/sh

# JAGS
cd jags

NSAMPLES=20000 #
NBURNIN=50000 #
Rscript FitDDMtoTaskData.R processed_data_balanced_censored.RData linear_drift_no_pooling_ddm $NSAMPLES $NBURNIN > ../logs/log-linear_drift_no_pooling_ddm-${NSAMPLES}-${NBURNIN}.txt &
#Rscript FitDDMtoTaskData.R processed_data_balanced_censored_100.RData linear_drift_no_pooling_ddm $NSAMPLES $NBURNIN > ../logs/log-linear_drift_no_pooling_ddm-${NSAMPLES}-${NBURNIN}_100.txt &

#Rscript FitDDMtoTaskData.R processed_data_censored-simulated.RData linear_drift_no_pooling_ddm $NSAMPLES $NBURNIN > ../logs/log-linear_drift_no_pooling_ddm-${NSAMPLES}-${NBURNIN}.txt &
