#!/bin/sh

# STAN
NSAMPLES=1000
cd stan
#Rscript FitDDMtoTaskData.R processed_data_censored.RData linear_drift_ddm $NSAMPLES > ../logs/log-linear_drift_ddm-${NSAMPLES}-stan.txt &

# JAGS
cd ../jags

NSAMPLES=10000
NBURNIN=50000

Rscript FitDDMtoTaskData.R processed_data_censored.RData linear_drift_variability_no_pooling_ddm $NSAMPLES $NBURNIN > ../logs/log-linear_drift_variability_no_pooling_ddm-${NSAMPLES}-${NBURNIN}.txt &
Rscript FitDDMtoTaskData.R processed_data_censored.RData linear_drift_no_pooling_ddm $NSAMPLES $NBURNIN > ../logs/log-linear_drift_no_pooling_ddm-${NSAMPLES}-${NBURNIN}.txt &

Rscript FitDDMtoTaskData.R processed_data_censored.RData linear_drift_variability_no_pooling_bd_ddm $NSAMPLES $NBURNIN > ../logs/log-linear_drift_variability_no_pooling_bd_ddm-${NSAMPLES}-${NBURNIN}.txt &
Rscript FitDDMtoTaskData.R processed_data_censored.RData linear_drift_no_pooling_bd_ddm $NSAMPLES $NBURNIN > ../logs/log-linear_drift_no_pooling_bd_ddm-${NSAMPLES}-${NBURNIN}.txt &



#NBURNIN=10000
#Rscript FitDDMtoTaskData.R processed_data_censored.RData linear_drift_variability_no_pooling_ddm $NSAMPLES $NBURNIN > ../logs/log-linear_drift_variability_no_pooling_ddm-${NSAMPLES}-${NBURNIN}.txt

NSAMPLES=10000
NBURNIN=50000
#Rscript FitDDMtoTaskData.R processed_data_censored.RData linear_drift_ddm $NSAMPLES $NBURNIN > ../logs/log-linear_drift_ddm-${NSAMPLES}-${NBURNIN}.txt
##Rscript FitDDMtoTaskData.R processed_data_censored.RData linear_drift_noise_ddm_test $NSAMPLES $NBURNIN #> ../logs/log-linear_drift_ddm-${NSAMPLES}-${NBURNIN}.txt
