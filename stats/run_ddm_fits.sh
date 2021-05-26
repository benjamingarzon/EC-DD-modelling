#!/bin/sh
# JAGS
NSAMPLES=10000
NBURNIN=100000
#Rscript FitDDMtoTaskDatawJAGS.R processed_data.RData attributewise_ddm $NSAMPLES $NBURNIN > logs/log-${NSAMPLES}-${NBURNIN}.txt &
#Rscript FitDDMtoTaskDatawJAGS.R processed_data.RData attributewise_ddm_2 $NSAMPLES $NBURNIN > logs/log-attributewise_ddm_2-${NSAMPLES}-${NBURNIN}.txt &
#Rscript FitDDMtoTaskDatawJAGS.R processed_data.RData attributewise_simple_ddm $NSAMPLES $NBURNIN > logs/log-attributewise_simple_ddm-${NSAMPLES}-${NBURNIN}.txt ""&
#Rscript FitDDMtoTaskDatawJAGS.R processed_data.RData attributewise_simple_ddm $NSAMPLES $NBURNIN > logs/log-attributewise_simple_ddm-${NSAMPLES}-${NBURNIN}.txt ""&
#Rscript FitDDMtoTaskDatawJAGS.R processed_data.RData attributewise_exponential_ddm $NSAMPLES $NBURNIN > logs/log-attributewise_exponential_ddm-${NSAMPLES}-${NBURNIN}.txt "" &
#Rscript FitDDMtoTaskDatawJAGS.R processed_data.RData attributewise_simple_ddm $NSAMPLES $NBURNIN > logs/log-attributewise_simple_ddm-${NSAMPLES}-${NBURNIN}-subsample.txt results/included_subjects.txt &
#Rscript FitDDMtoTaskDatawJAGS.R processed_data.RData attributewise_exponential_ddm $NSAMPLES $NBURNIN > logs/log-attributewise_exponential_ddm-${NSAMPLES}-${NBURNIN}-subsample.txt results/included_subjects.txt &

# STAN
NSAMPLES=2500
Rscript FitDDMtoTaskData.R processed_data.RData value_diff_ddm $NSAMPLES &
#Rscript FitDDMtoTaskData.R processed_data.RData dd_hyperbolic_nl_ddm $NSAMPLES &
#Rscript FitDDMtoTaskData.R processed_data.RData dd_hyperbolic_nl2_ddm $NSAMPLES &


