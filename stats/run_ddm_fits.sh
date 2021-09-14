#!/bin/sh
# JAGS
NSAMPLES=10000
#NBURNIN=500000
for NBURNIN in 300000; do 
echo " "
# 20000 is  - 4
#Rscript FitDDMtoTaskDatawJAGS.R processed_data_censored.RData value_diff_simple_ddm $NSAMPLES $NBURNIN > logs/log-value_diff_simple_ddm-${NSAMPLES}-${NBURNIN}.txt nocalib ""&
#Rscript FitDDMtoTaskDatawJAGS.R processed_data.RData dd_hyperbolic_simple_ddm $NSAMPLES $NBURNIN > logs/log-dd_hyperbolic_simple_ddm-${NSAMPLES}-${NBURNIN}.txt nocalib ""&
#Rscript FitDDMtoTaskDatawJAGS.R processed_data.RData dd_hyperbolic_simple_ddm $NSAMPLES $NBURNIN > logs/log-dd_hyperbolic_simple_ddm-${NSAMPLES}-${NBURNIN}-calib.txt calib ""&
#Rscript FitDDMtoTaskDatawJAGS.R processed_data.RData dd_hyperbolic_calibration_ddm $NSAMPLES $NBURNIN > logs/log-dd_hyperbolic_calibration_ddm-${NSAMPLES}-${NBURNIN}-calib.txt calib ""&
#Rscript FitDDMtoTaskDatawJAGS.R processed_data.RData value_diff_ddm $NSAMPLES $NBURNIN > logs/log-value_diff_ddm-${NSAMPLES}-${NBURNIN}.txt ""&
#Rscript FitDDMtoTaskDatawJAGS.R processed_data.RData dd_hyperbolic_ddm $NSAMPLES $NBURNIN > logs/log-dd_hyperbolic_ddm-${NSAMPLES}-${NBURNIN}.txt ""&
#Rscript FitDDMtoTaskDatawJAGS.R processed_data.RData dd_hyperbolic_simple2_ddm $NSAMPLES $NBURNIN > logs/log-dd_hyperbolic_simple2_ddm-${NSAMPLES}-${NBURNIN}.txt ""&
#Rscript FitDDMtoTaskDatawJAGS.R processed_data.RData undiscounted_value_diff_ddm $NSAMPLES $NBURNIN > logs/log-undiscounted_value_diff_ddm-${NSAMPLES}-${NBURNIN}.txt ""&
done
#exit 1

#Rscript FitDDMtoTaskDatawJAGS.R processed_data.RData attributewise_ddm $NSAMPLES $NBURNIN > logs/log-${NSAMPLES}-${NBURNIN}.txt &
#Rscript FitDDMtoTaskDatawJAGS.R processed_data.RData attributewise_ddm_2 $NSAMPLES $NBURNIN > logs/log-attributewise_ddm_2-${NSAMPLES}-${NBURNIN}.txt &
#Rscript FitDDMtoTaskDatawJAGS.R processed_data.RData attributewise_simple_ddm $NSAMPLES $NBURNIN > logs/log-attributewise_simple_ddm-${NSAMPLES}-${NBURNIN}.txt ""&
#Rscript FitDDMtoTaskDatawJAGS.R processed_data.RData attributewise_simple_ddm $NSAMPLES $NBURNIN > logs/log-attributewise_simple_ddm-${NSAMPLES}-${NBURNIN}.txt ""&
#Rscript FitDDMtoTaskDatawJAGS.R processed_data.RData attributewise_exponential_ddm $NSAMPLES $NBURNIN > logs/log-attributewise_exponential_ddm-${NSAMPLES}-${NBURNIN}.txt "" &
#Rscript FitDDMtoTaskDatawJAGS.R processed_data.RData attributewise_simple_ddm $NSAMPLES $NBURNIN > logs/log-attributewise_simple_ddm-${NSAMPLES}-${NBURNIN}-subsample.txt results/included_subjects.txt &
#Rscript FitDDMtoTaskDatawJAGS.R processed_data.RData attributewise_exponential_ddm $NSAMPLES $NBURNIN > logs/log-attributewise_exponential_ddm-${NSAMPLES}-${NBURNIN}-subsample.txt results/included_subjects.txt &

# STAN
# 3000
# 3500 wider priors
# 4000 cauchy
NSAMPLES=11000
#Rscript FitDDMtoTaskData.R processed_data.RData dd_hyperbolic_ddm $NSAMPLES > logs/log-dd_hyperbolic_ddm-${NSAMPLES}-stan.txt &

Rscript FitDDMtoTaskData.R processed_data_censored.RData linear_drift_ddm $NSAMPLES > logs/log-linear_drift_ddm-${NSAMPLES}-stan.txt &
Rscript FitDDMtoTaskData.R processed_data_censored.RData linear_nobias_drift_ddm $NSAMPLES > logs/log-linear_nobias_drift_ddm-${NSAMPLES}-stan.txt &

#Rscript FitDDMtoTaskData.R processed_data.RData value_diff_ddm $NSAMPLES > logs/log-value_diff_ddm-${NSAMPLES}-stan.txt &
#Rscript FitDDMtoTaskData.R processed_data.RData undiscounted_value_diff_ddm $NSAMPLES > logs/log-undiscounted_value_diff_ddm-${NSAMPLES}-stan.txt &
#Rscript FitDDMtoTaskData.R processed_data.RData dd_hyperbolic_nl_ddm $NSAMPLES 
#Rscript FitDDMtoTaskData.R processed_data.RData dd_hyperbolic_nl2_ddm $NSAMPLES &


