#!/bin/sh
NSAMPLES=1500
Rscript FitDDMtoTaskData.R processed_data.RData dd_hyperbolic_ddm $NSAMPLES &
Rscript FitDDMtoTaskData.R processed_data.RData dd_hyperbolic_nl_ddm $NSAMPLES &
Rscript FitDDMtoTaskData.R processed_data.RData dd_hyperbolic_nl2_ddm $NSAMPLES &