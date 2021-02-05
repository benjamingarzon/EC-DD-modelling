#!/bin/sh
Rscript FitDDMtoTaskData.R processed_data.RData dd_hyperbolic_ddm &
Rscript FitDDMtoTaskData.R processed_data.RData dd_hyperbolic_exp_ddm &