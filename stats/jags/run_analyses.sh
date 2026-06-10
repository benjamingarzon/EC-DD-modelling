
#export TMPDIR=~/ddm/EC-DD-modelling/stats/tmp

Rscript -e "rmarkdown::render('AnalysisDDMModelParameters.Rmd')" > ../logs/AnalysisDDMModelParameters.log &
#Rscript -e "rmarkdown::render('ParameterRecovery.Rmd')" > ../logs/ParameterRecovery.Rmd &
