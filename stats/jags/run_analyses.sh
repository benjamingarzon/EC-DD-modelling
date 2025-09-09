Rscript -e "rmarkdown::render('AnalysisDDMModelParameters.Rmd')" > ../logs/AnalysisDDMModelParameters.log &
Rscript -e "rmarkdown::render('ParameterRecovery.Rmd')" > ../logs/ParameterRecovery.Rmd &

#Rscript -e "rmarkdown::render('AnalysisDDMModelParameters.Rmd', params = list(init100 = 'true'), 
#output_file = 'AnalysisDDMModelParameters_init100.html')" > ../logs/AnalysisDDMModelParameters_init100.log 
