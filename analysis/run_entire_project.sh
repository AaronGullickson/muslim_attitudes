#!/bin/bash

#Remove all logs, output, and html reports
rm output/*
rm logs/*
rm *.html

#check for necessary R packages
Rscript check_packages.R

#Process and clean the raw data, including multiple imputation
Rscript construct_analytical_data.R

#Run the factor analysis
Rscript -e "rmarkdown::render('factor_analysis.Rmd')"

#Run the main analysis
Rscript -e "rmarkdown::render('analysis.Rmd')"
