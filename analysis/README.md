# Analysis directory

This is the main directory for the analysis. All of the scripts necessary to run the analysis are placed directly into this directory. All raw datasets are in the `input` directory. Constructerd analytical data and output is in the `output` directory. Logs of the last run of certain scripts are in the `logs` directory.

The file `run_entire_project.sh` contains information on the proper order to run all scripts and R Markdown files in to reproduce the analysis. We briefly review all the files here.

### Utility scripts

- `check_packages.R` - This will check for and load all of the required libraries for the project> If not found, it will attempt to install them from CRAN and then load them.
- `useful_functions.R` - A collection of functions that are used across multiple scripts.

### Organizing data scripts

- `construct_analytical_data.R` - A script that sources the various other scripts that are used to go from the raw data to the analytical data and combines the output to produce the final analytical dataset.
- `organize_world_muslim_pew_data.R` - This script reads in the raw data from the Pew data on *The World’s Muslims: Religion, Politics and Society*. It cleans and recodes the data as necessary and extracts a dataset of just the required variables and countries.
- `organize_african_pew_data.R` - This script reads in the raw data from the Pew data on *Tolerance and Tension: Islam and Christianity in Sub-Saharan Africa*. It cleans and recodes the data as necessary and extracts a dataset of just the required variables and countries.
- `imputation.Rmd` This R Markdown file runs a multiple imputation on missing values in the full analytical dataset.
- `add_country_data.R` - This file merges in country-level data (particularly the HDI and GDP measures) with the individual level pew data.
- `code_variables.R` - This file codes final variables needed from the analysis from the merged individual and country-level data.

### Analysis scripts

- `factor_analysis.Rmd` - This R Markdown files conducts an exploratory factor analysis of several constructs considered for the final analysis. 
- `analysis.Rmd` - This R Markdown file conducts the full analysis including a variety of multilevel models. 

All Rmd files produce associated html files that can be read as research logs.