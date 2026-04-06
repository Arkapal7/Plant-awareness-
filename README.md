This folder contains three R code scripts dedicated for data acquisition, collation, analysis and visualisation for the Plant Awareness project. 
The details of each R script are provided as follows: 

1) Part 1 - DataAcquisitionCollation
  This script file performs data acquisition from WikiData and Wikipedia, and collate the data with other datasets.
The steps are provided accordingly:
  a) Downloading and curating WCVP checklist
  b) Wiki QID retrieval
  c) Getting species titles of retrieved QIDs across all languages
  d) Retrieving monthly page views and getting total views from the available titles
  e) Collating all the language page views for each plant species into a dataset
  f) Curating WCUP dataset
  g) Collating WCUP data with the main dataset
  h) Integrating range distribution for each plant species into the dataset
  i) Preparing the local-language Wikipedia dataset and collating it with the main dataset

3) Part 2 - DataAnalysis
   This script performs the GLM analysis and following post-hoc tests for the study:
   a) Performing GLM analyses
   b) Extracting model info and plotting
   c) Checking contrasts for Wikipedia language across covariates
   d) Checking emmeans contrasts across models
   e) Other covariates

4) Part 3 - DataVisualisation
   This script performs the data visualisations used for exploratory data analysis and preparing maps:
   a) Visualising threat vs views
   b) Visualising threat and use vs views
   c) Visualising threat, use and range area vs views
   d) Visualising distribution maps

The provided code scripts will help the user go through the scripts and reproduce the complete data workflow followed for the research article titled:
"Rare and famous: Understanding vascular plant awareness globally through Wikipedia page views"
