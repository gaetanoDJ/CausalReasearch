#!/bin/sh
# zip the replication folder for use with Havard's Dataverse
# Dataverse flattens all files, except for zip files
# so we zip everything to maintain a useful datastructure

zip -r dofiles.zip dofiles

zip -r estimates.zip estimates

zip -r figures.zip figures

zip -r tables.zip tables

# zip data directory in parts to conform to Dataverse file size limits
zip -r data1.zip data -x data/totransfer.dta data/totransfer_ind.dta data/state_panels_with3quant1979_matched.dta data/grouplevelanalysis_final.dta data/matchedCPS_1979_2016.dta data/state_panels_with3quant1979.dta data/totransfer_2.dta

zip -r data2.zip data/totransfer.dta data/totransfer_ind.dta data/state_panels_with3quant1979_matched.dta

zip -r data3.zip data/grouplevelanalysis_final.dta data/matchedCPS_1979_2016.dta data/state_panels_with3quant1979.dta data/totransfer_2.dta
