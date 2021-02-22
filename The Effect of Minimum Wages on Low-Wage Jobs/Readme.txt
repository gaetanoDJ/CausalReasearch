This is the replication folder of "The effect of minimum wages on low-wage jobs."
It is created by Doruk Cengiz. Please contact me for any questions. E-mail: dcdorukcengiz_at_gmail.com

This archive contains
- Readme.txt: this file
- data1.zip: data files, 1 of 3
- data2.zip: data files, 2 of 3
- data3.zip: data files, 3 of 3
- dofiles.zip: Stata do-files
- estimates.zip: Stata estimates
- figures.zip: figures
- tables.zip: tables
- zipfordataverse.sh: the script that created the above zip files

To use the archive

(1) Decompress all of the .zip files. Make sure each each zip file decompressed
to a directory with the same name as the .zip file: dofiles/, estimates/,
figures/, tables/. All of the dataX.zip files should be decompressed to a
directory called data/.

(2) Specify the appropriate directory names in the global macros at the top
of dofiles/master_QJE.do. We recommend running do files from the master_QJE.do
at all times.

CREATING DATASET
state_panels_cents_new_QJE.do: Creates the main employment counts data.
state_panels_cents_balanced_QJE.do: Turns the data created above into a balanced data by replacing missing counts with 0.
state_panels_cents_demogadd_QJE.do: Adds demographic group populations.
clean_VZmwdata_QJE.do: Creates the minimum wage data.
QCEW_multiplier.do: Creates the QCEW multipliers that are used to correct state-level CPS counts into QCEW counts.
state_panels_tercile1979_QJE.do: Creates the primary state-by-wage-bin-by-quarter data used in the paper.
simplermethod_microdata_QJE.do: Creates the data that is used in the last column of Table 1. It is at state-by-quarter aggregation level.
create_stacked_events.do: Creates stacked data that is at state-by-wage-bin-by-quarter-by-event level.
state_panels_cents_new_ind_QJE.do: Creates data that includes industry-level counts.
CK_predicted_probabilities_for_QJE.do: Creates data that also shows the probability of an individual earning less than 125% of the MW.
matchedCPS_QJE.do: Creates data that turns the CPS data into longitudinal.
state_panels_cents_matched_QJE.do: Turns the matched data into state-by-wage-bin-by-quarter data.
create_to_combine_consecutive_events_QJE.do: Auxiliary data used to combine consecutive events.
stacked_data_event_specific_estimates_QJE.do: Creates event-specific estimates data using stacked data.
median_wage_state_quarter_QJE.do: Creates data that shows state-by-quarter median wages.
create_groupdata_QJE.do: Creates employment counts for 6 age and 4 education levels.
clean_administrativedata_QJE.do: Creates administrative data.
create_data_appendix_real.do: Creates data used in comparing CPS and administrative data.
clean_admin_cps_workingdata_allind_5cents.do: Creates data used in comparing CPS and administrative data (in real terms).
clean_admin_cps_workingdata_allind_5cents_nominal.do: Creates data used in comparing CPS and administrative data (in nominal terms).
CPS_nominal_1979onwards.do: Shows CPS employment counts by nominal wage bins
measurement_error_calculation_all_CPS_QJE.do: Useful for measurement error calculations.

create_programs.do: This is the do file that creates programs that are used almost in any do file that create tables or figures. It is necessary to run this do file at every session.


The do files that create tables and figures are named accordingly.
