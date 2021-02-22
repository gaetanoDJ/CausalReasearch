
*This do file is created by DC on 11/2/2018.
********************************************************************************
************      Creating stacked analysis table                      *********
********************************************************************************

*The missing estimate is the third column. The following code reproduces it.
clear
do "${dofiles}stacked_analysis_event_by_event_FP_confidence_intervals_wage_bins_clean_controls_QJE.do"
do "${dofiles}stacked_events_event_specific_regressions_DC_fulltable_clean_controls_QJE.do"

clear
eststo clear
*First column is the baseline estimates.
est use "${estimates}Table1_TWFE_estimates_full"
eststo col1

*The second column is baseline specification for stacked analysis.
est use "${estimates}stackedevents_wagebins_clean_controls_overall_model2_pre12_estadd"
local belsh  = `e(belsh)'
local minwpc = `e(minwpc)'  
local numevent = `e(numevent)'
local numobs = `e(numobs)'
eststo col2

*The third column is for manually averaged estimates
est use "${estimates}stackedevents_wagebins_clean_controls_overall_manually_averaged_model_pre12_estadd"
estadd local belsh "`belsh'"
estadd local minwpc "`minwpc'"
estadd local numevent = "`numevent'"
estadd local numobs = "`numobs'"

eststo col3


*Fourth column is for the specification that includes events occurred before or in 2012q1.
est use "${estimates}stackedevents_wagebins_clean_controls_overall_model22_pre12_estadd"
eststo col4


	esttab  col1 col2 col3 col4 using "${tables}TableD1.tex",	/// 
	replace stats(below_Eb below_Ese above_Eb above_Ese  blankspace WB_Eb WB_Ese  bunching_Eb bunching_Ese blankspace bunchelas_Eb bunchelas_Ese labordem_Eb labordem_Ese blankspace  belsh  minwpc  numevent numobs blankspace blankspace wagebinstate /*periodfe*/ wagebinperiod trend qtrend divtime stp, label("Missing jobs below new MW (`=char(36)' \Delta `=char(36)' b)" " " "Excess jobs above new MW (`=char(36)' \Delta `=char(36)' a)"  " " " "   "\%`=char(36)'\Delta`=char(36)' affected wages" " " "\%`=char(36)'\Delta`=char(36)' affected employment " " " " "  "Employment elasticity w.r.t. MW" " " "Emp. elasticity w.r.t. affected wage" " " " " "Jobs below new MW (`=char(36)'\overline{b}\ _{-1}`=char(36)')" "\%`=char(36)'\Delta`=char(36)' MW" "\# Event " "N " " " "\underline{\textit{Controls}}" "Bin-state FE" /*"Period FE"*/ "Bin-period FE " "Bin-state linear trends" "Bin-state quadratic trends" "Bin-division-period FE" "State-period FE" )) ///
	nodep nogaps cells(none) nomtitles fragment booktabs


