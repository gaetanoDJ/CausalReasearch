
*This do file is created by DC on 03/13/2016.
********************************************************************************
**********************          Master do file         *************************
********************************************************************************
*This do file replicates the tables and figures. All that is needed to be done is
*to modify the directories below. Note that the table outputs are .tex files.



clear all
eststo clear
set more off
macro drop _all
set matsize 11000
set rmsg on

*Please specify the globals below

global data " "
global dofiles " "
global tables " "
global figures " "
global estimates " "
global stn_bsamples " "


********************************************************************************
**********************         Specify ifclauses          **********************
********************************************************************************
*This part defines the treatments. There are three main ways to define the treatments:
local treatdef3 "D.MW_real>0.25  & D.MW_real<. & D.MW>0"						// Every real increase in MW that exceed $0.25 as treatment.


global ifclause "`treatdef3'"

if "$ifclause" == "`treatdef3'"{
global graphtablename "exceed25cents"
global distifclause "L.D.MW_real>0.25  & L.D.MW_real<.& L.D.MW>0"
global missedevents "D.MW_real> 0  & D.MW_real<.& D.MW>0 & D.MW_real<=0.25"
}
*


global Wmax 4
global Wmin 4

global Tmax 16
global Tmin 12

global Tmaxcont = $Tmax
global Tmincont = $Tmin


global numberquant=3

global limittreat "stateonly"
global disagg 	""

global controlmethod  "Absorb"
global contlead ""




********************************************************************************
*************         Creating the data                    *********************
********************************************************************************

			do "${dofiles}state_panels_cents_new_QJE.do"
			do "${dofiles}state_panels_cents_balanced_QJE.do"
			do "${dofiles}state_panels_cents_demogadd_QJE.do"									// This is the final one for creating state panels. This creates state_panels_cents_balanced_add.
			do "${dofiles}clean_VZmwdata_QJE.do"												// Maximum MW of that quarter.
			do "${dofiles}QCEW_multiplier.do"


			do "${dofiles}state_panels_tercile1979_QJE.do"
			do "${dofiles}simplermethod_microdata_QJE.do"


			do "${dofiles}QCEW_multiplier.do"
			do "${dofiles}create_stacked_events.do"
			do "${dofiles}state_panels_cents_new_ind_QJE.do"
			do "${dofiles}CK_predicted_probabilities_for_QJE.do"
			do "${dofiles}matchedCPS_QJE.do"
			do "${dofiles}state_panels_cents_matched_QJE.do"

			do "${dofiles}create_to_combine_consecutive_events_QJE.do"
			do "${dofiles}stacked_data_event_specific_estimates_QJE.do"
			

			do "${dofiles}median_wage_state_quarter_QJE.do"
			do "${dofiles}create_groupdata_QJE.do"
			do "${dofiles}clean_administrativedata_QJE.do"
			
			do "${dofiles}create_data_appendix_real.do"
			do "${dofiles}clean_admin_cps_workingdata_allind_5cents.do"
			do "${dofiles}clean_admin_cps_workingdata_allind_5cents_nominal.do"
			do "${dofiles}CPS_nominal_1979onwards.do"
			do "${dofiles}measurement_error_calculation_all_CPS_QJE.do"
			do "${dofiles}create_quarterly_state_panel.do"
			


********************************************************************************
**************        Define Programs                    ***********************
********************************************************************************

do "${dofiles}create_programs.do"


********************************************************************************
********                   Tables and Figures                          *********
********************************************************************************

do "${dofiles}Table1_for_QJE.do"
do "${dofiles}Table2_for_QJE.do"
do "${dofiles}Table3_for_QJE.do"
do "${dofiles}Table4_for_QJE.do"

do "${dofiles}Figure2_for_QJE.do"
do "${dofiles}Figure4_for_QJE.do"
do "${dofiles}Figure5_for_QJE.do"
do "${dofiles}Figure6_for_QJE.do"												// Also produces Figure A11



********************************************************************************
****************        Apeendix                     ***************************
********************************************************************************

do "${dofiles}Appendix_Figure_A1.do"
do "${dofiles}Appendix_imputation_rate.do"
do "${dofiles}Appendix_self_employment_rate.do"
do "${dofiles}Appendix_Figure_A4.do"
do "${dofiles}Appendix_Figure_A6.do"
do "${dofiles}Appendix_Figure_A7.do"
do "${dofiles}Appendix_Figure_A8.do"
do "${dofiles}Appendix_Figures_A9_A10.do"


do "${dofiles}Appendix_Table_A1_A2_CK.do"
do "${dofiles}Appendix_Table_A1_A2_demog.do"
do "${dofiles}Appendix_Table_A4.do"
do "${dofiles}Appendix_Table_A4_col9.do"
do "${dofiles}Appendix_Table_A5.do"
do "${dofiles}Appendix_Table_A6.do"
do "${dofiles}Appendix_Table_A7.do"



do "${dofiles}Appendix_Figures_C1_C3.do"
do "${dofiles}Appendix_Figure_C2.do"
do "${dofiles}Appendix_Figure_C4.do"


do "${dofiles}Appendix_Figure_D1.do"
do "${dofiles}Appendix_Figure_D2.do"
do "${dofiles}Appendix_Figure_D3_Table_D1_cols2_4.do"
do "${dofiles}Appendix_Table_D1.do"

do "${dofiles}Appendix_Figure_F1_Table_F1_rows_3_4_5.do"
do "${dofiles}Appendix_Table_F1_cols_1_2.do"
do "${dofiles}Appendix_Figure_F2.do"
do "${dofiles}Appendix_Figure_F3.do"
do "${dofiles}Appendix_Table_F2.do"
*At this point, you need to run R code called "deconvolution_QJE.R" to produce deconvolved data.
do "${dofiles}create_me_corrected_logwages_data_QJE.do"
do "${dofiles}Appendix_Figure_F4.do"
do "${dofiles}Appendix_Table_F3.do"


do "${dofiles}Appendix_Figures_G1_A_G3_A_C_G6.do"
do "${dofiles}Appendix_Figures_G1_B_G3_B_D.do"
do "${dofiles}Appendix_Figures_G2_A_C.do"
do "${dofiles}Appendix_Figures_G2_B_D.do"
do "${dofiles}Appendix_Figure_G4_Table_G6.do"
do "${dofiles}Appendix_Figure_G5.do"

do "${dofiles}Appendix_TableG2_col1.do"
do "${dofiles}Appendix_TableG2_cols_2_3_4.do"
do "${dofiles}Appendix_Tables_G3_G4_G7.do"
do "${dofiles}Appendix_Table_G5.do"
do "${dofiles}Appendix_Table_G5_EB.do"
do "${dofiles}Appendix_Table_G8.do"


