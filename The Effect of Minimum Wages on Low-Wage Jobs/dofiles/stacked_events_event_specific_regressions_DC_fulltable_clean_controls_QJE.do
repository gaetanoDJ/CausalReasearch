set more off
set trace off
set matsize 11000
clear all

do "${dofiles}create_programs.do"

* prepare the data and treatment vector for regressions
*foreach pre of numlist 12 20{
local pre 12
use "${data}stackedevents_regready_pre`pre'.dta", clear

* remove data where we can't determine when there are wage imputations
keep if cleansample == 1



* define treatment vector
local treat F*treat L*treat
global treat F*treat L*treat
* ensure events are numbered properly: N events indexed 1...N
sum event
local numevents = r(max)
tab event, nofreq
assert `r(r)' == `numevents'

tsset eventstate time

rename overallexcessepop above_epop
rename overallmissingepop below_epop
rename overallupperepop upper_epop
rename overalltotalepop all_epop
gen affected_epop = above_epop + below_epop
gen totalbelow_epop = all_epop - (above_epop + upper_epop)

gen byte yearbeforeevent = F1.origevent == 1 | F2.origevent == 1 | F3.origevent == 1 | F4.origevent == 1
*

* save the regression-ready sample
compress
egen eventtime = group(event time)
egen eventorigcontrol = group(event control_origeventpost)
egen eventfedcontrol = group(event control_fedeventpost)
egen eventothercontrol = group(event control_othereventpost)

bys statenum event: egen states_to_drop = max(control_origeventpost)
drop if states_to_drop!=0 & treatedstatenum!=statenum
assert control_origeventpost==0 if treatedstatenum!=statenum
sum control_origeventpost,meanonly
assert r(mean)!=0

tempfile regready
save `regready'

	tab event, nofreq
	global numevents = `r(r)'

	* pre-treatment epop of treated states
sum all_epop [aw=aveoverallpop] if origevent_treated == 1 & yearbeforeevent == 1
global epop = r(mean)


*The following are from baseline regressions.
global EWB = .350835006841847
global wagemult = 8.774882267779565
global mwpc = .1012900314042275
local mwpc2 : di  %04.3f ${mwpc}
global B = .0864819457562104
* scaling factor policy elasticity
global C = 1/(${epop}*${mwpc})






keep overallp0epop overallp1epop overallp2epop overallp3epop overallp4epop overallm1epop overallm2epop overallm3epop overallm4epop eventstate eventtime 
reshape long overall, i(eventstate eventtime) j(wage_bin) string
rename overall countpc
merge m:1 eventstate eventtime using `regready', assert(3) nogenerate keepusing(aveoverallpop eventorigcontrol eventfedcontrol eventothercontrol statenum quarterdate treatedstate treatdate event ${treat})
foreach vv of varlist $treat {
	foreach num of numlist 0(1)4{
	gen `vv'_p`num' = `vv' * (wage_bin=="p`num'epop")
	}
}
foreach vv of varlist $treat {
	foreach num of numlist 1(1)4{
	gen `vv'_m`num' = `vv' * (wage_bin=="m`num'epop")
	}
}

global treatvector F*treat_* L*treat_*
egen wage_bin_num = group(wage_bin)
*We define regready again here.
tempfile regready
save `regready'


* define cumulative effects
* everything normalized to F4
local maxlag = 16
foreach i of numlist 0(4)`maxlag' {
	foreach num of numlist 0(1)4{
	global lincomaggL`i'_p`num' (_b[L`i'treat_p`num'] - _b[F4treat_p`num'])
	}
}
foreach i of numlist 4(4)12 {
	foreach num of numlist 0(1)4{
	global lincomaggF`i'_p`num' (_b[F`i'treat_p`num'] - _b[F4treat_p`num'])
	}
}
*
foreach i of numlist 0(4)`maxlag' {
	foreach num of numlist 1(1)4{
	global lincomaggL`i'_m`num' (_b[L`i'treat_m`num'] - _b[F4treat_m`num'])
	}
}
foreach i of numlist 0(4)12 {
	foreach num of numlist 1(1)4{
	global lincomaggF`i'_m`num' (_b[F`i'treat_m`num'] - _b[F4treat_m`num'])
	}
}
*
*For wage bins
foreach num of numlist 0(1)4{
	global lincomagg_p`num' "0 "
	foreach i of numlist 0(4)`maxlag' {
	global lincomagg_p`num' " ${lincomagg_p`num'} +  ${lincomaggL`i'_p`num'}"
	}
	global lincomagg_p`num' "(${lincomagg_p`num'})" 
}

*
foreach num of numlist 1(1)4{
	global lincomagg_m`num' " 0"
	foreach i of numlist 0(4)`maxlag' {
	global lincomagg_m`num' " ${lincomagg_m`num'} +  ${lincomaggL`i'_m`num'}"
	}
	global lincomagg_m`num' "(${lincomagg_m`num'})" 

}
*
*wage effect globals


foreach t of numlist -`pre'(4)`maxlag' {
				
	if `t' < -4  {	
		local nt = -`t' 
		global aboveWBF`nt' "((${lincomaggF`nt'_p0})*(${wagemult}+0) )"
		forval j = 1/4 {
		global aboveWBF`nt' "${aboveWBF`nt'} + ((${lincomaggF`nt'_p`j'})*(${wagemult}+`j') )"
	}
	global belowWBF`nt' "((${lincomaggF`nt'_m1})*(`wagemult'-1) )"
	forval j = 2/4 {
		global belowWBF`nt' "${belowWBF`nt'} + ((${lincomaggF`nt'_m`j'})*(${wagemult}-`j') )"
	}

	}

					
	if `t' >= 0 {	
		 
		global aboveWBL`t' "((${lincomaggL`t'_p0})*(${wagemult}+0))"
		forval j = 1/4 {
			global aboveWBL`t' "${aboveWBL`t'} + ((${lincomaggL`t'_p`j'})*(${wagemult}+`j'))"
		}
		global belowWBL`t' "((${lincomaggL`t'_m1})*(${wagemult}-1) )"
		forval j = 2/4 {
			global belowWBL`t' "${belowWBL`t'} + ((${lincomaggL`t'_m`j'})*(${wagemult}-`j'))"
		}

	}
	
 
} 
 			global aboveWBF4 = 0
 			global belowWBF4 = 0
			
			

global aboveWB_full "${aboveWBL0}"
global belowWB_full "${belowWBL0}"

forval t = 4(4)`maxlag' {
			
				global belowWB_full "${belowWB_full} + ${belowWBL`t'}"
				global aboveWB_full "${aboveWB_full} + ${aboveWBL`t'}"
			}
*



*For time paths
foreach i of numlist 0(4)`maxlag'{
global lincomaboveL`i' "0"
	foreach num of numlist 0(1)4{
	global lincomaboveL`i' " ${lincomaboveL`i'} +  ${lincomaggL`i'_p`num'}"
	}
	global lincomaboveL`i' "(${lincomaboveL`i'})" 
}
foreach i of numlist 4(4)12{
global lincomaboveF`i' "0"
	foreach num of numlist 0(1)4{
	global lincomaboveF`i' " ${lincomaboveF`i'} +  ${lincomaggF`i'_p`num'}"
	}
	global lincomaboveF`i' "(${lincomaboveF`i'})" 
}
*
foreach i of numlist 0(4)`maxlag'{
global lincombelowL`i' "0"
	foreach num of numlist 1(1)4{
	global lincombelowL`i' " ${lincombelowL`i'} +  ${lincomaggL`i'_m`num'}"
	}
	global lincombelowL`i' "(${lincombelowL`i'})" 
}
foreach i of numlist 4(4)12{
global lincombelowF`i' "0"
	foreach num of numlist 1(1)4{
	global lincombelowF`i' " ${lincombelowF`i'} +  ${lincomaggF`i'_m`num'}"
	}
	global lincombelowF`i' "(${lincombelowF`i'})" 
}
*


global lincomabove "(${lincomagg_p0} + ${lincomagg_p1} + ${lincomagg_p2} + ${lincomagg_p3} + ${lincomagg_p4}) "
global lincombelow "(${lincomagg_m1} + ${lincomagg_m2} + ${lincomagg_m3} + ${lincomagg_m4}) "

local group overall
cap drop panel_var
egen panel_var = group(eventstate wage_bin_num)
xtset panel_var quarterdate
use `regready', clear




local group overall 
	foreach eventno of numlist 1(1)138 {
	qui{
		use if event==`eventno' using `regready', clear
		* model 2 - controls for other events

		reghdfe countpc ${treatvector} [aw=aveoverallpop], a(i.eventstate#i.wage_bin_num i.eventtime#i.wage_bin_num i.eventorigcontrol#i.wage_bin_num i.eventfedcontrol#i.wage_bin_num i.eventothercontrol#i.wage_bin_num) 
			global E = ${epop}
			local denominator = 1/5
			*Wage bins
			foreach num of numlist 0(1)4{
				lincom ${lincomagg_p`num'}*(`denominator')*(1/${E})	
				local p`num' = r(estimate)
			}
			foreach num of numlist 1(1)4{
				lincom ${lincomagg_m`num'}*(`denominator')*(1/${E})	
				local m`num' = r(estimate)
			}
			
			lincom (${lincombelow})*(`denominator')*(1/${E})							 				// Not Multiplying by 4 because bins are in $1 precision. To average by time, dividing by 5.
			local below = r(estimate)
			lincom (${lincomabove})*(`denominator')*(1/${E})							
			local above  = r(estimate)
			lincom ((${lincomabove} + ${lincombelow})*(`denominator')*(1/${E}))/${B} 	 			
			local aff = r(estimate)
			lincom (${lincombelow} + ${lincomabove})*(`denominator')*${C}				
			local mwelast = r(estimate)
			
			nlcom ((((${belowWB_full}+${aboveWB_full})*(`denominator')/$EWB ) - ///
			(((${lincombelow} + ${lincomabove})*(`denominator')*(1/${E}))/${B}))) / ///
			(1+(((${lincombelow} + ${lincomabove})*(`denominator')*(1/${E}))/${B}))	
			mat temp1 = r(b)
			local wage_eff = temp1[1,1]
			*Sometimes nlcom cannot reach the answer. In those cases, it is convenient to multiply the estimate by some large number, then divide it
			nlcom (a1: 1000* (((${lincombelow} + ${lincomabove})*(`denominator')*(1/${E}))/${B}) /  ///
			(((((${belowWB_full}+${aboveWB_full})*(`denominator')/$EWB ) - ///
			(((${lincombelow} + ${lincomabove})*(`denominator')*(1/${E}))/${B}))) / ///
			(1+(((${lincombelow} + ${lincomabove})*(`denominator')*(1/${E}))/${B})))), post
			
			nlcom _b[a1]/1000
			
			mat temp1 = r(b)
			local emp_elas_wage = temp1[1,1]
		
			sum treatedstate, meanonly
			local state = r(mean)
			sum treatdate ,meanonly
			local quarter = r(mean)
			sum aveoverallpop if statenum == treatedstate,meanonly
			local pop = r(mean)
			clear
			set obs 1
			gen event_no = `eventno'
			gen below_estimate = `below'
			gen above_estimate = `above'
			gen perc_aff_emp_estimate = `aff'
			gen mwelas_estimate = `mwelast'
			gen wage_estimate = `wage_eff'
			gen emp_elas_wage_estimate = `emp_elas_wage'
			gen statenum = `state'
			gen quarterdate= `quarter'
			gen epop = ${epop}
			gen C = $C
			gen mwpc = $mwpc
			gen belowshare = $B
			gen population = `pop'
			gen denominator = `denominator'
			foreach num of numlist 0(1)4{
				gen p`num' = `p`num''
			}
			foreach num of numlist 1(1)4{
				gen m`num' = `m`num''
			}
			if `eventno'==1{
				save "${data}stacked_events_event_specific_fulltable_clean_controls.dta", replace
			}
			else{
				append using "${data}stacked_events_event_specific_fulltable_clean_controls.dta"
				compress
				save "${data}stacked_events_event_specific_fulltable_clean_controls.dta", replace
			}
	
	
		}
	di "Event `eventno' denominator is `denominator'."

}
*



use "${data}stacked_events_event_specific_fulltable_clean_controls.dta", clear


foreach num of numlist 0(1)4{
cap drop _p`num' 
	sum p`num' [aw=pop], meanonly
	gen _p`num' = r(mean)
}
*
foreach num of numlist 1(1)4{
	cap drop _m`num' 
	sum m`num' [aw=pop], meanonly
	gen _m`num' = r(mean)
}
*

cap drop perc_aff_emp_estimate2 
gen perc_aff_emp_estimate2 = (_p0 + _p1 + _p2 + _p3 + _p4 + _m1 + _m2 + _m3 + _m4)/${B}

cap drop wage_estimate2 
gen wage_estimate2 = _p0*epop*${wagemult}
foreach num of numlist 1(1)4{
replace wage_estimate2 = wage_estimate2 + _p`num'*epop*(${wagemult}+ `num')
}

foreach num of numlist 1(1)4{
replace wage_estimate2= wage_estimate2+ _m`num'*epop*(${wagemult} - `num')
}



replace wage_estimate2 = wage_estimate2/${EWB}
replace wage_estimate2= (wage_estimate2-perc_aff_emp_estimate2)/(1+perc_aff_emp_estimate2)
compress

*Estimates
cap drop below_estimate2 
cap drop above_estimate2 
cap drop mw_elast2
sum below_estimate [aw=pop], meanonly
gen below_estimate2 = r(mean) 
sum above_estimate [aw=pop], meanonly
gen above_estimate2 = r(mean) 

gen mw_elast2 = (below_estimate2 +above_estimate2 )*epop*C

save "${data}stacked_events_event_specific_fulltable_clean_controls.dta" , replace


********************************************************************************
****************          FP confidence intervals by wage bin         **********
********************************************************************************

use "${data}stacked_events_event_specific_fulltable_clean_controls.dta" , clear
sum epop ,meanonly
local epop = r(mean)

sum wage_estimate2 [aw=pop], meanonly
global new_wage_null = r(mean)
sum perc_aff_emp_estimate2 [aw=pop], meanonly
global new_emp_null = r(mean)

sum below_estimate2  [aw=pop], meanonly
global new_below_null = r(mean)
sum above_estimate2  [aw=pop], meanonly
global new_above_null = r(mean)
sum mw_elast2 [aw=pop], meanonly
global new_mwelast_null = r(mean)

gen emp_elas_wrt_wage = perc_aff_emp_estimate2/wage_estimate2
sum emp_elas_wrt_wage [aw=pop],meanonly
global new_emp_elas_wage_null = r(mean)




set more off

*Required, takes some time.
global num_iter = 1000

foreach vv in  m4 m3 m2 m1 p0 p1 p2 p3 p4{ 
set seed 9856840
	forval num =1(1)$num_iter {
	di "`num' for `vv'."
		use "${data}FP_errors_post_wagebin_`vv'_clean_controls.dta", clear
		qui sample 0.1, by(event_no)
		sum B1 [aw=population], meanonly
		if `num' == 1{
			mat `vv' = r(mean)
		}
		else{
			mat `vv' = (`vv' \ r(mean))
		}
	}

	}
*

foreach vv in m4 m3 m2 m1 p0 p1 p2 p3 p4{ 
	clear
	svmat `vv'
	replace `vv'= `vv' / `epop'	
	rename `vv'1 `vv'
	tempfile `vv'
	save ``vv''
}
*

use `m4', clear
foreach vv in m4 m3 m2 m1 p0 p1 p2 p3 p4{ 
merge 1:1 _n using ``vv'', nogenerate assert(3) 
}
*
save "${data}FP_bin_estimates_for_confidence_intervals_clean_controls.dta", replace


use "${data}FP_bin_estimates_for_confidence_intervals_clean_controls.dta", clear
*Get confidence intervals and standard errors of estimates
gen below_estimate = m1 + m2 + m3 + m4
gen above_estimate = p0 + p1 + p2 + p3 + p4
gen perc_aff_emp_estimate = (p0 + p1 + p2 + p3 + p4 + m1 + m2 + m3 + m4)/${B}
gen mw_elast = (below_estimate + above_estimate)*${epop}*${C}

gen wage_estimate = p0*${epop}*${wagemult}
foreach num of numlist 1(1)4{
replace wage_estimate = wage_estimate + p`num'*${epop}*(${wagemult}+ `num')
}

foreach num of numlist 1(1)4{
replace wage_estimate= wage_estimate+ m`num'*${epop}*(${wagemult} - `num')
}
*
replace wage_estimate = wage_estimate/${EWB}
replace wage_estimate= (wage_estimate-perc_aff_emp_estimate)/(1+perc_aff_emp_estimate)


foreach vv of varlist below_estimate above_estimate perc_aff_emp_estimate mw_elast wage_estimate{
_pctile `vv' , nq(200)
gen `vv'_5 = r(r5)
gen `vv'_195 = r(r195)
local `vv'_se = (r(r195) - r(r5))/(2*2)
}

*Impose null
replace perc_aff_emp_estimate = perc_aff_emp_estimate  + ${new_emp_null}
replace wage_estimate = wage_estimate  + ${new_wage_null}


*For delta method
corr perc_aff_emp_estimate wage_estimate, cov
mat delta_method_mid_mat = r(C)

mat A = (1/${new_wage_null} , - (${new_emp_null}/(${new_wage_null})^2) )
mat delta_method_var = A*delta_method_mid_mat*A'
scalar delta_method_var = delta_method_var[1,1]
scalar delta_method_se = delta_method_var^0.5  
local emp_elas_wrt_wage_se : di %04.3f `=delta_method_se'
reg m4


local new_below_null: di %04.3f ${new_below_null}
estadd local below_Eb "`new_below_null'"
local new_above_null: di %04.3f ${new_above_null}
estadd local above_Eb "`new_above_null'"
local new_emp_null: di %04.3f ${new_emp_null}
estadd local bunching_Eb "`new_emp_null'"
local new_wage_null: di %04.3f ${new_wage_null}
estadd local WB_Eb "`new_wage_null'"
local new_mwelast_null: di %04.3f ${new_mwelast_null}
estadd local bunchelas_Eb "`new_mwelast_null'"
local new_emp_elas_wage_null: di %04.3f ${new_emp_elas_wage_null}
estadd local labordem_Eb "`new_emp_elas_wage_null'"
*Standard errors
foreach vv of varlist below_estimate above_estimate perc_aff_emp_estimate mw_elast wage_estimate{
local `vv'_se: di %04.3f ``vv'_se'
}
*
estadd local below_Ese "(`below_estimate_se')"
estadd local above_Ese "(`above_estimate_se')"
estadd local bunching_Ese "(`perc_aff_emp_estimate_se')"
estadd local WB_Ese "(`wage_estimate_se')"
estadd local bunchelas_Ese "(`mw_elast_se')"
estadd local labordem_Ese "(`emp_elas_wrt_wage_se')"

eststo overall_m2AS

est save "${estimates}stackedevents_wagebins_clean_controls_overall_manually_averaged_model_pre12_estadd", replace


