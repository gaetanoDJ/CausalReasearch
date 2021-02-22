

capture program drop addobs
program define addobs
local obsstring : di %15.0gc e(N)
estadd local obs "\multicolumn{1}{c}{`obsstring'}"
end




* quarterly unemployment rates
use "${data}state_level_quarterly_unemp_rate.dta", clear
xtset statenum quarterdate
tempfile unemp_rate
save `unemp_rate'

* presidential elections
use "${data}presidential_elections_from 1976_2016.dta", clear
keep  if year>1990
collapse (sum) dem_vote rep_vote, by(statenum statefull)
gen political_orientation = "Rep" if rep_vote>dem_vote
replace political_orientation = "Dem" if rep_vote<dem_vote
tempfile votes
save `votes'

use "${data}Urbanshare_ver13.dta", clear
rename pop2010 urbanshare
replace urbanshare = urbanshare/100
tempfile urban
save `urban'

* median wages
use "${data}median_wage_state_quarter_QJE.dta", clear
xtset statenum quarterdate
merge 1:1 statenum quarterdate using "${data}VZmw_quarterly_lagsleads_1979_2016.dta", keepusing(logmw) assert(3) nogenerate
gen MW = exp(logmw)
gen MWtomed = MW/med_wage

merge m:1 statenum using  `votes', nogenerate
merge 1:1 statenum quarterdate using `unemp_rate', nogenerate
merge 1:1 statenum quarterdate using "${data}eventclassification", keep(3) nogenerate
merge m:1 statenum using `urban', keep(3) nogenerate

keep if overallcountgr>0 & overallcountgr!=. & fedincrease!=1
tempfile necessary_constants
save `necessary_constants'

use "${data}stacked_events_new_event_specific_estimates_clean_controls.dta", clear
*use "${data}stacked_events_new_event_specific_estimates_alt_control.dta", clear
rename ests1 event_estimate_post
rename ests2 event_estimate_pre
rename ests3 eventno
rename ests4 placebono
rename ests5 _category
rename ests6 statenum
rename ests7 quarterdate
rename ests8 placebo_statenum
rename ests9 E_all
rename ests10 avetotalpopulation

gen category = "_above" 	if _category==1
replace category = "_below" 	if _category==2
replace category = "_upper" 	if _category==3
replace category = "_all" 	if _category==4
replace category = "_bunching" 	if _category==5

keep if category == "_above" | category=="_below"
cap drop _category
cap drop event_estimate_pre
ds category statenum quarterdate, not
reshape wide `r(varlist)', i(statenum quarterdate) j(category) string
replace event_estimate_post_above =  event_estimate_post_above /E_all_above
replace event_estimate_post_below =  event_estimate_post_below /E_all_above
rename event_estimate_post_above above_estimate
rename event_estimate_post_below below_estimate
cap drop emp_estimate
gen emp_estimate = below_estimate + above_estimate

drop eventno_above

rename eventno event
merge 1:1 statenum quarterdate using `necessary_constants', keep(3) nogenerate


egen decade = cut(year), at(1970(10)2020)
sum decade
assert r(min) == 1980
assert r(max) == 2010
foreach decade of numlist 1990(10)2010 {
  gen decade_`decade' = year >= `decade' & year <= `decade' + 9
  lab var decade_`decade' "Decade = `decade'"
}
*


*
gen republican_states = (political_orientation=="Rep")
tempfile regready
save `regready'

*Stacked
local controls "unemp_rate urbanshare decade_* republican "
			local counter = `counter' + 1
			use `regready', clear
		
			reg emp_estimate MWtomed `controls' [aw=avetotalpopulation_above] if statenum!=11,r
			local b_est : di %04.3f _b[MWtomed]
			local se_est : di %04.3f _se[MWtomed]
			
			binscatter emp_estimate MWtomed if statenum!=11 ///
				[aw=avetotalpopulation_above], controls(`controls') mcolor(green*1.25) lcolor(green*1.25) ///
				nq(10)  

				`e(graphcmd)' ///
				xtitle("Minimum-to-median wage ratio", height(6)) ytitle("Change in employment relative to pre-treatment total employment", size(small) height(6))  ///
				ylabel(-0.04(0.01)0.04, format(%03.2f)) legend(off) xlabel(0.4(0.05)0.55, format(%03.2f)) ///
				text(0.03 0.43  "Emp. change, slope =  `b_est' (`se_est')" , color(green*1.25)  fcolor(white) margin(0.2 0.2 1 1) justification(right) )
				gr export "${figures}Figure5_a.pdf", replace

			local counter = `counter' + 1
			use `regready', clear

			reg below_estimate MWtomed `controls' [aw=avetotalpopulation_above] if statenum!=11,r
			
			local b_estb : di %04.3f _b[MWtomed]
			local se_estb : di %04.3f _se[MWtomed]
			
			binscatter below_estimate MWtomed if statenum!=11 ///
				[aw=avetotalpopulation_above], controls(`controls') mcolor(red*1.25) lcolor(red*1.25) ///
				nq(10)  
			local temp "`e(graphcmd)'"
				local pos = strpos("`temp'", ", g")  - 1 - 6
				local graphcommandb= substr("`temp'", 7, `pos')
			di "`graphcommandb'"
				
			reg above_estimate  `controls' MWtomed  [aw=avetotalpopulation_above] if statenum!=11,r
			
			local b_esta : di %04.3f _b[MWtomed]
			local se_esta : di %04.3f _se[MWtomed]
			
			binscatter above_estimate MWtomed if statenum!=11 ///
				[aw=avetotalpopulation_above], controls(`controls') mcolor(ltblue*1.25) lcolor(ltblue*1.25) ///
				nq(10)  
			local temp "`e(graphcmd)'"
			local pos = strpos("`temp'", ", g")  - 1 - 6
			local graphcommanda= substr("`temp'", 7, `pos')
			di "`graphcommanda'"

				twoway `graphcommanda' `graphcommandb' ///
				, xtitle("Minimum-to-median wage ratio", height(6)) ytitle("Change in employment relative to pre-treatment total employment", size(small) height(6))  ///
				ylabel(-0.04(0.01)0.04, format(%03.2f)) legend(off) xlabel(0.4(0.05)0.55, format(%03.2f)) graphregion(color(white))  ///
				text(0.04 0.43  "Excess jobs, slope =  `b_esta' (`se_esta')" , color(ltblue*1.25)  fcolor(white) margin(0.2 0.2 1 1) justification(right) ) ///
				text(-0.04 0.43  "Missing jobs, slope =  `b_estb' (`se_estb')" , color(red*1.25)  fcolor(white) margin(0.2 0.2 1 1) justification(right) )
				gr export "${figures}Figure5_b.pdf", replace

	
