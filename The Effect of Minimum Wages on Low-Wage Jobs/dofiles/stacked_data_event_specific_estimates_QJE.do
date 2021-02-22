
clear 
set seed 9856840
set matsize 11000

use "${data}stackedevents_regready_pre12.dta", clear
merge m:1 treatedstatenum treatdate using "${data}to_combine_consecutive_events.dta", assert(1 3) 

keep if cleansample == 1

* remove data where we can't determine when there are wage imputations
set rmsg on
set more off
* define cumulative effects
* everything normalized to F4
local maxlag = 16
foreach i of numlist 0(4)`maxlag' {
  local j = `i' - 4
  if `i' == 0 local lincomaggL`i' (L`i'treat - F4treat)
  else local lincomaggL`i' (`lincomaggL`j'' + L`i'treat - F4treat)
}
mac li _all

* define treatment vector
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
rename aveoverallpop avetotalpopulation
gen bunching_epop = above_epop + below_epop
gen totalbelow_epop = all_epop - (above_epop + upper_epop)
gen byte yearbeforeevent = F1.origevent == 1 | F2.origevent == 1 | F3.origevent == 1 | F4.origevent == 1

gen control_origeventpost2 = control_origeventpost  
replace control_origeventpost2 = 0 if treatedstate==statenum 

	levelsof event if _merge==3, local(temploc)
	gen control_othereventpost2 = control_othereventpost
foreach eventno of local temploc{
	sum control_othereventpost if event==`eventno' & statenum==treatedstatenum & quarterdate<treatdate
	local temp = r(mean)
	if `temp'<0.00001{
	replace control_othereventpost2 = 0 if event==`eventno' & statenum==treatedstatenum 
	}
}
*
* save the regression-ready sample
levelsof event, local(allevents)
levelsof event if _merge==3, local(allevents2)

cap drop eventorigcontrol 
egen eventorigcontrol = group(event control_origeventpost)
cap drop eventorigcontrol3 
gen eventorigcontrol3 = eventorigcontrol 
replace eventorigcontrol3 =  eventorigcontrol3+1000 if treatedstatenum==statenum & control_origeventpost>0


bys statenum event: egen states_to_drop = max(control_origeventpost)
drop if states_to_drop!=0 & treatedstatenum!=statenum
assert control_origeventpost==0 if treatedstatenum!=statenum
sum control_origeventpost,meanonly
assert r(mean)!=0
xtset eventstate quarterdate
tempfile regready2
save `regready2'

foreach eventno of numlist `allevents'{
		use if event==`eventno' using `regready2', clear
		sum all_epop if cleansample==1  [aw=avetotalpopulation]
		local epop = r(mean)
		sum avetotalpopulation if treatedstate==statenum, meanonly
		local population = r(mean)
			
		local cat_counter = 0
			foreach cat in above below upper all bunching {
			local cat_counter = `cat_counter' + 1
			reghdfe `cat'_epop $treat [aw=avetotalpopulation] if  cleansample==1 , a(i.event#i.statenum i.event#i.time i.event#i.control_origeventpost i.event#control_fedeventpost i.event#control_othereventpost) cluster(eventstate) 
			lincom (`lincomaggL16') * (1/5) 
			global mean_effect_`cat' = r(estimate)

			lincom F12treat - F4treat
			global pre_effect_`cat' = r(estimate)
			
			sum treatedstate, meanonly
			local treatedstate = r(mean)
			sum treatdate, meanonly
			local treatdate= r(mean)
			
			
				if `cat_counter' == 1 & `eventno'==1 {
				mat ests = (${mean_effect_`cat'}, ${pre_effect_`cat'} , `eventno', 0,  `cat_counter', `treatedstate', `treatdate', `treatedstate', `epop', `population' )
				}
				else{
				mat ests = (ests \ ${mean_effect_`cat'}, ${pre_effect_`cat'} , `eventno', 0,  `cat_counter', `treatedstate', `treatdate', `treatedstate', `epop', `population' )	
				}
			

			}

}
*
			clear
			qui svmat ests
			qui compress
			save "${data}stacked_events_new_event_specific_estimates_clean_controls.dta", replace
			
			
			
			