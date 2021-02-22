
*This do file is created by DC on 04/19/2017.
********************************************************************************
***************           QCEW-multiplier                          *************
********************************************************************************


use "${data}qcew_state_overall_counts_2016.dta", clear
rename statefips statenum
gen quarterdate = yq(year, quarter)
tempfile qcew
save `qcew'


use "${data}state_panels_with3quant1979.dta", clear
keep countall statenum quarterdate
duplicates drop
egen tagger = tag(statenum quarterdate)
keep if tagger
drop tagger


merge 1:1 statenum quarterdate using `qcew'
assert _merge==3 if quarterdate!=227
keep if _merge==3
cap drop _merge


gen qcew_multiplier = emp/countall
gen multiplier = qcew_multiplier
replace multiplier = 1 if multiplier==0

save "${data}qcew_multiplier.dta", replace
