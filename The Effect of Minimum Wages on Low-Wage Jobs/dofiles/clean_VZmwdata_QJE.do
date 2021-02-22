
*This do file is created by DC on 01/15/2016.
********************************************************************************
********************          MW data          *********************************
********************************************************************************



use "${data}VZ_mw_state_quarterly_new",clear
rename quarterly_date quarterdate
rename statefips statenum
cap drop logmw
gen mw = max_mw
gen logmw= log(max_mw)
gen year = year(dofq(quarterdate))
gen quarter = quarter(dofq(quarterdate))



xtset statenum quarterdate
forvalues index = 1/12 {
	foreach lagorlead in L F {
		gen `lagorlead'`index'logmw = log(`lagorlead'`index'.mw)
	}
}
gen L0logmw = logmw
*
keep if year>=1979 & year<=2016
xtset statenum quarterdate
assert "`r(balanced)'" == "strongly balanced"
keep quarterdate statenum logmw L*mw F*mw


preserve
use "${data}macrocps_basic_teens.dta",clear
egen tagger=tag(statenum)
keep if tagger
keep statenum division
tempfile divisions
save `divisions'
restore

merge m:1 statenum using `divisions',assert(3) nogenerate
compress







save "${data}VZmw_quarterly_lagsleads_1979_2016.dta", replace




