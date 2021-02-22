set more off
set trace off
clear all

* QCEW
use "${data}qcew_multiplier.dta", clear
keep statenum quarterdate multiplier
tempfile qcew
save `qcew'

* wage-bin emp counts
use "${data}state_panels_cents_balanced_add_QJE.dta", clear
* normalize overall counts to overall QCEW
merge m:1 statenum quarterdate using `qcew', nogenerate assert(3)
rename count overallcount
rename countall overallcountall
* for some reason I don't understand, overallcountall is not constant across stateXquarter
* the differences are small, but this is weird
* here is a hack in the meantime
egen meanoverallcountall = mean(overallcountall), by(statenum quarterdate)
replace overallcountall = meanoverallcountall
rename population overallpop
rename wagebins wagebin
foreach group in overall {
	replace `group'count = `group'count * multiplier
	egen ave`group'pop = mean(`group'pop), by(statenum)
}
keep statenum quarterdate wagebin overallcount overallpop aveoverallpop
compress
tempfile empcounts
save `empcounts'

* CPI
use "${data}cpiursai1977-2016.dta", clear
*Dropping an empty column and column that calculates yearly average (simple)
drop v14 avg
reshape long month, i(year) j(m)
rename month cpi
rename m month
keep if year >= 1979
local baseyear=2016
sum cpi if year == `baseyear', meanonly
local cpibase = r(mean)
replace cpi = 100 * cpi / `cpibase'
gen monthdate = ym(year,month)
gen quarterdate = qofd(dofm(monthdate))
collapse cpi, by(quarterdate)
tempfile cpiquarter
save `cpiquarter'

* minimum wages
use "${data}VZmw_quarterly_lagsleads_1979_2016.dta", clear
merge m:1 quarterdate using `cpiquarter', nogenerate assert(3)
tsset statenum quarterdate
gen MW_real = exp(logmw) / (cpi/100)
gen DMW_real = D.MW_real
gen Dlogmw = D.logmw
gen allchanges = D.logmw > 0 & D.logmw ~= .
gen int MW_real_wagebin = floor(MW_real*100/25)*25
keep statenum quarterdate logmw Dlogmw MW_real DMW_real MW_real_wagebin allchanges
tempfile mw
save `mw'

* events
use "${data}eventclassification.dta", clear
merge 1:1 statenum quarterdate using `mw', keepusing(allchanges logmw Dlogmw MW_real DMW_real MW_real_wagebin) assert(2 3)
assert allchanges == 1 if _merge == 3
keep if allchanges == 1
drop _merge
gen origevent = 1 if fedincrease != 1 & overallcountgr > 0 & overallcountgr ~= .
gen fedevent = 1 if fedincrease == 1
gen otherevent = 1 if origevent ~= 1 & fedevent ~= 1
gen event = _n 
keep statenum quarterdate event origevent fedevent otherevent MW_real DMW_real MW_real_wagebin
compress
tempfile allevents
save `allevents'

* count emp relative to min wage for every origevent
use `allevents', clear
keep if origevent == 1
egen neweventnum = group(event)
keep event neweventnum MW_real_wagebin
sum neweventnum
local maxevents = r(max)
tempfile origevents
save `origevents'
forvalues i = 1/`maxevents' {
	di "Working on wage bins for event `i' of `maxevents'"
	use if neweventnum == `i' using `origevents', clear
	assert _N == 1
	local mwbin: di MW_real_wagebin
	local event: di event
	local belowbin = -400 + `mwbin'
	local abovebin =  500 + `mwbin'
	use `empcounts', clear
	gen overallmissing = (wagebin >= `belowbin' & wagebin < `mwbin') * overallcount
	gen overallexcess = (wagebin >= `mwbin' & wagebin < `abovebin') * overallcount
	gen overallupper = (wagebin >= `abovebin') * overallcount
	gen overalltotal = overallcount
	foreach num of numlist 100(100)400{
	local num2 = `num'/100
		gen overallm`num2' = (wagebin >= `mwbin' - `num' & wagebin < `mwbin' - `num' +100) * overallcount
		gen overallp`num2' = (wagebin >= `mwbin'+`num' & wagebin < `mwbin' + `num' + 100) * overallcount
	}
	
		gen overallp0 = (wagebin >= `mwbin' & wagebin < `mwbin' + 100) * overallcount

	gcollapse (sum) overallp0 overallp1 overallp2 overallp3 overallp4 overallm1 overallm2 overallm3 overallm4 overallmissing overallexcess overallupper overalltotal (first) overallpop aveoverallpop, by(statenum quarterdate)
	gen int event = `event'
	qui compress
	tempfile event`i'
	save `event`i''
}
forvalues i = 1/`maxevents' {
	if `i' == 1 use `event`i'', clear
	else append using `event`i''
}
foreach group in overall {
	foreach stat in missing excess upper total m4 m3 m2 m1 p0 p1 p2 p3 p4 {
		gen `group'`stat'epop = `group'`stat' / overallpop
	}
}
compress
tempfile outcomes
save `outcomes'

* create panel template
use `outcomes', clear
egen tag = tag(statenum quarterdate)
keep if tag == 1
keep statenum quarterdate
tempfile panelstructure
save `panelstructure'

* create regression-ready stacked panels with different pre-treatment periods
foreach preperiod of numlist /*80 40 12*/ /*20*/ 12 {
	* create stacked panel of treatments
	use statenum quarterdate event using `allevents', clear
	rename quarterdate treatdate
	gen time = -`preperiod'
	tempfile beginning
	save `beginning'
	use statenum quarterdate event using `allevents', clear
	rename quarterdate treatdate
	gen time = 19
	append using `beginning'
	tsset event time
	tsfill, full
	* create event indicators and treatment indicators
	foreach var of varlist statenum treatdate {
	  egen _`var' = max(`var'), by(event)
	  replace `var' = _`var'
	  drop _`var'
	}
	merge m:1 event using `allevents', assert(3) nogenerate keepusing(*event)
	gen quarterdate = treatdate + time
	format %tq quarterdate treatdate
	foreach var of varlist origevent fedevent otherevent {
	  replace `var' = 0 if `var' == .
	  replace `var' = 0 if time != 0
	}
	foreach var of varlist origevent fedevent otherevent {
	  gen `var'post = 0
	  egen max`var' = max(`var'), by(event)
	  replace `var'post = max`var' if time >= 0
	  gen `var'_treated = max`var' == 1
	  drop max`var'
	}
	rename statenum treatedstatenum
	tempfile treatments
	save `treatments'

	* create control variables
	* given an event, we need to calculate treatment status across all
	* other events at that time
	use `treatments', clear
	sum event
	local numevents = r(max)
	qui forvalues event = 1/`numevents' {
	  noi di "Calculating controls for event `event' of `numevents'"
	  use `treatments', clear
	  keep if event ~= `event'
	  gcollapse (max) origeventpost fedeventpost othereventpost, by(treatedstatenum quarterdate)
	  foreach var of varlist origeventpost fedeventpost othereventpost {
	    rename `var' control_`var'
	  }
	  gen event = `event'
	  rename treatedstatenum statenum
	  keep event statenum quarterdate control_*
	  tempfile event`event'
	  save `event`event''
	}
	forvalues event = 1/`numevents' {
	  if `event' == 1 use `event`event'', clear
	  else append using `event`event''
	}
	tempfile othereventcontrols
	save `othereventcontrols'

	* create stacked panels of treatments + controls
	use `treatments', clear
	* keep only primary events
	egen maxorigevent = max(origevent), by(event)
	keep if maxorigevent == 1
	* add treatments + controls
	joinby quarterdate using `panelstructure'
	* make actual event variables zero for newly merged control states
	foreach var of varlist origevent fedevent otherevent {
	  replace `var' = 0 if statenum ~= treatedstatenum
	  replace `var'post = 0 if statenum ~= treatedstatenum
	  replace `var'_treated = 0 if statenum ~= treatedstatenum
	}
	* merge control variables
	merge 1:1 event statenum quarterdate using `othereventcontrols', keep(1 3) nogenerate
	recode control_origeventpost control_fedeventpost control_othereventpost (. = 0)

	* create controls that are always be on if ever on during an event
	egen eventstate = group(event statenum)
	tsset eventstate time
	foreach var in control_orig control_fed control_other {
		tsspell `var'
		replace `var' = 1 if _spell >= 2
		drop _spell _seq _end
	}
	drop eventstate

	* original event count
	tab event, nofreq
	di "original events = " r(r)

	* add outcomes and min wages
	merge 1:1 event statenum quarterdate using `outcomes', assert(2 3)
	keep if _merge == 3
	drop _merge
	merge m:1 statenum quarterdate using `mw', assert(2 3)
	keep if _merge == 3
	drop _merge

	* recode event numbers
	rename event _event
	egen event = group(_event)
	drop _event
	egen eventstate = group(event statenum)

	* define treatment variables
	tsset eventstate time
	gen _treat = origevent
	gen treat = ( _treat +  L._treat + L2._treat + L3._treat)
	foreach j of numlist 4(4)`preperiod' {
	  gen F`j'treat = F`j'.treat
	  replace F`j'treat = 0 if F`j'treat ==.
	}
	foreach j of numlist 4(4)16 {
	  gen L`j'treat = L`j'.treat
	  replace L`j'treat = 0 if L`j'treat ==.
	}
	replace treat = 0 if treat == .
	rename treat L0treat
	drop _treat

	* clean sample
	gen byte cleansample = 1
	replace cleansample = 0 if quarterdate >= tq(1994q1) & quarterdate <= tq(1995q3)

	compress
  saveold "${data}stackedevents_regready_pre`preperiod'.dta", version(13) replace
}
