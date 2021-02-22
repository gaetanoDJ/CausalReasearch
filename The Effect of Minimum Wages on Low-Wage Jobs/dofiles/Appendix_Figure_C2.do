

use "${data}VZmw_quarterly_lagsleads_1979_2016.dta", clear 
gen MW = exp(logmw)
bys quarterdate: egen fedMW = min(MW) 
gen year = year(dofq(quarterdate))
tab statenum if MW>fedMW & year==1995
levelsof statenum if MW>fedMW+0.001 & year==1995, local(higherthanfed)
 
set more off
use "${data}Admin_CPS_workingdata_in2016dollars_allind_new.dta", clear


global treatdata  "Admin"


local hiw = 17
local loww = 5

tempfile origdata
save `origdata'

foreach vv in statenum quarterdate wagebins{
preserve
keep `vv'
duplicates drop
tempfile `vv'
save ``vv''
restore
}
*

global firstyear 1992
global pre "( year>=1996 & year<=1998)"

local firstyear = ${firstyear}


use `statenum', clear
cross using `quarterdate'
cross using `wagebins'
tempfile balance
save `balance'
use `origdata',clear
cap drop _merge
merge 1:1 statenum quarterdate wagebins using `balance', assert(2 3) nogenerate
replace year = year(dofq(quarterdate)) if year==.
replace count = 0 if count==.
replace acount = 0 if acount==.
replace MNcount = 0 if MNcount==.
bys statenum quarterdate: egen _pop = max(pop)
replace pop = _pop if pop==.

g treatstate = statenum==53
g _control = 1  if year>=1996 & year<=2004
replace _control = 0 if DMW>0 & DMW<. & (year>=1998 & year<=2004)
egen control = min(_control), by(statenum)
keep if year>=$firstyear & year<=2004
keep if treatstate==1 | control==1
keep if wagebins!=.

g wagebin100 = wagebins - mod(wagebins, 100)
replace wagebin100 = int(wagebin100/100)
replace count = 0 if count==.
assert acount!=. if statenum==53
assert MNcount!=. if statenum==27
cap drop countall
cap drop acountall
cap drop MNcountall

bys statenum quarterdate: egen countall = total(count) 
bys statenum quarterdate: egen acountall = total(acount)  
bys statenum quarterdate: egen MNcountall = total(MNcount) 


*Get annual average
collapse (mean)  count acount pop MNcount countall acountall MNcountall, by(year wagebins statenum control treatstate  )
g wagebinmissing = wagebins<900 & wagebins>=500
g wagebinexcess = wagebins>=900 & wagebins<1400


*Collapse by wagebin100
collapse (sum) MNcount count acount (mean) pop  countall acountall MNcountall, by(year wagebinmissing wagebinexcess control treatstate statenum  )
*Collapse by treat/control state
collapse (mean) MNcount count acount (mean) pop  countall acountall MNcountall, by(year wagebinmissing wagebinexcess control treatstate   )


keep if wagebinmissing==1 | wagebinexcess==1


gen acountsh = acount/pop
gen countsh = count/pop


keep if year!=1999
*To turn control line the counterfactual line.
foreach vv in wagebinexcess wagebinmissing {
sum acountsh if `vv'==1 & treat==1 & year>=1996 & year<=1998
scalar treat_pre = r(mean)
sum countsh if `vv'==1 & treat!=1 & year>=1996 & year<=1998
scalar cont_pre = r(mean)
replace countsh = countsh + (treat_pre - cont_pre) if `vv'==1 & treat!=1
}

twoway ///
(connected acountsh year if wagebinexcess==1 & treat==1, lcolor(ltblue) mcolor(ltblue) lpattern(dash) lwidth(thick)) ///				
(connected acountsh year if wagebinmissing == 1 & treat==1, lcolor(red)  lpattern(dash) xline(1998, lcolor(gs6) lpattern(dash)) xline(1999, lcolor(gs6) lpattern(dash)) mcolor(red) lwidth(thick) ) ///				
(connected countsh year if wagebinexcess==1 & treat!=1, lcolor(ltblue) mcolor(ltblue)  lwidth(thick)) ///				
(connected countsh year if wagebinmissing == 1 & treat!=1, lcolor(red) xline(1998, lcolor(gs6) lpattern(dash)) xline(1999,  lcolor(gs6)  lpattern(dash)) mcolor(red) lwidth(thick) ) ///				
, legend(order(2 "WA, [$5, $8]" 1 "WA, [$9, $13]" 4 "Counterfactual, [$5, $8]" 3 "Counterfactual, [$9, $13]" ) size(3.3))  ///
graphregion(color(white))  xtitle("Year") ylabel(0 "0" 0.03(0.03)0.21 ,format(%03.2f)) xlabel(1992(2)2004) ytitle("Employment counts per capita", height(10)) ///
  text(0.2 1998 "8.4% MW inc.", size(small) color(black) place(l) margin(r=1)) ///
  text(0.2 1999 "10.3% MW inc.", size(small) color(black) place(r) margin(l=1)) 
gr export "${figures}FigureC2.pdf", replace


