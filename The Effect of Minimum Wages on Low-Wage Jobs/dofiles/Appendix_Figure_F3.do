
clear 
set more off
use "${data}Admin_CPS_workingdata_allind_5cents_new.dta", clear
keep if statenum == 27 | statenum == 53 | statenum==41



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
replace ORcount = 0 if ORcount==.

egen wagebinstate = group(wagebins statenum)
xtset wagebinstate quarterdate
assert "`r(balanced)'" == "strongly balanced"

local vv ORMNWA


if "`vv'"=="WA" {
	drop if statenum == 27 | statenum==41
}


if "`vv'"=="MN" {
	drop if statenum == 53 | statenum==41
}
if "`vv'"=="OR" {
	drop if statenum == 53 | statenum==27
}
*

local increment = 100

gen _MW_real = floor(MW_real*100/5)*5
replace MW_real = _MW_real

cap drop _MW_real
bys statenum quarterdate: egen _MW_real = max(MW_real)
replace MW_real = _MW_real if MW_real==.

gen int wagebins_relMW = wagebins - MW_real

bys statenum quarterdate: egen _population = max(population)
replace population = _population if population==.

g wagebins_relMW100 = wagebins_relMW - mod(wagebins_relMW, 100)
replace wagebins_relMW100 = int(wagebins_relMW100/100)


drop if MNcountall==. & statenum==27
drop if acountall==. & statenum==53
drop if ORcountall==. & statenum==41


bys statenum quarterdate: egen _DMW = max(DMW)
replace DMW = _DMW if DMW==.

assert DMW!=.
gen quarterofevent = DMW>0
drop if quarterofevent == 1
collapse (firstnm) population acountall MNcountall ORcountall (sum) ORcount acount MNcount count, by(statenum wagebins_relMW100 quarterdate year)
bys statenum quarterdate: egen countall = total(count)
bys statenum quarterdate: egen ORcountall2 = total(ORcount)
sum ORcountall* if statenum==41 & year==2003 

*drop if (year<2001 | year>2015) & statenum == 27
sum year if statenum == 27, meanonly
scalar MNbeg = r(min) 
scalar MNend = r(max) 
sum year if statenum == 53, meanonly
scalar WAbeg = r(min) 
scalar WAend = r(max) 
sum year if statenum == 41, meanonly
scalar ORbeg = r(min) 
scalar ORend = r(max) 


*Since everything is QCEW re-weighted.
assert countall <= MNcountall + 1 & countall>=MNcountall - 1 if statenum == 27
assert countall <= acountall + 1 & countall>=acountall - 1 if statenum == 53
assert countall <= ORcountall + 1 & countall>=ORcountall - 1 if statenum == 41


assert population>= MNcountall
assert population>= acountall
assert population>= countall
assert population>= ORcountall

replace count = 0 if count==.
replace acount = 0 if acount==.
replace MNcount = 0 if MNcount==.
replace ORcount = 0 if ORcount==.

tempfile uncollapsed
save `uncollapsed'



********************************************************************************
****************          Above/below                 **************************
********************************************************************************



use `uncollapsed', clear
sort statenum quarterdate wagebins_relMW100

keep if wagebins_relMW100>=-4 & wagebins_relMW100<=4

gen acountpc = acount/population
gen MNcountpc = MNcount/population
gen countpc = count/population
gen ORcountpc = ORcount/population



foreach vv in acountpc MNcountpc countpc ORcountpc{
assert `vv' != .
}

gen below_above = (wagebins_relMW100>=0)

collapse (sum) acountpc MNcountpc countpc ORcountpc, by(statenum year below_above)

drop if year==2016
foreach vv in acountpc MNcountpc countpc ORcountpc{
replace `vv' = `vv'/4
}
*


twoway ///
(line acountpc year if statenum == 53 & below_above==0, lwidth(thick) lcolor(red) lpattern(dash) ) ///
(line countpc year if statenum == 53 & below_above==0, lwidth(thick) lcolor(red) ) ///
(line acountpc year if statenum == 53 & below_above==1, lwidth(thick) lcolor(ltblue) lpattern(dash) ) ///
(line countpc year if statenum == 53 & below_above==1, lwidth(thick) lcolor(ltblue) ) ///
, graphregion(color(white)) xtitle("Year") ytitle("Employment counts per capita") legend(order(3 "Admin - at & above" 4 "CPS - at & above" 1 "Admin - below" 2 "CPS - below")) ///
ylabel(0 "0" 0.04(0.04)0.2, format(%03.2f)) title("WA") 

gr export "${figures}FigureF3_WA.pdf", replace

twoway ///
(line MNcountpc year if statenum == 27 & below_above==0, lwidth(thick) lcolor(red) lpattern(dash) ) ///
(line countpc year if statenum == 27 & below_above==0, lwidth(thick) lcolor(red) ) ///
(line MNcountpc year if statenum == 27 & below_above==1, lwidth(thick) lcolor(ltblue) lpattern(dash) ) ///
(line countpc year if statenum == 27 & below_above==1, lwidth(thick) lcolor(ltblue) ) ///
, graphregion(color(white)) xtitle("Year") ytitle("Employment counts per capita") legend(order(3 "Admin - at & above" 4 "CPS - at & above" 1 "Admin - below" 2 "CPS - below")) ///
ylabel(0 "0" 0.04(0.04)0.2, format(%03.2f)) title("MN") xlabel(1998(3)2013 2015)

gr export "${figures}FigureF3_MN.pdf", replace


twoway ///
(line ORcountpc year if statenum == 41 & below_above==0, lwidth(thick) lcolor(red) lpattern(dash) ) ///
(line countpc year if statenum == 41 & below_above==0, lwidth(thick) lcolor(red) ) ///
(line ORcountpc year if statenum == 41 & below_above==1, lwidth(thick) lcolor(ltblue) lpattern(dash) ) ///
(line countpc year if statenum == 41 & below_above==1, lwidth(thick) lcolor(ltblue) ) ///
, graphregion(color(white)) xtitle("Year") ytitle("Employment counts per capita") legend(order(3 "Admin - at & above" 4 "CPS - at & above" 1 "Admin - below" 2 "CPS - below")) ///
ylabel(0 "0" 0.04(0.04)0.2, format(%03.2f)) title("OR") 

gr export "${figures}FigureF3_OR.pdf", replace




keep if year>=1998
gen admincountpc = acountpc + MNcountpc + ORcountpc
collapse (mean) countpc admincountpc, by(year below_above)

twoway ///
(line admincountpc year if  below_above==0, lwidth(thick) lcolor(red) lpattern(dash) ) ///
(line countpc year if below_above==0, lwidth(thick) lcolor(red) ) ///
(line admincountpc year if below_above==1, lwidth(thick) lcolor(ltblue) lpattern(dash) ) ///
(line countpc year if below_above==1, lwidth(thick) lcolor(ltblue) ) ///
, graphregion(color(white)) xtitle("Year") ytitle("Employment counts per capita") legend(order(3 "Admin - at & above" 4 "CPS - at & above" 1 "Admin - below" 2 "CPS - below")) ///
ylabel(0 "0" 0.04(0.04)0.2, format(%03.2f)) title("OR & WA & MN")  xlabel(1998(3)2013 2015)

gr export "${figures}FigureF3_ORMNWA.pdf", replace
