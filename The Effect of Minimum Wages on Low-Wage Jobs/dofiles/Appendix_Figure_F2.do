
use "${data}VZ_mw_state_quarterly_new.dta", clear
rename quarterly_date quarterdate
rename statefips statenum
keep mean_mw max_mw min_mw statenum quarterdate
tempfile mwdata
save `mwdata'

local realornominal "real"


use "${data}Admin_CPS_workingdata_allind_5cents_new.dta", clear
drop MW
rename MW_real MW
local graphtitle "In 2016 $"
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

local vv MNWAOR


if "`vv'"=="WA" {
	drop if statenum == 27 | statenum==41
}


if "`vv'"=="MN" {
	drop if statenum == 53 | statenum==41
}
if "`vv'"=="OR"{
	drop if statenum == 27  | statenum==53
} 
*

local increment = 100
gen MW2 = floor(MW*100)
replace MW2 = int(MW2)
drop MW
rename MW2 MW
*We need to adjust wage bins, because, for instance if MW is $7.34, $7.30 bin is actually the MW bin.
replace MW = floor(MW/5)*5
gen wagebins_relMW = wagebins - MW 

tempfile origdata
save `origdata'


********************************************************************************
****************         Relative to MW                       ******************
********************************************************************************



foreach year of numlist 2000 2005 2010{
use `origdata', clear
local year2 = `year' + 4

g wagebins_relMW100 = wagebins_relMW - mod(wagebins_relMW, 100)
replace wagebins_relMW100 = int(wagebins_relMW100/100)


drop if MNcountall==. & statenum==27
drop if acountall==. & statenum==53
drop if ORcountall==. & statenum==41
assert DMW!=.
gen quarterofevent = DMW>0
*The following line affects the balanced structure of the data.
drop if quarterofevent == 1

collapse (firstnm) population acountall MNcountall ORcountall (sum) ORcount acount MNcount count, by(statenum wagebins_relMW100 quarterdate year)
bys statenum quarterdate: egen countall = total(count)


keep if (year>=`year' & year<=`year2') 
sum year if statenum == 27, meanonly
scalar MNbeg = r(min) 
scalar MNend = r(max) 
sum year if statenum == 53, meanonly
scalar WAbeg = r(min) 
scalar WAend = r(max) 
*Since everything is QCEW re-weighted.
assert countall <= MNcountall + 1 & countall>=MNcountall - 1 if statenum == 27
assert countall <= acountall + 1 & countall>=acountall - 1 if statenum == 53
assert countall<= ORcountall + 1 & countall>=ORcountall - 1 if statenum == 41
assert population>= countall

egen statequarter = group(statenum quarterdate)
xtset statequarter wagebins_relMW100
tsfill, full
foreach vv of varlist statenum quarterdate year population{
bys statequarter: egen _`vv' = max(`vv')
replace `vv' = _`vv' if `vv' == .
}



replace count = 0 if count==.
replace acount = 0 if acount==.
replace MNcount = 0 if MNcount==.
replace ORcount = 0 if ORcount==.
foreach vv of varlist countall acountall MNcountall ORcountall {
bys statenum quarterdate: egen _`vv' = max(`vv')
replace `vv' = _`vv' if `vv' == .
}


collapse (mean) population ORcountall countall acountall MNcountall acount MNcount count ORcount, by(statenum wagebins_relMW100)
bys statenum: egen ORcountall2 = total(ORcount)
gen acountsh = acount/pop if statenum == 53
gen MNcountsh = MNcount/pop if statenum == 27
gen ORcountsh = ORcount/pop if statenum == 41
gen countsh  = count / pop 



tempfile temp
save `temp'

*All three combined


use `temp', clear

cap drop wagebins3_relMW100
gen wagebins3_relMW100 = floor(wagebins_relMW100/3)*3
replace wagebins3_relMW = 21 if wagebins3_relMW>21
replace wagebins3_relMW = -9 if wagebins3_relMW<=-9
bys statenum wagebins3_relMW100 (wagebins_relMW100): egen acountsh_3 = total(acountsh) if statenum == 53
bys statenum wagebins3_relMW100 (wagebins_relMW100): egen MNcountsh_3 = total(MNcountsh) if statenum == 27
bys statenum wagebins3_relMW100 (wagebins_relMW100): egen ORcountsh_3 = total(ORcountsh) if statenum == 41
bys statenum wagebins3_relMW100 (wagebins_relMW100): egen countsh_3 = total(countsh) 
cap drop tagger
egen tagger = tag(statenum  wagebins3_relMW100)
keep if tagger

twoway ///
(scatter countsh_3 wagebins3_relMW100 if tagger==1 & statenum==53 & wagebins3_relMW100<=21,  msize(large) msymbol(O) mcolor(blue) ) ///				
(scatter acountsh_3 wagebins3_relMW100  if tagger==1  & statenum==53 & wagebins3_relMW100<=21,  msize(large) msymbol(Sh) mcolor(red) ) ///
, graphregion(color(white))  xtitle("Wage bins relative to the MW", height(7)) ylabel( ,format(%03.2f)) ///
xlabel(-9 "<-6" -6 "[-6, -3)" -3 "[-3, 0)" 0 "[0, 3)" 3 "[3, 6)" 6 "[6, 9)" 9 "[9, 12)" 12 "[12, 15)" 15 "[15, 18)" 18 "[18, 21)" 21 "21+") ///
 legend(order(1 "CPS" 2 "Admin")) ytitle("Employment counts per capita") ///
ylabel(0 "0" 0.04(0.04)0.20, format(%03.2f))  title("WA, `year'-`year2'")

gr export "${figures}FigureF2_WA_`year'_`year2'.pdf", replace


twoway ///
(scatter countsh_3 wagebins3_relMW100 if tagger==1 & statenum==27 & wagebins3_relMW100<=21,  msize(large) msymbol(O) mcolor(blue) ) ///				
(scatter MNcountsh_3 wagebins3_relMW100  if tagger==1  & statenum==27 & wagebins3_relMW100<=21,  msize(large) msymbol(Sh) mcolor(red) ) ///
, graphregion(color(white))  xtitle("Wage bins relative to the MW", height(7)) ylabel( ,format(%03.2f)) ///
xlabel(-9 "<-6" -6 "[-6, -3)" -3 "[-3, 0)" 0 "[0, 3)" 3 "[3, 6)" 6 "[6, 9)" 9 "[9, 12)" 12 "[12, 15)" 15 "[15, 18)" 18 "[18, 21)" 21 "21+") ///
 legend(order(1 "CPS" 2 "Admin"))  ytitle("Employment counts per capita") ///
ylabel(0 "0"  0.04(0.04)0.20, format(%03.2f))  title("MN, `year'-`year2'")

gr export "${figures}FigureF2_MN_`year'_`year2'.pdf", replace

twoway ///
(scatter countsh_3 wagebins3_relMW100 if tagger==1 & statenum==41 & wagebins3_relMW100<=21,  msize(large) msymbol(O) mcolor(blue) ) ///				
(scatter ORcountsh_3 wagebins3_relMW100  if tagger==1  & statenum==41 & wagebins3_relMW100<=21,  msize(large) msymbol(Sh) mcolor(red) ) ///
, graphregion(color(white))  xtitle("Wage bins relative to the MW", height(7)) ylabel( ,format(%03.2f)) ///
xlabel(-9 "<-6" -6 "[-6, -3)" -3 "[-3, 0)" 0 "[0, 3)" 3 "[3, 6)" 6 "[6, 9)" 9 "[9, 12)" 12 "[12, 15)" 15 "[15, 18)" 18 "[18, 21)" 21 "21+") ///
 legend(order(1 "CPS" 2 "Admin"))  ytitle("Employment counts per capita") ///
ylabel(0 "0"  0.04(0.04)0.20, format(%03.2f))  title("OR, `year'-`year2'")

gr export "${figures}FigureF2_OR_`year'_`year2'.pdf", replace


*OR, MN, WA

gen admindata_sh_3 = ORcountsh_3 if statenum==41
replace admindata_sh_3 = acountsh_3 if statenum==53
replace admindata_sh_3 = MNcountsh_3 if statenum==27

gen admindataall = ORcountall + MNcountall + acountall

preserve
collapse admindata_sh_3 [aw=admindataall], by(wagebins3_relMW100)
tempfile a1
save `a1'
restore

collapse countsh_3 [aw=countall], by(wagebins3_relMW100)
merge 1:1 wagebins3_relMW100 using `a1', assert(3) nogenerate

twoway ///
(scatter countsh_3 wagebins3_relMW100 if wagebins3_relMW100<=21,  msize(large) msymbol(O) mcolor(blue) ) ///				
(scatter admindata_sh_3 wagebins3_relMW100  if wagebins3_relMW100<=21,  msize(large) msymbol(Sh) mcolor(red) ) ///
, graphregion(color(white))  xtitle("Wage bins relative to the MW", height(7)) ylabel( ,format(%03.2f)) ///
xlabel(-9 "<-6" -6 "[-6, -3)" -3 "[-3, 0)" 0 "[0, 3)" 3 "[3, 6)" 6 "[6, 9)" 9 "[9, 12)" 12 "[12, 15)" 15 "[15, 18)" 18 "[18, 21)" 21 "21+") ///
 legend(order(1 "CPS" 2 "Admin"))  ytitle("Employment counts per capita") ///
ylabel(0 "0"  0.04(0.04)0.20, format(%03.2f))  title("OR, MN, WA; `year'-`year2'")


gr export "${figures}FigureF2_ORMNWA_`year'_`year2'.pdf", replace


}
*




