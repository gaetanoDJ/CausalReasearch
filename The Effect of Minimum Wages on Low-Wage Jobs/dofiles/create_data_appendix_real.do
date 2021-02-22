

clear 


use "${data}cpiursai1977-2016.dta", clear
*Dropping an empty column and column that calculates yearly average (simple)
drop v14 avg
reshape long month, i(year) j(m)
rename month cpi
rename m month
keep if year >= 1979
*If base year needs to be changed, use here.
local baseyear=2016
sum cpi if year == `baseyear', meanonly
local cpibase = r(mean)
replace cpi = 100 * cpi / `cpibase'
gen monthdate = ym(year,month)
gen quarterdate = qofd(dofm(monthdate))
collapse cpi, by(quarterdate)
tempfile cpiquarter
save `cpiquarter'


use "${data}qcew_multiplier.dta", clear
keep statenum quarterdate multiplier
assert multiplier != 0
tempfile qcew
save `qcew'



********************************************************************************
*************         Minnesota                                 ****************
********************************************************************************


use "${data}MN admin data post 1998.dta", clear
replace wage_bin = wage_bin*100
gen int wage2 = round(wage_bin)
drop wage_bin
rename wage2 wage_bin


gen quarterdate = yq(year, quarter)
merge m:1 quarterdate using `cpiquarter', assert(2 3) keep(3) nogenerate
gen wagebin = (wage_bin+2.5)/(cpi/100)



*g quarterdate = (year-1979)*4 + quarter + 75
rename wagebin wagebinold
assert wagebinold!=.
replace wagebinold = 3000 if wagebinold>=3000
replace wagebinold = 100 if wagebinold<100



gen int wagebins = floor(wagebinold/25) * 25
collapse  (sum) tot_emp  , by(  wagebins year  quarterdate )
xtset wagebins quarterdate
assert "`r(balanced)'" == "strongly balanced"

g statenum = 27
rename tot_emp admincount  
  
sort statenum quarterdate wagebins

tempfile MNadmin
save `MNadmin'




********************************************************************************
***************            Oregon                           ********************
********************************************************************************


use "${data}ORAdmin_5cent_bins.dta", clear
gen quarterdate = yq(year, quarter)
merge m:1 quarterdate using `cpiquarter', keep(3) nogenerate
replace wagebin = (wagebin+2.5)/(cpi/100)
rename wagebin wagebinold
assert wagebinold!=.
replace wagebinold = 3000 if wagebinold>=3000
replace wagebinold = 100 if wagebinold<100


gen int wagebins = floor((wagebinold+0.001)/25) * 25
collapse (sum) count=tot_emp  , by(  wagebins  quarterdate year )
xtset wagebins quarterdate
tsfill, full
replace count = 0 if count==.

g statenum = 41

rename count admincount

tempfile ORadmin
save `ORadmin'


********************************************************************************
*******************              Washington                      ***************
********************************************************************************


import delimited using "${data}arinmitnominal", clear
replace wagebin = wagebin*100
gen int wage2 = round(wagebin)
drop wagebin
rename wage2 wage_bin



cap drop year quarter
g year = .
g quarter = .


forval y=1990/2015 {
	forval q = 1/4 {
	
	replace year = `y' if period==`y'`q'
	replace quarter = `q' if period ==`y'`q'
	
	}
}

gen quarterdate = yq(year, quarter)
merge m:1 quarterdate using `cpiquarter', assert(2 3) keep(3) nogenerate
gen wagebin = (wage_bin+2.5)/(cpi/100)

rename wagebin wagebinold
assert wagebinold!=.
replace wagebinold = 3000 if wagebinold>=3000
replace wagebinold = 100 if wagebinold<100

gen int wagebins = floor(wagebinold/25) * 25


collapse (sum) count  , by(  wagebins  quarterdate year )
xtset wagebins quarterdate
assert "`r(balanced)'" == "strongly balanced"


g statenum = 53
rename count admincount  
  
sort statenum quarterdate wagebins

append using `MNadmin'
append using `ORadmin'
keep if year<=2015
tempfile admindata
save `admindata'


use statenum quarterdate wagebins count year if year>=1990 using "${data}state_panels_with3quant1979.dta", clear
merge m:1 statenum quarterdate using `qcew', assert(2 3) nogenerate keep(3)


keep if statenum==53 | statenum==27 | statenum==41

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

bys statenum quarterdate: egen _multiplier = max(multiplier)
bys statenum quarterdate: egen _multiplier2 = min(multiplier)

replace multiplier = _multiplier if multiplier==.

count if count==.
replace count = 0 if count==.
gen rw_count = count*multiplier

tempfile CPSdata
save `CPSdata'


use `admindata', clear
merge 1:1 statenum quarterdate wagebins using `CPSdata', assert(2 3) keep(3) nogenerate

*
drop *multiplier*
order statenum quarterdate year admincount count rw_count

egen wagebinstate = group(wagebin statenum)
xtset wagebinstate quarterdate
drop if wagebins==3000

foreach vv of varlist admincount count rw_count{
assert `vv' !=.
gen D`vv' = D.`vv'
gen D4`vv' = S4.`vv'
gen D20`vv' = S20.`vv'
}

save "${data}data_appendix_real.dta", replace
