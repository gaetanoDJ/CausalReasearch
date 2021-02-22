

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


local filelist: dir "${data}" files "me_corrected_logwage_statenum_*"
foreach file of local filelist {
use "${data}`file'", clear
compress
save "${data}`file'", replace
}
*
clear
local counter = 0
foreach file of local filelist {
local counter = `counter'  +  1
di "`counter'"
if `counter' == 1{
use "${data}`file'", clear
}
else{
append using "${data}`file'"
}
}
*


gen wage_me = exp(logwage)
merge m:1 quarterdate using `cpiquarter', assert(3) nogenerate
gen wage_orig = wage_me
replace wage_me = wage_me /(cpi/100)

gen wagebins=0
replace wagebins=100 if wage_me <125
replace wagebins=floor(wage_me/25)*25 if wage_me >=125 & wage_me <3000
replace wagebins=3000 if wage_me >=3000
assert wagebins!=0

collapse (sum) density, by(statenum quarterdate wagebins)
compress
save "${data}me_corrected_logwages.dta", replace
