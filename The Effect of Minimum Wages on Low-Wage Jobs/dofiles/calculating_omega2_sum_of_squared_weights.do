
*This do file is created by DC on 08/16/2018.
********************************************************************************
*********    Number of observations in the raw sample, by event        *********
********************************************************************************
use "${data}eventclassification.dta", clear
gen temp = sum(overallcountgr!=. & fedincrease!=1) if overallcountgr!=. & fedincrease!=1
drop overallcountgr
rename temp overallcountgroup
keep statenum quarterdate overallcountgroup belowshare fedincrease

foreach num of numlist 1(1)138{
di "`num'"
qui{
preserve
keep if overallcountgroup==`num'


			foreach vv of varlist statenum quarterdate belowshare{
				sum `vv', meanonly
				scalar `vv'= r(mean)
			}


use statenum quarterdate earnwt if quarterdate>= `=quarterdate'-12 & quarterdate<= `=quarterdate'+19 using  "${data}totransfer.dta", clear
gen omega2 = (earnwt/3)^2
collapse (sum) omega2, by(statenum quarterdate)
gen treatedstatenum = `=statenum'
gen treatdate = `=quarterdate'

tempfile event`num'
save `event`num''
restore
}
}
*

use `event1' , clear
foreach num of numlist 2(1)138{
append using `event`num''
}
*
compress
save "${data}omega2_sum_of_squared_weights.dta", replace
