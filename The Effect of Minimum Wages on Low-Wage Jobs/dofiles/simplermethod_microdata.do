* CPI
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

use "${data}VZmw_quarterly_lagsleads_1979_2016.dta", clear
merge 1:1 statenum quarterdate using "${data}eventclassification.dta"
merge m:1 quarterdate using `cpiquarter', nogenerate assert(3)
g MW_real = exp(logmw)/(cpi/100)
keep if overallcountgr > 0 & overallcountgr ~= . & fedincrease ~= 1
assert _N == 138
local upperw = 15

* use microdata to aggregate outcomes below upperw
use "${data}totransfer.dta", clear
keep month state age marital race sex esr ethnic uhours earnhr uearnwk earnwt ///
uhourse paidhre earnhre earnwke I25a I25b I25c I25d year lfsr89 lfsr94 statenum ///
orgwt monthdate quarterdate quarter division region censusnum stfips gradeat gradecp ihigrdc ///
grade92 unioncov unionmme hourslw class class94 smsa80 smsa93 smsa04 smsastat

preserve
keep monthdate quarterdate statenum earnwt
bysort statenum monthdate: egen totalpopulation=total(earnwt)
collapse (mean) totalpopulation, by(statenum quarterdate)
tempfile totpop
save `totpop'
restore

*Drop self-employed
drop if (class==5 | class==6) & year<=1993
drop if (class94==6 | class94==7) & year>=1994
drop class class94
*** Getting rid of imputed ones and those whose incomes are not positive.
gen wage = earnhre / 100 if paidhre == 1
replace wage = earnwke/uhourse if paidhre == 2

* use allocation variables to determine hours, earnings, and hourly wage imputations
gen hoursimputed = I25a > 0 & I25a ~= .
gen earningsimputed = I25d > 0 & I25d ~= .
gen wageimputed = I25c > 0 & I25c ~= .

foreach var of varlist hoursimputed earningsimputed wageimputed {
	* use different method to identify imputations in 1989-1993 data
	replace `var' = 0 if year >= 1989 & year <= 1993
	* no reliable imputation data from Jan 1994 through Aug 1995; see Hirsch & Schumacher
	replace `var' = 0 if year == 1994 | (year == 1995 & month <= 8)
}

* use method analogous to that employed by Hirsch & Schumacher to identify imputations
* in 1989-1993 data
replace wageimputed = 1 if year >= 1989 & year <= 1993 & (earnhr == . | earnhr == 0) & (earnhre > 0 & earnhre ~= .)
replace hoursimputed = 1 if year >= 1989 & year <= 1993 & (uhours == . | uhours == 0) & (uhourse > 0 & uhourse ~= .)
replace earningsimputed = 1 if year >= 1989 & year <= 1993 & (uearnwk == . | uearnwk == 0) & (earnwke > 0 & earnwke ~= .)

gen imputed = 0
replace imputed = 1 if paidhre == 2 & (hoursimputed == 1 | earningsimputed == 1)
replace imputed = 1 if paidhre == 1 & (wageimputed == 1)
replace wage = . if imputed == 1
gen logwage = log(wage)
drop if logwage == .

merge m:1 quarterdate using `cpiquarter', nogenerate assert(3)
replace wage = wage/(cpi/100)

* keep only those below upper W
keep if wage < `upperw'
*keep if wage <= `upperw'

gen all = 1
replace earnwt = earnwt / 3
gcollapse (sum) emp=all wage=wage [pw=earnwt], by(statenum quarterdate)
merge m:1 statenum quarterdate using `totpop', nogenerate assert(3)
format %tq quarterdate
compress
save "${data}statequarter_counts_maxupperw.dta", replace
