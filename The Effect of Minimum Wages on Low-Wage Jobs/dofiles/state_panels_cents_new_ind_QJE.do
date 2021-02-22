****************
*** CPI data ***
****************
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
tempfile cpi
save `cpi'



******************
*** Clean MORG ***
******************
use "${data}totransfer_ind.dta", clear
keep month state age marital race sex /*esr*/ ethnic uhours earnhr uearnwk earnwt ///
uhourse paidhre earnhre earnwke I25a I25b I25c I25d year lfsr89 lfsr94 statenum ///
monthdate quarterdate quarter division region censusnum stfips /*gradeat gradecp*/ ihigrdc ///
grade92 hourslw class class94 censusind ind_*

*Drop self-employed
drop if (class == 5 | class == 6) & year <= 1993
drop if (class94 == 6 | class94 == 7) & year >= 1994

gen pubsector = class94 >= 1 & class94 <= 3 if year >= 1994
replace pubsector = 1 if class >= 2 & class <= 4 & year <= 1993
gen prisector = class94 >= 4 & class94 <= 5 if year >= 1994
replace prisector = 1 if class == 1 & year <= 1993
drop class94 class

merge m:1 monthdate using `cpi', assert(2 3)
keep if _merge==3
drop _merge

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

*Now, let's turn nominal wages into 2014 $ wages (orig_wage variable is in nominal terms).
	gen orig_wage=wage
	replace wage=wage/(cpi/100)

*Turn wage variable into cents
replace wage=wage*100

********************************************************************************
****************     Wage bin variable *****************************************
********************************************************************************
*This variable will be useful in collapsing
gen wagebins=0
replace wagebins=100 if wage<125
replace wagebins=floor(wage/25)*25 if wage>=125 & wage<3000
replace wagebins=3000 if wage>=3000

assert wagebins!=0
recode sex (2=0)

cap drop mlr
gen mlr = .
*replace mlr = esr if year >= 1979 & year <= 1988
replace mlr = lfsr89 if year >= 1989 & year <= 1993
replace mlr = lfsr94 if year >= 1994

gen emp = mlr == 1 | mlr == 2 if mlr ~= .
assert emp==1

****************************************************************************
*******************     Wage Bin Counts    *********************************
****************************************************************************
gen ind_all = 1
foreach var of varlist ind_* {
	gen `var'_overallsamplesize = `var'
	rename `var' `var'_overallcount
}

replace earnwt=earnwt/3  //Since when earnwt summed over month gives totalpopulation, we need to divide it to 3.
collapse (sum) ind_*overallcount (rawsum) ind_*overallsamplesize [iw=earnwt], by(statenum quarterdate wagebins) fast
* grab wagebin id information from master template data
* this ensures panel is balanced and uses wagebinstate codes from baseline data
merge 1:1 statenum quarterdate wagebins using "${data}state_panels_with3quant1979.dta", assert(2 3) nogenerate
* drop some variables not used in industry code
drop HSD*
drop HSL*
drop BH*
drop teen*
drop female*
* our industry data begins at a minimum in 1992
keep if year >= 1992
foreach vv of varlist ind_*overallcount {
	di "working on `vv'"
	bys statenum quarterdate: egen `vv'all = total(`vv')
	replace `vv'=0 if missing(`vv')
	tab statenum if `vv'all==0
	assert `vv'all!=.
}
* NOTE: DC is missing non-tradable and manufacturing observations...
* We should consider dropping DC.
foreach var of varlist ind_*overallcount {
	gen `var'pc = `var'/pop
	gen `var'pcall = `var'all/pop
}
compress
tempfile counts
save `counts'

foreach yearset in 19922016 {
	local beginyear = substr("`yearset'",1,4)
	local endyear = substr("`yearset'",5,4)
	use `counts' if year >= `beginyear' & year <= `endyear', clear
	save "${data}state_panels_with3quant_ind_`beginyear'_`endyear'.dta", replace
}
