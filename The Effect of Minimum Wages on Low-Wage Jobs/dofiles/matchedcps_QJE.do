
use "${data}totransfer.dta", clear

keep hhid hhnum lineno minsamp month state age marital race sex esr ethnic uhours earnhr uearnwk earnwt ///
uhourse paidhre earnhre earnwke I25a I25b I25c I25d year lfsr89 lfsr94 statenum ///
orgwt monthdate quarterdate quarter division region gradeat gradecp ihigrdc ///
grade92 class class94 dind* ind* occ* docc*
 
preserve
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
restore

merge m:1 monthdate using `cpi', nogenerate assert(3)
compress

*Create total population variable at stateXquarter level.

/* NOTE: The NBER documentation says:"this sums to the total population each month
and 12 times the population for each MORG file." Hence the method I will follow
is to create stateXmonth level total population, then collapse to turn it into
stateXquarterdate. So stateXquarterdate totalpopulation is 3 month's average 
total population for that state.
*/
*Total population
/*
preserve
keep monthdate quarterdate statenum earnwt
bysort statenum monthdate: egen totalpopulation=total(earnwt)
collapse (mean) totalpopulation, by(statenum quarterdate)
tempfile totpop
save `totpop'
restore 
*/
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

compress


**************************************************************************************************************
sort statenum monthdate

*Now, let's turn nominal wages into 2014 $ wages (orig_wage variable is in nominal terms).
	gen orig_wage=wage
	replace wage=wage/(cpi/100)

*Turn wage variable into cents
replace wage=wage*100	
	
/*	
*To create shares (everyone should be at least in one group):
forval base=300(25)2975 {
	cap drop popcount`base'
	gen popcount`base'=0
	if `base'==300{
	replace popcount`base'=1 if wage<`base'+25
	}
	else {
	replace popcount`base'=1 if wage<`base'+25 & wage>=`base'
	}
	}
cap drop popcount3000
gen popcount3000=0
replace popcount3000=1 if wage>=3000
*
*/



/* Additional variables used in previous papers with the same names to prevent confusion. */
cap drop hispanic
gen byte hispanic = 0
replace hispanic = 1 if year >= 1976 & year <= 2002 & ethnic >= 1 & ethnic <= 7
replace hispanic = 1 if year >= 2003 & year <= 2013 & ethnic >= 1 & ethnic <= 5
replace hispanic = 1 if (year >= 2014) & (ethnic >= 1 & ethnic <= 8)

* race is white only, black only, other
* large classification begin in 2003
recode race (3/max = 3)
**********************************************
*Non-hispanic black
cap drop black
gen black=0
replace black=1 if race==2 & hispanic==0

cap drop dmarried
gen dmarried= marital <= 2 if marital ~= .

recode sex (2=0)

cap drop mlr
gen mlr = .
replace mlr = esr if year >= 1979 & year <= 1988
replace mlr = lfsr89 if year >= 1989 & year <= 1993
replace mlr = lfsr94 if year >= 1994

gen emp = mlr == 1 | mlr == 2 if mlr ~= .


* Create an education variable that is consistent throughout all years.
* Following nbermorgworkprecise.do    

*Education
/*The problem with education variable is that the question asked in the survey
has been changed in 1992. It was changed again in 1998 and thanks to that
the people who created NBER-MORG have been able to come up with ihigrdc. However
between 1992 and 1997 has not been fixed by them. 
The author who has proposed the way to fix post-1998 education data has a proposal
to fix 1992-1997 data as well. I will follow Jaeger 1997.*/

cap drop hgradecp
generate hgradecp=gradeat if gradecp==1
replace hgradecp=gradeat-1 if gradecp==2
replace hgradecp=ihigrdc if ihigrdc!=. & hgradecp==.

local grade92code "31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46"
local impute92code "0 2.5 5.5 7.5 9 10 11 12 12 13 14 14 16 18 18 18"

local n1 : word count `grade92code'

forval i=1/`n1' {
	local a : word `i' of `grade92code'
	local b : word `i' of `impute92code'
	replace hgradecp=`b' if grade92==`a'
	}
replace hgradecp=0 if hgradecp==-1
label variable hgradecp "highest grade completed"
*
**** Create high school or less variable.

cap drop hsl
gen hsl=0
label variable hsl "high school or less"
replace hsl=1 if hgradecp<=12


********************************************************************************
*************     Highschool dropout               *****************************
********************************************************************************
cap drop hsd
gen hsd=0
label var hsd "highschool dropout"
replace hsd=1 if grade92<=38 & year>=1992
replace hsd=1 if hgradecp<12 & year<1992

assert hsl==1 if hsd==1

compress

tempfile alldata
save `alldata'

local counter = 0
forval num=1979(1)2015{
	local counter = `counter' + 1
	use `alldata', clear
	keep if year==`num' & minsamp==4
	count
	if `counter' == 1{
	mat totalobs = ( r(N), `num' )
	}
	else{
	mat totalobs = ( totalobs \ r(N), `num' )	
	}
	egen identify = group(month hhid hhnum lineno statenum)
	bys identify: gen temp=_N
	count if temp>1
	if `counter' == 1{
	mat manytomany = ( r(N), `num' )
	}
	else{
	mat manytomany = ( manytomany \ r(N), `num' )	
	}
	keep if temp==1
	cap drop identify
	cap drop temp
	foreach vv of varlist * {
		rename `vv' `vv'_1
	}

	foreach vv in month hhid hhnum lineno  statenum {
	gen `vv' = `vv'_1
	}
	ds *_1
	keep `r(varlist)' month hhid hhnum lineno  statenum 
	
	cap drop _merge*
		tempfile firstyear_`num'
		save `firstyear_`num''

		local second = `num' + 1
	use `alldata', clear
	keep if year==`second' & minsamp==8
	egen identify = group( month hhid hhnum lineno  statenum)
	bys identify: gen temp=_N
		
	keep if temp==1
	cap drop identify
	cap drop temp
	cap drop _merge*

	tempfile secondyear_`num'
	save `secondyear_`num''

	
	*Merge
	use `firstyear_`num'', clear
	merge 1:1 month hhid hhnum lineno  statenum using `secondyear_`num''
	drop if _merge==2
	count if _merge==3
	if `counter' == 1{
	mat totalmatch = ( r(N), `num' )
	}
	else{
	mat totalmatch = ( totalmatch \ r(N), `num' )	
	}

	
	gen badmatches = (sex!=sex_1 | race!=race_1 | (age < age_1 - 1 | age > age_1 + 3)) if _merge==3
	count if badmatches==1
	if `counter' == 1{
	mat badmatch = ( r(N), `num' )
	}
	else{
	mat badmatch = ( badmatch \ r(N), `num' )	
	}
	
	
	tempfile merged_`num'
	save `merged_`num''

	

}
*


use `merged_1979', clear
forval num=1980(1)2015{
append using `merged_`num'' 

}
*

keep if badmatches==0 & _merge==3

save "${data}matchedCPS_1979_2016.dta", replace

