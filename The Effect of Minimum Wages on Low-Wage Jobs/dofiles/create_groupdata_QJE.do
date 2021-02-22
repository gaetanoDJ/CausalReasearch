

use "${data}totransfer.dta", clear

keep month state age marital race sex esr ethnic uhours earnhr uearnwk earnwt ///
uhourse paidhre earnhre earnwke I25a I25b I25c I25d year lfsr89 lfsr94 statenum ///
orgwt monthdate quarterdate quarter division region censusnum stfips gradeat gradecp ihigrdc ///
grade92 unioncov unionmme hourslw class class94 smsa80 smsa93 smsa04 smsastat

save "${data}totransfer_2.dta", replace
 
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

preserve
keep monthdate quarterdate statenum earnwt
bysort statenum monthdate: egen totalpopulation=total(earnwt)
collapse (mean) totalpopulation, by(statenum quarterdate)
tempfile totpop
save `totpop'
restore 

*Drop self-employed


*Education
/*The problem with education variable is that the question asked in the survey
has been changed in 1992. It was changed again in 1998 and thanks to that
the people who created NBER-MORG have been able to come up with ihigrdc. However
between 1992 and 1997 has not been fixed by them. 
The author who has proposed the way to fix post-1998 education data has a proposal
to fix 1992-1997 data as well. I will follow Jaeger 1997 (http://www.djaeger.org/research/pubs/cpsedmlr.pdf).*/

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


********************************************************************************
*************        Some college          *************************************
********************************************************************************

cap drop sc
gen sc=0
label var sc "some college"
replace sc=1 if (grade92>=40 & grade92<=42) & year>=1992
replace sc=1 if (hgradecp==13 | hgradecp==14 | hgradecp==15) & year<1992


********************************************************************************
*************         College graduate         *********************************
********************************************************************************

cap drop coll
gen coll=0
label var coll "college graduate"
replace coll=1 if (grade92>42) & year>=1992
replace coll=1 if (hgradecp>15) & year<1992


gen conshours=uhourse if I25a==0

gen hsl40 = 0
replace hsl40 = 1 if hsl==1 & age<40
label var hsl40 "high school or less, under 40"
compress

gen hsd40 = 0
replace hsd40 = 1 if hsd==1 & age<40
label var hsd40 "high school dropout, under 40"
compress

assert hsl40==1 if hsd40==1
assert age<=99
*High school graduate
gen byte hsg = hsl - hsd
assert hsg==0 | hsg==1

********************************************************************************
******************       Education X Age          ******************************
********************************************************************************

foreach vv in hsd hsg sc coll{
gen byte `vv'0020 = (`vv'==1 & age<20)
gen byte `vv'6099 = (`vv'==1 & age>=60 & age<=99)
}
*
foreach j of numlist 20 30 40 50{
	foreach vv in hsd hsg sc coll{
			local k = `j' + 10
			gen byte `vv'`j'`k' = (`vv'==1 & age>=`j' & age<`k')
	}
}
*


********************************************************************************
***************			Population counts				************************
********************************************************************************
ds hsd* hsg* sc* coll*

preserve
replace earnwt=earnwt/3          												//Since when earnwt summed over month gives totalpopulation, we need to divide it to 3.
collapse (sum) `r(varlist)' [iw=earnwt], ///
by(statenum quarterdate) fast

ds hsd* hsg* sc* coll*
save "${data}temp.dta", replace

foreach vv of varlist `r(varlist)'  {
assert `vv' !=.
rename `vv' `vv'pop
}
compress
tempfile grouppop
save `grouppop'
	
restore




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

compress


**************************************************************************************************************
sort statenum monthdate

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
assert emp==1



****************************************************************************
*******************     Wage Bin Counts    *********************************
****************************************************************************
compress
	
preserve
replace earnwt=earnwt/3          												//Since when earnwt summed over month gives totalpopulation, we need to divide it to 3.
collapse (firstnm) year (sum) count=earnwt, by(statenum quarterdate wagebins) fast
tempfile qtrwagebincounts
save `qtrwagebincounts'
	
restore


********************************************************************************
*********************     Demographic Counts    ********************************
********************************************************************************


preserve
replace earnwt=earnwt/3          												//Since when earnwt summed over month gives totalpopulation, we need to divide it to 3.
ds hsd* hsg* sc* coll*

collapse (sum) `r(varlist)' ///
[iw=earnwt], ///
by(statenum quarterdate wagebins) fast
ds hsd* hsg* sc* coll*
foreach vv of varlist `r(varlist)' {
rename `vv' `vv'count
}

tempfile demogbincounts
save `demogbincounts'
restore






********************************************************************************
**************               Merging CPI          ******************************
********************************************************************************

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
gen quarterdate = qofd(dofm(monthdate))
collapse cpi, by(quarterdate)
tempfile cpiquarter
save `cpiquarter'
restore



********************************************************************************
*******************        Merge and Save       ********************************
********************************************************************************
use `qtrwagebincounts', clear
merge 1:1 statenum quarterdate wagebins using `demogbincounts'	,nogenerate assert(3)
merge m:1 statenum quarterdate          using `totpop'        	,nogenerate assert(3)
merge m:1 		   quarterdate			using `cpiquarter'		,nogenerate assert(3)
compress
tempfile grouppanels
save `grouppanels'
save "${data}grouplevelanalysis.dta", replace

use "${data}grouplevelanalysis.dta", clear
tempfile grouppanels
save `grouppanels'

use `grouppanels' , clear
keep if year!=.
sum quarterdate, meanonly
scalar numberqrt=r(max) - r(min) + 1
scalar minqrt=r(min)
egen tagger= tag(wagebins statenum)
keep if tagger
keep wagebins statenum
*collapse , by(wagebins statenum)
*drop all

tempfile wagebinstateshell

expand numberqrt
bys wagebins statenum: gen quarterdate = _n+ minqrt - 1

sort wagebins statenum quarterdate
save "`wagebinstateshell'"

use `grouppanels' , clear

cap drop _merge
merge 1:1 wagebins statenum quarterdate using "`wagebinstateshell'", keep(2 3) nogenerate
compress
***  fill in state-by-period level variables ****

foreach x of varlist year cpi  ///
			   {
	cap drop _`x'
	egen _`x' = mean(`x'), by(statenum quarterdate)
	replace `x' = _`x' if `x'==.
	drop _`x'
}
*
ds *count
foreach x of varlist `r(varlist)' {
		replace `x' = 0 if `x'==.
	}


cap drop _merge


bys statenum quarterdate: egen countall = total(count)

*To assert that the panel is balanced.
egen wagebinstate = group(wagebins statenum)
xtset wagebinstate quarterdate
assert "`r(balanced)'" == "strongly balanced"

merge m:1 statenum quarterdate          using `grouppop'        ,nogenerate assert(3)

foreach vv in hsd hsg sc coll{
	foreach vv2 in 0020 2030 3040 4050 5060 6099{
	bys statenum quarterdate: egen `vv'`vv2'countall = total(`vv'`vv2'count)
	}
}
*
foreach vv in hsd {
	bys statenum quarterdate: egen `vv'countall = total(`vv'count)
	}
*
cap drop countall
bys statenum quarterdate: egen countall = total(count)
sum hsdcountall

compress


save "${data}grouplevelanalysis2.dta", replace



use "${data}grouplevelanalysis2.dta", clear

	cap drop _totalpopulation
	egen _totalpopulation = mean(totalpopulation), by(statenum quarterdate)
	replace totalpopulation = _totalpopulation if totalpopulation==.
	drop _totalpopulation


*Continue from here.
*Check whether sum of hsd`numbers'* is hsd*.

rename totalpopulation population

*** SET PANEL VARS ***
foreach vv in hsd hsg sc coll{
	foreach vv2 in 0020 2030 3040 4050 5060 6099{
	gen `vv'`vv2'countpc = `vv'`vv2'count/`vv'`vv2'pop
	}
}	
*
 
g overallcountpc = count/pop
g hsdcountpc = hsdcount/hsdpop

*
g overallcountpcall = countall/pop


foreach vv in hsd hsg sc coll{
	foreach nn in 0020 2030 3040 4050 5060 6099{
	gen `vv'`nn'countpcall = `vv'`nn'countall/`vv'`nn'pop
	}
}
*
compress 
merge 1:1 wagebinstate quarterdate using "${data}state_panels_with3quant1979.dta", keepusing(*treat* *cont* *window* cleansample *MW* fedincrease overallcountgr wagequarterdate)

foreach vv in hsd hsg sc coll{
	foreach vv2 in 0020 2030 3040 4050 5060 6099{
		foreach b of numlist 1979{
					assert `vv'`vv2'pop!=.
					egen wt`vv'`vv2'`b' = mean(`vv'`vv2'pop) if year>=`b' & cleansample ==1 , by(statenum quarterdate)
					global weight`vv'`vv2'`b' "[aw=wt`vv'`vv2'`b']"  
		}
	}
}
*
compress

xtset wagebinstate quarterdate
compress


cap rm "${data}grouplevelanalysis.dta"
save "${data}grouplevelanalysis_final.dta", replace
