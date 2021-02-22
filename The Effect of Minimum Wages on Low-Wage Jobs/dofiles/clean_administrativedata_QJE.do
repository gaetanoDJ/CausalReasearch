
use "${data}qcew_state_overall_counts_2016.dta", clear
rename statefips statenum
gen quarterdate = yq(year, quarter)
keep emp statenum quarterdate year

tempfile qcew_raw
save `qcew_raw'


use "${data}state_panels_with3quant1979.dta", clear
cap drop tagger
egen tagger = tag(statenum quarterdate)
keep if tagger
keep statenum quarterdate pop MW_real DMW year
tempfile pop
save `pop'


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
tempfile qcew
save `qcew'


use "${data}totransfer.dta", clear


keep month state age marital race sex esr ethnic uhours earnhr uearnwk earnwt ///
uhourse paidhre earnhre earnwke I25a I25b I25c I25d year lfsr89 lfsr94 statenum ///
orgwt monthdate quarterdate quarter division region censusnum stfips gradeat gradecp ihigrdc ///
grade92 unioncov unionmme hourslw class class94 smsa80 smsa93 smsa04 smsastat
*Drop self-employed
drop if (class==5 | class==6) & year<=1993
drop if (class94==6 | class94==7) & year>=1994

keep if year>=1990
compress




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
replace wage=wage*100	
	
g count = earnwt/3

keep count wage statenum year quarterdate
compress
drop if wage==. | wage==0
merge m:1 statenum quarterdate using `qcew', keep(3) nogenerate
merge m:1 quarterdate using `cpiquarter', keep(3) nogenerate
gen wagebins = floor(wage/5) * 5
replace wagebins = wagebins/(cpi/100)													// Wage is already in cents.
replace wagebins = 3500 if wagebins>=3500

replace wagebins = floor(wagebins/25)*25

collapse (sum) count ,by(statenum quarterdate year wagebins)



tempfile CPS_real
save `CPS_real'

********************************************************************************
*****************             MN                                     ***********
********************************************************************************


use "${data}MN admin data post 1998.dta", clear
replace wage_bin = wage_bin*100


gen quarterdate = yq(year, quarter)
*g quarterdate = (year-1979)*4 + quarter + 75
merge m:1 quarterdate using `cpiquarter', keep(3) nogenerate
rename wage_bin wagebinold
replace wagebinold = wagebinold/(cpi/100)
replace wagebinold = 3500 if wagebinold>=3500


gen wagebins = floor(wagebinold/25) * 25

collapse (sum) tot_emp  , by(  wagebins year  quarterdate )

g statenum = 27
rename tot_emp MNcount  
  
sort statenum quarterdate wagebins

tempfile MNadmin
save `MNadmin'


********************************************************************************
****************                       WA                              *********
********************************************************************************

 
import delimited using "${data}arinmitnominal", clear
replace wagebin = wagebin*100

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
*g quarterdate = (year-1979)*4 + quarter + 75
merge m:1 quarterdate using `cpiquarter', keep(3) nogenerate
rename wagebin wagebinold
replace wagebinold = wagebinold/(cpi/100)
replace wagebinold = 3500 if wagebinold>=3500

g wagebins  = floor(wagebinold/25) * 25
* replace wagebin  = int(wagebin100/100)

collapse (sum) count  , by(  wagebins  quarterdate year )

g statenum = 53
rename count acount  
  
sort statenum quarterdate wagebins




cap drop _merge
merge 1:1 statenum quarterdate year wagebins using `CPS_real', nogenerate
merge 1:1 statenum quarterdate year wagebins using `MNadmin', nogenerate
merge m:1 statenum quarterdate year using `pop', assert(2 3) nogenerate



********************************************************************************
*********************         OR                              ******************
********************************************************************************
preserve
use "${data}ORAdmin_5cent_bins.dta", clear
gen quarterdate = yq(year, quarter)
merge m:1 quarterdate using `cpiquarter', keep(3) nogenerate
rename wagebin wagebinold
replace wagebinold = wagebinold/(cpi/100)
replace wagebinold = 3500 if wagebinold>=3500

g wagebins  = floor(wagebinold/25) * 25


collapse (sum) ORcount = tot_emp , by(  wagebins  quarterdate year )
gen statenum = 41
tempfile ORadmin
save `ORadmin'

restore

merge 1:1 statenum quarterdate year wagebins using `ORadmin', nogenerate



* QCEW REWEIGHTING *
*CPS
merge m:1 statenum quarterdate using `qcew', keep(3)

replace count = count * multiplier
cap drop _merge

*Admin data
merge m:1 statenum quarterdate year using `qcew_raw', assert(3)

cap drop countall
cap drop acountall

bys statenum quarterdate: egen acountall = total(acount) 
bys statenum quarterdate: egen MNcountall = total(MNcount)
bys statenum quarterdate: egen ORcountall = total(ORcount)

cap drop multiplier_2

gen multiplier_2 = emp/acountall if statenum == 53
replace multiplier_2 = emp/MNcountall if statenum == 27
replace multiplier_2 = emp/ORcountall if statenum==41

replace MNcount = MNcount * multiplier_2
replace acount = acount * multiplier_2
replace ORcount = ORcount * multiplier_2

replace MNcountall = MNcountall * multiplier_2
replace acountall = acountall * multiplier_2
replace ORcountall = ORcountall * multiplier_2

keep if year>=1990



save "${data}Admin_CPS_workingdata_in2016dollars_allind_new.dta", replace
