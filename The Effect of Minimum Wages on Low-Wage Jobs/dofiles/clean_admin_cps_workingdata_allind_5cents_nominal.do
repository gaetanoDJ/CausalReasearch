
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
keep statenum quarterdate pop MW DMW year
tempfile pop
save `pop'


use "${data}qcew_multiplier.dta", clear
keep statenum quarterdate multiplier
tempfile qcew
save `qcew'

local centsincrement = 5


use "${data}totransfer.dta", clear


keep month state age marital race sex esr ethnic uhours earnhr uearnwk earnwt ///
uhourse paidhre earnhre earnwke I25a I25b I25c I25d year lfsr89 lfsr94 statenum ///
orgwt monthdate quarterdate quarter division region censusnum stfips gradeat gradecp ihigrdc ///
grade92 unioncov unionmme hourslw class class94 smsa80 smsa93 smsa04 smsastat
*Drop self-employed
drop if (class==5 | class==6) & year<=1993
drop if (class94==6 | class94==7) & year>=1994

drop class class94
keep if year>=1983
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
drop if year<1990
merge m:1 statenum quarterdate using `qcew', keep(3) nogenerate
gen int wage2 = round(wage)
drop wage
rename wage2 wage
gen int wagebins = floor(wage/5) * 5
replace wagebins = 3500 if wagebins>=3500

*replace wagebins = floor(wagebins/25)*25

collapse (sum) count ,by(statenum quarterdate year wagebins)



tempfile CPS_nominal
save `CPS_nominal'



use "${data}MN admin data post 1998.dta", clear
replace wage_bin = wage_bin*100
gen int wage2 = round(wage_bin)
drop wage_bin
rename wage2 wage_bin


gen quarterdate = yq(year, quarter)
*g quarterdate = (year-1979)*4 + quarter + 75
rename wage_bin wagebinold
replace wagebinold = 3500 if wagebinold>=3500


gen int wagebins = floor(wagebinold/5) * 5

collapse (sum) tot_emp  , by(  wagebins year  quarterdate )

g statenum = 27
rename tot_emp MNcount  
  
sort statenum quarterdate wagebins

tempfile MNadmin
save `MNadmin'

 
import delimited using "${data}arinmitnominal", clear
replace wagebin = wagebin*100
gen int wage2 = round(wagebin)
drop wagebin
rename wage2 wagebin



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
rename wagebin wagebinold
replace wagebinold = 3500 if wagebinold>=3500
gen int wagebins = floor(wagebinold/5) * 5

*g wagebins  = floor(wagebinold/25) * 25
*replace wagebin  = int(wagebin100/100)

collapse (sum) count  , by(  wagebins  quarterdate year )

g statenum = 53
rename count acount  
  
sort statenum quarterdate wagebins


tempfile WAadmin
save `WAadmin' 



use "${data}ORAdmin_5cent_bins.dta", clear
gen quarterdate = yq(year, quarter)
rename wagebin wagebinold
replace wagebinold = 3500 if wagebinold>=3500
gen int wagebins = floor((wagebinold+0.001)/5) * 5
collapse (sum) count=tot_emp  , by(  wagebins  quarterdate year )
xtset wagebins quarterdate
tsfill, full
replace count = 0 if count==.

g statenum = 41

rename count ORcount  
bys statenum quarterdate: egen ORcountall = total(ORcount)
  
sort statenum quarterdate wagebins

bys quarterdate: egen _year = max(year)
replace year = _year if year==.
assert year!=.


tempfile ORadmin
save `ORadmin'



use `WAadmin',clear
cap drop _merge
merge 1:1 statenum quarterdate year wagebins using `CPS_nominal', nogenerate
merge 1:1 statenum quarterdate year wagebins using `MNadmin', nogenerate
merge 1:1 statenum quarterdate year wagebins using `ORadmin', nogenerate
merge m:1 statenum quarterdate year using `pop', assert(2 3) nogenerate

* QCEW REWEIGHTING *
*CPS
merge m:1 statenum quarterdate using `qcew', keep(3)

replace count = count * multiplier

*Admin data
merge m:1 statenum quarterdate year using `qcew_raw', nogenerate assert(3)

cap drop countall
cap drop acountall
cap drop ORcountall

bys statenum quarterdate: egen acountall = total(acount) 
bys statenum quarterdate: egen MNcountall = total(MNcount)
bys statenum quarterdate: egen ORcountall = total(ORcount)


cap drop multiplier_2

gen multiplier_2 = emp/acountall if statenum == 53
replace multiplier_2 = emp/MNcountall if statenum == 27
replace multiplier_2 = emp/ORcountall if statenum == 41


replace MNcount = MNcount * multiplier_2
replace acount = acount * multiplier_2
replace ORcount = ORcount * multiplier_2


replace MNcountall = MNcountall * multiplier_2
replace acountall = acountall * multiplier_2
replace ORcountall = ORcountall * multiplier_2


keep if year>=1990



save "${data}Admin_CPS_workingdata_nominal_allind_5cents_new.dta", replace
