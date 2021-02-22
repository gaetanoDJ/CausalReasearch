
*do "${dofiles}totransfer.do"

*****************************

use quarterdate statenum using "${data}VZmw_quarterly_lagsleads_1979_2016.dta", clear
tempfile forbalance
save `forbalance'

use "${data}eventclassification.dta", clear
merge 1:1 statenum quarterdate using `forbalance', nogenerate assert(2 3)
*Post and pre-periods of primary events (to be used in the prediction step)
replace overallcountgr = 0 if overallcountgr==.
replace overallcountgr = 0 if fedincrease==1
replace overallcountgr = 1 if overallcountgr>1 & overallcountgr!=.
xtset statenum quarterdate

gen prewindow = 0
gen postwindow = overallcountgr

forval num = 1(1)12{
gen L`num'overallcountgr = L`num'.overallcountgr
gen F`num'overallcountgr = F`num'.overallcountgr

replace L`num'overallcountgr = 0 if L`num'overallcountgr ==. 
replace F`num'overallcountgr = 0 if F`num'overallcountgr ==. 
replace postwindow = postwindow + L`num'overallcountgr
replace prewindow = prewindow + F`num'overallcountgr


}
*
forval num = 13(1)19{
gen L`num'overallcountgr = L`num'.overallcountgr
replace L`num'overallcountgr = 0 if L`num'overallcountgr ==. 
replace postwindow = postwindow + L`num'overallcountgr

}
*



replace postwindow = 1 if postwindow>=1
replace prewindow = 1 if prewindow>=1
replace prewindow = 0 if postwindow==1


keep statenum quarterdate prewindow
tempfile prewindow
save `prewindow'

*
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


use "${data}totransfer.dta", clear

keep month state age marital race sex esr ethnic uhours earnhr uearnwk earnwt ///
uhourse paidhre earnhre earnwke I25a I25b I25c I25d year lfsr89 lfsr94 statenum ///
orgwt monthdate quarterdate quarter division region censusnum stfips gradeat gradecp ihigrdc ///
grade92 unioncov unionmme hourslw class class94 smsa80 smsa93 smsa04 smsastat

 
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


*Prepare predictors
*Race-gender-teen
cap drop _race
cap drop teen
cap drop race_gender_teen 
gen _race = race
replace _race = 2 if race>2
gen teen = age<20
egen race_gender_teen = group(_race sex teen)
*Race-gender-young adult
cap drop young_adult 
cap drop race_gender_ya 
gen young_adult = age>=20 & age<=25
egen race_gender_ya = group(_race sex young_adult)
*Labor market experience
*If high school dropout: 	Age - 16
*If high school graduate: 	Age - 18
*If some college:			Age - 20
*If college:				Age - 22
gen 	lm_exp = age - 16 if hsd==1
replace lm_exp = age - 18 if hsd==0 & hsl==1
replace lm_exp = age - 20 if sc==1
replace lm_exp = age - 22 if coll==1

replace lm_exp = 0 if lm_exp <0


gen lm_exp2 = lm_exp^2
gen lm_exp3 = lm_exp^3


gen educ_exp_gender1 = hgradecp * lm_exp * (sex==1)
gen educ_exp_gender0 = hgradecp * lm_exp * (sex==0)

merge m:1 statenum quarterdate using "${data}VZmw_quarterly_lagsleads_1979_2016.dta", assert(3) keep(3) nogenerate
merge m:1 statenum quarterdate using "`prewindow'", assert(3) keep(3) nogenerate

gen MW = exp(logmw)
cap drop outcome_var 
gen outcome_var = (orig_wage/MW) <=1.25 if orig_wage!=.

reg outcome_var i.race_gender_teen i.race_gender_ya i.hsd hgradecp lm_exp lm_exp2 lm_exp3 i._race i.sex i.hispanic educ_exp_gender1 educ_exp_gender0 [pw=earnwt] if prewindow==1
est save "${estimates}CK_linear_model", replace
predict preds, xb
cap drop prewindow
****************************************************************************
*******************     Wage Bin Counts    *********************************
****************************************************************************
_pctile preds [aw=earnwt] ,nq(100)
scalar first_cutoff 	=  r(r90)
scalar second_cutoff 	=  r(r85)
scalar third_cutoff 	=  r(r80)
scalar fourth_cutoff		=  r(r50)

preserve
keep earnwt wage logwage MW statenum quarterdate preds 
replace earnwt = earnwt/3
gen one = 1
gen 	preds_group =  1 if preds >=  `=first_cutoff'
replace	preds_group =  2 if preds>= `=fourth_cutoff' & preds< `=first_cutoff'
replace preds_group =  3 if preds< `=fourth_cutoff' 
collapse (min) min_preds = preds (rawsum) preds_group_countall=earnwt (mean) logwage wage MW  [aw=earnwt], by(statenum quarterdate preds_group)
saveold "${data}CK_predicted_groups_collapsed.dta", version(12) replace
restore



replace earnwt=earnwt/3          												//Since when earnwt summed over month gives totalpopulation, we need to divide it to 3.
gen first_count = earnwt * (preds >=  `=first_cutoff')
gen second_count = earnwt * (preds>= `=second_cutoff')
gen third_count = earnwt * (preds>= `=third_cutoff')
gen fourth_count = earnwt * (preds>= `=fourth_cutoff' & preds< `=first_cutoff')
gen alt_fourth_count = earnwt * (preds>= `=fourth_cutoff' & preds< `=third_cutoff')
gen fifth_count = earnwt * (preds< `=fourth_cutoff' )

collapse (firstnm) year (sum) first_count second_count third_count fourth_count alt_fourth_count fifth_count , by(statenum quarterdate wagebins) fast

save "${data}CK_groups.dta" , replace


use "${data}totransfer.dta", clear

keep month state age marital race sex esr ethnic uhours earnhr uearnwk earnwt ///
uhourse paidhre earnhre earnwke I25a I25b I25c I25d year lfsr89 lfsr94 statenum ///
orgwt monthdate quarterdate quarter division region censusnum stfips gradeat gradecp ihigrdc ///
grade92 smsastat smsa80 smsa93 smsa04 

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

gen conshours=uhourse if I25a==0
/*replace conshours= hourslw if conshours==.
assert I25a<=0 if uhourse!=.
assert conshours!=.
*/
*NOTE: The ratio of people who is hsl and not is almost 1.

gen hsl40 = 0
replace hsl40 = 1 if hsl==1 & age<40
label var hsl40 "high school or less, under 40"
compress

gen hsd40 = 0
replace hsd40 = 1 if hsd==1 & age<40
label var hsd40 "high school dropout, under 40"
compress

assert hsl40==1 if hsd40==1


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


*Prepare predictors
*Race-gender-teen
cap drop _race
cap drop teen
cap drop race_gender_teen 
gen _race = race
replace _race = 2 if race>2
gen teen = age<20
egen race_gender_teen = group(_race sex teen)
*Race-gender-young adult
cap drop young_adult 
cap drop race_gender_ya 
gen young_adult = age>=20 & age<=25
egen race_gender_ya = group(_race sex young_adult)
*Labor market experience
*If high school dropout: 	Age - 16
*If high school graduate: 	Age - 18
*If some college:			Age - 20
*If college:				Age - 22
gen 	lm_exp = age - 16 if hsd==1
replace lm_exp = age - 18 if hsd==0 & hsl==1
replace lm_exp = age - 20 if sc==1
replace lm_exp = age - 22 if coll==1

replace lm_exp = 0 if lm_exp <0


gen lm_exp2 = lm_exp^2
gen lm_exp3 = lm_exp^3


gen educ_exp_gender1 = hgradecp * lm_exp * (sex==1)
gen educ_exp_gender0 = hgradecp * lm_exp * (sex==0)



est use "${estimates}CK_linear_model"
predict preds, xb


gen first_pop 		= (preds>=`=first_cutoff')
gen second_pop 		= (preds>=`=second_cutoff')
gen third_pop 		= (preds>=`=third_cutoff')
gen fourth_pop 		= (preds>=`=fourth_cutoff' & preds< `=first_cutoff')
gen alt_fourth_pop 	= (preds>=`=fourth_cutoff' & preds< `=third_cutoff')
gen fifth_pop 		= (preds< `=fourth_cutoff' )

replace earnwt = earnwt/3
collapse (sum) first_pop second_pop third_pop fourth_pop alt_fourth_pop fifth_pop [iw = earnwt], by(statenum quarterdate) fast


merge 1:m statenum quarterdate using "${data}CK_groups.dta" , assert(3) nogenerate
compress
save "${data}CK_groups.dta" , replace
