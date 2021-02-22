*This do file is created by DC on 12/15/2015
********************************************************************************
*******     Demographic and Imputation Related Additions to Panels  ************
********************************************************************************

use "${data}state_panels_cents_balanced_QJE.dta", clear

********************************************************************************
***************    Rename Variables   ******************************************
********************************************************************************

cap rename aveconshours avehours
cap rename totalpopulation population
cap rename totpopcount count


save "${data}state_panels_cents_balanced_QJE.dta", replace
clear

********************************************************************************
********************      Now create demographic counts  ***********************
********************************************************************************


use "${data}totransfer.dta", clear

keep month state age marital race sex esr ethnic uhours earnhr uearnwk earnwt ///
uhourse paidhre earnhre earnwke I25a I25b I25c I25d year lfsr89 lfsr94 statenum ///
orgwt monthdate quarterdate quarter division region censusnum stfips gradeat gradecp ihigrdc ///
grade92 smsastat smsa80 smsa93 smsa04 
 

cap drop hispanic
gen byte hispanic = 0
replace hispanic = 1 if year >= 1976 & year <= 2002 & ethnic >= 1 & ethnic <= 7 & ethnic!=.
replace hispanic = 1 if year >= 2003 & year <= 2013 & ethnic >= 1 & ethnic <= 5 & ethnic!=.
replace hispanic = 1 if (year >= 2014) & (ethnic >= 1 & ethnic <= 8)

* race is white only, black only, other
* large classification begin in 2003
recode race (3/max = 3)
**********************************************
*Non-hispanic black
cap drop black
gen black=0
replace black=1 if race==2 & hispanic==0 & hispanic!=.

cap drop dmarried
gen dmarried= marital <= 2 if marital ~= .

recode sex (2=0)                                                                /* Male=1, Female=0 */

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

cap drop hsd
gen hsd=0
label var hsd "highschool dropout"
replace hsd=1 if grade92<=38 & year>=1992
replace hsd=1 if hgradecp<12 & year<1992


gen hsl40 = 0
replace hsl40 = 1 if hsl==1 & age<40
label var hsl40 "high school or less, under 40"
compress

gen hsd40 = 0
replace hsd40 = 1 if hsd==1 & age<40
label var hsd40 "high school dropout, under 40"


********************************************************************************
*************       Demographic Variables   ************************************
********************************************************************************



replace earnwt=earnwt/3          												//Since when earnwt summed over month gives totalpopulation, we need to divide it to 3.
cap drop teen
gen byte teen=0
replace teen=1 if age>=16 & age<=19
cap drop white
gen byte white=0
replace white=1 if race==1 & hispanic==0
collapse (sum) hispanicpop=hispanic dmarriedpop=dmarried hslpop=hsl hsl40pop=hsl40  hsdpop=hsd hsd40pop=hsd40 blackpop=black whitepop=white genderpop=sex teenpop=teen [iw=earnwt], ///
by(statenum quarterdate) fast

merge 1:m statenum quarterdate using "${data}state_panels_cents_balanced_QJE.dta", assert(3) nogenerate
save "${data}state_panels_cents_balanced_add_QJE.dta", replace
