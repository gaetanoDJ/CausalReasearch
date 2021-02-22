

clear


use "${data}qcew_multiplier.dta", clear
keep statenum quarterdate multiplier
assert multiplier!=0
tempfile qcew
save `qcew'

use "${data}eventclassification.dta", clear
gen year = year(dofq(quarterdate))
cap drop treat
gen treat = overallcountgr>0 & overallcountgr!=. & fedincrease!=1
gen statetreat = alltreat if fedincrease!=1
bys statenum: egen _temp =  max(treat) if year<1992
bys statenum: egen temp =  max(_temp)
bys statenum: egen _temp2 =  max(treat) if year<1996
bys statenum: egen temp2 =  max(_temp2)
levelsof statenum if temp==1, local(temploc)
levelsof statenum if temp2==1, local(temploc2)


use "${data}VZmw_quarterly_lagsleads_1979_2016",clear
gen mw = exp(logmw)
gen year = year(dofq(quarterdate))
gen quarter = quarter(dofq(quarterdate))
collapse  mw, by(statenum year)


xtset statenum year
forvalues index = 1/5 {
	foreach lagorlead in L F {
		gen `lagorlead'`index'logmw = log(`lagorlead'`index'.mw)
	}
}
gen logmw = log(mw)
*
keep if year>=1979 & year<=2015
bys year: egen fedmw = min(mw)
xtset statenum year
assert "`r(balanced)'" == "strongly balanced"
keep year statenum logmw L*mw F*mw
egen state_order = group(statenum)
compress

tempfile mwdata
save `mwdata'



use "${data}state_panels_cents_balanced_add_QJE.dta", clear

		merge m:1 statenum quarterdate using `qcew', assert(3) nogenerate
		replace count = count * multiplier
		replace countall = countall * multiplier
		replace teencount = teencount * multiplier		
		replace teencountall = teencountall * multiplier



gen wagebindollar = floor(wagebin/100)
drop wagebins
rename wagebindollar wagebins

		************************************************************************
		**************          Ones that should be summed       ***************
		************************************************************************

		preserve
			collapse (sum) teencount count, by(statenum quarterdate wagebins)
			tempfile tempsum
			save `tempsum'
		restore

		************************************************************************
		******        Ones that are already at statenumXquarterdate   **********
		************************************************************************

		preserve
			egen tagger=tag(statenum quarterdate)
			keep if tagger==1
			keep statenum quarterdate countall population year teencountall teenpop
			tempfile temptag
			save `temptag'
		restore


		************************************************************************
		*********    Merge ave's and sum's      ********************************
		************************************************************************

		use `tempsum', clear

		reshape wide teencount count , i(statenum quarterdate) j(wagebin)
		merge 1:1 statenum quarterdate using `temptag', nogenerate assert(3)




********************************************************************************
******************       Replicating previous results     **********************
********************************************************************************


ds count* teen*
collapse (mean) population `r(varlist)'  ,by(statenum year)
merge 1:1 statenum year using `mwdata'

gen MW_increasers= 0
foreach ll of local temploc{
replace MW_increasers = 1 if statenum == `ll'
}

gen MW_increasers2= 0
foreach ll of local temploc2{
replace MW_increasers2 = 1 if statenum == `ll'
}

rename population totalpopulation
	forval j = 1/30 {
	g lncount`j' = ln(count`j')
	g percap`j' = count`j'/totalpopulation

}
*

g epop    = countall/totalpopulation
g teenepop = teencountall/teenpop
xtset statenum year
gen Depop = D.epop
sum Depop
gen error_term = rnormal(r(mean), r(sd))
gen base_epop = epop if year==1992
bys statenum (year): egen sim_epop = max(base_epop) if year>=1992
replace sim_epop = epop if year<1992
gen sim_epop4 = sim_epop
replace sim_epop4 = L.sim_epop4 * 0 if year>=1993
gen sim_epop5 = epop - sim_epop4


g cleansample = 1
replace cleansample = 0 if year >=1994 & year <=1995

egen avetotalpopulation = mean(totalpopulation) , by(statenum)
egen aveteenpop = mean(teenpop) , by(statenum)



********************************************************************************
* ADDITIONAL CONTROLS
********************************************************************************
* Industry/occ shares
* incorporate base period routine occupation data
merge m:1 statenum using "${data}state_rocc_scores.dta", assert(3) nogenerate
* incorporate base period industry shares
merge m:1 statenum using "${data}state_majind_shares.dta", assert(3) nogenerate

* PVI: this code is copied directly from cautionary_cont_bunch_motivation_annual_newestimates_pvi.do
merge m:1 statenum using "${data}pvi_7616.dta",assert(3) nogenerate
gen _pvi_cont= .
forval num = 1976(4)2016{
	replace _pvi_cont = pvi`num' if year==`num'
}
bys statenum: ipolate _pvi_cont year, gen(pvi_cont)


xtset statenum year
gen Dpvi_cont = D.pvi_cont




*Note that we do not use most of these either, yet I keep this global in case it is needed.
gen byte one = 1

global a1  i.statenum i.year  // TWFE
global a2  i.statenum##c.year i.year  // State trends
global a321 i.statenum i.year i.year#(c.indmajor_* c.occmajor_*)
global a611 i.statenum i.year i.one#c.pvi_cont

global fda1 i.year
global fda2 i.statenum i.year
global fda321 i.year i.year#(c.indmajor_* c.occmajor_*)
global fda611 i.year i.one#c.Dpvi_cont


********************************************************************************
**************       Define Treatments         *********************************
********************************************************************************
*To keep all as flexible as possible, I am adding lincom too.

global treat   		F2logmw F1logmw logmw L1logmw L2logmw L3logmw L4logmw
global lincom0    	logmw
global lincom1    	logmw + L1logmw
global lincom2    	logmw + L1logmw + L2logmw
global lincom3    	logmw + L1logmw + L2logmw + L3logmw
global lincom4    	logmw + L1logmw + L2logmw + L3logmw + L4logmw
global lincomagg ((${lincom0} + ${lincom1} + ${lincom2} + ${lincom3} + ${lincom4}) * 1/5)
global lincomm2    	-F1logmw
global lincomm3   	-F1logmw-F2logmw
global lincomm1   	0



global treat_FD   			D.F2logmw D.F1logmw D.logmw D.L1logmw D.L2logmw D.L3logmw D.L4logmw

global lincom0_FD D.logmw
global lincom1_FD D.logmw + D.L1logmw
global lincom2_FD D.logmw + D.L1logmw + D.L2logmw
global lincom3_FD D.logmw + D.L1logmw + D.L2logmw + D.L3logmw
global lincom4_FD D.logmw + D.L1logmw + D.L2logmw + D.L3logmw + D.L4logmw
global lincomagg_FD ((${lincom0_FD} + ${lincom1_FD} + ${lincom2_FD} + ${lincom3_FD} + ${lincom4_FD}) * 1/5)

global lincomm2_FD    	-D.F1logmw
global lincomm3_FD   	-D.F1logmw -D.F2logmw
global lincomm1_FD   	0





global label1 TWFE
global grlabel1 TWFE
global label2 ST
global grlabel2 ST



********************************************************************************
***************       The Regressions ******************************************
********************************************************************************



gen all=1
xtset statenum year

gen teenaltweight = ((teenpop * L.teenpop)/(teenpop + L.teenpop))
gen altweight = ((totalpopulation * L.totalpopulation)/(totalpopulation + L.totalpopulation))


tempfile temptemp
save `temptemp', replace

local regsample ""
local graphname "o"
global fdweight1 [aw=altweight]
global weight1 [aw=avetotalpopulation]
global weight0 [aw=all]
global teenfdweight1 [aw=teenaltweight]
global teenweight1 [aw=aveteenpop]
local ylabel `"ylabel(-0.3 "-0.3" -0.2 "-0.2" -0.1 "-0.1" 0 "0" 0.1 "0.1" , labsize(medium))"'

local lags 4
local leads 3

local b 1979
foreach w in 1  {
	foreach s in 1 2 321 611 {
		sum epop if  year>=`b' & cleansample==1  ${weight`w'}
		local denominator = r(mean)
		sum teenepop if  year>=`b' & cleansample==1  ${teenweight`w'}
		local teendenominator = r(mean)
		
		sum teenepop if  year>=`b' & cleansample==1  ${teenfdweight`w'}
		local FDteendenominator = r(mean)
		di "`FDteendenominator'"
		use `temptemp', clear

		*TWFE
		qui reghdfe epop $treat  if  year>=`b' & cleansample==1 ${weight`w'} , absorb(${a`s'}) cluster(statenum)
		local dof = e(df_r)
		lincomestadd (${lincomagg})/`denominator', statname(ests)
		eststo actual_TWFE`s'

		
		qui reghdfe teenepop $treat  if  year>=`b' & cleansample==1 ${teenweight`w'} , absorb(${a`s'}) cluster(statenum)
		local dof = e(df_r)
		lincomestadd (${lincomagg})/`teendenominator', statname(ests)
		eststo teenactual_TWFE`s'
		
		
		*FD
		reghdfe D.epop ${treat_FD} if  year>=`b' & cleansample==1   ${fdweight`w'}, a(${fda`s'}) cluster(statenum)
		lincomestadd (${lincomagg_FD})/`denominator', statname(ests)
		
		local dof = e(df_r)
		estadd local period "1979-2016"
		tab	statenum if e(sample), nofreq
		local temp = r(r)
		estadd local states "`temp'"
		estadd local outcome "Actual epop"
		
		eststo actual_FD`s'

		reghdfe D.teenepop ${treat_FD} if  year>=`b' & cleansample==1   ${teenfdweight`w'}, a(${fda`s'}) cluster(statenum)
		lincomestadd (${lincomagg_FD})/`FDteendenominator', statname(ests)
		local dof = e(df_r)
		estadd local period "1979-2016"
		tab	statenum if e(sample), nofreq
		local temp = r(r)
		estadd local states "`temp'"
		estadd local outcome "Actual epop"
		
		eststo teenactual_FD`s'

		*post-1992
		*TWFE
		reghdfe epop $treat  if  year>=1993 & cleansample==1  ${weight`w'} , absorb(${a`s'}) cluster(statenum)
		local dof = e(df_r)
		lincomestadd (${lincomagg})/`denominator', statname(ests)
		eststo post1992_TWFE`s'

		reghdfe teenepop $treat  if  year>=1993 & cleansample==1  ${teenweight`w'} , absorb(${a`s'}) cluster(statenum)
		local dof = e(df_r)
		lincomestadd (${lincomagg})/`teendenominator', statname(ests)
		eststo teenpost1992_TWFE`s'
		
		
		*FD
		reghdfe D.epop ${treat_FD} if  year>=1993 & cleansample==1  ${fdweight`w'}, a(${fda`s'}) cluster(statenum)
		lincomestadd (${lincomagg_FD})/`denominator', statname(ests)
		local dof = e(df_r)
		estadd local period "1993-2016"
		tab	statenum if e(sample), nofreq
		local temp = r(r)
		estadd local states "`temp'"
		estadd local outcome "Actual epop"
		
		eststo post1992_FD`s'

		reghdfe D.teenepop ${treat_FD} if  year>=1993 & cleansample==1  ${teenfdweight`w'}, a(${fda`s'}) cluster(statenum)
		lincomestadd (${lincomagg_FD})/`teendenominator', statname(ests)
		local dof = e(df_r)
		estadd local period "1993-2016"
		tab	statenum if e(sample), nofreq
		local temp = r(r)
		estadd local states "`temp'"
		estadd local outcome "Actual epop"
		
		eststo teenpost1992_FD`s'
		
		
		}
}

do "${dofiles}Appendix_Tables_G3_G4_G7_add.do"


esttab actual_TWFE1 actual_TWFE2 actual_TWFE321 post1992_TWFE1 ///
using "${tables}TableG3.tex", replace ///
stats(blankspace blankspace estsb estsse  blankspace N, labels("\multicolumn{1}{l}{Panel A: FE}" "\cmidrule{1-1}" "Emp. elas. wrt MW" " " "Number of observations" " ")) ///
mgroups("Baseline" "\specialcell{State-specific \\ linear trends}" "\specialcell{Base period \\ occ. \& ind. shares}" "Post-1992 sample", pattern(1 1 1 1)) ///
mlabels("(1)" "(2)" "(3)" "(4)", prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(l{0.2cm}r{0.2cm}){@span})) ///
booktabs fragment nonotes nolines nomtitles nonumbers nogaps compress cells(none) nodepvar

esttab actual_EBC1 actual_EBC2 actual_EBC321 post1992_EBC1 ///
using "${tables}TableG3.tex", append ///
stats(blankspace blankspace estsb estsse N blankspace , labels("\multicolumn{1}{l}{Panel B: EB}" "\cmidrule{1-1}" "Emp. elas. wrt MW" " " "Number of observations " " " )) ///
booktabs fragment nonotes nolines nomtitles nonumbers nogaps compress cells(none) nodepvar

esttab actual_FD1 actual_FD2 actual_FD321 post1992_FD1  ///
using "${tables}TableG3.tex", append ///
stats(blankspace blankspace estsb estsse blankspace  states period N, labels("\multicolumn{1}{l}{Panel C: FD}" "\cmidrule{1-1}" "Emp. elas. wrt MW" " " " " "Number of states" "Period estimated" "Number of observations")) ///
booktabs fragment nonotes nolines nomtitles nonumbers nogaps compress cells(none) nodepvar substitute(_\ \)



esttab teenactual_TWFE1 teenactual_TWFE2  teenpost1992_TWFE1 teenpost1992_TWFE2 ///
using "${tables}TableG7.tex", replace ///
stats(blankspace blankspace estsb estsse  N blankspace, labels("\multicolumn{1}{l}{Panel A: FE}" "\cmidrule{1-1}" "Emp. elas. wrt MW" " " "Number of observations " " ")) ///
mgroups("Baseline" "\specialcell{State-specific \\ linear trends}" "Post-1992 sample"  "Post-1992 sample \& \\ State-specific \\ linear trends", pattern(1 1 1 1)) ///
mlabels("(1)" "(2)" "(3)" "(4)", prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(l{0.2cm}r{0.2cm}){@span})) ///
booktabs fragment nonotes nolines nomtitles nonumbers nogaps compress cells(none) nodepvar

esttab teenactual_EBC1 teenactual_EBC2  teenpost1992_EBC1 teenpost1992_EBC2 ///
using "${tables}TableG7.tex", append ///
stats(blankspace blankspace estsb estsse N blankspace , labels("\multicolumn{1}{l}{Panel B: EB}" "\cmidrule{1-1}" "Emp. elas. wrt MW" " " "Number of observations " " " )) ///
booktabs fragment nonotes nolines nomtitles nonumbers nogaps compress cells(none) nodepvar

esttab teenactual_FD1 teenactual_FD2  teenpost1992_FD1  teenpost1992_FD2  ///
using "${tables}TableG7.tex", append ///
stats(blankspace blankspace estsb estsse blankspace  states period N, labels("\multicolumn{1}{l}{Panel C: FD}" "\cmidrule{1-1}" "Emp. elas. wrt MW" " " " " "Number of states" "Period estimated" "Number of observations")) ///
booktabs fragment nonotes nolines nomtitles nonumbers nogaps compress cells(none) nodepvar substitute(_\ \)



esttab actual_TWFE1 actual_TWFE611 ///
using "${tables}TableG4.tex", replace ///
stats(blankspace blankspace estsb estsse N  blankspace, labels("\multicolumn{1}{l}{Panel A: FE}" "\cmidrule{1-1}" "Emp. elas. wrt MW" " " "Number of observations " " ")) ///
mgroups("Baseline" "\specialcell{With continuous \\ PVI control}", pattern(1 1)) ///
mlabels("(1)" "(2)", prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(l{0.2cm}r{0.2cm}){@span})) ///
booktabs fragment nonotes nolines nomtitles nonumbers nogaps compress cells(none) nodepvar

esttab actual_EBC1 actual_EBC611  ///
using "${tables}TableG4.tex", append ///
stats(blankspace blankspace estsb estsse N blankspace , labels("\multicolumn{1}{l}{Panel B: EB}" "\cmidrule{1-1}" "Emp. elas. wrt MW" " " "Number of observations " " ")) ///
booktabs fragment nonotes nolines nomtitles nonumbers nogaps compress cells(none) nodepvar

esttab actual_FD1 actual_FD611  ///
using "${tables}TableG4.tex", append ///
stats(blankspace blankspace estsb estsse blankspace states period N, labels("\multicolumn{1}{l}{Panel C: FD}" "\cmidrule{1-1}" "Emp. elas. wrt MW" " " " " "Number of states" "Period estimated" "Number of observations")) ///
booktabs fragment nonotes nolines nomtitles nonumbers nogaps compress cells(none) nodepvar substitute(_\ \)
