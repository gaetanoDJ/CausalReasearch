
*This do file is created by DC on 03/25/2016

clear

capture program drop lincomestadd
program define lincomestadd
syntax anything, statname(name)

lincom `anything'
local pvalue = 2*ttail(r(df), abs(r(estimate)/r(se)))
assert `pvalue' ~= .

local b : di  %04.3f `r(estimate)'
local se : di %04.3f `r(se)'

local stars ""
if `pvalue' < 0.10 local stars *
if `pvalue' < 0.05 local stars **
if `pvalue' < 0.01 local stars ***
local bstring `b'`stars'
local sestring (`se')

estadd local `statname'b "`bstring'"
estadd local `statname'se "`sestring'"

end



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
*bys statenum: egen _temp2 =  max(statetreat) if year<1996
*bys statenum: egen temp2 =  max(_temp2) 
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
		
		

gen wagebindollar = floor(wagebin/100)
drop wagebins
rename wagebindollar wagebins

		************************************************************************
		**************          Ones that should be summed       ***************
		************************************************************************

		preserve 
			collapse (sum)  count, by(statenum quarterdate wagebins)
			tempfile tempsum
			save `tempsum'
		restore

		************************************************************************
		******        Ones that are already at statenumXquarterdate   **********
		************************************************************************
		
		preserve
			egen tagger=tag(statenum quarterdate)
			keep if tagger==1
			keep statenum quarterdate countall population year 
			tempfile temptag
			save `temptag'
		restore
		
		
		************************************************************************
		*********    Merge ave's and sum's      ********************************
		************************************************************************
		
		use `tempsum', clear

		reshape wide  count , i(statenum quarterdate) j(wagebin)
		merge 1:1 statenum quarterdate using `temptag', nogenerate assert(3)


		

********************************************************************************
******************       Replicating previous results     **********************
********************************************************************************


ds count*
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
xtset statenum year
gen base_epop = epop if year==1992
bys statenum (year): egen sim_epop = max(base_epop) if year>=1992
replace sim_epop = epop if year<1992
gen sim_epop4 = sim_epop 
replace sim_epop4 = L.sim_epop4 * 0 if year>=1993
gen sim_epop5 = epop - sim_epop4


g cleansample = 1
replace cleansample = 0 if year >=1994 & year <=1995

egen avetotalpopulation = mean(totalpopulation) , by(statenum)


*Note that we do not use most of these either, yet I keep this global in case it is needed.

global a1  i.statenum i.year  // TWFE

global fda1 i.year

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


global lincom0_inter D.logmw - D.F1logmw
global lincom1_inter D.L1logmw - D.F1logmw
global lincom2_inter D.L2logmw - D.F1logmw
global lincom3_inter D.L3logmw  - D.F1logmw
global lincom4_inter D.L4logmw  - D.F1logmw

global lincomm2_inter    	D.F2logmw -D.F1logmw 
global lincomm3_inter   	-D.F1logmw 
global lincomm1_inter   	0
global lincomagg_inter ((${lincom0_inter} + ${lincom1_inter} + ${lincom2_inter} + ${lincom3_inter} + ${lincom4_inter}) * 1/5)


	
global label1 TWFE
global grlabel1 TWFE
global label2 ST
global grlabel2 ST



********************************************************************************
***************       The Regressions ******************************************
********************************************************************************		
		


gen all=1
xtset statenum year

tempfile temptemp
save `temptemp', replace	

local regsample ""
local graphname "o"
global weight1 [aw=avetotalpopulation]
local ylabel `"ylabel(-0.3 "-0.3" -0.2 "-0.2" -0.1 "-0.1" 0 "0" 0.1 "0.1" , labsize(medium))"' 

local lags 4
local leads 3

local b 1979
foreach w in 1  {
		foreach s in 1 {
		sum epop if  year>=`b' & cleansample==1  ${weight`w'}
		local denominator = r(mean)
		use `temptemp', clear
*Column 1
		*TWFE
			qui reghdfe epop $treat  if  year>=`b' & cleansample==1 ${weight`w'} , absorb(${a`s'}) cluster(statenum)
			local dof = e(df_r)
			lincomestadd (${lincomagg})/`denominator', statname(ests)
			eststo actual_TWFE
			
			
			*FD
			reghdfe D.epop ${treat_FD} if  year>=`b' & cleansample==1   ${weight`w'}, a( ${fda`s'}  ) cluster(statenum)
			lincomestadd (${lincomagg_FD})/`denominator', statname(ests)
			local dof = e(df_r)
			eststo actual_FD
			
*Column 2			
			*TWFE
			reghdfe epop $treat  if  year>=1993 & cleansample==1  ${weight`w'} , absorb(${a`s'}) cluster(statenum)
			local dof = e(df_r)
			lincomestadd (${lincomagg})/`denominator', statname(ests)
			eststo post1990_TWFE
			
			
			*FD
			reghdfe D.epop ${treat_FD} if  year>=1993 & cleansample==1  ${weight`w'}, a( ${fda`s'}  ) cluster(statenum)
			lincomestadd (${lincomagg_FD})/`denominator', statname(ests)
			local dof = e(df_r)
			eststo post1990_FD
*Columns 3				
			*TWFE
			reghdfe epop $treat  if  year>=`b' & cleansample==1 & MW_increasers==0 ${weight`w'} , absorb(${a`s'}) cluster(statenum)
			local dof = e(df_r)
			lincomestadd (${lincomagg})/`denominator', statname(ests)
			eststo nonMWinc_TWFE
			
			
			*FD
			reghdfe D.epop ${treat_FD} if  year>=`b' & cleansample==1  & MW_increasers==0 ${weight`w'}, a( ${fda`s'}  ) cluster(statenum)
			lincomestadd (${lincomagg_FD})/`denominator', statname(ests)
			local dof = e(df_r)
			eststo nonMWinc_FD

*Column 4			
			*TWFE
			reghdfe sim_epop4 $treat  if  year>=1979 & cleansample==1 & MW_increasers==0 ${weight`w'} , absorb(${a`s'}) cluster(statenum)
			local dof = e(df_r)
			lincomestadd (${lincomagg})/`denominator', statname(ests)
			estadd local rho "0"
			eststo sim3_4_TWFE
			
			
			*FD
			reghdfe D.sim_epop4 ${treat_FD} if  year>=1979 & cleansample==1  & MW_increasers==0 ${weight`w'}, a( ${fda`s'}  ) cluster(statenum)
			lincomestadd (${lincomagg_FD})/`denominator', statname(ests)
			local dof = e(df_r)
			estadd local rho "0"
			eststo sim3_4_FD


			
*Column 5			
			*TWFE
			reghdfe sim_epop5 $treat  if  year>=1979 & cleansample==1 & MW_increasers==0 ${weight`w'} , absorb(${a`s'}) cluster(statenum)
			local dof = e(df_r)
			lincomestadd (${lincomagg})/`denominator', statname(ests)
			estadd local rho "0"
			eststo sim3_anti4_TWFE
			
			
			*FD
			reghdfe D.sim_epop5 ${treat_FD} if  year>=1979 & cleansample==1  & MW_increasers==0 ${weight`w'}, a( ${fda`s'}  ) cluster(statenum)
			lincomestadd (${lincomagg_FD})/`denominator', statname(ests)
			local dof = e(df_r)
			estadd local rho "0"
			eststo sim3_anti4_FD
			
			
			
*Column 6			
			*TWFE
			reghdfe sim_epop4 $treat  if  year>=1979 & cleansample==1 & MW_increasers2==0 ${weight`w'} , absorb(${a`s'}) cluster(statenum)
			local dof = e(df_r)
			lincomestadd (${lincomagg})/`denominator', statname(ests)
			eststo sim3_5_TWFE
			
			
			*FD
			reghdfe D.sim_epop4 ${treat_FD} if  year>=1979 & cleansample==1  & MW_increasers2==0 ${weight`w'}, a( ${fda`s'}  ) cluster(statenum)
			lincomestadd (${lincomagg_FD})/`denominator', statname(ests)
			local dof = e(df_r)
			estadd local rho "0"
			eststo sim3_5_FD

	
			
		}
	}	
	*		
*


esttab actual_TWFE  post1990_TWFE  nonMWinc_TWFE sim3_4_TWFE sim3_anti4_TWFE sim3_5_TWFE  ///
using "${tables}TableG5.tex", replace ///
stats(blankspace blankspace estsb estsse  blankspace, labels("\multicolumn{1}{l}{Panel A: FE}" "\cmidrule{1-1}" "Emp. elas. wrt MW" " " " ")) ///
mgroups("Actual" "Post 1993 sample"  "\multicolumn{3}{c}{Excl. states with pre-1992 events}" "\bigcell{c}{Excl. states with \\ pre-1996 events}", pattern(1 1 1 1) end(\cmidrule(l{0.2cm}r{0.2cm}){2-2}\cmidrule(l{0.2cm}r{0.2cm}){3-3}\cmidrule(l{0.2cm}r{0.2cm}){4-6}\cmidrule(l{0.2cm}r{0.2cm}){7-7}) )  ///
mlabels("(1)" "(2)" "(3)" "(4)" "(5)" "(6)" "(7)" , prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(l{0.2cm}r{0.2cm}){@span})) ///
booktabs fragment nonotes nolines nomtitles nonumbers nogaps compress cells(none) nodepvar substitute("&         &         &         &         &         &         " "")

esttab actual_FD post1990_FD nonMWinc_FD sim3_4_FD sim3_anti4_FD sim3_5_FD  ///
using "${tables}TableG5.tex", append ///
stats(blankspace blankspace estsb estsse blankspace , labels("\multicolumn{1}{l}{Panel B: FD}" "\cmidrule{1-1}" "Emp. elas. wrt MW" " " " " )) ///
booktabs fragment nonotes nolines nomtitles nonumbers nogaps compress cells(none) nodepvar substitute("&         &         &         &         &         &         " "")

esttab actual_EBC post1990_EBC nonMWinc_EBC sim3_4_EBC sim3_anti4_EBC sim3_5_EBC  ///
using "${tables}TableG5.tex", append ///
stats(N states period  blankspace outcome, labels("Observations" "Number of states" "Period estimated" " " "Outcome variable")) ///
booktabs fragment nonotes nolines nomtitles nonumbers nogaps compress cells(none) nodepvar substitute("&         &         &         &         &         &         " "")

