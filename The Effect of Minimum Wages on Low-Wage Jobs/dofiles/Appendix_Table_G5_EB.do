
*This do file is created by DC on 03/25/2016
********************************************************************************
***********          Alternate Specifications of Jobs Dist.      ***************
********************************************************************************


clear

use "${data}qcew_multiplier.dta", clear
keep statenum quarterdate multiplier
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



use "${data}state_panels_cents_balanced_add_QJE.dta", clear
		merge m:1 statenum quarterdate using `qcew', nogenerate assert(3)
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


merge 1:1 statenum quarterdate using "${data}VZmw_quarterly_lagsleads_1979_2016.dta"
assert _merge==3 if quarterdate!=227
drop _merge
merge 1:1 statenum quarterdate using "${data}eventclassification.dta", nogenerate assert(1 3)
replace overallcountgroup = 0 if overallcountgroup==.
assert overallcountgroup!=0 if alltreat==1 & toosmall==0

gen MW_increasers= 0
foreach ll of local temploc{
replace MW_increasers = 1 if statenum == `ll'
}
	
gen MW_increasers2= 0
foreach ll of local temploc2{
replace MW_increasers2 = 1 if statenum == `ll'
}
*	


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


cap drop _merge
merge m:1 quarterdate using `cpiquarter', nogenerate assert(3)


xtset statenum quarterdate	


g MW = exp(logmw) 
g DMW = D.MW
g MW_real = exp(logmw)/(cpi/100) 
g DMW_real = D.MW_real 

g MW_realM25 = MW_real - mod(MW_real,0.25)
g DMW_realM25 = D.MW_realM25
	
	
	
compress
rename population totalpopulation
forval j = 1/30 { 
	g lncount`j' = ln(count`j')
	g percap`j' = count`j'/totalpopulation
	
}
*

g epop    = countall/totalpopulation

g below_epop = percap1
forval j = 2/14 { 
	replace below_epop = below_epop  + percap`j'
}
*

gen upper_epop  = epop - below_epop


xtset statenum quarterdate

	cap drop treat 
	cap drop _treat
	g _treat_p=0
		replace _treat = 1 if overallcountgroup>0 & fedincrease!=1
	 g treat = ( _treat +  L._treat + L2._treat + L3._treat)
	cap drop _treat



cap drop  Dtreat
g Dtreat = D.treat

cap drop _cont
g _cont = 0
replace _cont = 1 if ($missedevents | toosmall==1) 
g tempcont = _cont + L._cont + L2._cont + L3._cont

local Tmax3 = 16 + 3
forval i = 0(1)`Tmax3'{
	if `i' == 0{
	g postcont = _cont
	}
	else {
	replace postcont = postcont + L`i'._cont
	sum postcont, meanonly
	assert r(mean)!=0
	}
}
cap drop _cont


cap drop _contf
cap drop postcontf

g _contf = 0
replace _contf = 1 if (fedincrease==1 &  overallcountgroup>0 )
g tempcontf = _contf + L._contf + L2._contf + L3._contf

local Tmax3 = 16 + 3
forval i = 0(1)`Tmax3'{
	if `i' == 0{
	g postcontf = _contf

	}
	else {
	replace postcontf = postcontf + L`i'._contf
	sum postcontf, meanonly
	assert r(mean)!=0
	}
}
cap drop _contf

g precontf = F4.tempcontf
g precont = F4.tempcont

replace precontf = 0 if precontf == .
replace precont = 0 if precont  == .


g earlycontf = F12.tempcontf + F8.tempcontf
g earlycont = F12.tempcont + F8.tempcont

replace earlycontf = 0 if earlycontf == .
replace earlycont = 0 if earlycont  == .



foreach j of numlist 4(4)16{
		cap drop F`j'treat
		cap drop L`j'treat
		
		g F`j'treat = F`j'.treat
 		g L`j'treat = L`j'.treat
		
		replace F`j'treat = 0 if F`j'treat ==.
		replace L`j'treat = 0 if L`j'treat ==.
		

	}
*

replace postcont = 0 if postcont == .
replace postcontf = 0 if postcontf == .


g cleansample = 1
replace cleansample = 0 if quarterdate >=136 & quarterdate <=142

egen avetotalpopulation = mean(totalpopulation) , by(statenum)


*Note that we do not use most of these either, yet I keep this global in case it is needed.

global a1  i.statenum i.quarterdate  // TWFE
global a2  i.statenum##c.quarterdate i.quarterdate  // TWFE



global treat   		F12treat F8treat F4treat treat  L4treat L8treat L12treat L16treat 

global minus (F4treat)
global lincomagg 	(treat-$minus)
forval i = 4(4)16{
global lincomagg "$lincomagg + (L`i'treat - $minus ) "
}
*

global label1 TWFE
global grlabel1 TWFE

global label2 ST
global grlabel2 ST



********************************************************************************
***************       The Regressions ******************************************
********************************************************************************		
		


gen all=1

gen base_epop = epop if year==1992
bys statenum (year): egen sim_epop = max(base_epop) if year>=1992
xtset statenum quarterdate

replace sim_epop = epop if year<1992
gen sim_epop4 = sim_epop 
replace sim_epop4 = L.sim_epop4 * 0 if year>=1993
gen sim_epop5 = epop - sim_epop4



tempfile temptemp
save `temptemp', replace	


local regsample ""
local graphname "o"
global weight1 [aw=avetotalpopulation]
global weight0 [aw=all] 
local ylabel "ylabel( , labsize(medium))" 





foreach b in   1979   {
	foreach w in 1 {
		foreach s in      1   {
		use `temptemp', clear
			
				sum epop if  year>=`b' & cleansample==1  ${weight`w'},meanonly
				local epop = r(mean)
				sum DMW_real if fedincrease!=1 & overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`w'}
				local mwc = r(mean)
				sum MW_real if  F.fedincrease!=1 & F.overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`w'}
				local mw = r(mean)
				local mwpc = `mwc'/`mw'
				local mwpc2 : di  %04.3f `mwpc'

				global E = `epop'
					
				global C = 1/(`epop'*`mwpc')
				
				
			qui reghdfe `regsample'epop $treat if  year>=`b' & cleansample==1 ${weight`w'} , absorb(i.statenum i.quarterdate  postcont postcontf  precont precontf earlycont earlycontf ) cluster(statenum)
			sum quarterdate if e(sample)==1
			estadd local begquarter = r(min)
			estadd local endquarter = r(max)

			
			local dof = e(df_r)

			lincom (($lincomagg) * (1/5))*${C}
			
			scalar bst_`w'=r(estimate)
			scalar sest_`w'=r(se)
			scalar pst_`w' = ttail(r(df), r(estimate)/r(se))
			scalar N = e(N)
			
			qui reghdfe epop $treat if  year>=1993 & cleansample==1 ${weight`w'} , absorb(i.statenum i.quarterdate  postcont postcontf  precont precontf earlycont earlycontf ) cluster(statenum)
			sum quarterdate if e(sample)==1
			estadd local begquarter = r(min)
			estadd local endquarter = r(max)

			
			local dof = e(df_r)

			lincom (($lincomagg) * (1/5))*${C}

			scalar bst2_`w'=r(estimate)
			scalar sest2_`w'=r(se)
			scalar pst2_`w' = ttail(r(df), r(estimate)/r(se))
			scalar N2 = e(N)

			

			qui reghdfe epop $treat if  year>=`b' & MW_increasers==0 & cleansample==1 ${weight`w'} , absorb(i.statenum i.quarterdate  postcont postcontf  precont precontf earlycont earlycontf ) cluster(statenum)
			sum quarterdate if e(sample)==1
			estadd local begquarter = r(min)
			estadd local endquarter = r(max)

			
			local dof = e(df_r)

			lincom (($lincomagg) * (1/5))*${C}
			
			scalar bst3_`w'=r(estimate)
			scalar sest3_`w'=r(se)
			scalar pst3_`w' = ttail(r(df), r(estimate)/r(se))
			scalar N3 = e(N)

			qui reghdfe sim_epop4 $treat if  year>=`b' & cleansample==1 & MW_increasers==0  ${weight`w'}  , absorb(i.statenum i.quarterdate  postcont postcontf  precont precontf earlycont earlycontf ) cluster(statenum)
			sum quarterdate if e(sample)==1
			estadd local begquarter = r(min)
			estadd local endquarter = r(max)
			
			local dof = e(df_r)

			lincom (($lincomagg) * (1/5))*${C}
			
			scalar bst4_`w'=r(estimate)
			scalar sest4_`w'=r(se)
			scalar pst4_`w' = ttail(r(df), r(estimate)/r(se))
			scalar N4 = e(N)

			
			qui reghdfe sim_epop5 $treat if  year>=`b' & cleansample==1 & MW_increasers==0  ${weight`w'} , absorb(i.statenum i.quarterdate  postcont postcontf  precont precontf earlycont earlycontf ) cluster(statenum)
			sum quarterdate if e(sample)==1
			estadd local begquarter = r(min)
			estadd local endquarter = r(max)

			
			local dof = e(df_r)

			lincom (($lincomagg) * (1/5))*${C}
			scalar bst5_`w'=r(estimate)
			scalar sest5_`w'=r(se)
			scalar pst5_`w' = ttail(r(df), r(estimate)/r(se))
			scalar N5 = e(N)

			qui reghdfe sim_epop4 $treat if  year>=`b' & cleansample==1 & MW_increasers2==0  ${weight`w'}  , absorb(i.statenum i.quarterdate  postcont postcontf  precont precontf earlycont earlycontf ) cluster(statenum)
			sum quarterdate if e(sample)==1
			estadd local begquarter = r(min)
			estadd local endquarter = r(max)

			local dof = e(df_r)

			lincom (($lincomagg) * (1/5))*${C}
			
			scalar bst6_`w'=r(estimate)
			scalar sest6_`w'=r(se)
			scalar pst6_`w' = ttail(r(df), r(estimate)/r(se))
			scalar N6 = e(N)

			
			

			
			}
		}
	}
*		



