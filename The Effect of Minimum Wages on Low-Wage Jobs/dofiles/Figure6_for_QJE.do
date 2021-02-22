
*This do file is created by DC on 03/25/2016


clear all
set more off

********************************************************************************
**************    Reshaping the original data    *******************************
********************************************************************************

use "${data}qcew_multiplier.dta", clear
keep statenum quarterdate multiplier
assert multiplier!=0
tempfile qcew
save `qcew'




use "${data}VZmw_quarterly_lagsleads_1979_2016",clear
gen mw = exp(logmw)
gen year = year(dofq(quarterdate))
gen quarter = quarter(dofq(quarterdate))
collapse  mw, by(statenum year)
gen logmw = log(mw)

xtset statenum year
forvalues index = 1/4 {
	foreach lagorlead in L F {
		gen `lagorlead'`index'logmw = log(`lagorlead'`index'.mw)
	}
}
gen L0logmw = logmw
*
keep if year>=1979 & year<=2015
xtset statenum year
assert "`r(balanced)'" == "strongly balanced"
keep year statenum logmw L*mw F*mw

compress

tempfile mwdata
save `mwdata'

use "${data}state_panels_cents_balanced_add_QJE.dta", clear

		merge m:1 statenum quarterdate using `qcew', assert(3) nogenerate
		replace count = count * multiplier		
		replace countall = countall * multiplier
		
		

cap drop *impute*
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
	
rename population totalpopulation	
	forval j = 1/30 { 
	g lncount`j' = ln(count`j')
	g percap`j' = count`j'/totalpopulation
	
}
*

g epop    = countall/totalpopulation




g cleansample = 1
replace cleansample = 0 if year >=1994 & year <=1995

egen avetotalpopulation = mean(totalpopulation) , by(statenum)


*Note that we do not use most of these either, yet I keep this global in case it is needed.

global a1  i.statenum i.year  // TWFE


********************************************************************************
**************       Define Treatments         *********************************
********************************************************************************
*To keep all as flexible as possible, I am adding lincom too.

global treat   		F2logmw F1logmw logmw L1logmw L2logmw L3logmw L4logmw

		global lincom0_TWFE    	logmw
		global lincom1_TWFE    	logmw + L1logmw
		global lincom2_TWFE    	logmw + L1logmw + L2logmw
		global lincom3_TWFE    	logmw + L1logmw + L2logmw + L3logmw
		global lincom4_TWFE    	logmw + L1logmw + L2logmw + L3logmw + L4logmw

		global lincom12 ((${lincom0_TWFE} + ${lincom1_TWFE} + ${lincom2_TWFE} + ${lincom3_TWFE} + ${lincom4_TWFE}) * 1/5)
	
global label1 TWFE
global grlabel1 TWFE



********************************************************************************
***************       The Regressions ******************************************
********************************************************************************		
		


gen all=1
xtset statenum year

tempfile temptemp
save `temptemp', replace	

local graphname "o"
global weight2 [aw=altweight]
global weight1 [aw=avetotalpopulation]
global weight0 [aw=all]
local ylabel `"ylabel(-0.2 "-0.2" -0.1 "-0.1" 0 "0" 0.1 "0.1" , labsize(medium))"' 


*To get semi elasticity, get rid of denominator.

foreach b in    1979   {
	foreach w in 1  0{
		
		sum epop if  year>=`b' & cleansample==1  ${weight`w'}
		local denominator = r(mean)
		foreach s in      1   {
		use `temptemp', clear
			forval k=1(1)30{

			 reghdfe percap`k' $treat  if  year>=`b' & cleansample==1  ${weight`w'} , absorb(${a`s'}) cluster(statenum)
				
				local dof = e(df_r)
				
				foreach q in 12 {
				lincom (${lincom`q'})/`denominator'
				local lowb	= r(estimate) - invttail(`dof', 0.025)*r(se) 
				local highb	= r(estimate) + invttail(`dof', 0.025)*r(se) 
				
				
				if `k' == 1 {
							mat countmat`q' = [`k', r(estimate), `lowb' , `highb']
				} // end of if
				else {
					mat countmat`q' = [countmat`q' \ [`k', r(estimate), `lowb' , `highb' ]]
				} // end of else
			} // end of q 
			} // end of k
			foreach q in 12 {
				forval j = 31/34 {   // 32 to 31: couner changed here; but 34 kept same
					mat countmat`q' = [countmat`q' \ [`j', .,.,.] ]
				} // end of j
			} // end of q

			qui reghdfe epop $treat  if  year>=`b' & cleansample==1 ${weight`w'} , absorb(${a`s'}) cluster(statenum)
			local dof = e(df_r)

			foreach q in 12 {	
				lincom (${lincom`q'})/`denominator'
				scalar ba_`w' = r(estimate)
				scalar sea_`w'= r(se)
				scalar ta_`w'= r(estimate)/r(se)				
				scalar pa_`w' = ttail(r(df), r(estimate)/r(se))
				local lowb	= r(estimate) - invttail(`dof', 0.025)*r(se) 
				local highb	= r(estimate) + invttail(`dof', 0.025)*r(se) 
				mat countmat`q' = [countmat`q' \ [35, r(estimate), `lowb' , `highb' ]]

				svmat countmat`q'
				cap drop wagebin est low high totest
				rename countmat`q'1 wagebin
				rename countmat`q'2 est
				rename countmat`q'3 low
				rename countmat`q'4 high

 				g totest = est if _n==1
				replace totest = totest[_n-1] + est if _n>1
				
				local ba: di %04.3f `=ba_`w''
				local se: di %04.3f `=sea_`w''
				if `w' == 1{
				twoway ( bar est wagebin if wagebin<32, fcolor(ltblue) ) ( bar est wagebin if wagebin==35, fcolor(purple) lcolor(purple) ) ///
				(rcap low  high wagebin if wagebin<32, lcolor(green) lwidth(thick) ) (line totest wagebin, lpat(dot) lwidth(thick) lcolor(purple)) , ///
				xlabel( 5 "$5"  10 "$10" 15 "$15" 20 "$20" 25 "$25" 31 ">$30" 35 "Total", labsize(medium)) ///
				ytitle("Employment elasticity") ///
				`ylabel' ///
				scheme(s1color) xtitle("Wage") legend(off) ///
				text(-0.18 8  "Employment elas. = `ba' (`se')" , box  fcolor(white) margin(0.2 0.2 1 1) justification(left) ) 
				graph export "${figures}Figure6.pdf", replace 
				}
				if `w' == 0{
				twoway ( bar est wagebin if wagebin<32, fcolor(ltblue) ) ( bar est wagebin if wagebin==35, fcolor(purple) lcolor(purple) ) ///
				(rcap low  high wagebin if wagebin<32, lcolor(green) lwidth(thick) ) (line totest wagebin, lpat(dot) lwidth(thick) lcolor(purple)) , ///
				xlabel( 5 "$5"  10 "$10" 15 "$15" 20 "$20" 25 "$25" 31 ">$30" 35 "Total", labsize(medium)) ///
				ytitle("Employment elasticity") ///
				`ylabel' ///
				scheme(s1color) xtitle("Wage") legend(off) ///
				text(-0.18 8  "Employment elas. = `ba' (`se')" , box  fcolor(white) margin(0.2 0.2 1 1) justification(left) ) 
				graph export "${figures}FigureA11.pdf", replace 
				
				}
				

				
			}
		}
	}
}
*		

