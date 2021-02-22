
*This do file is created by DC on 03/25/2016
********************************************************************************
***********          Alternate Specifications of Jobs Dist.      ***************
********************************************************************************
*This specification puts  leads and lags. Starts from 1995 and only clean sample
*Overall sample is used.


clear 

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


xtset statenum year
forvalues index = 1/5 {
	foreach lagorlead in L F {
		gen `lagorlead'`index'logmw = log(`lagorlead'`index'.mw)
	}
}
gen logmw = log(mw)
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
			keep statenum quarterdate countall population year  teenpop
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

g limited_epop = percap1
forval j = 2/14 { 
	replace limited_epop = limited_epop  + percap`j'
}
*
gen upper_epop = epop - limited_epop
g cleansample = 1
replace cleansample = 0 if year >=1994 & year <=1995

egen avetotalpopulation = mean(totalpopulation) , by(statenum)


*Note that we do not use most of these either, yet I keep this global in case it is needed.

global a1  i.statenum i.year  // TWFE
global a2  i.statenum##c.year i.year  // State trends



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
global weight2 [aw=altweight]
global weight1 [aw=avetotalpopulation]
global weight0 [aw=all]
local ylabel `"ylabel(-0.3 "-0.3" -0.2 "-0.2" -0.1 "-0.1" 0 "0" 0.1 "0.1" , labsize(medium))"' 




local lags 4
local leads 3

use `temptemp', clear

foreach b in  1993 1979{
foreach w in 1  {
		foreach s in 1  {
		sum epop if  year>=`b' & cleansample==1  ${weight`w'}
		local denominator = r(mean)
		use `temptemp', clear
		*TWFE
			qui reghdfe epop $treat  if  year>=`b' & cleansample==1 ${weight`w'} , absorb(${a`s'}) cluster(statenum)
			local dof = e(df_r)
			lincom (${lincomagg})/`denominator'
			
			
			forval years = 0(1)`lags'{
				qui lincom (${lincom`years'})/`denominator'
				local lowb	= r(estimate) - invttail(`dof', 0.025)*r(se) 
				local highb	= r(estimate) + invttail(`dof', 0.025)*r(se) 

			if `years' == 0{
			mat ests = (`years', r(estimate), `lowb', `highb')
			}
			else{
			mat ests = (ests \ `years', r(estimate), `lowb', `highb')
			}
			}
						
			forval years = 1(1)`leads'{
				qui lincom (${lincomm`years'})/`denominator'
				local lowb	= r(estimate) - invttail(`dof', 0.025)*r(se) 
				local highb	= r(estimate) + invttail(`dof', 0.025)*r(se) 

			mat ests = (ests \ -`years' , r(estimate), `lowb', `highb')
			}
				
				
				
		*TWFE + LT
			qui reghdfe epop $treat  if  year>=`b' & cleansample==1 ${weight`w'} , absorb(${a2}) cluster(statenum)
			local dof = e(df_r)
			lincom (${lincomagg})/`denominator'
			
			
			forval years = 0(1)`lags'{
				qui lincom (${lincom`years'})/`denominator'
				local lowb	= r(estimate) - invttail(`dof', 0.025)*r(se) 
				local highb	= r(estimate) + invttail(`dof', 0.025)*r(se) 

			if `years' == 0{
			mat ests_lt = (`years', r(estimate), `lowb', `highb')
			}
			else{
			mat ests_lt = (ests_lt \ `years', r(estimate), `lowb', `highb')
			}
			}
						
			forval years = 1(1)`leads'{
				qui lincom (${lincomm`years'})/`denominator'
				local lowb	= r(estimate) - invttail(`dof', 0.025)*r(se) 
				local highb	= r(estimate) + invttail(`dof', 0.025)*r(se) 

			mat ests_lt = (ests_lt \ -`years' , r(estimate), `lowb', `highb')
			}
								
						
			
			*Figure
			svmat ests 
			rename ests1 time
			rename ests2 est
			rename ests3 low
			rename ests4 high
			
			
			sort time
			
			
		local title1979 FigureG1_a
		local title1993 FigureG6
		
				twoway ( connected est time if time!=. )  ///
				(rcap low  high time if time!=. ) , graphregion(color(white)) ///
				/*xlabel(-4 "{&le} -4" "-3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "{&ge} 5")*/ xlabel(-3 "{&le} -3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "{&ge} 4") ///
				xtitle("Event time in years") ytitle("Employment elasticity") ylabel(-0.3(0.1)0.2 0 "0" , format(%02.1f)) /*title("All wage bins")*/ /*legend(order(1 "Point estimate" 2 "95% CI")) */ legend(off)
				gr export "${figures}`title`b''.pdf", replace
				
				

				
			
		}
	}	
}
*		



local lags 4
local leads 3

local b 1979
foreach w in 1  {
		foreach s in 1  {
		sum epop if  year>=`b' & cleansample==1  ${weight`w'}
		local denominator = r(mean)
		use `temptemp', clear
		*TWFE, under
			qui reghdfe limited_epop $treat  if  year>=`b' & cleansample==1 ${weight`w'} , absorb(${a`s'}) cluster(statenum)
			local dof = e(df_r)
			lincom (${lincomagg})/`denominator'

			
			
			forval years = 0(1)`lags'{
				qui lincom (${lincom`years'})/`denominator'
				local lowb	= r(estimate) - invttail(`dof', 0.025)*r(se) 
				local highb	= r(estimate) + invttail(`dof', 0.025)*r(se) 

			if `years' == 0{
			mat ests = (`years', r(estimate), `lowb', `highb')
			}
			else{
			mat ests = (ests \ `years', r(estimate), `lowb', `highb')
			}
			}
						
			forval years = 1(1)`leads'{
				qui lincom (${lincomm`years'})/`denominator'
				local lowb	= r(estimate) - invttail(`dof', 0.025)*r(se) 
				local highb	= r(estimate) + invttail(`dof', 0.025)*r(se) 

			mat ests = (ests \ -`years' , r(estimate), `lowb', `highb')
			}
				
				
				
		*TWFE, above
			qui reghdfe upper_epop $treat  if  year>=`b' & cleansample==1 ${weight`w'} , absorb(${a`s'}) cluster(statenum)
			local dof = e(df_r)
			lincom (${lincomagg})/`denominator'
	
			
			forval years = 0(1)`lags'{
				qui lincom (${lincom`years'})/`denominator'
				local lowb	= r(estimate) - invttail(`dof', 0.025)*r(se) 
				local highb	= r(estimate) + invttail(`dof', 0.025)*r(se) 

			if `years' == 0{
			mat ests_above = (`years', r(estimate), `lowb', `highb')
			}
			else{
			mat ests_above = (ests_above \ `years', r(estimate), `lowb', `highb')
			}
			}
						
			forval years = 1(1)`leads'{
				qui lincom (${lincomm`years'})/`denominator'
				local lowb	= r(estimate) - invttail(`dof', 0.025)*r(se) 
				local highb	= r(estimate) + invttail(`dof', 0.025)*r(se) 

			mat ests_above = (ests_above \ -`years' , r(estimate), `lowb', `highb')
			}
								
				
		
					
			
			*Figure
			svmat ests 
			svmat ests_above
			rename ests1 time
			rename ests2 est
			rename ests3 low
			rename ests4 high
			
			foreach uporrel in _above {
			rename ests`uporrel'1 time_`uporrel'
			rename ests`uporrel'2 est_`uporrel'
			rename ests`uporrel'3 low_`uporrel'
			rename ests`uporrel'4 high_`uporrel'
			}
			
			sort time
			
				twoway ( connected est time if time!=. )  ///
				(rcap low  high time if time!=. ) , graphregion(color(white)) ///
				/*xlabel(-4 "{&le} -4" "-3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "{&ge} 5")*/ xlabel(-3 "{&le} -3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "{&ge} 4") ///
				xtitle("Event time in years") ytitle("Employment elasticity") ylabel(-0.3(0.1)0.2 0 "0" , format(%02.1f)) /*title("All wage bins")*/ /*legend(order(1 "Point estimate" 2 "95% CI")) */ legend(off)
				gr export "${figures}FigureG3_c.pdf", replace
				
			
				twoway ( connected est__above time__above if time__above!=. )  ///
				(rcap low__above  high__above time__above if time__above!=. ) , graphregion(color(white)) ///
				/*xlabel(-4 "{&le} -4" "-3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "{&ge} 5")*/ xlabel(-3 "{&le} -3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "{&ge} 4") ///
				xtitle("Event time in years") ytitle("Employment elasticity") ylabel(-0.3(0.1)0.2 0 "0" , format(%02.1f)) /*title("All wage bins")*/ /*legend(order(1 "Point estimate" 2 "95% CI")) */ legend(off)
				gr export "${figures}FigureG3_a.pdf", replace
				
				
			
		}
	}	
*		






