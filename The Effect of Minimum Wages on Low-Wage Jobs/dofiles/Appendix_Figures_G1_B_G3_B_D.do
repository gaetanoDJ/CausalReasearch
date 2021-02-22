
*This do file is created by DC on 03/25/2016
********************************************************************************
***********          Alternate Specifications of Jobs Dist.      ***************
********************************************************************************


********************************************************************************
**************    Reshaping the original data    *******************************
********************************************************************************



use "${data}qcew_multiplier.dta", clear
keep statenum quarterdate multiplier
tempfile qcew
save `qcew'



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


merge 1:1 statenum quarterdate using "${data}VZmw_quarterly_lagsleads_1979_2016.dta"
assert _merge==3 if quarterdate!=227
drop _merge
merge 1:1 statenum quarterdate using "${data}eventclassification.dta", nogenerate assert(1 3)
replace overallcountgroup = 0 if overallcountgroup==.
assert overallcountgroup!=0 if alltreat==1 & toosmall==0



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


********************************************************************************
**************       Define Treatments         *********************************
********************************************************************************
*To keep all as flexible as possible, I am adding lincom too.

global treat   		F12treat F8treat F4treat treat  L4treat L8treat L12treat L16treat 

*global minus 0
global minus (F4treat)
global lincomagg 	(treat-$minus)
forval i = 4(4)16{
global lincomagg "$lincomagg + (L`i'treat - $minus ) "
}
*

global label1 TWFE
global grlabel1 TWFE

global label2 ST



********************************************************************************
***************       The Regressions ******************************************
********************************************************************************		
		


gen all=1

gen logMW = log(MW_real)

tempfile temptemp
save `temptemp', replace	


local regsample ""
local graphname "o"
global weight1 [aw=avetotalpopulation]
global weight0 [aw=all] 
local ylabel "ylabel( , labsize(medium))" 





global lincomm12 F12treat - F4treat
global lincomm8 F8treat - F4treat
global lincomm4 F4treat - F4treat
global lincom0 treat - F4treat
global lincom4 L4treat - F4treat
global lincom8 L8treat - F4treat
global lincom12 L12treat - F4treat
global lincom16 L16treat - F4treat




local lags 4
local leads 3
global B  = .0864819457562104
local b 1979
foreach w in 1  {
		sum epop if  year>=`b' & cleansample==1  ${weight`w'}
		local denominator = r(mean)
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
				*global C = 1/( `epop'*${B} )
				
			qui reghdfe epop $treat if  year>=`b' & cleansample==1 ${weight`w'} , absorb(i.statenum i.quarterdate  postcont postcontf  precont precontf earlycont earlycontf ) cluster(statenum)
			
			local dof = e(df_r)

			forval years = 0(1)`lags'{
			local year = `years' * 4
				qui lincom (${lincom`year'})*${C}
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
				local year = `years' * 4
				qui lincom (${lincomm`year'})*${C}
				local lowb	= r(estimate) - invttail(`dof', 0.025)*r(se) 
				local highb	= r(estimate) + invttail(`dof', 0.025)*r(se) 

			mat ests = (ests \ -`years' , r(estimate), `lowb', `highb')
			}
					
			*Relevant region		

			qui reghdfe below_epop $treat if  year>=`b' & cleansample==1 ${weight`w'} , absorb(i.statenum i.quarterdate  postcont postcontf  precont precontf earlycont earlycontf ) cluster(statenum)
			
			local dof = e(df_r)

			forval years = 0(1)`lags'{
			local year = `years' * 4
				qui lincom (${lincom`year'})*${C}
				local lowb	= r(estimate) - invttail(`dof', 0.025)*r(se) 
				local highb	= r(estimate) + invttail(`dof', 0.025)*r(se) 

			if `years' == 0{
			mat ests_rel = (`years', r(estimate), `lowb', `highb')
			}
			else{
			mat ests_rel = (ests_rel \ `years', r(estimate), `lowb', `highb')
			}
			}
						
			forval years = 1(1)`leads'{
				local year = `years' * 4
				qui lincom (${lincomm`year'})*${C}
				local lowb	= r(estimate) - invttail(`dof', 0.025)*r(se) 
				local highb	= r(estimate) + invttail(`dof', 0.025)*r(se) 

			mat ests_rel = (ests_rel \ -`years' , r(estimate), `lowb', `highb')
			}

			
			
					
			*Upper tail
			qui reghdfe upper_epop $treat if  year>=`b' & cleansample==1 ${weight`w'} , absorb(i.statenum i.quarterdate  postcont postcontf  precont precontf earlycont earlycontf ) cluster(statenum)
			
			local dof = e(df_r)

			forval years = 0(1)`lags'{
			local year = `years' * 4
				qui lincom (${lincom`year'})*${C}
				local lowb	= r(estimate) - invttail(`dof', 0.025)*r(se) 
				local highb	= r(estimate) + invttail(`dof', 0.025)*r(se) 

			if `years' == 0{
			mat ests_upper = (`years', r(estimate), `lowb', `highb')
			}
			else{
			mat ests_upper = (ests_upper \ `years', r(estimate), `lowb', `highb')
			}
			}
						
			forval years = 1(1)`leads'{
				local year = `years' * 4
				qui lincom (${lincomm`year'})*${C}
				local lowb	= r(estimate) - invttail(`dof', 0.025)*r(se) 
				local highb	= r(estimate) + invttail(`dof', 0.025)*r(se) 

			mat ests_upper = (ests_upper \ -`years' , r(estimate), `lowb', `highb')
			}


			
			*Figure
			svmat ests 
			svmat ests_upper
			svmat ests_rel
			rename ests1 time
			rename ests2 est
			rename ests3 low
			rename ests4 high
			
			foreach uporrel in upper rel{
			rename ests_`uporrel'1 time_`uporrel'
			rename ests_`uporrel'2 est_`uporrel'
			rename ests_`uporrel'3 low_`uporrel'
			rename ests_`uporrel'4 high_`uporrel'
			}
			
			sort time
			
				
				
				
				twoway ( connected est time if time!=. )  ///
				(rcap low  high time if time!=. ) , graphregion(color(white)) ///
				/*xlabel(-4 "{&le} -4" "-3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "{&ge} 5")*/ xlabel(-3 "-3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "4") ///
				xtitle("Event time in years") ytitle("Employment elasticity") ylabel(-0.3(0.1)0.2 0 "0" , format(%02.1f)) legend(off)
				gr export "${figures}FigureG1_b.pdf", replace
				
				
				twoway ( connected est_upper time_upper if time_upper!=. )  ///
				(rcap low_upper  high_upper time if time!=. ) , graphregion(color(white)) ///
				/*xlabel(-4 "{&le} -4" "-3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "{&ge} 5")*/ xlabel(-3 "-3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "4") ///
				xtitle("Event time in years") ytitle("Employment elasticity") ylabel(-0.3(0.1)0.2 0 "0" , format(%02.1f)) legend(off) 
				gr export "${figures}FigureG3_b.pdf", replace

				
				twoway ( connected est_rel time_upper if time_upper!=. )  ///
				(rcap low_rel  high_rel time if time!=. ) , graphregion(color(white)) ///
				/*xlabel(-4 "{&le} -4" "-3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "4" 5 "{&ge} 5")*/ xlabel(-3 "-3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "4") ///
				xtitle("Event time in years") ytitle("Employment elasticity") ylabel(-0.3(0.1)0.2 0 "0" , format(%02.1f)) legend(off)
				gr export "${figures}FigureG3_d.pdf", replace

				
			
		}
		
*		













