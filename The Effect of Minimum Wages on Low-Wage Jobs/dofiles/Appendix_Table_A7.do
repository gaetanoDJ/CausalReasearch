
*This do file is created by DC on 04/19/2018.
********************************************************************************
***************         CK predicted probabilities                **************
********************************************************************************



use "${data}qcew_multiplier.dta", clear
keep statenum quarterdate multiplier
assert multiplier!=0
tempfile qcew
save `qcew'


use statenum quarterdate first_pop fourth_pop fifth_pop using "${data}CK_groups.dta", clear
egen tagger = tag(statenum quarterdate)
rename first_pop 	totalpopulation1
rename fourth_pop 	totalpopulation2
rename fifth_pop 	totalpopulation3
gen totalpopulation4 = totalpopulation1 + totalpopulation2
keep if tagger
drop tagger
tempfile grouppop
save `grouppop'


use statenum quarterdate population using "${data}state_panels_cents_balanced_add_QJE.dta", clear
egen tagger = tag(statenum quarterdate)
keep if tagger
drop tagger
tempfile pop
save `pop'


use "${data}CK_predicted_groups_collapsed.dta", clear
		merge m:1 statenum quarterdate using `qcew', nogenerate assert(3)
		replace preds_group_countall = preds_group_countall* multiplier		
		keep preds_group_countall logwage wage min_preds statenum quarterdate preds_group
		reshape wide preds_group_countall logwage wage min_preds , i(statenum quarterdate) j(preds_group)
		gen preds_group_countall4 = preds_group_countall1 +  preds_group_countall2 

merge 1:1 statenum quarterdate using "${data}VZmw_quarterly_lagsleads_1979_2016.dta", assert(3) nogenerate
merge 1:1 statenum quarterdate using `pop', assert(3) nogenerate
merge 1:1 statenum quarterdate using `grouppop', assert(3) nogenerate
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
forval j = 1/4 { 
	g percap`j' = preds_group_countall`j'/totalpopulation`j'
	
}
*
gen countall  = preds_group_countall1
forval j = 2/3 { 
	replace countall= countall +  preds_group_countall`j'
}

g epop    = countall/totalpopulation


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
replace postcontf = 1 if postcontf == .


g cleansample = 1
replace cleansample = 0 if quarterdate >=136 & quarterdate <=142

egen avetotalpopulation = mean(totalpopulation) , by(statenum)
egen avetotalpopulation1 = mean(totalpopulation1) , by(statenum)
egen avetotalpopulation2 = mean(totalpopulation2) , by(statenum)
egen avetotalpopulation3 = mean(totalpopulation3) , by(statenum)


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



*global minus 0
global minus_emp ([emp_ests_mean]F4treat)
global lincomagg_emp 	([emp_ests_mean]treat-$minus_emp)
forval i = 4(4)16{
global lincomagg_emp "$lincomagg_emp + ([emp_ests_mean]L`i'treat - $minus_emp ) "
}
*
global minus_wage ([wage_ests_mean]F4treat)
global lincomagg_wage 	([wage_ests_mean]treat-$minus_wage)
forval i = 4(4)16{
global lincomagg_wage "$lincomagg_wage + ([wage_ests_mean]L`i'treat - $minus_wage ) "
}
*

global label1 TWFE
global grlabel1 TWFE



********************************************************************************
***************       The Regressions ******************************************
********************************************************************************		
		


gen all=1
gen year = year(dofq(quarterdate))
tempfile temptemp
save `temptemp', replace	

local regsample ""
local graphname "o"
global weight1 [aw=avetotalpopulation]
global weight11 [aw=avetotalpopulation1]
global weight12 [aw=avetotalpopulation2]
global weight13 [aw=avetotalpopulation3]
global weight0 [aw=all] 
local ylabel "ylabel( , labsize(medium))" 


foreach b in   1979   {
	foreach w in 1 /*0*/   {
		foreach s in      1   {
		use `temptemp', clear
			
				
			forval k = 1(1)3{	
				sum percap`k'  if ((F.fedincrease != 1 & F.fedincrease != . & F.overallcountgr>0 & F.overallcountgr!=. ) | ///
				(F2.fedincrease != 1 & F2.overallcountgr>0 & F2.fedincrease != . & F2.overallcountgr!=. ) ///
				| (F3.fedincrease != 1 & F3.overallcountgr>0 & F3.fedincrease != . & F3.overallcountgr!=. ) ///
				| (F4.fedincrease != 1 & F4.overallcountgr>0 & F4.fedincrease != . & F4.overallcountgr!=. ) ) & year>=`b' & cleansample ==1 ${weight`w'`k'}

				local epop = r(mean)

				
				
				sum wage`k'  if ((F.fedincrease != 1 & F.fedincrease != . & F.overallcountgr>0 & F.overallcountgr!=. ) | ///
				(F2.fedincrease != 1 & F2.overallcountgr>0 & F2.fedincrease != . & F2.overallcountgr!=. ) ///
				| (F3.fedincrease != 1 & F3.overallcountgr>0 & F3.fedincrease != . & F3.overallcountgr!=. ) ///
				| (F4.fedincrease != 1 & F4.overallcountgr>0 & F4.fedincrease != . & F4.overallcountgr!=. ) ) & year>=`b' & cleansample ==1 ${weight`w'`k'}

				global ave_wage`k' = r(mean)/100
				
				sum DMW_real if fedincrease!=1 & overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`w'`k'}
				local mwc = r(mean)
				sum MW_real if  (F.fedincrease != 1 & F.fedincrease != . & F.overallcountgr>0 & F.overallcountgr!=. )  & year>=`b' & cleansample ==1   ${weight`w'`k'}
				local mw = r(mean)
				local mwpc = `mwc'/`mw'
				local mwpc2 : di  %04.3f `mwpc'

				global E = `epop'
					
				global C = 1/(`epop'*`mwpc')
			
				reghdfe percap`k' $treat if  year>=`b' & cleansample==1 ${weight`w'`k'}, a(i.statenum i.quarterdate i.postcont i.postcont i.postcontf  i.precont i.precontf i.earlycont i.earlycontf ) cluster(statenum)
				lincomestadd `mwpc'*(($lincomagg ) * (1/5))*${C}, statname(main_est)
				
				eststo emp_`k'
				local dof =  e(df_r)
				
				reghdfe logwage`k' $treat if  year>=`b' & cleansample==1 ${weight`w'`k'}, a(i.statenum i.quarterdate i.postcont i.postcont i.postcontf  i.precont i.precontf i.earlycont i.earlycontf ) cluster(statenum)
				lincomestadd `mwpc'*(($lincomagg ) * (1/5))*1/(`mwpc'), statname(main_est)
				
				eststo wage_`k'
				
			reg percap`k' $treat i.statenum i.quarterdate i.postcont i.postcont i.postcontf  i.precont i.precontf i.earlycont i.earlycontf if  year>=`b' & cleansample==1 ${weight`w'`k'}
			est sto emp_ests
			reg logwage`k' $treat i.statenum i.quarterdate i.postcont i.postcont i.postcontf  i.precont i.precontf i.earlycont i.earlycontf if  year>=`b' & cleansample==1 ${weight`w'`k'}
			est sto wage_ests
			suest emp_ests wage_ests, vce(cluster statenum)
			
			
			
			nlcomestadd ((${lincomagg_emp})/${E})/(${lincomagg_wage}), statname(main_est) dof(`dof')
			if `k'==1{
			estadd local group "High prob."
			}
			if `k'==2{
			estadd local group "Middle"
			}
			if `k'==3{
			estadd local group "Low prob."
			}
			
			eststo emp_elas_wrt_wage_`k'

			local dof = e(df_r)

			
				}
			}
		}
	}

*	



esttab emp_1 emp_2 emp_3   ///
using "${tables}TableA7.tex", replace	///
stats(main_estb main_estse blankspace , label("\% $\delta$ overall employment" " " " ")) ///
nodep addnotes("`footnotes'") nogaps cells(none) nomtitles fragment booktabs



esttab wage_1 wage_2 wage_3 ///
using "${tables}TableA7.tex", append	///
stats(main_estb main_estse blankspace , label("\% $\delta$ average wage" " " " ")) ///
nodep addnotes("`footnotes'") nogaps cells(none) nomtitles fragment booktabs nolines nodepvar nonumber


esttab emp_elas_wrt_wage_1   ///
using "${tables}TableA7.tex", append	///
stats(main_estb main_estse blankspace group , label("Employment elasticity wrt wage" " " " " "\underline{Group:}")) ///
nodep addnotes("`footnotes'") nogaps cells(none) nomtitles fragment booktabs nolines nodepvar nonumber

