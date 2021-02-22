
*This do file is created by DC on 04/19/2018.
********************************************************************************
***************         CK predicted probabilities                **************
********************************************************************************
clear

use statenum quarterdate wagebins first_pop fourth_pop fifth_pop first_count fourth_count fifth_count using "${data}CK_groups.dta", clear
egen tagger = tag(statenum quarterdate)
rename first_pop 	totalpopulation1
rename fourth_pop 	totalpopulation2
rename fifth_pop 	totalpopulation3

foreach vv in first_ fourth_ fifth_{
bys statenum quarterdate: egen _limited_`vv'countall = total(`vv'count) if wagebins<1500
bys statenum quarterdate: egen _upper_`vv'countall = total(`vv'count) if wagebins>=1500
bys statenum quarterdate: egen limited_`vv'countall  = max(_limited_`vv'countall)
bys statenum quarterdate: egen upper_`vv'countall  = max(_upper_`vv'countall)
}
*
keep if tagger
drop tagger
cap drop _*
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
		keep preds_group_countall logwage wage min_preds statenum quarterdate preds_group multiplier
		reshape wide preds_group_countall logwage wage min_preds multiplier , i(statenum quarterdate) j(preds_group)
		

merge 1:1 statenum quarterdate using "${data}VZmw_quarterly_lagsleads_1979_2016.dta", assert(3) nogenerate
merge 1:1 statenum quarterdate using `pop', assert(3) nogenerate
merge 1:1 statenum quarterdate using `grouppop', assert(3) nogenerate
merge 1:1 statenum quarterdate using "${data}eventclassification.dta", nogenerate assert(1 3)

foreach vv in first_ fourth_ fifth_{
replace limited_`vv'countall = limited_`vv'countall * multiplier1		
replace upper_`vv'countall = upper_`vv'countall * multiplier1		
}
*
cap drop multiplier*
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
local vv1 "first_"
local vv2 "fourth_"
local vv3 "fifth_"

forval j = 1/3 { 
	g percap`j' 		= preds_group_countall`j'/totalpopulation
	g limited_percap`j' = limited_`vv`j''countall/totalpopulation
	g upper_percap`j' 	= upper_`vv`j''countall/totalpopulation
	
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
replace postcontf = 0 if postcontf == .


g cleansample = 1
replace cleansample = 0 if quarterdate >=136 & quarterdate <=142

egen avetotalpopulation = mean(totalpopulation) , by(statenum)

*Note that we do not use most of these either, yet I keep this global in case it is needed.

global a1  i.statenum i.quarterdate  // TWFE
global a2  i.statenum##c.quarterdate i.quarterdate  // TWFE



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

global label2 ST
global grlabel2 ST



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
global weight0 [aw=all] 
local ylabel "ylabel( , labsize(medium))" 



foreach b in   1979   {
	foreach w in 1 {
		foreach s in      1  2 {
		use `temptemp', clear
			
				
			forval k = 1(1)3{	
				sum epop if  year>=`b' & cleansample==1  ${weight`w'},meanonly
				local epop = r(mean)
				  				
				sum DMW_real if fedincrease!=1 & overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`w'}
				local mwc = r(mean)
				sum MW_real if  (F.fedincrease != 1 & F.fedincrease != . & F.overallcountgr>0 & F.overallcountgr!=. )  & year>=`b' & cleansample ==1    ${weight`w'}
				local mw = r(mean)
				local mwpc = `mwc'/`mw'
				local mwpc2 : di  %04.3f `mwpc'

				global E = `epop'
					
				global C = 1/(`epop'*`mwpc')

				reghdfe percap`k' $treat if  year>=`b' & cleansample==1   ${weight`w'}, a( ${a`s'} i.postcont i.postcont i.postcontf  i.precont i.precontf i.earlycont i.earlycontf ) cluster(statenum)
				lincomestadd (($lincomagg ) * (1/5))*${C}, statname(allemp)
				local allempb = e(allempb)
				local allempse = e(allempse)
				stop
				
				reghdfe limited_percap`k' $treat if  year>=`b' & cleansample==1   ${weight`w'}, a( ${a`s'}  i.postcont i.postcont i.postcontf  i.precont i.precontf i.earlycont i.earlycontf ) cluster(statenum)
				lincomestadd (($lincomagg ) * (1/5))*${C}, statname(below15emp)

				local below15empb = e(below15empb)
				local below15empse = e(below15empse)
				
				reghdfe upper_percap`k' $treat if  year>=`b' & cleansample==1   ${weight`w'}, a( ${a`s'}  i.postcont i.postcont i.postcontf  i.precont i.precontf i.earlycont i.earlycontf ) cluster(statenum)
				lincomestadd (($lincomagg ) * (1/5))*${C}, statname(over15emp)
								
				estadd local below15empb "`below15empb'"
				estadd local below15empse "`below15empse'"

				estadd local allempb "`allempb'"
				estadd local allempse "`allempse'"
				
				est save "${estimates}eventbased_aggregate_CKgroup`k'_empelas_${label`s'}", replace				
				
				}
			}
		}
	}
*
*	



