clear 

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
forvalues index = 1/4 {
	foreach lagorlead in L F {
		gen `lagorlead'`index'logmw = log(`lagorlead'`index'.mw)
	}
}
gen logmw = log(mw)
*
keep if year>=1979 
xtset statenum year
assert "`r(balanced)'" == "strongly balanced"
keep year statenum logmw L*mw F*mw

compress

tempfile mwdata
save `mwdata'


********************************************************************************
****************               Reshaping the data                  *************
********************************************************************************


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
		
merge 1:1 statenum quarterdate using `pop', assert(3) nogenerate
merge 1:1 statenum quarterdate using `grouppop', assert(3) nogenerate

foreach vv in first_ fourth_ fifth_{
replace limited_`vv'countall = limited_`vv'countall * multiplier1		
replace upper_`vv'countall = upper_`vv'countall * multiplier1		
}

foreach vv of varlist limited* upper* *population* preds_group_countall* {
assert `vv'!=.
}

gen year = year(dofq(quarterdate))
collapse (mean) limited* upper* *population* preds_group_countall* logwage*, by(statenum year)


merge 1:1 statenum year using `mwdata',assert(3) nogenerate
	
	
	
	
********************************************************************************
*************            Preparing the data for regressions       **************
********************************************************************************	
	
	
rename population totalpopulation	

*

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


g cleansample = 1
replace cleansample = 0 if year >=1994 & year <=1995

egen avetotalpopulation = mean(totalpopulation) , by(statenum)
egen avetotalpopulation1 = mean(totalpopulation1) , by(statenum)
egen avetotalpopulation2 = mean(totalpopulation2) , by(statenum)
egen avetotalpopulation3 = mean(totalpopulation3) , by(statenum)


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
global weight2 [aw=altweight]
global weight1 [aw=avetotalpopulation]
global weight11 [aw=avetotalpopulation]
global weight12 [aw=avetotalpopulation]
global weight13 [aw=avetotalpopulation]
global weight0 [aw=all]
local ylabel `"ylabel(-0.2 "-0.2" -0.1 "-0.1" 0 "0" 0.1 "0.1" , labsize(medium))"' 

local first_title 		"Top 10%"
local fourth_title 		"10%-50%"
local fifth_title 		"Bottom 50%"



*To get semi elasticity, get rid of denominator.
foreach b in   1979   {
	foreach w in 1  {
		
		sum epop if  year>=`b' & cleansample==1  ${weight`w'}
		local denominator = r(mean)
		foreach s in      1 {
		use `temptemp', clear
			forval k=1(1)3{

				local epop = `denominator'
			
				global E = `epop'
								
				reghdfe percap`k' $treat if  year>=`b' & cleansample==1   ${weight`w'}, a( ${a`s'} ) cluster(statenum)
				lincomestadd ( $lincomagg ) / ${E}, statname(allemp)

				local allempb = e(allempb)
				local allempse = e(allempse)
				
				
				reghdfe limited_percap`k' $treat if  year>=`b' & cleansample==1   ${weight`w'}, a( ${a`s'} ) cluster(statenum)
				lincomestadd ( $lincomagg ) /  ${E}, statname(below15emp)

				local below15empb = e(below15empb)
				local below15empse = e(below15empse)
				
				reghdfe upper_percap`k' $treat if  year>=`b' & cleansample==1   ${weight`w'}, a( ${a`s'}  ) cluster(statenum)
				lincomestadd ( $lincomagg ) /  ${E}, statname(over15emp)
								
				estadd local below15empb "`below15empb'"
				estadd local below15empse "`below15empse'"

				estadd local allempb "`allempb'"
				estadd local allempse "`allempse'"
				
				est save "${estimates}continuous_aggregate_CKgroup`k'_empelas_${label`s'}", replace				
				
				

				reghdfe D.percap`k' ${treat_FD} if  year>=`b' & cleansample==1   ${weight`w'}, a( ${fda`s'}  ) cluster(statenum)
				lincomestadd ( $lincomagg_FD ) / ${E}, statname(allemp)
				local allempb = e(allempb)
				local allempse = e(allempse)
				
				
				reghdfe D.limited_percap`k' ${treat_FD} if  year>=`b' & cleansample==1   ${weight`w'}, a( ${fda`s'}  ) cluster(statenum)
				lincomestadd ( $lincomagg_FD ) /  ${E}, statname(below15emp)

				local below15empb = e(below15empb)
				local below15empse = e(below15empse)
				
				reghdfe D.upper_percap`k' ${treat_FD} if  year>=`b' & cleansample==1   ${weight`w'}, a( ${fda`s'}  ) cluster(statenum)
				lincomestadd ( $lincomagg_FD ) /  ${E}, statname(over15emp)
								
				estadd local below15empb "`below15empb'"
				estadd local below15empse "`below15empse'"

				estadd local allempb "`allempb'"
				estadd local allempse "`allempse'"
				est save "${estimates}continuous_FD_aggregate_CKgroup`k'_empelas_${label`s'}", replace				
				
								
				
			} // k
		} // s
	} // w
} // b
*


