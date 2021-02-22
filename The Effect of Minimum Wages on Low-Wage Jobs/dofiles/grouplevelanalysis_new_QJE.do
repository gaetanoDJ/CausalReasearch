
set more off

foreach vv in hsd hsg sc coll{
	foreach vv2 in 0020 2030 3040 4050 5060 6099{
		foreach b of numlist 1979{
					global weight`vv'`vv2'`b' "[aw=wt`vv'`vv2'`b']"  
		}
	}
}

do "${dofiles}create_programs.do"

use "${data}qcew_state_overall_counts_2016.dta", clear
rename statefips statenum
gen quarterdate = yq(year, quarter)
tempfile qcew
save `qcew'


treatmentcontrolwindows

use "${data}grouplevelanalysis_final.dta", clear
cap drop _merge

merge m:1 statenum quarterdate using `qcew',assert(3) nogenerate


foreach vv in hsd hsg sc coll{
	foreach vv2 in 0020 2030 3040 4050 5060 6099{
		foreach b of numlist 1979{

	replace `vv'`vv2'count = `vv'`vv2'count*emp/countall if emp!=0
	cap drop `vv'`vv2'countpc
	gen `vv'`vv2'countpc=`vv'`vv2'count/`vv'`vv2'pop
	cap drop `vv'`vv2'countpcall
	gen `vv'`vv2'countpcall = `vv'`vv2'countall*(emp/countall)/`vv'`vv2'pop  if emp!=0

		}
	}
}
*





********************************************************************************
**************     Sample and Control Specific Macros           ****************
********************************************************************************


local limittreat "_stateonly"


xtset wagebinstate quarterdate



********************************************************************************
**************      To name the graph              *****************************
********************************************************************************

if "${disagg}" == "Y"{
local temp1 "disaggcont"
}
else{
local temp1 "aggcont"
}

if "$controlmethod" == "Linear"{
local temp2 "L"
}
else{
if "$controlmethod" == "None"{
local temp1 "nocont"
}
else{
local temp2 "A"
}
}
cap drop one
g one =1   	


foreach s in 0a 0 1 1b 3 4 5 6 7 8 9 10 11 12 13 14{
	global rtype`s' reghdfe
}


global a1  i.wagebinstate i.wagequarterdate  // distributional TWFE
global a4 i.wagebinstate##c.quarterdate i.wagequarterdate // state-trends
global a5 i.wagebinstate i.wagequarterdate##i.division // div-period
global a6 i.wagebinstate##c.quarterdate i.wagequarterdate##i.division // div-period, statetrends
global a10 i.wagebinstate##c.quarterdate i.wagebinstate##c.quarterdatesq i.wagequarterdate // state-quadratic-trends
global a11 i.wagebinstate##c.quarterdate i.wagebinstate##c.quarterdatesq i.wagequarterdate##i.division // state-quadratic-trends
global a12 i.wagebinstate i.wagequarterdate i.quarterdate##i.statenum // stateXquarter FE.
global a13 i.wagebinstate i.wagequarterdate i.quarterdate##i.statenum // stateXquarter FE.
global a14 i.wagebinstate i.wagequarterdate i.quarterdate##i.statenum // stateXquarter FE.


global opt2 f(wagequarterdate wagebinstate, 3)
 

global label0a aTWFE
global label0 Canonical
global label1 TWFE
global label1b TWFE
global label2 IFE
global label3 Ind-ST
global label4 ST
global label5 DP
global label6 ST_DP
global label7 Ind-ST-DP
global label8 CSP
global label9 ST-CSP
global label10 STQ
global label11 STQ_DP
global label12 STP
global label13 STP15
global label14 STP20



******************************************************************************** 
**********************       REGRESSIONS AND FIGURES        ********************
********************************************************************************
xtset wagebinstate quarterdate
compress

foreach vv in hsd hsg sc coll{
	foreach vv2 in 0020 2030 3040 4050 5060 6099{
foreach b in    1979 {
					***  define MW change ***
				sum DMW_real if fedincrease!=1 & overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`vv'`vv2'`b'}
				local mwc = r(mean)
				sum MW_real if  F.fedincrease!=1 & F.overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`vv'`vv2'`b'}
				local mw = r(mean)
				local mwpc = `mwc'/`mw'
				local mwpc2 : di  %04.3f `mwpc'

				sum `vv'`vv2'countpcall if ((F.fedincrease != 1 & F.overallcountgr>0 ) | (F2.fedincrease != 1 & F2.overallcountgr>0 ) | (F3.fedincrease != 1 & F3.overallcountgr>0 ) | (F4.fedincrease != 1 & F4.overallcountgr>0 ) ) & year>=`b' & cleansample ==1   ${weight`vv'`vv2'`b'} // CHECK THIS  2/2/16
				local epop = r(mean)
				global E = `epop'
					
				global C = 1/(`epop'*`mwpc')
			
			
				*Below share

				
			
				foreach s in  1     {		
				*** REGRESSION *** 
				di "${rtype`s'} `vv'`vv2'countpc  ${treatafter} one if  year>=`b'   & weight`vv'`vv2'`b'>0 & cleansample==1 ${if`s'}  ${weight`vv'`vv2'`b'} , a(${a`s'}  ${control} ${controlbefore} ${controlf} ${window} )  "
				${rtype`s'} `vv'`vv2'countpc  ${treatafter} one if  year>=`b'  & wt`vv'`vv2'`b'>0 & cleansample==1 ${if`s'}  ${weight`vv'`vv2'`b'} , a(${a`s'}  ${control} ${controlbefore} ${controlf} ${window} )  cluster(statenum)
				est save "${estimates}after`vv'`vv2'`limitsample'`limittreat'_`s'_`vv'`vv2'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}", replace 

			
		}
			
	}
 }
}
*








