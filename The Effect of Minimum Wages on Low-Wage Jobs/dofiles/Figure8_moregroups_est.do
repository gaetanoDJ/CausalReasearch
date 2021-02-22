
*This do file is created by DC on 03/30/2016
********************************************************************************
***************************   Figure 8 _baseline      **************************
********************************************************************************
*Results on non-linear effects: The treatment events into quintiles by the below mass share.
*Estimate the effect of all five quintiles separately. 
*1979 onwards
*Note that regression is commented out. Figure creation is also commented out.



********************************************************************************
********************************************************************************
********************************************************************************


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

use "${data}qcew_state_overall_counts_2016.dta", clear
rename statefips statenum
gen quarterdate = yq(year, quarter)
tempfile qcew
save `qcew'



treatmentcontrolwindows

use "${data}grouplevelanalysis_final.dta", clear

abovebelowbunch

abovebelowfull

cap drop _merge

merge m:1 statenum quarterdate using `qcew'


foreach vv in hsd hsg sc coll{
	foreach vv2 in 0020 2030 3040 4050 5060 6099{
		foreach b of numlist 1979{

	replace `vv'`vv2'count = `vv'`vv2'count*emp/countall  if emp!=0
	cap drop `vv'`vv2'countpc
	gen `vv'`vv2'countpc=`vv'`vv2'count/`vv'`vv2'pop
	cap drop `vv'`vv2'countpcall
	gen `vv'`vv2'countpcall = `vv'`vv2'countall*(emp/countall)/`vv'`vv2'pop  if emp!=0
		
		
		}
	}
}
*


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
*
********************************************************************************
***********************           Weights           ****************************
********************************************************************************

cap drop wt 
foreach Y in   overall teen  HSL female BH  { 
	foreach b of numlist 1995 1979{
					global weight`Y'`b' "[aw=wt`Y'`b']" 
					}
}  
*


foreach Y in   overall teen  HSL female BH  {
		foreach H in FTE{
	foreach b of numlist /*1995*/ 1979{
					global weight`Y'`H'`b' "[aw=wt`Y'`b']" 
					}
	}  
}
*	

cap drop statequarterdate
egen statequarterdate = group(state quarterdate)
sort statequarterdate wagebins


foreach vv in hsd hsg sc coll{
	foreach vv2 in 0020 2030 3040 4050 5060 6099{
local Y `vv'`vv2'	
cap drop `Y'countpcrsum
g `Y'countpcrsum=0
replace `Y'countpcrsum = `Y'countpc if wagebins==100
replace `Y'countpcrsum = `Y'countpcrsum[_n-1] + `Y'countpc if wagebins>100
}	
}
*	

******************************************************************************** 
**********************       REGRESSIONS AND FIGURES        ********************
********************************************************************************
xtset wagebinstate quarterdate
compress

foreach vv in hsd hsg sc coll{
	foreach vv2 in 0020 2030 3040 4050 5060 6099{
	foreach b in   /* 1995 */ 1979 {
					***  define MW change ***
					local Y `vv'`vv2'
				sum DMW_real if fedincrease!=1 & overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`Y'`b'}
				local mwc = r(mean)
				sum MW_real if  F.fedincrease!=1 & F.overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`Y'`b'}
				local mw = r(mean)
				local mwpc = `mwc'/`mw'
				local mwpc2 : di  %04.3f `mwpc'

				sum `Y'countpcall if ((F.fedincrease != 1 & F.fedincrease != . & F.overallcountgr>0 & F.overallcountgr!=. ) | ///
				(F2.fedincrease != 1 & F2.overallcountgr>0 & F2.fedincrease != . & F2.overallcountgr!=. ) ///
				| (F3.fedincrease != 1 & F3.overallcountgr>0 & F3.fedincrease != . & F3.overallcountgr!=. ) ///
				| (F4.fedincrease != 1 & F4.overallcountgr>0 & F4.fedincrease != . & F4.overallcountgr!=. ) ) & year>=`b' & cleansample ==1 ///
				 ${weight`Y'`b'}		 // CHECK THIS  2/2/16

				 local epop = r(mean)
				global E = `epop'
					
				global C = 1/(`epop'*`mwpc')
			
				sum `Y'countpcrsum if ( F.fedincrease!=1 & F.overallcountgr>0 & F.fedincrease!=. & F.overallcountgr!=.  & (wagebins+25)/100==F1MW_realM25) | ///
				 ( F2.fedincrease!=1 & F2.overallcountgr>0 & F2.fedincrease!=. & F2.overallcountgr!=. & (wagebins+25)/100==F2MW_realM25 ) | ///
				( F3.fedincrease!=1 & F3.overallcountgr>0 & F3.fedincrease!=. & F3.overallcountgr!=.  & (wagebins+25)/100==F3MW_realM25) | ///
				 ( F4.fedincrease!=1 & F4.overallcountgr>0 & F4.fedincrease!=. & F4.overallcountgr!=. & (wagebins+25)/100==F4MW_realM25 ) ///
				& year>=`b' & cleansample ==1	   ${weight`Y'`b'}					
				
				global B = r(mean)/${E}
			
			
			
				local s = 1			
				
				est use "${estimates}after`vv'`vv2'_stateonly_1_`vv'`vv2'_1979_aggcont_A_1612"

				local denominator = 1/5
				
			********************************************************************
			****************          Table           **************************
			********************************************************************

			
			
			lincom (${below_full})*(4*`denominator')*(1/${E})							 				// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			scalar belowest = -r(estimate)
			scalar belowse	= r(se)
			lincom (${above_full})*(4*`denominator')*(1/${E})											// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			scalar aboveest = r(estimate)
			scalar abovese	= r(se)
			
			mat forfigure = (forfigure \ aboveest , belowest, ${B})
			global lastcol "${lastcol} `Y' "
			
		}
			
	}
}
 
*


