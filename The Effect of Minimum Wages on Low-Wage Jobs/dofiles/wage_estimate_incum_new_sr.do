
*This do file is created by DC on 10/07/2016


********************************************************************************
********************************************************************************
********************************************************************************


		
**** DEFINE REGRESSION COMMAND AND CONTROLS *****

 
foreach s in 1 {
	global rtype`s' reghdfe
}
*

local truewmin = 4
local truewmax = 4
local placebo1wmax = 13
local prevwmax = `placebo1wmax'
local placebo2wmax = 16


global a1  i.wagebinstate i.wagequarterdate  // distributional TWFE

global label1 TWFE

use "${data}qcew_state_overall_counts_2016.dta", clear
rename statefips statenum
gen quarterdate = yq(year, quarter)
tempfile qcew
save `qcew'



treatmentcontrolwindows


placebowindows1, truewmax(`truewmax') wmax(`placebo1wmax')

placebowindows2, prevwmax(`prevwmax') wmax(`placebo2wmax')


use "${data}state_panels_with3quant1979_matched.dta", clear
cap drop wagebinstate
gen wagebinstate =  wagebinstateprev

merge m:1 statenum quarterdate using `qcew', keep(1 3)
cap drop multiplier
gen multiplier = emp/mt_countall
replace multiplier = 1 if multiplier==0

foreach Y in   m_ mt_   { 
replace `Y'count = `Y'count*multiplier
replace `Y'overallcountpc = `Y'overallcountpc*multiplier
replace `Y'overallcountpcall = `Y'overallcountpcall *multiplier
replace `Y'countall = `Y'countall*multiplier
}


abovebelowbunch

abovebelowfull


********************************************************************************
***********************           Weights           ****************************
********************************************************************************

foreach Y in   overall { 
	foreach b of numlist  1979{
					global weightm_`Y'`b' "[aw=wt`Y'`b']" 
					global weightmt_`Y'`b' "[aw=wt`Y'`b']" 
					}
}  
*


********************************************************************************
**************     Sample and Control Specific Macros           ****************
********************************************************************************


local limittreat "_stateonly"


xtset wagebinstateprev quarterdate



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
cap drop counter
g one =1   	
gen counter = ((_n-1)*4) - $Tmin if _n<=8


	
********************************************************************************
***************          Useful to Find Below Share Wages               ********
********************************************************************************

cap drop statequarterdate
egen statequarterdate = group(state quarterdate)
sort emp_prev statequarterdate wagebins


foreach vv in mt_ m_ {

cap drop `vv'overallWB*
g `vv'overallWBFH = wagebins*`vv'count/(100)
g `vv'overallWBpcFH = wagebins*`vv'count/(100*population)

cap drop `vv'overallWBpcrsumFH
g `vv'overallWBpcrsumFH=0
replace `vv'overallWBpcrsumFH = `vv'overallWBpcFH if wagebins==100
replace `vv'overallWBpcrsumFH = `vv'overallWBpcrsumFH[_n-1] + `vv'overallWBpcFH if wagebins>100
	
cap drop `vv'overallcountpcrsum
g `vv'overallcountpcrsum=0
replace `vv'overallcountpcrsum = `vv'overallcountpc if wagebins==100
replace `vv'overallcountpcrsum = `vv'overallcountpcrsum[_n-1] + `vv'overallcountpc if wagebins>100
}	
*	

*Sanity check.

sort statequarterdate wagebins emp_prev 

assert mt_overallcountpcrsum == mt_overallcountpcrsum[_n-1] if statequarterdate== statequarterdate[_n-1] & wagebins == wagebins[_n-1] & emp_prev!=emp_prev[_n-1]

******************************************************************************** 
**********************       REGRESSIONS AND FIGURES        ********************
********************************************************************************
xtset wagebinstateprev quarterdate
*Because the data is long.


compress

local if1 "& emp_prev==1"
local if2 "& emp_prev==0"
local if3 "& emp_prev==1" 														// In the third case, it does not matter whether emp_prev==1 or emp_prev==0.
local Ycounter = 0
foreach Y in   m_overall m_overall  { 
local Ycounter = `Ycounter' + 1
foreach b in    1979 {
					***  define MW change ***
				sum DMW_real if fedincrease!=1 & overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`Y'`b'}
				local mwc = r(mean)
				sum MW_real if  F.fedincrease!=1 & F.overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`Y'`b'}
				local mw = r(mean)
				local mwpc = `mwc'/`mw'
				local mwpc2 : di  %04.3f `mwpc'

				sum `Y'countpcall if ((F.fedincrease != 1 & F.fedincrease != . & F.overallcountgr>0 & F.overallcountgr!=. ) | ///
				(F2.fedincrease != 1 & F2.overallcountgr>0 & F2.fedincrease != . & F2.overallcountgr!=. ) ///
				| (F3.fedincrease != 1 & F3.overallcountgr>0 & F3.fedincrease != . & F3.overallcountgr!=. ) ///
				| (F4.fedincrease != 1 & F4.overallcountgr>0 & F4.fedincrease != . & F4.overallcountgr!=. ) ) & year>=`b' & cleansample ==1 	`if`Ycounter''   ///
				 ${weight`Y'`b'}		 // CHECK THIS  2/2/16

				local epop = r(mean)
				global E = `epop'
					
				global C = 1/(`epop'*`mwpc')
			
			
				*Below share
				
				sum `Y'countpcrsum if ( F.fedincrease!=1 & F.overallcountgr>0 & F.fedincrease!=. & F.overallcountgr!=.  & (wagebins+25)/100==F1MW_realM25) | ///
				 ( F2.fedincrease!=1 & F2.overallcountgr>0 & F2.fedincrease!=. & F2.overallcountgr!=. & (wagebins+25)/100==F2MW_realM25 ) | ///
				( F3.fedincrease!=1 & F3.overallcountgr>0 & F3.fedincrease!=. & F3.overallcountgr!=.  & (wagebins+25)/100==F3MW_realM25) | ///
				 ( F4.fedincrease!=1 & F4.overallcountgr>0 & F4.fedincrease!=. & F4.overallcountgr!=. & (wagebins+25)/100==F4MW_realM25 ) ///
				& year>=`b' & cleansample ==1	`if`Ycounter''    ${weight`Y'`b'}					
				
								
				global B = r(mean)/${E}
				local belsh2 : di  %04.3f $B

				*Affected share
				
				sum `Y'countpcrsum if (  F.fedincrease!=1 & F.overallcountgr>0 & F.fedincrease!=. & F.overallcountgr!=.  & (wagebins)/100==MW_realM25) | ///
				 ( F2.fedincrease!=1 & F2.overallcountgr>0 & F2.fedincrease!=. & F2.overallcountgr!=. & (wagebins)/100==MW_realM25 ) | ///
				( F3.fedincrease!=1 & F3.overallcountgr>0 & F3.fedincrease!=. & F3.overallcountgr!=.  & (wagebins)/100==MW_realM25) | ///
				 ( F4.fedincrease!=1 & F4.overallcountgr>0 & F4.fedincrease!=. & F4.overallcountgr!=. & (wagebins)/100==MW_realM25 ) ///
				& year>=1979 & cleansample ==1	  ${weight`Y'`b'}			
				
				global AS`Y' = $B - (r(mean)/${E})
				local affsh`Y' : di  %04.3f ${AS`Y'}
				
				
				
				****************************************************************
				*************    Wagebill related globals       ****************
				****************************************************************
				
				sum `Y'WBpcrsumFH if (  F.fedincrease!=1 & F.overallcountgr>0 & F.fedincrease!=. & F.overallcountgr!=.   & (wagebins+25)/100==F1MW_realM25) | ///
				 (F2.fedincrease!=1 & F2.overallcountgr>0 & F2.fedincrease!=. & F2.overallcountgr!=. & (wagebins+25)/100==F2MW_realM25 ) | ///
				(F3.fedincrease!=1 & F3.overallcountgr>0 & F3.fedincrease!=. & F3.overallcountgr!=.  & (wagebins+25)/100==F3MW_realM25) | ///
				 (  F4.fedincrease!=1 & F4.overallcountgr>0 & F4.fedincrease!=. & F4.overallcountgr!=.  & (wagebins+25)/100==F4MW_realM25 ) ///
				& year>=`b' & cleansample ==1	`if`Ycounter''  ${weight`Y'`b'}			
					
				global EWB = r(mean)
				local EWB2 : di  %04.3f $EWB
				
				cap drop tempweight	
				global CWB = 1/(${EWB}*`mwpc')

				sum wagebins if treat_p0 == 1  & year>=`b' & cleansample ==1   ${weight`Y'`b'}, meanonly
				global wagemult = r(mean)/100
				
				
				*We have to do this here because multipliers are calculated in the loop.
				
				abovebelowWBbunch,wagemult(${wagemult})
				abovebelowWBfull
				
			
				foreach s in  1    {		
				

				est use "${estimates}`Y'`Ycounter'pl0after`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
				local dof = e(df_r)

			********************************************************************
			****************          Table           **************************
			********************************************************************

			*Method 1

			est use "${estimates}`Y'`Ycounter'pl0after`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
			
			abovebelowfull, tmax(0)
			abovebelowWBfull, tmax(0)
			local denominator = 1

			nlcomestadd ((((${belowWB_full}+${aboveWB_full})*(4*`denominator')/$EWB ) - ///
			(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B}))) / ///
			(1+(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B}))		, statname(WB_E)  dof(`dof') 					// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			eststo m`Ycounter'_2

			*Method 3
			est use "${estimates}`Y'`Ycounter'pl0after`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
			abovebelowWBbunch_alt, secondmethod(Y) tmax(0)
			abovebelowWBfull_alt,  secondmethod(Y) tmax(0)
			nlcomestadd ((${aboveWB_fullY})/${EWB})*(4*`denominator'), statname(WB_E) dof(`dof') 
			eststo m`Ycounter'_3
			
			est use "${estimates}`Y'`Ycounter'pl0after`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
			nlcom (a1:((((${belowWB_full}+${aboveWB_full})*(4*`denominator')/$EWB ) - ///
			(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B}))) / ///
			(1+(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B}))) (a2:((${aboveWB_fullY})/${EWB})*(4*`denominator')), post
			nlcomestadd    1-(_b[a2]/_b[a1])  , statname(WB_E) dof(`dof') 
			eststo m`Ycounter'_spill
			
			
			
			
			}
		}
}
			*
			
			