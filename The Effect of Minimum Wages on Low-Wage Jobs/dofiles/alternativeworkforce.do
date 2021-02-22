
*This do file is created by DC on 03/30/2016
********************************************************************************
***************************   Table 1      *************************************
********************************************************************************
*Results on non-linear effects: The treatment events into quintiles by the below mass share.
*Estimate the effect of all five quintiles separately. 
*1979 onwards
*Note that regression is commented out. Figure creation is also commented out.



********************************************************************************
********************************************************************************
********************************************************************************


		
**** DEFINE REGRESSION COMMAND AND CONTROLS *****

 
foreach s in 0a 0 1 1b 3 4 5 6 7 8 9 10 11 12{
	global rtype`s' reghdfe
}
global rtype2 regifewtab


global a1  i.wagebinstate i.wagequarterdate  // distributional TWFE
global a4 i.wagebinstate##c.quarterdate i.wagequarterdate // state-trends
global a5 i.wagebinstate i.wagequarterdate##i.division // div-period
global a6 i.wagebinstate##c.quarterdate i.wagequarterdate##i.division // div-period, statetrends
global a10 i.wagebinstate##c.quarterdate i.wagebinstate##c.quarterdatesq i.wagequarterdate // state-quadratic-trends
global a11 i.wagebinstate##c.quarterdate i.wagebinstate##c.quarterdatesq i.wagequarterdate##i.division // state-quadratic-trends
global a12 i.wagebinstate i.wagequarterdate i.quarterdate##i.statenum // stateXquarter FE.


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


use "${data}qcew_multiplier.dta", clear
keep statenum quarterdate multiplier
assert multiplier!=0
tempfile qcew
save `qcew'



treatmentcontrolwindows

*The following calls the data.
treatmentdefinition, restriction(`1')
di "You restricted the sample to `1' workers only."
abovebelowbunch

abovebelowfull

merge m:1 statenum quarterdate using `qcew',assert(3) nogenerate
replace count = count*multiplier
replace overallcountpc=overallcountpc*multiplier
replace overallcountpcall = overallcountpcall * multiplier


********************************************************************************
***********************           Weights           ****************************
********************************************************************************

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

********************************************************************************
**************     Sample and Control Specific Macros           ****************
********************************************************************************


local limittreat "_stateonly"


xtset wagebinstate quarterdate



********************************************************************************
**************      To name the graph              *****************************
********************************************************************************


if "`1'"=="hourly"{
local workforce "h"
}
if "`1'"=="nontip"{
local workforce "n"
}
if "`1'"=="hourlynontip"{
local workforce "hn"
}



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
sort statequarterdate wagebins

cap drop overallWB*
g overallWBFH = wagebins*count/(100)
g overallWBpcFH = wagebins*count/(100*population)

cap drop overallWBpcrsumFH
g overallWBpcrsumFH=0
replace overallWBpcrsumFH = overallWBpcFH if wagebins==100
replace overallWBpcrsumFH = overallWBpcrsumFH[_n-1] + overallWBpcFH if wagebins>100
	
cap drop overallcountpcrsum
g overallcountpcrsum=0
replace overallcountpcrsum = overallcountpc if wagebins==100
replace overallcountpcrsum = overallcountpcrsum[_n-1] + overallcountpc if wagebins>100
	
	

******************************************************************************** 
**********************       REGRESSIONS AND FIGURES        ********************
********************************************************************************
xtset wagebinstate quarterdate
compress

foreach Y in   overall /*overallFTE*/ /*teen  teenFTE HSL /*HSLFTE*/ female /*femaleFTE*/ BH /*BHFTE*/ */ { 
foreach b in   /* 1995 */ 1979 {
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
				| (F4.fedincrease != 1 & F4.overallcountgr>0 & F4.fedincrease != . & F4.overallcountgr!=. ) ) & year>=`b' & cleansample ==1 ///
				 ${weight`Y'`b'}		 // CHECK THIS  2/2/16

				 local epop = r(mean)
				global E = `epop'
					
				global C = 1/(`epop'*`mwpc')
			
			
				*Below share

				
				sum `Y'countpcrsum if ( F.fedincrease!=1 & F.overallcountgr>0 & F.fedincrease!=. & F.overallcountgr!=.  & (wagebins+25)/100==F1MW_realM25) | ///
				 ( F2.fedincrease!=1 & F2.overallcountgr>0 & F2.fedincrease!=. & F2.overallcountgr!=. & (wagebins+25)/100==F2MW_realM25 ) | ///
				( F3.fedincrease!=1 & F3.overallcountgr>0 & F3.fedincrease!=. & F3.overallcountgr!=.  & (wagebins+25)/100==F3MW_realM25) | ///
				 ( F4.fedincrease!=1 & F4.overallcountgr>0 & F4.fedincrease!=. & F4.overallcountgr!=. & (wagebins+25)/100==F4MW_realM25 ) ///
				& year>=`b' & cleansample ==1	   ${weight`Y'`b'}					
				
				global B = r(mean)/${E}
				local belsh2 : di  %04.3f $B

				*Affected share
				
				sum `Y'countpcrsum if (  F.fedincrease!=1 & F.overallcountgr>0 & F.fedincrease!=. & F.overallcountgr!=.  & (wagebins)/100==MW_realM25) | ///
				 ( F2.fedincrease!=1 & F2.overallcountgr>0 & F2.fedincrease!=. & F2.overallcountgr!=. & (wagebins)/100==MW_realM25 ) | ///
				( F3.fedincrease!=1 & F3.overallcountgr>0 & F3.fedincrease!=. & F3.overallcountgr!=.  & (wagebins)/100==MW_realM25) | ///
				 ( F4.fedincrease!=1 & F4.overallcountgr>0 & F4.fedincrease!=. & F4.overallcountgr!=. & (wagebins)/100==MW_realM25 ) ///
				& year>=1979 & cleansample ==1	  ${weight`Y'`b'}			
				
				global AS`1' = $B - (r(mean)/${E})
				local affsh`1' : di  %04.3f ${AS`1'}
				
				
				
				****************************************************************
				*************    Wagebill related globals       ****************
				****************************************************************
				sum `Y'WBpcrsumFH if (  F.fedincrease!=1 & F.overallcountgr>0 & F.fedincrease!=. & F.overallcountgr!=.   & (wagebins+25)/100==F1MW_realM25) | ///
				 (F2.fedincrease!=1 & F2.overallcountgr>0 & F2.fedincrease!=. & F2.overallcountgr!=. & (wagebins+25)/100==F2MW_realM25 ) | ///
				(F3.fedincrease!=1 & F3.overallcountgr>0 & F3.fedincrease!=. & F3.overallcountgr!=.  & (wagebins+25)/100==F3MW_realM25) | ///
				 (  F4.fedincrease!=1 & F4.overallcountgr>0 & F4.fedincrease!=. & F4.overallcountgr!=.  & (wagebins+25)/100==F4MW_realM25 ) ///
				& year>=`b' & cleansample ==1	  ${weight`Y'`b'}			
	
				global EWB = r(mean)
				local EWB2 : di  %04.3f $EWB
				cap drop tempweight	
				global CWB = 1/(${EWB}*`mwpc')

				sum avewage if treat_p0 == 1  & year>=`b' & cleansample ==1   ${weight`Y'`b'}, meanonly
				global wagemult = r(mean)/100
				
				
				*We have to do this here because multipliers are calculated in the loop.
				
				abovebelowWBbunch,wagemult(${wagemult})
				abovebelowWBfull
				
			
				foreach s in /*0*/ 1 /*4 5 6 10 11 12*/ /*0a 0b  4  5 6*/     {		
				*** REGRESSION *** 
				*${rtype`s'} `Y'countpc  ${treatbefore} one if  year>=`b'  & cleansample==1  ${weight`Y'`b'} , a(${a`s'}  ${control} ${controlafter} ${controlf} ${window} )  cluster(statenum)
				*est save "${estimates}Table1abefore`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}", replace 
				*${rtype`s'} `Y'countpc  ${treatafter} one if  year>=`b'  & cleansample==1  ${weight`Y'`b'} , a(${a`s'}  ${control} ${controlbefore} ${controlf} ${window} )  cluster(statenum) 
				*est save "${estimates}altwfafter_wf`workforce'_`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}", replace 
				est use "${estimates}altwfafter_wf`workforce'_`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
 				local dof = e(df_r)
				local numberobs = e(N)
				count if fedincrease!=1 & overallcountgr>0 & wagebins==300		// The wage bin does not matter.
				local numberevent = r(N)
				
				
				local denominator = (1/(1+(${Tmax}/4)))

				
			********************************************************************
			****************          Table           **************************
			********************************************************************

				est use "${estimates}altwfafter_wf`workforce'_`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
			
			
			
			lincomestadd (${below_full})*(4*`denominator')*(1/${E})							, statname(below_E) 				// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			lincomestadd (${above_full})*(4*`denominator')*(1/${E})							, statname(above_E)					// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
*			lincomestadd (${below_full} + ${above_full})*(4*`denominator')*(1/${E}) 		, statname(chemp_E)					// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5. 
			lincomestadd ((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B} 	, statname(bunching_E)				// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5. 			
			lincomestadd (${below_full} + ${above_full})*(4*`denominator')*${C}				, statname(bunchelas_E)				// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
/*			lincomestadd ((((${belowWB_full}+${aboveWB_full})*(4*`denominator')/$EWB ) - ///
			(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B})))		, statname(WB_E)					// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			nlcomestadd (((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B}) / ///
			(((${belowWB_full}+${aboveWB_full})*(4*`denominator')/$EWB ) - ///
			(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B}))			, statname(labordem_E) dof(`dof') 
*/
			nlcomestadd ((((${belowWB_full}+${aboveWB_full})*(4*`denominator')/$EWB ) - ///
			(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B}))) / ///
			(1+(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B}))		, statname(WB_E)  dof(`dof') 					// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			
			nlcomestadd (((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B}) /  ///
			(((((${belowWB_full}+${aboveWB_full})*(4*`denominator')/$EWB ) - ///
			(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B}))) / ///
			(1+(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B})))		, statname(labordem_E)	 dof(`dof') 				// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.

			
			estadd local numobs = "`numberobs'"
			estadd local numevent = "`numberevent'"
			estadd local minwpc	= "`mwpc2'"
			estadd local belsh	= "`belsh2'"
			estadd local wagebinstate 		= "Y"			

			
			if `s' == 0{
			estadd local periodfe 		= "Y"
			}
			if `s' != 0{
			estadd local wagebinperiod 		= "Y"
			}

			
			
			
			if `s' == 4{
			estadd local trend 		= "Y"
			}
			if `s' == 5{
			estadd local divtime 	= "Y"
			}
			
			if `s' == 6{
			estadd local trend 		= "Y"
			estadd local divtime 	= "Y"
			}
			
			if `s' == 10{
			estadd local trend 		= "Y"
			estadd local qtrend		= "Y"
			}

			if `s' == 11{
			estadd local trend 		= "Y"
			estadd local qtrend		= "Y"
			estadd local divtime 	= "Y"
			}

			if `s' == 12{
			estadd local stp		= "Y"
			
			}
			
			eststo ta_`Y'_`b'_wf`workforce'

			
		}
			
	}
 }
*

