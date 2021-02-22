
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

 
foreach s in 1 {
	global rtype`s' reghdfe
}
 

global label1 TWFE

use "${data}qcew_multiplier.dta", clear
keep statenum quarterdate multiplier
assert multiplier != 0
tempfile qcew
save `qcew'



if "`1'"=="Y" & "`2'"!="Y"{
local events f
}
if "`1'"=="Y" & "`2'"=="Y"{
local events fs
}
if "`1'"!="Y" & "`2'"=="Y"{
local events s
}

treatmentcontrolwindows

treatmentdefinition, federal("`1'") small("`2'")

abovebelowbunch

abovebelowfull

merge m:1 statenum quarterdate using `qcew',assert(3) nogenerate

replace count = count * multiplier
replace overallcountpc=overallcountpc*multiplier
replace overallcountpcall = overallcountpcall * multiplier




********************************************************************************
***********************           Weights           ****************************
********************************************************************************

foreach Y in   overall { 
	foreach b of numlist 1979{
					global weight`Y'`b' "[aw=wt`Y'`b']" 
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


foreach Y in   overall { 
foreach b in    1979 {
					***  define MW change ***
				if "`1'"=="Y" & "`2'"!="Y"{
				assert overallcountgr!=.
				sum DMW_real if overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`Y'`b'}
				local mwc = r(mean)
				sum MW_real if  F.overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`Y'`b'}
				local mw = r(mean)
				local mwpc = `mwc'/`mw'
				local mwpc2 : di  %04.3f `mwpc'

				
				sum `Y'countpcall if (( F.overallcountgr>0 & F.overallcountgr!=. ) | ///
				(F2.overallcountgr>0  & F2.overallcountgr!=. ) ///
				| ( F3.overallcountgr>0 & F3.overallcountgr!=. ) ///
				| (F4.overallcountgr>0 & F4.overallcountgr!=. ) ) & year>=`b' & cleansample ==1 ///
				 ${weight`Y'`b'}		
				
				local epop = r(mean)
				global E = `epop'
					
				global C = 1/(`epop'*`mwpc')
			
			
				*Below share

				
				sum overallcountpcrsum if (  F.overallcountgr>0  & F.overallcountgr!=.  & (wagebins+25)/100==F1MW_realM25) | ///
				 (  F2.overallcountgr>0 & F2.overallcountgr!=. & (wagebins+25)/100==F2MW_realM25 ) | ///
				( F3.overallcountgr>0 & F3.overallcountgr!=. & (wagebins+25)/100==F3MW_realM25) | ///
				 ( F4.overallcountgr>0 & F4.overallcountgr!=. & (wagebins+25)/100==F4MW_realM25 ) ///
				& year>=`b' & cleansample ==1	  ${weight`Y'`b'}			
				
				global B = r(mean)/${E}
				local belsh2 : di  %04.3f $B
				
				
				sum overallcountpcrsum if ( F.overallcountgr>0  & F.overallcountgr!=.  & (wagebins)/100==MW_realM25) | ///
				 ( F2.overallcountgr>0 & F2.overallcountgr!=. &  (wagebins)/100==MW_realM25 ) | ///
				( F3.overallcountgr>0 & F3.overallcountgr!=. & (wagebins)/100==MW_realM25) | ///
				 ( F4.overallcountgr>0 & F4.overallcountgr!=. & (wagebins)/100==MW_realM25 ) ///
				& year>=1979 & cleansample ==1	  ${weight`Y'`b'}			
				
				
				global ASstf = ${B} -  r(mean)/${E}
				
				
				
				****************************************************************
				*************    Wagebill related globals       ****************
				****************************************************************
				sum overallWBpcrsumFH if (  F.overallcountgr>0  &  F.overallcountgr!=.  & (wagebins+25)/100==F1MW_realM25) | ///
				 (  F2.overallcountgr>0 & F2.overallcountgr!=.   & (wagebins+25)/100==F2MW_realM25 ) | ///
				(  F3.overallcountgr>0  & F3.overallcountgr!=.   & (wagebins+25)/100==F3MW_realM25) | ///
				 (  F4.overallcountgr>0  & F4.overallcountgr!=.   & (wagebins+25)/100==F4MW_realM25 ) ///
				& year>=`b' & cleansample ==1	  ${weight`Y'`b'}			
	
				global EWB = r(mean)
				local EWB2 : di  %04.3f $EWB
				cap drop tempweight	
				global CWB = 1/(${EWB}*`mwpc')
				} // end of the first event definition specific "if".
				
				****************************************************************
				********      Federal and small events          ****************
				****************************************************************
				*This part has not been updated.
				if "`1'"=="Y" & "`2'"=="Y"{
				cap drop allevents
				gen allevents = (D.MW_real> 0  & D.MW_real<. & D.MW>0)
				replace allevents = 0 if missing(allevents)
				sum DMW_real if allevents>0 & year>=`b' & cleansample ==1   ${weight`Y'`b'}
				local mwc = r(mean)
				sum MW_real if  F.allevents>0 & year>=`b' & cleansample ==1   ${weight`Y'`b'}
				local mw = r(mean)
				local mwpc = `mwc'/`mw'
				local mwpc2 : di  %04.3f `mwpc'

				sum `Y'countpcall if (( F.allevents>0 )| ( F2.allevents>0 ) | ( F3.allevents>0 ) | ( F4.allevents>0 ) ) & year>=`b' & cleansample ==1   ${weight`Y'`b'} // CHECK THIS  2/2/16
				local epop = r(mean)
				global E = `epop'
					
				global C = 1/(`epop'*`mwpc')
			
			
				*Below share

				
				sum overallcountpcrsum if (  F.allevents>0  & (wagebins+25)/100==F1MW_realM25) | ///
				 (  F2.allevents>0 & (wagebins+25)/100==F2MW_realM25 ) | ///
				( F3.allevents>0  & (wagebins+25)/100==F3MW_realM25) | ///
				 ( F4.allevents>0 & (wagebins+25)/100==F4MW_realM25 ) ///
				& year>=`b' & cleansample ==1	  ${weight`Y'`b'}			
				
				global B = r(mean)/${E}
				local belsh2 : di  %04.3f $B
				
				
				
				****************************************************************
				*************    Wagebill related globals       ****************
				****************************************************************
				sum overallWBpcrsumFH if (  F.allevents>0  & (wagebins+25)/100==F1MW_realM25) | ///
				 (  F2.allevents>0 & (wagebins+25)/100==F2MW_realM25 ) | ///
				(  F3.allevents>0  & (wagebins+25)/100==F3MW_realM25) | ///
				 (  F4.allevents>0 & (wagebins+25)/100==F4MW_realM25 ) ///
				& year>=`b' & cleansample ==1	  ${weight`Y'`b'}			
	
				global EWB = r(mean)
				local EWB2 : di  %04.3f $EWB
				cap drop tempweight	
				global CWB = 1/(${EWB}*`mwpc')
				} // end of the second event definition specific "if".
				

				sum wagebins if treat_p0 == 1  & year>=`b' & cleansample ==1   ${weight`Y'`b'}, meanonly
				global wagemult = r(mean)/100
				
				
				*We have to do this here because multipliers are calculated in the loop.
				
				abovebelowWBbunch,wagemult(${wagemult})
				abovebelowWBfull
				
			
				foreach s in 1 {		
				*** REGRESSION *** 

				di "${rtype`s'} `Y'countpc  ${treatafter} one if  year>=`b'  & cleansample==1  ${weight`Y'`b'} , a(${a`s'}  ${control} ${controlbefore} ${window} )  cluster(statenum)"
				${rtype`s'} `Y'countpc  ${treatafter} one if  year>=`b'  & cleansample==1  ${weight`Y'`b'} , a(${a`s'}  ${control} ${controlbefore} ${window} )  cluster(statenum)
				est save "${estimates}altedafter_ed`events'_`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}", replace 
				
				est use "${estimates}altedafter_ed`events'_`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
 				local dof = e(df_r)
				local numberobs = e(N)

				if "`events'"=="f"{
				count if  overallcountgr>0 & toosmall!=1 & wagebins==300
				local numberevent = r(N)
				}
				if "`events'"=="fs"{
				count if (D.MW_real> 0  & D.MW_real<. & D.MW>0) & quarterdate>94 & quarterdate<=211 & wagebins==300
				local numberevent = r(N)
				}
				local denominator = (1/(1+(${Tmax}/4)))

	
	

				
			********************************************************************
			****************          Table           **************************
			********************************************************************

			est use "${estimates}altedafter_ed`events'_`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
		
			
			
			lincomestadd (${below_full})*(4*`denominator')*(1/${E})							, statname(below_E) 				// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			lincomestadd (${above_full})*(4*`denominator')*(1/${E})							, statname(above_E)					// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			lincomestadd ((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B} 	, statname(bunching_E)				// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5. 			
			lincomestadd (${below_full} + ${above_full})*(4*`denominator')*${C}				, statname(bunchelas_E)				// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
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

			
			estadd local wagebinperiod 		= "Y"
			
			eststo ta_`Y'_`b'_ed`events'

			
		}
			
	}
 }
*


