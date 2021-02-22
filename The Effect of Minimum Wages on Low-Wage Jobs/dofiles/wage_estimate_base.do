
*This do file is created by DC on 03/30/2016

********************************************************************************
********************************************************************************
********************************************************************************


		
**** DEFINE REGRESSION COMMAND AND CONTROLS *****

 
foreach s in 1 {
	global rtype`s' reghdfe
}


global a1  i.wagebinstate i.wagequarterdate  // distributional TWFE

global label1 TWFE

use "${data}qcew_state_overall_counts_2016.dta", clear
rename statefips statenum
gen quarterdate = yq(year, quarter)
tempfile qcew
save `qcew'


treatmentcontrolwindows

treatmentdefinition

merge m:1 statenum quarterdate using `qcew',assert(3) nogenerate
replace count = count*emp/countall if emp!=0
cap drop overallcountpc
gen overallcountpc=count/population 
replace overallcountpcall = emp/population  if emp!=0

abovebelowbunch

abovebelowfull

********************************************************************************
***********************           Weights           ****************************
********************************************************************************

cap drop wt 
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
cap drop counter
g one =1   	
gen counter = ((_n-1)*4) - $Tmin if _n<=8

foreach s in  0 1  4  5  6 10  11 12 {
	cap drop be_timeb
	cap drop be_timese
	cap drop be_timeupper
	cap drop be_timelower
	
	g be_timeb=.
	g be_timese=.
	g be_timeupper=.
	g be_timelower=.

	replace be_timeb=0 if counter==-4
	replace be_timese=0 if counter==-4
	replace be_timeupper=0 if counter==-4
	replace be_timelower=0 if counter==-4
	
	}
*


	
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

				
				sum overallcountpcrsum if ( F.fedincrease!=1 & F.overallcountgr>0 & F.fedincrease!=. & F.overallcountgr!=.  & (wagebins+25)/100==F1MW_realM25) | ///
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
				
				global AS`Y' = $B - (r(mean)/${E})
				local affsh`Y' : di  %04.3f ${AS`Y'}
				
				
				****************************************************************
				*************    Wagebill related globals       ****************
				****************************************************************
				sum overallWBpcrsumFH if (  F.fedincrease!=1 & F.overallcountgr>0 & F.fedincrease!=. & F.overallcountgr!=.   & (wagebins+25)/100==F1MW_realM25) | ///
				 (F2.fedincrease!=1 & F2.overallcountgr>0 & F2.fedincrease!=. & F2.overallcountgr!=. & (wagebins+25)/100==F2MW_realM25 ) | ///
				(F3.fedincrease!=1 & F3.overallcountgr>0 & F3.fedincrease!=. & F3.overallcountgr!=.  & (wagebins+25)/100==F3MW_realM25) | ///
				 (  F4.fedincrease!=1 & F4.overallcountgr>0 & F4.fedincrease!=. & F4.overallcountgr!=.  & (wagebins+25)/100==F4MW_realM25 ) ///
				& year>=`b' & cleansample ==1	  ${weight`Y'`b'}			
	
				global EWB = r(mean)
				local EWB2 : di  %04.3f $EWB
				cap drop tempweight	
				global CWB = 1/(${EWB}*`mwpc')


				sum wagebins if treat_p0 == 1  & year>=`b' & cleansample ==1   ${weight`Y'`b'}, meanonly
				global wagemult = r(mean)/100
				
				*We have to do this here because multipliers are calculated in the loop.
				
				abovebelowWBbunch,wagemult(${wagemult})
				abovebelowWBfull
				
			
				foreach s in  1 {		
				*** REGRESSION *** 
				est use  "${estimates}Table1aafterqcew`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
 				local dof = e(df_r)
				
				
			********************************************************************
			****************          Table           **************************
			********************************************************************

			*5 year averages
			est use  "${estimates}Table1aafterqcew`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
			local denominator = 0.2
			nlcomestadd ((((${belowWB_full}+${aboveWB_full})*(4*`denominator')/$EWB ) - ///
			(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B}))) / ///
			(1+(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B}))		, statname(WB_E)  dof(`dof') 					// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			eststo all_2

			*Method 3
			est use  "${estimates}Table1aafterqcew`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"			
			abovebelowWBbunch_alt, secondmethod(Y) 
			abovebelowWBfull_alt,  secondmethod(Y) 
			nlcomestadd ((${aboveWB_fullY})/${EWB})*(4*`denominator'), statname(WB_E) dof(`dof') 
			eststo all_3
			
			est use  "${estimates}Table1aafterqcew`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"			
			nlcom (a1:((((${belowWB_full}+${aboveWB_full})*(4*`denominator')/$EWB ) - ///
			(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B}))) / ///
			(1+(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B}))) (a2:((${aboveWB_fullY})/${EWB})*(4*`denominator')), post
			nlcomestadd  1-(_b[a2]/_b[a1]) , statname(WB_E) dof(`dof') 
			eststo all_spill

						
			
		}
			
	}
 }
*
