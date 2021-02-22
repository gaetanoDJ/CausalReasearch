
*This do file is created by DC on 05/09/2016.

		
**** DEFINE REGRESSION COMMAND AND CONTROLS *****

 
	global rtype1 reghdfe


global a1  i.wagebinstate i.wagequarterdate  // distributional TWFE


global label1 TWFE
local truewmin = 4
local truewmax = 4
local placebo1wmax = 13
local prevwmax = `placebo1wmax'
local placebo2wmax = 16


use "${data}qcew_state_overall_counts_2016.dta", clear
rename statefips statenum
gen quarterdate = yq(year, quarter)
tempfile qcew
save `qcew'

treatmentcontrolwindows

placebowindows1, truewmax(`truewmax') wmax(`placebo1wmax')

placebowindows2, prevwmax(`prevwmax') wmax(`placebo2wmax')


*use "${stn_bsamples}annual_me_corrected_logwages.dta", clear
use "${stn_bsamples}me_corrected_logwages.dta", clear
merge 1:1 statenum quarterdate wagebins using "${data}state_panels_with3quant1979.dta", nogenerate assert(2 3) 
replace density=0 if density==.
cap drop count
cap drop overallcountpc
cap drop overallcountpcall
bys statenum quarterdate: egen density_all = total(density)
gen count = countall * density/density_all

merge m:1 statenum quarterdate using `qcew',assert(3) nogenerate


	replace count = count*emp/countall  if emp!=0
	replace countall = countall*emp/countall  if emp!=0
	gen overallcountpc = count/population
	gen overallcountpcall = countall/population
*


********************************************************************************
********************      Dropping unnecessary variables        ****************
********************************************************************************

cap drop HSD*
cap drop HSL*
cap drop black*
cap drop female*
cap drop BH*
cap drop teen*	

********************************************************************************
*****************          Placebo sample correction          ******************
********************************************************************************

placebosamplecorr1, wmax(`placebo1wmax') truewmax(`truewmax') 
placebosamplecorr2, wmax(`placebo2wmax') prevwmax(`placebo1wmax')


abovebelowbunch

abovebelowfull


********************************************************************************
****************           Weights                                **************
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

local counter  = 1
cap drop one
gen one = 1
foreach Y in   overall { 
foreach b in   1979 {
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

				local counter = `counter' + 1
				cap drop temp 
				gen temp = (overallcountgr>0 & overallcountgr!=. & fedincrease!=1) 
				count if temp==1
				assert r(N) == 16146

			
				foreach s in   1 {		
				*** REGRESSION *** 
				*di "${rtype`s'} `Y'countpc  ${treatafter} one if  year>=`b' & temp==0 & cleansample==1 ${weight`Y'`b'} , a(${a`s'} ${controlbefore}  ${window} ${placebocontafter1} ${placebobefore1} ${windowpl1} ${placebocontafter2} ${placebobefore2} ${windowpl2} ${controlf}  ${control}  )  cluster(statenum)"
				*${rtype`s'} `Y'countpc  ${treatafter} one if  year>=`b'  & temp==0 & cleansample==1 ${weight`Y'`b'} , a(${a`s'} ${controlbefore}  ${window} ${placebocontafter1} ${placebobefore1} ${windowpl1} ${placebocontafter2} ${placebobefore2} ${windowpl2} ${controlf}  ${control}  )  cluster(statenum)
				*est save "${estimates}forplacebofig_mec_true`counter'`limitsample'`limittreat'_`s'_`Y'_`b'_w`w'_`temp1'_`temp2'_${Tmax}${Tmin}", replace 


				local tmax = 16
				*Alternatively, we can use the original estimated values for the true window
				est use "${estimates}forplacebofig_mec_true`counter'`limitsample'`limittreat'_`s'_`Y'_`b'_w`w'_`temp1'_`temp2'_${Tmax}${Tmin}" 
				*est use  "${estimates}Table1aafterqcew`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"

 				local dof = e(df_r)
				local numberobs = e(N)
				count if fedincrease!=1 & overallcountgr>0 & wagebins==300		// The wage bin does not matter.
				local numberevent = r(N)

				
				local denominator = (1/(1+(`tmax'/4)))
				local wmax = `truewmax'
				local wmin = `truewmin'
				
				
				
			 				
			********************************************************************
			****************          Table           **************************
			********************************************************************

				est use "${estimates}forplacebofig_mec_true`counter'`limitsample'`limittreat'_`s'_`Y'_`b'_w`w'_`temp1'_`temp2'_${Tmax}${Tmin}" 
			
			
			
			lincomestadd (${below_full})*(4*`denominator')*(1/${E})							, statname(below_E) 				// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			lincomestadd (${above_full})*(4*`denominator')*(1/${E})							, statname(above_E)					// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
*			lincomestadd (${below_full} + ${above_full})*(4*`denominator')*(1/${E}) 		, statname(chemp_E)					// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5. 
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

			

			
			eststo ta_`Y'_`b'_${label`s'}_`counter'

			
		}
			
	}
 }
*


			est use "${estimates}Table1_TWFE_estimates_full"
			eststo ta_overall_1979_TWFE



Table_create TWFE  TWFE_2  , group(overall) tablename(measurement_error_corrected) footnotes(Affected share is `affshoverall')
