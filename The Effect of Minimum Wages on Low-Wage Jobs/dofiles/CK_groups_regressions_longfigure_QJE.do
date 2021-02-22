
*This do file is created by DC on 05/09/2016.
********************************************************************************
***************************   Table 1      *************************************
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


local truewmin = 4
local truewmax = 4
local placebo1wmax = 13
local prevwmax = `placebo1wmax'
local placebo2wmax = 16


treatmentcontrolwindows


placebowindows1, truewmax(`truewmax') wmax(`placebo1wmax')

placebowindows2, prevwmax(`prevwmax') wmax(`placebo2wmax')


use "${data}CK_groups.dta", clear
merge 1:1 statenum quarterdate wagebins using "${data}state_panels_with3quant1979.dta", nogenerate assert(2 3) 
foreach vv in first_  fourth_ fifth_  /*second_ third_*/ {
replace `vv'count=0 if `vv'count==.
bys statenum quarterdate: egen _`vv'pop = max(`vv'pop)
replace `vv'pop = _`vv'pop if `vv'pop == .
bys statenum quarterdate: egen `vv'countall = total(`vv'count)

}
*
merge m:1 statenum quarterdate using `qcew',assert(3) nogenerate

foreach Y in  first_  fourth_ fifth_  { 

	replace `Y'count = `Y'count*emp/countall  if emp!=0
	cap drop `Y'countpc
	gen `Y'countpc=`Y'count/`Y'pop
	cap drop `Y'countpcall
	cap drop `Y'countall 
	bys statenum quarterdate: egen `Y'countall = total(`Y'count)
	bys statenum quarterdate: egen `Y'countpcall = total(`Y'countpc)	
}
*
*Sixth is first + fourth
cap drop sixth_*
gen sixth_count = first_count + fourth_count
gen sixth_pop = first_pop + fourth_pop
gen sixth_countall = first_countall + fourth_countall
gen sixth_countpc = (first_count + fourth_count)/(first_pop + fourth_pop)
gen sixth_countpcall = (first_countall + fourth_countall)/(first_pop + fourth_pop)


placebosamplecorr1, wmax(`placebo1wmax') truewmax(`truewmax') 
placebosamplecorr2, wmax(`placebo2wmax') prevwmax(`placebo1wmax')


*
********************************************************************************
***********************           Weights           ****************************
********************************************************************************

cap drop wt 
foreach Y in   first_ fourth_ fifth_  sixth_   { 
	foreach b of numlist 1995 1979{
					global weight`Y'`b' "[aw=`Y'pop]" 
					}
}  
*

*

if "$limitsample" == "hourly"{
local limitsample "_hourly"
}
else {
local limitsample ""
}

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

foreach Y in  first_ fourth_ fifth_  sixth_  /*second_ third_*/ { 

cap drop `Y'WB*
g `Y'WBFH = wagebins*`Y'count/(100)
g `Y'WBpcFH = wagebins*`Y'count/(100*`Y'pop)

cap drop `Y'WBpcrsumFH
g `Y'WBpcrsumFH=0
replace `Y'WBpcrsumFH = `Y'WBpcFH if wagebins==100
replace `Y'WBpcrsumFH = `Y'WBpcrsumFH[_n-1] + `Y'WBpcFH if wagebins>100
	
cap drop `Y'countpcrsum
g `Y'countpcrsum=0
replace `Y'countpcrsum = `Y'countpc if wagebins==100
replace `Y'countpcrsum = `Y'countpcrsum[_n-1] + `Y'countpc if wagebins>100
}
*

******************************************************************************** 
**********************       REGRESSIONS AND FIGURES        ********************
********************************************************************************
xtset wagebinstate quarterdate

abovebelowbunch
abovebelowfull

compress

foreach Y in   first_  fourth_ fifth_  sixth_  { 
foreach b in   1979 {
					***  define MW change ***
				sum DMW_real if fedincrease!=1 & overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`Y'`b'}
				local mwc = r(mean)
				sum MW_real if  (F.fedincrease != 1 & F.fedincrease != . & F.overallcountgr>0 & F.overallcountgr!=. ) & year>=`b' & cleansample ==1   ${weight`Y'`b'}
				local mw = r(mean)
				local mwpc = `mwc'/`mw'
				global MWPC = `mwpc'
				local mwpc2 : di  %04.3f `mwpc'

				sum `Y'countpcall if ((F.fedincrease != 1 & F.fedincrease != . & F.overallcountgr>0 & F.overallcountgr!=. ) | ///
				(F2.fedincrease != 1 & F2.overallcountgr>0 & F2.fedincrease != . & F2.overallcountgr!=. ) ///
				| (F3.fedincrease != 1 & F3.overallcountgr>0 & F3.fedincrease != . & F3.overallcountgr!=. ) ///
				| (F4.fedincrease != 1 & F4.overallcountgr>0 & F4.fedincrease != . & F4.overallcountgr!=. ) ) & year>=`b' & cleansample ==1 ///
				 ${weight`Y'`b'}		

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
				
				global AS`Y' = $B - (r(mean)/${E})
				local affsh`Y' : di  %04.3f ${AS`Y'}
				
				
				
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
				

				sum wagebins if treat_p0 == 1  & year>=`b' & cleansample ==1   ${weight`Y'`b'}, meanonly
				global wagemult = r(mean)/100

				*We have to do this here because multipliers are calculated in the loop.
				abovebelowWBbunch,wagemult(${wagemult})
				abovebelowWBfull
				
				
				
				
				
				
				
				**  ADS
			
			
				foreach s in 1 {		
												
				*** REGRESSION *** 
				
				di "${rtype`s'} `Y'countpc  ${treatafter}  if  year>=`b'  & cleansample==1 ${weight`Y'`b'} , a(${a`s'} ${controlbefore}  ${window} ${placebocontafter1} ${placebobefore1} ${windowpl1} ${placebocontafter2} ${placebobefore2} ${windowpl2} ${controlf}  ${control}  )  cluster(statenum)"
				${rtype`s'} `Y'countpc  ${treatafter}  if  year>=`b'  & cleansample==1 ${weight`Y'`b'} , a(${a`s'} ${controlbefore}  ${window} ${placebocontafter1} ${placebobefore1} ${windowpl1} ${placebocontafter2} ${placebobefore2} ${windowpl2} ${controlf}  ${control}  )  cluster(statenum)
				est save "${estimates}CKforplacebofig_true`limitsample'`limittreat'_`s'_`Y'_`b'_w`w'_`temp1'_`temp2'_${Tmax}${Tmin}", replace 

				di "${rtype`s'} `Y'countpc ${placeboafter1} if  year>=`b'  & cleansample==1 ${weight`Y'`b'} , a(${a`s'} ${controlbefore}  ${controlafter}  ${window}  ${placebocontafter2} ${windowpl2}  ${placebobefore2} ${placebobefore1} ${windowpl1} ${controlf}  ${control}  )  cluster(statenum)"
				${rtype`s'} `Y'countpc ${placeboafter1} if  year>=`b'  & cleansample==1 ${weight`Y'`b'} , a(${a`s'} ${controlbefore}  ${controlafter}  ${window}  ${placebocontafter2} ${windowpl2}  ${placebobefore2} ${placebobefore1} ${windowpl1} ${controlf}  ${control}  )  cluster(statenum)
				est save "${estimates}CKforplacebofig_pl1`limitsample'`limittreat'_`s'_`Y'_`b'_w`w'_`temp1'_`temp2'_${Tmax}${Tmin}", replace 
				
				di "${rtype`s'} `Y'countpc ${placeboafter2} if  year>=`b'  & cleansample==1 ${weight`Y'`b'} , a(${a`s'} ${controlafter}  ${controlbefore}  ${window} ${placebocontafter1}  ${placebobefore1} ${windowpl1} ${placebobefore2} ${windowpl2} ${controlf}  ${control}  )  cluster(statenum)"
				${rtype`s'} `Y'countpc ${placeboafter2} if  year>=`b'  & cleansample==1 ${weight`Y'`b'} , a(${a`s'} ${controlafter}  ${controlbefore}  ${window} ${placebocontafter1}  ${placebobefore1} ${windowpl1} ${placebobefore2} ${windowpl2} ${controlf}  ${control}  )  cluster(statenum)
				est save "${estimates}CKforplacebofig_pl2`limitsample'`limittreat'_`s'_`Y'_`b'_w`w'_`temp1'_`temp2'_${Tmax}${Tmin}", replace 
				
			
			********************************************************************
			****************          Table                  *******************
			********************************************************************
			
				est use "${estimates}CKforplacebofig_true`limitsample'`limittreat'_`s'_`Y'_`b'_w`w'_`temp1'_`temp2'_${Tmax}${Tmin}"
 				local dof = e(df_r)
				local numberobs = e(N)
				count if fedincrease!=1 & overallcountgr>0 & wagebins==300		// The wage bin does not matter.
				local numberevent = r(N)

				local wmax = `truewmax'
				local wmin = `truewmin'
				
				local tmax = 16
				
				local denominator = (1/(1+(`tmax'/4)))


			
			
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

			
			
			
			eststo ta_gr_`b'_`Y'
			
			
		}
			
	}
 }
*


