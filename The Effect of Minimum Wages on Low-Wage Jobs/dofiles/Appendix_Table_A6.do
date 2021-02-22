
*This do file is created by DC on 03/30/2016
********************************************************************************
***************************   Table 1      *************************************
********************************************************************************
*Alternative time windows


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


local alt_tminvals 12 12 12 20 4 4
local alt_tmaxvals 16 8  24 16 16 4

local n : word count `alt_tminvals'

forvalues i = 1/`n' {
	local tmin : word `i' of `alt_tminvals'
	local tmax : word `i' of `alt_tmaxvals'
treatmentcontrolwindows, tmax(`tmax') tmin(`tmin')

treatmentdefinition


if `tmax'>16 | `tmin'>16{

if `tmax'>`tmin'{
local endpoint `tmax'
}
else{
local endpoint `tmin'
}

forval j = 20(4)`endpoint'{
	foreach k in  m4 m3 m2 m1 p0 p1 p2 p3 p4 {
		cap drop F`j'treat_`k'
		cap drop L`j'treat_`k'
		
		g F`j'treat_`k' = F`j'.treat_`k'
 		g L`j'treat_`k' = L`j'.treat_`k'

		
		replace F`j'treat_`k' = 0 if F`j'treat_`k' == .
 		replace L`j'treat_`k' = 0 if L`j'treat_`k' == .
		
		
		
	}
} 

*


}
*
compress
foreach pm in p m{
	foreach num of numlist 0(1)4{
		global create_windowvar`pm'`num'  "0"
			foreach num2 of numlist 4(4)`tmin'{
			global create_windowvar`pm'`num' "${create_windowvar`pm'`num'} + F`num2'treat_`pm'`num'"
		}
	}
}
*
foreach pm in p m{
	foreach num of numlist 0(1)4{
			foreach num2 of numlist 4(4)`tmax'{
			global create_windowvar`pm'`num' "${create_windowvar`pm'`num'} + L`num2'treat_`pm'`num'"
		}
	}
}
*
foreach pm in p m{
	foreach num of numlist 0(1)4{
			global create_windowvar`pm'`num' "${create_windowvar`pm'`num'} + treat_`pm'`num'"
		}
}
*

foreach pm in p m{
	foreach num of numlist 1(1)4{
		cap drop window_`pm'`num' 
		gen window_`pm'`num' =  ${create_windowvar`pm'`num'}   
		}
}

local pm p
local num 0
cap drop window_`pm'`num' 
gen window_`pm'`num' =  ${create_windowvar`pm'`num'} 

local tfull = `tmin' + `tmax'

if `tfull'>28{
local clustertype "vce(robust)"

}
else{
local clustertype "cluster(statenum)"
}
di "`clustertype'"



merge m:1 statenum quarterdate using `qcew',assert(3) nogenerate
replace count = count*emp/countall if emp!=0
cap drop overallcountpc
gen overallcountpc=count/population 
replace overallcountpcall = emp/population  if emp!=0

abovebelowbunch, tmax(`tmax') tmin(`tmin')

abovebelowfull, tmax(`tmax')

********************************************************************************
***********************           Weights           ****************************
********************************************************************************

cap drop wt 
foreach Y in   overall  { 
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
foreach b in  1979 {
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
				
				abovebelowWBbunch,wagemult(${wagemult})  tmax(`tmax') tmin(`tmin')
				abovebelowWBfull, tmax(`tmax') 
				
			
				foreach s in /*0*/  1 {		
				*** REGRESSION *** 
				
				${rtype`s'} `Y'countpc  ${treatbefore} one if  year>=`b'  & cleansample==1 ${if`s'}  ${weight`Y'`b'} , a(${a`s'}  ${control} ${controlafter} ${controlf} ${window} )  `clustertype'
				est save "${estimates}Table_alternative_timewindows_`tmax'_`tmin'_pre", replace 
				di "${rtype`s'} `Y'countpc  ${treatafter} one if  year>=`b'  & cleansample==1 ${if`s'}  ${weight`Y'`b'} , a(${a`s'}  ${control} ${controlbefore} ${controlf} ${window} )  `clustertype'"
				${rtype`s'} `Y'countpc  ${treatafter} one if  year>=`b'  & cleansample==1 ${if`s'}  ${weight`Y'`b'} , a(${a`s'}  ${control} ${controlbefore} ${controlf} ${window} )  `clustertype'
				est save "${estimates}Table_alternative_timewindows_`tmax'_`tmin'", replace 
				est use "${estimates}Table_alternative_timewindows_`tmax'_`tmin'"
 				local dof = e(df_r)
				local numberobs = e(N)
				count if fedincrease!=1 & overallcountgr>0 & wagebins==300		// The wage bin does not matter.
				local numberevent = r(N)
				
				
				local denominator = (1/(1+(`tmax'/4)))
				

				est use "${estimates}Table_alternative_timewindows_`tmax'_`tmin'"

			
			
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

			
			eststo ta_`Y'_`b'__`tmin'`tmax'

			
		}
			
	}
 }
 }
*

Table_create  _1216  _128 _1224 _2016 _416  _44 , group(overall) tablename(Table_A6) footnotes(Affected share is `affshoverall')

