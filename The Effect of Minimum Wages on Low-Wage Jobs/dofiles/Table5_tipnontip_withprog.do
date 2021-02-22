
*This do file is created by DC on 03/30/2016



********************************************************************************
********************************************************************************
********************************************************************************


		
**** DEFINE REGRESSION COMMAND AND CONTROLS *****

 
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

global if13 "& wagebins<=1500"
global if14 "& wagebins<=2000"

use "${data}qcew_multiplier.dta", clear
keep statenum quarterdate multiplier
assert multiplier != 0
tempfile qcew
save `qcew'

if "`1'"=="notip"{
	local tipornotip = "notipstates"
}
else{
	if "`1'"=="tip"{
		local tipornotip = "tipstates"
	}
	else{ 
	di "The option has to be tip or notip."
	assert 2==3
	}
}
treatmentcontrolwindows

treatmentdefinition

abovebelowbunch

abovebelowfull

merge m:1 statenum quarterdate using `qcew',assert(3) nogenerate

replace count = count*multiplier
replace overallcountpc=overallcountpc*multiplier
replace overallcountpcall = overallcountpcall * multiplier


********************************************************************************
*****************        States by tip credit            ***********************
********************************************************************************

xtset wagebinstate quarterdate

gen byte notipstates = (statenum==2 | statenum==6 | statenum==27 | statenum==30 | statenum==32 | statenum==41 | statenum==53) 
gen byte tipstates = 1-notipstates 




xtset wagebinstate quarterdate
forval k = 0/4  {
	cap drop _treat_p`k'
	g _treat_p`k'=0
	* replace _treat_p`k' = 1 if (  D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))	
	*	replace _treat_p`k' = 1 if (D.MW_real > 0  & D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))
		replace _treat_p`k' = 1 if $ifclause &  (wageM25>=(MW_realM25+`k' - 0) &  wageM25 < (MW_realM25 +1 +`k' - 0)) & overallcountgroup>0 & fedincrease!=1 & `tipornotip'!=1
	*	replace _treat_p`k' = 1 if (D.MW_real>=0.75  & D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))
	 g byte alttreat_p`k' = ( _treat_p`k' +  L._treat_p`k' + L2._treat_p`k' + L3._treat_p`k')
	replace treat_p`k' = treat_p`k' - alttreat_p`k' 
	cap drop _treat_p`k'
	assert treat_p`k'>-1 if treat_p`k'!=.
}

forval k = 1/4  {
	cap drop _treat_m`k'
	g _treat_m`k'=0
	*	replace _treat_m`k' = 1 if ( D.MW_real<.& D.MW>0) & (wageM25<(MW_realM25 - `k' + 1)  &  wageM25 >= (MW_realM25 -`k' ))
	*	replace _treat_m`k' = 1 if(D.MW_real> 0  & D.MW_real<.& D.MW>0) & (wageM25<(MW_realM25 - `k' + 1)  &  wageM25 >= (MW_realM25 -`k' ))
		replace _treat_m`k' = 1 if $ifclause & (wageM25<(MW_realM25 - `k' + 1 -0)  &  wageM25 >= (MW_realM25 -`k' -0 ))  & overallcountgroup>0 & fedincrease!=1 & `tipornotip'!=1
	*	replace _treat_m`k' = 1 if(D.MW_real>=0.75  & D.MW_real<.& D.MW>0) & (wageM25<(MW_realM25 - `k' + 1)  &  wageM25 >= (MW_realM25 -`k' ))
	g byte alttreat_m`k' = ( _treat_m`k' + L._treat_m`k' + L2._treat_m`k' + L3._treat_m`k' )
	replace treat_m`k' = treat_m`k' - alttreat_m`k' 
	cap drop _treat_m`k'
	assert treat_m`k'>-1 if treat_m`k'!=.
}  
*


foreach j in  4 8 12 16 {
	foreach k in m4 m3 m2 m1 p0 p1 p2 p3 p4 {
		cap drop F`j'alttreat_`k'
		cap drop L`j'alttreat_`k'
		
		g byte F`j'alttreat_`k' = F`j'.alttreat_`k'
 		g byte L`j'alttreat_`k' = L`j'.alttreat_`k'
		
		replace F`j'alttreat_`k' = 0 if F`j'alttreat_`k'  == . 
 		replace L`j'alttreat_`k' = 0 if  L`j'alttreat_`k' == .
		
		
		
		replace F`j'treat_`k' = F`j'treat_`k' - F`j'alttreat_`k' 
		replace L`j'treat_`k' = L`j'treat_`k' - L`j'alttreat_`k'
		assert F`j'treat_`k'>-1 if F`j'treat_`k'!=.
		assert L`j'treat_`k'>-1 if L`j'treat_`k'!=.
		
	}
} 
*

foreach pm in p m{
	foreach num of numlist 1(1)4{
		gen byte altwindow_`pm'`num' =  F12alttreat_`pm'`num' +  F8alttreat_`pm'`num' +  F4alttreat_`pm'`num' + alttreat_`pm'`num' + L4alttreat_`pm'`num'  + L8alttreat_`pm'`num'  + L12alttreat_`pm'`num'  + L16alttreat_`pm'`num'  
		replace window_`pm'`num' = window_`pm'`num' - altwindow_`pm'`num'
		assert window_`pm'`num'>-1 if window_`pm'`num'!=.
	}
}

local pm p
local num 0
gen altwindow_`pm'`num' =  F12alttreat_`pm'`num' +  F8alttreat_`pm'`num' +  F4alttreat_`pm'`num' + alttreat_`pm'`num' + L4alttreat_`pm'`num'  + L8alttreat_`pm'`num'  + L12alttreat_`pm'`num'  + L16alttreat_`pm'`num'  
replace window_`pm'`num' = window_`pm'`num' - altwindow_`pm'`num'
assert window_`pm'`num'>-1 if window_`pm'`num'!=.
compress
*Define control globals 

local tmax 16
local tmin 12
local wmax 4
local wmin 4

global altcontrol = " "

forval k = 0(4)`tmax'   {
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmin'(1)1       {
		global altcontrol = "$altcontrol i.one#c.`K'alttreat_m`j'"
	}
	foreach j of numlist  0(1)`wmax' {
		global altcontrol = "$altcontrol i.one#c.`K'alttreat_p`j'"
	}
	}
* 
forval k = -`tmin'(4)-4   {
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmin'(1)1       {
		global altcontrol = "$altcontrol i.one#c.`K'alttreat_m`j'"
	}
	foreach j of numlist  0(1)`wmax' {
		global altcontrol = "$altcontrol i.one#c.`K'alttreat_p`j'"
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

foreach Y in   overall /*overallFTE*/ /*teen  teenFTE HSL /*HSLFTE*/ female /*femaleFTE*/ BH /*BHFTE*/ */ { 
foreach b in   /* 1995 */ 1979 {
					***  define MW change ***
				sum DMW_real if fedincrease!=1 & overallcountgr>0 & year>=`b' & cleansample ==1 & `tipornotip'==1 ${weight`Y'`b'}
				local mwc = r(mean)
				sum MW_real if  F.fedincrease!=1 & F.overallcountgr>0 & year>=`b' & cleansample ==1   & `tipornotip'==1 ${weight`Y'`b'}
				local mw = r(mean)
				local mwpc = `mwc'/`mw'
				local mwpc2 : di  %04.3f `mwpc'

				sum `Y'countpcall if ((F.fedincrease != 1 & F.fedincrease != . & F.overallcountgr>0  & F.overallcountgr!=.  & `tipornotip'==1 )| ///
				(F2.fedincrease != 1 & F2.fedincrease != . & F2.overallcountgr>0   & F2.overallcountgr!=.  & `tipornotip'==1 ) |  ///
				(F3.fedincrease != 1 & F3.fedincrease != . & F3.overallcountgr>0   & F3.overallcountgr!=.  & `tipornotip'==1 ) | ///
				(F4.fedincrease != 1 & F4.fedincrease != . & F4.overallcountgr>0   & F4.overallcountgr!=.  & `tipornotip'==1 ) ) & year>=`b' ///
				& cleansample ==1   ${weight`Y'`b'} // CHECK THIS  2/2/16
				local epop = r(mean)
				global E = `epop'
					
				global C = 1/(`epop'*`mwpc')
			
			
				*Below share

				
				sum overallcountpcrsum if  ///
				(F.fedincrease != 1 & F.fedincrease != . & F.overallcountgr>0  & F.overallcountgr!=.   & (wagebins+25)/100==F1MW_realM25  & `tipornotip'==1) | ///
				 (F2.fedincrease != 1 & F2.fedincrease != . & F2.overallcountgr>0   & F2.overallcountgr!=.  & (wagebins+25)/100==F2MW_realM25  & `tipornotip'==1) | ///
				(F3.fedincrease != 1 & F3.fedincrease != . & F3.overallcountgr>0   & F3.overallcountgr!=.    & (wagebins+25)/100==F3MW_realM25  & `tipornotip'==1) | ///
				 (F4.fedincrease != 1 & F4.fedincrease != . & F4.overallcountgr>0   & F4.overallcountgr!=.  & (wagebins+25)/100==F4MW_realM25  & `tipornotip'==1 ) ///
				& year>=`b' & cleansample ==1	  ${weight`Y'`b'}			
				
				global B = r(mean)/${E}
				local belsh2 : di  %04.3f $B

				*Affected share
				
				sum `Y'countpcrsum if ( F.fedincrease != 1 & F.fedincrease != . & F.overallcountgr>0  & F.overallcountgr!=.   & (wagebins)/100==MW_realM25  & `tipornotip'==1) | ///
				 (F2.fedincrease != 1 & F2.fedincrease != . & F2.overallcountgr>0   & F2.overallcountgr!=.  & (wagebins)/100==MW_realM25  & `tipornotip'==1 ) | ///
				(F3.fedincrease != 1 & F3.fedincrease != . & F3.overallcountgr>0   & F3.overallcountgr!=.   & (wagebins)/100==MW_realM25  & `tipornotip'==1) | ///
				 (F4.fedincrease != 1 & F4.fedincrease != . & F4.overallcountgr>0   & F4.overallcountgr!=. & (wagebins)/100==MW_realM25  & `tipornotip'==1) ///
				& year>=1979 & cleansample ==1	  ${weight`Y'`b'}			
				
				global AS`Y' = $B - (r(mean)/${E})
				local affsh`Y' : di  %04.3f ${AS`Y'}
				
				
				
				****************************************************************
				*************    Wagebill related globals       ****************
				****************************************************************
				sum overallWBpcrsumFH if ( F.fedincrease != 1 & F.fedincrease != . & F.overallcountgr>0  & F.overallcountgr!=.  & (wagebins+25)/100==F1MW_realM25  & `tipornotip'==1) | ///
				 (F2.fedincrease != 1 & F2.fedincrease != . & F2.overallcountgr>0   & F2.overallcountgr!=. & (wagebins+25)/100==F2MW_realM25  & `tipornotip'==1) | ///
				( F3.fedincrease != 1 & F3.fedincrease != . & F3.overallcountgr>0   & F3.overallcountgr!=.  & (wagebins+25)/100==F3MW_realM25 & `tipornotip'==1) | ///
				 (F4.fedincrease != 1 & F4.fedincrease != . & F4.overallcountgr>0   & F4.overallcountgr!=. & (wagebins+25)/100==F4MW_realM25  & `tipornotip'==1) ///
				& year>=`b' & cleansample ==1	  ${weight`Y'`b'}			
	
				global EWB = r(mean)
				local EWB2 : di  %04.3f $EWB
				cap drop tempweight	
				global CWB = 1/(${EWB}*`mwpc')

				sum wagebins if treat_p0 == 1  & year>=`b' & cleansample ==1   & `tipornotip'==1 ${weight`Y'`b'}, meanonly
				global wagemult = r(mean)/100
				
				
				*We have to do this here because multipliers are calculated in the loop.
				
				abovebelowWBbunch,wagemult(${wagemult})
				abovebelowWBfull
				
			
				foreach s in /*0*/ 1 /*4 5 6 10 11 12 13 14*/ /*0a 0b  4  5 6*/     {		
				*** REGRESSION *** 
				*${rtype`s'} `Y'countpc  ${treatbefore} one if  year>=`b'  & cleansample==1  ${weight`Y'`b'} , a(${a`s'}  ${control} ${controlafter} ${controlf} ${window} )  cluster(statenum)
				*est save "${estimates}Table1abefore`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}", replace 
				*${rtype`s'} `Y'countpc  ${treatafter} one if  year>=`b'  & cleansample==1 ${if`s'}  ${weight`Y'`b'} , a(${a`s'} ${altcontrol} ${control} ${controlbefore} ${controlf} ${window} )  cluster(statenum)
				*est save "${estimates}Table5a_`tipornotip'_`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_`temp3'_`temp4'_${Tmax}${Tmin}", replace 

				est use "${estimates}Table5a_`tipornotip'_`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_`temp3'_`temp4'_${Tmax}${Tmin}"
 				local dof = e(df_r)
				local numberobs = e(N)
				count if fedincrease!=1 & overallcountgr>0  & wagebins==300 & `tipornotip'==1
				local numberevent = r(N)
				
				
				local denominator = (1/(1+(${Tmax}/4)))

				
				
			********************************************************************
			****************          Table           **************************
			********************************************************************

			est use "${estimates}Table5a_`tipornotip'_`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_`temp3'_`temp4'_${Tmax}${Tmin}"
			
			
			
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

			if `s' == 12 | `s'==13 | `s'==14 {
			estadd local stp		= "Y"
			
			}
			
			if "`tipornotip'" == "notipstates"{
			local eventname "nt"
			}
			if  "`tipornotip'" == "tipstates"{
			local eventname "ti"			
			}
			eststo ta_`Y'_`b'_ed`eventname'


			
		}
			
	}
 }
*

