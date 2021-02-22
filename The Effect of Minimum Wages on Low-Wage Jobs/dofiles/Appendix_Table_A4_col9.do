
*This do file is created by DC on 03/30/2016


		
**** DEFINE REGRESSION COMMAND AND CONTROLS *****

 
foreach s in 0a 1 {
	global rtype`s' reghdfe
}


global a1  i.wagebinstate i.wagequarterdate  // distributional TWFE
 

global label1 TWFE

local w_upper = `1'
local w_lower = `2'
local w_increment = `3'


use "${data}qcew_state_overall_counts_2016.dta", clear
rename statefips statenum
gen quarterdate = yq(year, quarter)
tempfile qcew
save `qcew'


treatmentcontrolwindows

finertreatmentwindow
treatmentdefinition																// This only calls the data.

merge m:1 statenum quarterdate using `qcew',assert(3) nogenerate
replace count = count*emp/countall if emp!=0
cap drop overallcountpc
gen overallcountpc=count/population 
replace overallcountpcall = emp/population  if emp!=0


finertreatmentdefinitionexact
finerabovebelowbunch, wmincent(`w_lower') wmaxcent(`w_upper') wincrement(`w_increment')
abovebelowfull

********************************************************************************
***********************           Weights           ****************************
********************************************************************************

cap drop wt 
foreach Y in   overall  { 
	foreach b of numlist 1995 1979{
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




local limittreat "_stateonly"

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
cap drop temp
	gen temp = 1* (wagebins==3000) + 2*1* (wagebins==2975)+ 3*1* (wagebins==2950)
	cap drop clustervar
	egen clustervar = group(statenum temp)

foreach Y in   overall  { 
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

				
				sum overallcountpcrsum if ( F.fedincrease!=1 & F.overallcountgr>0  & (wagebins+25)/100==F1MW_realM25) | ///
				 ( F2.fedincrease!=1 & F2.overallcountgr>0 & (wagebins+25)/100==F2MW_realM25 ) | ///
				( F3.fedincrease!=1 & F3.overallcountgr>0  & (wagebins+25)/100==F3MW_realM25) | ///
				 ( F4.fedincrease!=1 & F4.overallcountgr>0 & (wagebins+25)/100==F4MW_realM25 ) ///
				& year>=`b' & cleansample ==1	  ${weight`Y'`b'}			
				
				global B = r(mean)/${E}
				local belsh2 : di  %04.3f $B

				*Affected share
				
				sum `Y'countpcrsum if ( F.fedincrease!=1 & F.overallcountgr>0  & (wagebins)/100==MW_realM25) | ///
				 ( F2.fedincrease!=1 & F2.overallcountgr>0 & (wagebins)/100==MW_realM25 ) | ///
				( F3.fedincrease!=1 & F3.overallcountgr>0  & (wagebins)/100==MW_realM25) | ///
				 ( F4.fedincrease!=1 & F4.overallcountgr>0 & (wagebins)/100==MW_realM25 ) ///
				& year>=1979 & cleansample ==1	  ${weight`Y'`b'}			
				
				global AS`Y' = $B - (r(mean)/${E})
				local affsh`Y' : di  %04.3f ${AS`Y'}
				
				
				
				****************************************************************
				*************    Wagebill related globals       ****************
				****************************************************************

				sum overallWBpcrsumFH if ( F.fedincrease!=1 & F.overallcountgr>0  & (wagebins+25)/100==F1MW_realM25) | ///
				 ( F2.fedincrease!=1 & F2.overallcountgr>0 & (wagebins+25)/100==F2MW_realM25 ) | ///
				( F3.fedincrease!=1 & F3.overallcountgr>0  & (wagebins+25)/100==F3MW_realM25) | ///
				 ( F4.fedincrease!=1 & F4.overallcountgr>0 & (wagebins+25)/100==F4MW_realM25 ) ///
				& year>=`b' & cleansample ==1	  ${weight`Y'`b'}			
	
				global EWB = r(mean)
				local EWB2 : di  %04.3f $EWB
				cap drop tempweight	
				global CWB = 1/(${EWB}*`mwpc')

				sum wagebins if treat_p0 == 1  & year>=`b' & cleansample ==1   ${weight`Y'`b'}, meanonly
				global wagemult = r(mean)/100
				
				
				*We have to do this here because multipliers are calculated in the loop.
				
				finerabovebelowWBbunch,wagemult(${wagemult}) wmaxcent(`w_upper') wmincent(`w_lower')  wincrement(`w_increment')
				abovebelowWBfull

				di "Here!"
			
				foreach s in 1 {		
				*** REGRESSION *** 

				di "${rtype`s'} `Y'countpc  ${treatafterm400}  ${treatafterm200}   ${treatafterp0}   ${treatafterp250}     if  year>=`b'  & cleansample==1 ${if`s'}  ${weight`Y'`b'} , a(${a`s'}  ${control} ${controlf} ${window} ${controlbefore} )  cluster(clustervar)"
				${rtype`s'} `Y'countpc  ${treatafterm400}  ${treatafterm200}   ${treatafterp0}   ${treatafterp250}     if  year>=`b'  & cleansample==1 ${if`s'}  ${weight`Y'`b'} , a(${a`s'}  ${control} ${controlf} ${window} ${controlbefore} )   cluster(clustervar)
				est save "${estimates}finertreatmentall`w_increment'`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}", replace 
			
				
				est use "${estimates}finertreatmentall`w_increment'`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}" 

 				local dof = e(df_r)
				local numberobs = e(N)
				count if fedincrease!=1 & overallcountgr>0 & wagebins==300
				local numberevent = r(N)
				local denominator = 0.2
				


				
			********************************************************************
			****************          Table           **************************
			********************************************************************

				local wincrement = `w_increment'
				local multiplier = `wincrement'/25

			
			
			est use "${estimates}finertreatmentall`w_increment'`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
			
			
			
			lincomestadd (${below_full})*(`denominator')*(1/${E})*(`multiplier')							, statname(below_E) 				// Multiplying by some number because bins are in 25 cent precision. To average by time, dividing by 5.
			local est1b "`e(below_Eb)'"
			local est1se "`e(below_Ese)'"
			lincomestadd (${above_full})*(`denominator')*(1/${E})*(`multiplier')							, statname(above_E)					// Multiplying by some number because bins are in 25 cent precision. To average by time, dividing by 5.
			local est2b "`e(above_Eb)'"
			local est2se "`e(above_Ese)'"
			est use "${estimates}finertreatmentall`w_increment'`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
			
*			lincomestadd (${below_full} + ${above_full})*(4*`denominator')*(1/${E}) 		, statname(chemp_E)					// Multiplying by some number because bins are in 25 cent precision. To average by time, dividing by 5. 
			lincomestadd ((${below_full} + ${above_full})*(`denominator')*(`multiplier')*(1/${E}))/${B} 	, statname(bunching_E)				// Multiplying by some number because bins are in 25 cent precision. To average by time, dividing by 5. 			
			local est3b "`e(bunching_Eb)'"
			local est3se "`e(bunching_Ese)'"
			
			lincomestadd (${below_full} + ${above_full})*(`multiplier')*(`denominator')*${C}				, statname(bunchelas_E)				// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			local est4b "`e(bunchelas_Eb)'"
			local est4se "`e(bunchelas_Ese)'"

			est use "${estimates}finertreatmentall`w_increment'`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
			
			*Eexpression is too long.
			nlcom (a1:((${belowWB_full}+${aboveWB_full})*(`denominator')/$EWB )) ///
			(a2:(((${below_full} + ${above_full})*(`denominator')*(1/${E}))/${B})), post
			nlcomestadd  (_b[a1] - _b[a2])/(1+_b[a2])													, statname(WB_E) dof(`dof')	
			local est5b "`e(WB_Eb)'"
			local est5se "`e(WB_Ese)'"
			
			est use "${estimates}finertreatmentall`w_increment'`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
			

			nlcom (a1:((${belowWB_full}+${aboveWB_full})*(`denominator')/$EWB )) ///
			(a2:(((${below_full} + ${above_full})*(`denominator')*(1/${E}))/${B})), post
			
			nlcomestadd  (_b[a2])/((_b[a1] - _b[a2])/(1+_b[a2]))													, statname(labordem_E) dof(`dof')	
			local est6b "`e(labordem_Eb)'"
			local est6se "`e(labordem_Ese)'"
			
			
			reg one
			
			estadd local numobs = "`numberobs'"
			estadd local numevent = "`numberevent'"
			estadd local minwpc	= "`mwpc2'"
			estadd local belsh	= "`belsh2'"
			estadd local wagebinstate 		= "Y"			

			estadd local below_Eb = "`est1b'"
			estadd local below_Ese = "`est1se'"
			estadd local above_Eb = "`est2b'"
			estadd local above_Ese = "`est2se'"
			estadd local bunching_Eb = "`est3b'"
			estadd local bunching_Ese = "`est3se'"
			estadd local bunchelas_Eb = "`est4b'"
			estadd local bunchelas_Ese = "`est4se'"
			estadd local WB_Eb = "`est5b'"
			estadd local WB_Ese = "`est5se'"
			estadd local labordem_Eb = "`est6b'"
			estadd local labordem_Ese = "`est6se'"
			

			
			estadd local wagebinperiod 		= "Y"

			
			
			eststo ta_`Y'_`b'_${label`s'}

			
		}
			
	}
 }
*


Table_create  TWFE , group(overall) tablename(TableA4_col9) 
