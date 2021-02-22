
*This do file is created by DC on 05/09/2016.
********************************************************************************
***************************   Table 2      *************************************
********************************************************************************



		
**** DEFINE REGRESSION COMMAND AND CONTROLS *****

 
foreach s in 0a 0 1 1b 3 4 5 6 7 8 9 10 11{
	global rtype`s' reghdfe
}
global rtype2 regifewtab


global a1  i.wagebinstate i.wagequarterdate  // distributional TWFE

global a4 i.wagebinstate##c.quarterdate i.wagequarterdate // state-trends
global a5 i.wagebinstate i.wagequarterdate##i.division // div-period
global a6 i.wagebinstate##c.quarterdate i.wagequarterdate##i.division // div-period, statetrends
global a10 i.wagebinstate##c.quarterdate i.wagebinstate##c.quarterdatesq i.wagequarterdate // state-quadratic-trends
global a11 i.wagebinstate##c.quarterdate i.wagebinstate##c.quarterdatesq i.wagequarterdate##i.division // state-quadratic-trends



 

global label1 TWFE
global label4 ST
global label5 DP
global label6 ST_DP
global label10 STQ
global label11 STQ_DP

use "${data}qcew_state_overall_counts_2016.dta", clear
rename statefips statenum
gen quarterdate = yq(year, quarter)
tempfile qcew
save `qcew'


treatmentcontrolwindows

treatmentdefinition

merge m:1 statenum quarterdate using `qcew',assert(3) nogenerate

gen femalecount = (count-gendercount)
gen femalecountall = (countall-gendercountall)
g BHcount = (blackcount + hispaniccount)
g BHcountall = (blackcountall + hispaniccountall)


foreach Y in  teen   HSL    BH  HSL40 HSD HSD40 female { 

	replace `Y'count = `Y'count*emp/countall  if emp!=0
	cap drop `Y'countpc
	gen `Y'countpc=`Y'count/`Y'pop
	cap drop `Y'countpcall
	gen `Y'countpcall = `Y'countall*(emp/countall)/`Y'pop   if emp!=0

}
abovebelowbunch

abovebelowfull


*
********************************************************************************
***********************           Weights           ****************************
********************************************************************************

cap drop wt 
foreach Y in   overall teen  HSL female BH HSL40 HSD HSD40 { 
	foreach b of numlist 1995 1979{
					global weight`Y'`b' "[aw=wt`Y'`b']" 
					}
}  
*


foreach Y in   overall teen  HSL female BH  HSL40 HSD HSD40 {
		foreach H in FTE{
	foreach b of numlist /*1995*/ 1979{
					global weight`Y'`H'`b' "[aw=wt`Y'`b']" 
					}
	}  
}
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

foreach Y in  teen   HSL  female  BH  HSL40 HSD HSD40 { 

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


******************************************************************************** 
**********************       REGRESSIONS AND FIGURES        ********************
********************************************************************************
xtset wagebinstate quarterdate
compress

foreach Y in   /*overall*/ HSD /*overallFTE*/ teen  /*teenFTE*/ HSL /*HSLFTE*/ female /*femaleFTE*/ BH /*BHFTE*/ /*HSL40 HSD40*/ { 
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
				/*
				${rtype`s'} `Y'countpc  ${treatafter} one if  year>=`b'  & cleansample==1 & wt`Y'`b'>0  ${weight`Y'`b'} , a(${a`s'}  ${control} ${controlbefore} ${controlf} ${window} )  cluster(statenum)
				est save "${estimates}Table4aqcewafter`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}", replace 
				*/
				est use "${estimates}Table4aqcewafter`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
				
 				local dof = e(df_r)
				local numberobs = e(N)
				count if fedincrease!=1 & overallcountgr>0 & wagebins==300		// The wage bin does not matter.
				local numberevent = r(N)
				
				
				local denominator = (1/(1+(${Tmax}/4)))
				

			********************************************************************
			****************          Table           **************************
			********************************************************************
			est use "${estimates}Table4aqcewafter`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"

			
			
			lincomestadd (${below_full})*(4*`denominator')*(1/${E})							, statname(below_E) 				// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			lincomestadd (${above_full})*(4*`denominator')*(1/${E})							, statname(above_E)					// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			lincomestadd ((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B} 	, statname(bunching_E)				// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5. 			
			lincomestadd (${below_full} + ${above_full})*(4*`denominator')*${C}				, statname(bunchelas_E)				// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			lincomestadd ((((${belowWB_full}+${aboveWB_full})*(4*`denominator')/$EWB ) - ///
			(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B})))		, statname(WB_E)					// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			nlcomestadd (((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B}) / ///
			(((${belowWB_full}+${aboveWB_full})*(4*`denominator')/$EWB ) - ///
			(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B}))			, statname(labordem_E) dof(`dof') 
			

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

			
			eststo ta_gr_`b'_`Y'
		}
			
	}
 }
*


qui reg all
eststo blankest 

do "${dofiles}CK_groups_regressions_longfigure_QJE.do"

Table_create HSD  HSL  teen female BH first_ fourth_ fifth_ , group(gr) tablename(Table_2) 
