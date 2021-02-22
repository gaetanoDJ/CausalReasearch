
*This do file is created by DC on 07/08/2016.
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

local wageupper = `1'


if "`wagelower'"==""{
local wagelower = 4
}
else{
local wagelower = `2'
}

scalar forcluster = `wagelower' + `wageupper' + 1
if `=forcluster'>10{
local clustervar "clustervar"
}
else{
local clustervar "statenum"
}
*


use "${data}qcew_state_overall_counts_2016.dta", clear
rename statefips statenum
gen quarterdate = yq(year, quarter)
tempfile qcew
save `qcew'



treatmentcontrolwindows, wmax(`wageupper') wmin(`wagelower')
di "You set upper bound as `wageupper' and lower bound as `wagelower'."
*The following calls the data.
treatmentdefinition


merge m:1 statenum quarterdate using `qcew',assert(3) nogenerate

replace count = count*emp/countall if emp!=0
cap drop overallcountpc
gen overallcountpc=count/population
replace overallcountpcall = emp/population  if emp!=0


********************************************************************************
***********       Correcting Aggregate Controls         ************************
********************************************************************************

xtset wagebinstate quarterdate
correctaggcont, tmin(12) tmax(16) wmin(`wagelower') wmax(`wageupper')

********************************************************************************
********************************************************************************
********************************************************************************


abovebelowbunch, wmax(`wageupper') wmin(`wagelower')

abovebelowfull

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


if `=forcluster'>10{
	cap drop temp
	gen temp = (wagebins==3000)
	cap drop clustervar
	egen clustervar = group(statenum temp)
	cap drop temp
	sum clustervar, meanonly
	assert r(max)==102
}
*

********************************************************************************
**********************       REGRESSIONS AND FIGURES        ********************
********************************************************************************
xtset wagebinstate quarterdate
compress

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

foreach Y in   overall /*overallFTE teen teenFTE HSL HSLFTE female femaleFTE BH BHFTE */ {
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

				abovebelowWBbunch,wagemult(${wagemult}) wmax(`wageupper') wmin(`wagelower')
				abovebelowWBfull


				foreach s in /*0*/ 1 /*4 5 6 10 11 12*/ /*0a 0b  4  5 6*/     {
				*** REGRESSION ***
				/*
				di "${rtype`s'} `Y'countpc  ${treatbefore} one if  year>=`b'  & cleansample==1  ${weight`Y'`b'} , a(${a`s'}  ${control} ${controlafter} ${controlf} ${window} )  cluster(`clustervar')"
				${rtype`s'} `Y'countpc  ${treatbefore} one if  year>=`b'  & cleansample==1  ${weight`Y'`b'} , a(${a`s'}  ${control} ${controlafter} ${controlf} ${window} )  cluster(`clustervar')
				est save "${estimates}altwwqcew_`wageupper'_`wagelower'_before`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
*/
				${rtype`s'} `Y'countpc  ${treatafter} one if  year>=`b'  & cleansample==1  ${weight`Y'`b'} , a(${a`s'}  ${control} ${controlbefore} ${controlf} ${window} )  cluster(`clustervar')
				est save "${estimates}altwwqcew_`wageupper'_`wagelower'_after`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}", replace

				est use "${estimates}altwwqcew_`wageupper'_`wagelower'_after`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
 				local dof = e(df_r)
				local numberobs = e(N)
				count if fedincrease!=1 & overallcountgr>0 & wagebins==300		// The wage bin does not matter.
				local numberevent = r(N)


				local denominator = (1/(1+(${Tmax}/4)))


/*
				****************************************************************
				**************     Figure 4      *******************************
				****************************************************************
				*Defining globals to get time averaged estimates. Need to define here.
				preserve															// For some reason, sometimes svmat clears the data in memory. To prevent that, we preserve here.
				makefigure_massbywagebin
				graphexportpdf ${figures}Figure4`limitsample'`limittreat'_${label`s'}_`Y'_`temp1'_`temp2'.pdf, dropeps
				restore


				****************************************************************
				**************       Figure 5     ******************************
				****************************************************************

				est use "${estimates}Table1abefore`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
				makefigure_massovertime_before
				est use "${estimates}Table1aafter`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
				makefigure_massovertime_after

				graphexportpdf ${figures}Figure5`limitsample'`limittreat'_${label`s'}_`Y'_`temp1'_`temp2'.pdf, dropeps

				****************************************************************
				**************      Figure 7              **********************
				****************************************************************
*/

/*
				est use "${estimates}altwwqcew_`wageupper'_`wagelower'_before`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
				makefigure_chovertime_before
				est use "${estimates}altwwqcew_`wageupper'_`wagelower'_after`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
				makefigure_chovertime_after
				graphexportpdf ${figures}Figure7`limitsample'`limittreat'_${label`s'}_`temp1'_`temp2'_changepattern_from_`wagelower'_to_`wageupper'.pdf, dropeps

				preserve
				keep if be_timeb!=.
				keep counter be_timeb be_timeupper be_timelower
				save "${data}Figure7_altwage_from`wagelower'_to`wageupper'.dta", replace
				restore
				stop

*/


/*


				****************************************************************
				**************      Figure 6 (Wages)             ***************
				****************************************************************

						est use "${estimates}Table1abefore`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
						makefigure_chovertimeWB_before
						est use "${estimates}Table1aafter`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
						makefigure_chovertimeWB_after
						graphexportpdf ${figures}Figure6wage`limitsample'`limittreat'_${label`s'}_`temp1'_`temp2'_changepattern.pdf, dropeps
		*/


			********************************************************************
			****************          Table           **************************
			********************************************************************
			est use "${estimates}altwwqcew_`wageupper'_`wagelower'_after`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"


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

			eststo ta_`Y'_`b'_w`wageupper'_w`wagelower'


		}

	}
 }
*
