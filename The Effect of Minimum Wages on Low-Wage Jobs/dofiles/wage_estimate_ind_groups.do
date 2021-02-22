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

*****************
*** QCEW PREP ***
*****************
use "${data}qcew_state_sector_counts_2016.dta", clear
rename statefips statenum
gen quarterdate = yq(year, quarter)
foreach var of varlist ind_* {
	rename `var' emp_`var'_overall
}
tempfile qcew
save `qcew'

****************************
*** TREATMENT MACRO PREP ***
****************************
* this command should be independent of data source
treatmentcontrolwindows

************************************
*** ABOVE/BELOW SHARE MACRO PREP ***
************************************
* these commands should be independent of data source
abovebelowbunch
abovebelowfull



***************************************
*** LOOP OVER TIME BASED SUBSAMPLES ***
***************************************
foreach yearset in 19922016 /*20002016*/ {
	local beginyear = substr("`yearset'",1,4)
	local endyear = substr("`yearset'",5,4)
	local b = `beginyear'

	use "${data}state_panels_with3quant_ind_`beginyear'_`endyear'.dta", clear
	merge m:1 statenum quarterdate using `qcew', nogenerate keep(3)
	assert year <= `endyear' & year >= `beginyear'

	foreach Y in ind_all_overall ind_mstrad_overall ind_msnontrad_overall ind_msother_overall ind_mscon_overall ind_retail_overall ind_rest_overall ind_manuf_overall {
		replace `Y'count = `Y'count*emp_`Y'/countall
		cap drop `Y'countpc
		gen `Y'countpc=`Y'count/population
		cap drop `Y'countpcall
		gen `Y'countpcall = `Y'countall*(emp_`Y'/countall)/population
	}

	********************************************************************************
	***********************           Weights           ****************************
	********************************************************************************
	* follow Doruk's code in state_panels_tercile1979.do
	foreach Y in overall {
		egen wt`Y'`b' = mean(`Y'pop) if year>=`b' & cleansample ==1 , by(statenum quarterdate)
	}
	foreach Y in overall {
		global weightind_all_`Y'`b' "[aw=wt`Y'`b']"
		global weightind_mstrad_`Y'`b' "[aw=wt`Y'`b']"
		global weightind_msnontrad_`Y'`b' "[aw=wt`Y'`b']"
		global weightind_msother_`Y'`b' "[aw=wt`Y'`b']"
		global weightind_mscon_`Y'`b' "[aw=wt`Y'`b']"
		global weightind_retail_`Y'`b' "[aw=wt`Y'`b']"
		global weightind_rest_`Y'`b' "[aw=wt`Y'`b']"
		global weightind_manuf_`Y'`b' "[aw=wt`Y'`b']"
	}

	********************************************************************************
	**************     Sample and Control Specific Macros           ****************
	********************************************************************************
	local limittreat "_stateonly"

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

	********************************************************************************
	***************          Useful to Find Below Share Wages               ********
	********************************************************************************
	cap drop statequarterdate
	egen statequarterdate = group(state quarterdate)
	sort statequarterdate wagebins

	foreach vv in ind_all_ ind_mstrad_ ind_msnontrad_ ind_msother_ ind_mscon_ ind_retail_ ind_rest_ ind_manuf_ {
		cap drop `vv'overallWB*
		g `vv'overallWBFH = wagebins*`vv'overallcount/(100)
		g `vv'overallWBpcFH = wagebins*`vv'overallcount/(100*population)

		cap drop `vv'overallWBpcrsumFH
		g `vv'overallWBpcrsumFH=0
		replace `vv'overallWBpcrsumFH = `vv'overallWBpcFH if wagebins==100
		replace `vv'overallWBpcrsumFH = `vv'overallWBpcrsumFH[_n-1] + `vv'overallWBpcFH if wagebins>100

		cap drop `vv'overallcountpcrsum
		g `vv'overallcountpcrsum=0
		replace `vv'overallcountpcrsum = `vv'overallcountpc if wagebins==100
		replace `vv'overallcountpcrsum = `vv'overallcountpcrsum[_n-1] + `vv'overallcountpc if wagebins>100
	}

	********************************************************************************
	**********************       REGRESSIONS AND FIGURES        ********************
	********************************************************************************
	xtset wagebinstate quarterdate
	compress

	foreach Y in ind_mstrad_overall ind_msnontrad_overall {

		***  define MW change ***
		sum DMW_real if fedincrease!=1 & overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`Y'`b'}
		local mwc = r(mean)
		sum MW_real if  F.fedincrease!=1 & F.overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`Y'`b'}
		local mw = r(mean)
		local mwpc = `mwc'/`mw'
		local mwpc2 : di  %04.3f `mwpc'

		* EPOP mean
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
		& year>=`b' & cleansample ==1	  ${weight`Y'`b'}

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

		foreach s in /*0*/ 1 /*4 5 6 10 11 12*/ /*13 14*/ /*0a 0b  4  5 6*/     {

			est use "${estimates}`Y'after`limitsample'`limittreat'_`s'_`Y'_`beginyear'`endyear'_`temp1'_`temp2'_${Tmax}${Tmin}"
			local dof = e(df_r)
			local numberobs : di %15.0gc e(N)
			count if fedincrease!=1 & overallcountgr>0 & wagebins==300 & year >= `b'	// The wage bin does not matter.
			local numberevent : di %15.0gc r(N)
			local denominator = (1/(1+(${Tmax}/4)))


			********************************************************************
			****************          Table           **************************
			********************************************************************
			est use "${estimates}`Y'after`limitsample'`limittreat'_`s'_`Y'_`beginyear'`endyear'_`temp1'_`temp2'_${Tmax}${Tmin}"

			if "`Y'"== "ind_mstrad_overall"{
				local z "mstrad"
			}
			if "`Y'"== "ind_msnontrad_overall"{
				local z "msnontrad"
			}

			nlcomestadd ((((${belowWB_full}+${aboveWB_full})*(4*`denominator')/$EWB ) - ///
			(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B}))) / ///
			(1+(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B}))		, statname(WB_E)  dof(`dof') 					// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			eststo `z'_2

			*Method 3
			est use "${estimates}`Y'after`limitsample'`limittreat'_`s'_`Y'_`beginyear'`endyear'_`temp1'_`temp2'_${Tmax}${Tmin}"
			abovebelowWBbunch_alt, secondmethod(Y) 
			abovebelowWBfull_alt,  secondmethod(Y) 
			nlcomestadd ((${aboveWB_fullY})/${EWB})*(4*`denominator'), statname(WB_E) dof(`dof') 
			eststo `z'_3
			
			est use "${estimates}`Y'after`limitsample'`limittreat'_`s'_`Y'_`beginyear'`endyear'_`temp1'_`temp2'_${Tmax}${Tmin}"
			nlcom (a1:((((${belowWB_full}+${aboveWB_full})*(4*`denominator')/$EWB ) - ///
			(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B}))) / ///
			(1+(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B}))) (a2:((${aboveWB_fullY})/${EWB})*(4*`denominator')), post
			nlcomestadd     1-(_b[a2]/_b[a1])  , statname(WB_E) dof(`dof') 
			eststo `z'_spill



		}
	}

}
