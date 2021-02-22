**** DEFINE REGRESSION COMMAND AND CONTROLS *****
foreach s in 1 {
	global rtype`s' reghdfe
}

global a1  i.wagebinstate i.wagequarterdate  // distributional TWFE

global label1 TWFE

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
foreach yearset in 19922016 {
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

	foreach Y in ind_all_overall ind_mstrad_overall ind_msnontrad_overall ind_msother_overall ind_mscon_overall ind_retail_overall ind_rest_overall ind_manuf_overall {

		***  define MW change ***
		sum DMW_real if fedincrease!=1 & overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`Y'`b'}
		local mwc = r(mean)
		sum MW_real if  F.fedincrease!=1 & F.overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`Y'`b'}
		local mw = r(mean)
		local mwpc = `mwc'/`mw'
		local mwpc2 : di  %04.3f `mwpc'

		* define worker count
		sum `Y'samplesize if year>=`b' & cleansample ==1
		local workercount : di %15.0gc r(sum)

		* EPOP mean
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

		foreach s in  1 {
			
			*** REGRESSION ***
			di "${rtype`s'} `Y'countpc  ${treatafter} one if  year>=`b'  & cleansample==1 ${if`s'}  ${weight`Y'`b'} , a(${a`s'}  ${control} ${controlbefore} ${controlf} ${window} )  cluster(statenum)"
			${rtype`s'} `Y'countpc  ${treatafter} one if  year>=`b'  & cleansample==1 ${if`s'}  ${weight`Y'`b'} , a(${a`s'}  ${control} ${controlbefore} ${controlf} ${window} )  cluster(statenum)
			est save "${estimates}`Y'after`limitsample'`limittreat'_`s'_`Y'_`beginyear'`endyear'_`temp1'_`temp2'_${Tmax}${Tmin}", replace
			

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

			estadd local numobs "\multicolumn{1}{c}{`numberobs'}"
			estadd local numevent "\multicolumn{1}{c}{`numberevent'}"
			estadd local minwpc	= "`mwpc2'"
			estadd local belsh	= "`belsh2'"
			estadd local wagebinstate 		= "Y"
			estadd local workercount = "\multicolumn{1}{c}{`workercount'}"

				estadd local wagebinperiod 		= "Y"
			*For name only
			if "`Y'"== "ind_all_overall"{
				local z "all"
				local title "Overall"
			}
			if "`Y'"== "ind_mstrad_overall"{
				local z "mstrad"
				local title "Tradable"
			}
			if "`Y'"== "ind_msnontrad_overall"{
				local z "msnontrad"
				local title "Nontradable"
			}
			if "`Y'"== "ind_msother_overall"{
				local z "msother"
				local title "Other"
			}
			if "`Y'"== "ind_mscon_overall"{
				local z "mscon"
				local title "Construction"
			}
			if "`Y'"== "ind_rest_overall"{
				local z "rest"
				local title "Restaurants"
			}
			if "`Y'"== "ind_retail_overall"{
				local z "retail"
				local title "Retail"
			}
			if "`Y'"== "ind_manuf_overall"{
				local z "manuf"
				local title "Manufacturing"
			}

			eststo ta_ind_`b'_`z', title(`title')
			di "stored estimates called ta_ind_`b'_`z'" _n(3)
		}
	}

esttab ta_ind_`b'_all ta_ind_`b'_mstrad ta_ind_`b'_msnontrad ta_ind_`b'_mscon ta_ind_`b'_msother ta_ind_`b'_rest ta_ind_`b'_retail ta_ind_`b'_manuf using "${tables}Table3.tex", replace stats(below_Eb below_Ese above_Eb above_Ese blankspace WB_Eb WB_Ese bunching_Eb bunching_Ese blankspace bunchelas_Eb bunchelas_Ese labordem_Eb labordem_Ese blankspace belsh minwpc numevent numobs workercount, label("Missing jobs below MW (`=char(36)' \Delta `=char(36)' b)" " " "Excess jobs above MW (`=char(36)' \Delta `=char(36)' a)"  " " " "   "\%`=char(36)'\Delta`=char(36)' affected wages" " " "\%`=char(36)'\Delta`=char(36)' affected employment " " " " "  "Employment elasticity w.r.t. MW" " " "Emp. elasticity w.r.t. affected wage" " " " " "Below Share (`=char(36)'\overline{b}\ _{-1}`=char(36)')" "\%`=char(36)'\Delta`=char(36)' MW" "\# of events " "Observations" "\# of workers in the sample")) nodep nogaps cells(none) nomtitles fragment booktabs mlabels(,titles prefix(\multicolumn{@span}{c}{) suffix(}) span) substitute(\_ _)
}
