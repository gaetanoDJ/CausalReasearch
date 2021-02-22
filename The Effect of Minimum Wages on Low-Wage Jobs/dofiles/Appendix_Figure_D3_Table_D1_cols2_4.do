set more off
set trace off
clear 

do "${dofiles}create_programs.do"




* prepare the data and treatment vector for regressions
local pre 12
use "${data}stackedevents_regready_pre`pre'.dta", clear
* remove data where we can't determine when there are wage imputations
keep if cleansample == 1

* define treatment vector
local treat F*treat L*treat
global treat F*treat L*treat
* ensure events are numbered properly: N events indexed 1...N
sum event
local numevents = r(max)
tab event, nofreq
assert `r(r)' == `numevents'

tsset eventstate time

rename overallexcessepop above_epop
rename overallmissingepop below_epop
rename overallupperepop upper_epop
rename overalltotalepop all_epop
gen affected_epop = above_epop + below_epop
gen totalbelow_epop = all_epop - (above_epop + upper_epop)

gen byte yearbeforeevent = F1.origevent == 1 | F2.origevent == 1 | F3.origevent == 1 | F4.origevent == 1


* save the regression-ready sample
compress
egen eventtime = group(event time)
egen eventorigcontrol = group(event control_origeventpost)
egen eventfedcontrol = group(event control_fedeventpost)
egen eventothercontrol = group(event control_othereventpost)

bys statenum event: egen states_to_drop = max(control_origeventpost)
drop if states_to_drop!=0 & treatedstatenum!=statenum
assert control_origeventpost==0 if treatedstatenum!=statenum
sum control_origeventpost,meanonly
assert r(mean)!=0


tempfile regready
save `regready'

	tab event, nofreq
	global numevents = `r(r)'

	* pre-treatment epop of treated states
sum all_epop [aw=aveoverallpop] if origevent_treated == 1 & yearbeforeevent == 1
global epop = r(mean)


*The following are from baseline regressions.
global EWB = .350835006841847
global wagemult = 8.774882267779565
global mwpc = .1012900314042275
local mwpc2 : di  %04.3f ${mwpc}
global B = .0864819457562104
* scaling factor policy elasticity
global C = 1/(${epop}*${mwpc})


keep overallp0epop overallp1epop overallp2epop overallp3epop overallp4epop overallm1epop overallm2epop overallm3epop overallm4epop eventstate eventtime 
reshape long overall, i(eventstate eventtime) j(wage_bin) string
rename overall countpc
merge m:1 eventstate eventtime using `regready', assert(3) nogenerate keepusing(aveoverallpop eventorigcontrol eventfedcontrol eventothercontrol  statenum quarterdate treatedstate treatdate event ${treat} )
foreach vv of varlist $treat {
	foreach num of numlist 0(1)4{
	gen `vv'_p`num' = `vv' * (wage_bin=="p`num'epop")
	}
}
foreach vv of varlist $treat {
	foreach num of numlist 1(1)4{
	gen `vv'_m`num' = `vv' * (wage_bin=="m`num'epop")
	}
}

global treatvector F*treat_* L*treat_*

foreach num2 of numlist 0(1)4{
	gen window_p`num2' = F12treat_p`num2' + F8treat_p`num2' + F4treat_p`num2' + L0treat_p`num2' +  L4treat_p`num2' +  L8treat_p`num2' +  L12treat_p`num2' +  L16treat_p`num2'  
}
foreach num2 of numlist 1(1)4{
	gen window_m`num2' = F12treat_m`num2' + F8treat_m`num2' + F4treat_m`num2' + L0treat_m`num2' +  L4treat_m`num2' +  L8treat_m`num2' +  L12treat_m`num2' +  L16treat_m`num2'  
}
*
global window " "

foreach num2 of numlist 0(1)4{
		global window "${window} i.one##c.window_p`num2'"
}
foreach num2 of numlist 1(1)4{
		global window "${window} i.one##c.window_m`num2'"
}
*

global controlbefore " "

foreach num of numlist 8(4)12{
	foreach num2 of numlist 0(1)4{
		global controlbefore "${controlbefore} i.one##c.F`num'treat_p`num2'"
	}
}
*

foreach num of numlist 8(4)12{
	foreach num2 of numlist 1(1)4{
		global controlbefore "${controlbefore} i.one##c.F`num'treat_m`num2'"
	}
}
*






*
egen wage_bin_num = group(wage_bin)
*We define regready again here.
cap drop one
gen one = 1
tempfile regready
save `regready'


* define cumulative effects
* everything normalized to F4
local maxlag = 16

foreach i of numlist 0(4)`maxlag' {
	foreach num of numlist 0(1)4{
	global lincomaggL`i'_p`num' (_b[L`i'treat_p`num'] - _b[F4treat_p`num'])
	}
}
foreach i of numlist 4(4)12 {
	foreach num of numlist 0(1)4{
	global lincomaggF`i'_p`num' (_b[F`i'treat_p`num'] - _b[F4treat_p`num'])
	}
}
*
foreach i of numlist 0(4)`maxlag' {
	foreach num of numlist 1(1)4{
	global lincomaggL`i'_m`num' (_b[L`i'treat_m`num'] - _b[F4treat_m`num'])
	}
}
foreach i of numlist 0(4)12 {
	foreach num of numlist 1(1)4{
	global lincomaggF`i'_m`num' (_b[F`i'treat_m`num'] - _b[F4treat_m`num'])
	}
}


*
*For wage bins
foreach num of numlist 0(1)4{
	global lincomagg_p`num' "0 "
	foreach i of numlist 0(4)`maxlag' {
	global lincomagg_p`num' " ${lincomagg_p`num'} +  ${lincomaggL`i'_p`num'}"
	}
	global lincomagg_p`num' "(${lincomagg_p`num'})" 
}

*
foreach num of numlist 1(1)4{
	global lincomagg_m`num' " 0"
	foreach i of numlist 0(4)`maxlag' {
	global lincomagg_m`num' " ${lincomagg_m`num'} +  ${lincomaggL`i'_m`num'}"
	}
	global lincomagg_m`num' "(${lincomagg_m`num'})" 

}
*
*wage effect globals


foreach t of numlist -`pre'(4)`maxlag' {
				
	if `t' < -4  {	
		local nt = -`t' 
		global aboveWBF`nt' "((${lincomaggF`nt'_p0})*(${wagemult}+0) )"
		forval j = 1/4 {
		global aboveWBF`nt' "${aboveWBF`nt'} + ((${lincomaggF`nt'_p`j'})*(${wagemult}+`j') )"
	}
	global belowWBF`nt' "((${lincomaggF`nt'_m1})*(`wagemult'-1) )"
	forval j = 2/4 {
		global belowWBF`nt' "${belowWBF`nt'} + ((${lincomaggF`nt'_m`j'})*(${wagemult}-`j') )"
	}

	}

					
	if `t' >= 0 {	
		 
		global aboveWBL`t' "((${lincomaggL`t'_p0})*(${wagemult}+0))"
		forval j = 1/4 {
			global aboveWBL`t' "${aboveWBL`t'} + ((${lincomaggL`t'_p`j'})*(${wagemult}+`j'))"
		}
		global belowWBL`t' "((${lincomaggL`t'_m1})*(${wagemult}-1) )"
		forval j = 2/4 {
			global belowWBL`t' "${belowWBL`t'} + ((${lincomaggL`t'_m`j'})*(${wagemult}-`j'))"
		}

	}
	
 
} 
 			global aboveWBF4 = 0
 			global belowWBF4 = 0
			
			

global aboveWB_full "${aboveWBL0}"
global belowWB_full "${belowWBL0}"

forval t = 4(4)`maxlag' {
			
				global belowWB_full "${belowWB_full} + ${belowWBL`t'}"
				global aboveWB_full "${aboveWB_full} + ${aboveWBL`t'}"
			}
*



*For time paths
foreach i of numlist 0(4)`maxlag'{
global lincomaboveL`i' "0"
	foreach num of numlist 0(1)4{
	global lincomaboveL`i' " ${lincomaboveL`i'} +  ${lincomaggL`i'_p`num'}"
	}
	global lincomaboveL`i' "(${lincomaboveL`i'})" 
}
foreach i of numlist 4(4)12{
global lincomaboveF`i' "0"
	foreach num of numlist 0(1)4{
	global lincomaboveF`i' " ${lincomaboveF`i'} +  ${lincomaggF`i'_p`num'}"
	}
	global lincomaboveF`i' "(${lincomaboveF`i'})" 
}
*
foreach i of numlist 0(4)`maxlag'{
global lincombelowL`i' "0"
	foreach num of numlist 1(1)4{
	global lincombelowL`i' " ${lincombelowL`i'} +  ${lincomaggL`i'_m`num'}"
	}
	global lincombelowL`i' "(${lincombelowL`i'})" 
}
foreach i of numlist 4(4)12{
global lincombelowF`i' "0"
	foreach num of numlist 1(1)4{
	global lincombelowF`i' " ${lincombelowF`i'} +  ${lincomaggF`i'_m`num'}"
	}
	global lincombelowF`i' "(${lincombelowF`i'})" 
}
*


global lincomabove "(${lincomagg_p0} + ${lincomagg_p1} + ${lincomagg_p2} + ${lincomagg_p3} + ${lincomagg_p4}) "
global lincombelow "(${lincomagg_m1} + ${lincomagg_m2} + ${lincomagg_m3} + ${lincomagg_m4}) "

local group overall
use `regready', clear




foreach group in overall {
  use `regready', clear




	di "reghdfe countpc ${treatvector}  [aw=aveoverallpop], a(i.eventstate#i.wage_bin_num i.eventtime#i.wage_bin_num i.eventorigcontrol#i.wage_bin_num i.eventfedcontrol#i.wage_bin_num i.eventothercontrol#i.wage_bin_num) cluster(eventstate)"
	qui reghdfe countpc ${treatvector}  [aw=aveoverallpop], a(i.eventstate#i.wage_bin_num i.eventtime#i.wage_bin_num i.eventorigcontrol#i.wage_bin_num i.eventfedcontrol#i.wage_bin_num i.eventothercontrol#i.wage_bin_num) cluster(eventstate)
		estadd local epop = ${epop}
		estadd local C = ${C}
		estadd local mwpc = ${mwpc}
		estadd local belowshare	= ${B}	
	estimates save "${estimates}stackedevents_wagebins_clean_controls_overall_model2_pre`pre'.ster", replace


	di "reghdfe countpc ${treatvector}  [aw=aveoverallpop] if treatdate<=208, a(i.eventstate#i.wage_bin_num i.eventtime#i.wage_bin_num i.eventorigcontrol#i.wage_bin_num i.eventfedcontrol#i.wage_bin_num i.eventothercontrol#i.wage_bin_num) cluster(eventstate)"
	qui reghdfe countpc ${treatvector}  [aw=aveoverallpop]  if treatdate<=208, a(i.eventstate#i.wage_bin_num i.eventtime#i.wage_bin_num i.eventorigcontrol#i.wage_bin_num i.eventfedcontrol#i.wage_bin_num i.eventothercontrol#i.wage_bin_num) cluster(eventstate)
		estadd local epop = ${epop}
		estadd local C = ${C}
		estadd local mwpc = ${mwpc}
		estadd local belowshare	= ${B}	
	estimates save "${estimates}stackedevents_wagebins_clean_controls_overall_model22_pre`pre'.ster", replace


	*Excess/missing jobs
	foreach model of numlist 2 22 {
	if `model' != 22{
	global EWB = .350835006841847
	global wagemult = 8.774882267779565
	global mwpc = .1012900314042275
	local mwpc2 : di  %04.3f ${mwpc}
	global B = .0864819457562104
	* scaling factor policy elasticity
	global C = 1/(${epop}*${mwpc})
	}
	else{
	estimates use "${estimates}stackedevents_wagebins_clean_controls_overall_model`model'_pre`pre'.ster"	
	global EWB = .3468823014380139
	global wagemult = 8.582126952247453
	global mwpc = 0.107732648
	local mwpc2 : di  %04.3f ${mwpc}
	global B = 0.085549852
	* scaling factor policy elasticity
	global C = 1/(${epop}*${mwpc})
	
		estadd local epop = ${epop}, replace
		estadd local C = ${C}, replace
		estadd local mwpc = ${mwpc}, replace
		estadd local belowshare	= ${B}	, replace
	estimates save "${estimates}stackedevents_wagebins_clean_controls_overall_model`model'_pre`pre'.ster", replace	
	
	
	}
	
	
	estimates use "${estimates}stackedevents_wagebins_clean_controls_overall_model`model'_pre`pre'.ster"
	
	local dof = e(df_r)
	local numberobs = e(N)
	if `model' != 22{
	sum event, meanonly
	local numberevent = r(max)
	}
	else{
	tab event if treatdate<=208, nofreq
	local numberevent = r(r)
	}
	global E = `e(epop)'
	
	****************************************************************************
	****************         Time path figure                *******************
	****************************************************************************
	
	if `model' == 2{
	
					foreach t of numlist -`pre'(4)-4 {
			
			
					local nt = -`t'
					nlcom ("${lincomaboveF`nt'}") 
					mat b = r(b)
					mat V = r(V)
					local ab = b[1,1]*(1/${E})
					local ase = sqrt(V[1,1])*(1/${E})
					nlcom ("${lincombelowF`nt'}") 
					mat b = r(b)
					mat V = r(V)
					local bb =  (b[1,1])*(1/${E})
					local bse = sqrt(V[1,1])*(1/${E})

					if `nt' == `pre' {
						mat eventmat = [`t', `ab', `ab'-1.96*`ase', `ab'+1.96*`ase',  `bb', `bb'-1.96*`bse', `bb'+1.96*`bse'] 
					}
					else {
						mat eventmat = [eventmat \ [`t', `ab', `ab'-1.96*`ase', `ab'+1.96*`ase',  `bb', `bb'-1.96*`bse', `bb'+1.96*`bse']] 
					}
				}
	
				foreach t of numlist 0(4)`maxlag' {
					nlcom ("${lincomaboveL`t'}") 
					mat b = r(b)
					mat V = r(V)
					local ab =  b[1,1]*(1/${E})
					local ase = sqrt(V[1,1])*(1/${E})
					nlcom ("${lincombelowL`t'}") 
					mat b = r(b)
					mat V = r(V)
					local bb =  (b[1,1])*(1/${E})
					local bse = sqrt(V[1,1])*(1/${E})
					mat eventmat = [eventmat \ [`t', `ab', `ab'-1.96*`ase', `ab'+1.96*`ase',  `bb', `bb'-1.96*`bse', `bb'+1.96*`bse'] ]

					}
			
			cap drop time* 
			cap drop est* 
			cap drop low* high* 
			cap drop _est*
			
			cap drop eventmat*
			cap drop bunchest
			preserve
			clear
			svmat eventmat
			rename eventmat1 time
			rename eventmat2 estA			
			rename eventmat3 lowA
			rename eventmat4 highA
			rename eventmat5 estB			
			rename eventmat6 lowB
			rename eventmat7 highB
			
				twoway ( line estA time  , fcolor(ltblue) lcolor(ltblue)  lwidth(thick)  ) ///
				(rcap lowA  highA time  , lwidth(thick) lcolor(ltblue*1.25) )  ///
				( line estB time  , fcolor(red) lcolor(red) lwidth(thick) yline(0, lcolor(gs10) lwidth(vvthin)) xline(-4, lpattern(dash) lcolor(black) ) ) ///
				(rcap lowB  highB time  , lwidth(thick)  lcolor(red*1.25) ) , ///
				scheme(s1color) xtitle("Years relative to the minimum wage change", size(medsmall)) ///
				/*ytitle("Excess mass", height(5) axis(1) size(medsmall))*/   ///
				 ylabel(, axis(1) labsize(medsmall)) yscale(titlegap(0) axis(1)) ///
				 ytitle("Excess and missing jobs relative to the pre-treatment total employment", height(10) size(small))   ///
				 ///
				xlabel(-12 "-3" -8 "-2" -4 "-1" 0 "0" 4 "1" 8 "2" 12 "3" 16 "4", labsize(medsmall)) legend(off)  
 	
	
			gr export "${figures}FigureD3_a.pdf", replace
			
			restore
			
			
			
			
			foreach t of numlist -`pre'(4)-4 {
				
					local nt = -`t' 
					lincom ((${lincomaboveF`nt'}) + (${lincombelowF`nt'}))*(1/${E})*(1/${B})							// Multiplying by 4 because of the number of bins (treatments at 1$ increments, bins at 25 cents).
					local b = r(estimate)
					local se = r(se)
					if `nt' == `pre' {
						mat eventmat = [`t', `b', `b'-1.96*`se', `b'+1.96*`se'] 
					}
					else {
						mat eventmat = [eventmat \ [`t', `b', `b'-1.96*`se', `b'+1.96*`se']] 
					}
						
			}  //end of t<-4

				foreach t of numlist 0(4)`maxlag' {
 
					lincom ((${lincomaboveL`t'}) + (${lincombelowL`t'}))*(1/${E})*(1/${B})
					local b = r(estimate)
					local se = r(se)
					mat eventmat = [eventmat \ [`t', `b', `b'-1.96*`se', `b'+1.96*`se']] 
				}  
			preserve
			clear

			svmat eventmat
			rename eventmat1 time
			rename eventmat2 estA			
			rename eventmat3 lowA
			rename eventmat4 highA

			keep if time!=.
						twoway (scatter estA time  , msymbol(D) mcolor(blue) yline(0, lcolor(gs10) lwidth(vvthin)) xline(-4, lpattern(dash) lcolor(black) )) ///
						(line estA time , lcolor(blue) lpattern(dash)  lwidth(thick) ) ///
						(rcap lowA highA time, lcolor(blue) lwidth(thick) ), graphregion(color(white)) ///
						xlabel(-12 "-3" -8 "-2" -4 "-1" 0 "0" 4 "1" 8 "2" 12 "3" 16 "4", labsize(medsmall))	///
						ylabel(, labsize(medsmall)) legend(off) xtitle("Years relative to the minimum wage change", size(medsmall))  ylabel(-0.2(0.1)0.2) ///
						ytitle("Percentage change in employment of affected workers", height(6))   						
			
			gr export "${figures}FigureD3_b.pdf", replace

			restore

			
		}

			
			

			
			estimates use "${estimates}stackedevents_wagebins_clean_controls_overall_model`model'_pre`pre'.ster"
			
			local denominator = 1/5
			lincomestadd (${lincombelow})*(`denominator')*(1/${E})							, statname(below_E) 				// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			lincomestadd (${lincomabove})*(`denominator')*(1/${E})							, statname(above_E)					// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			lincomestadd ((${lincomabove} + ${lincombelow})*(`denominator')*(1/${E}))/${B} 	, statname(bunching_E)				// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5. 			
			if `model'==12{
				global new_emp_null = r(estimate)
			}
			lincomestadd (${lincombelow} + ${lincomabove})*(`denominator')*${C}				, statname(bunchelas_E)				// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			
			nlcomestadd ((((${belowWB_full}+${aboveWB_full})*(`denominator')/$EWB ) - ///
			(((${lincombelow} + ${lincomabove})*(`denominator')*(1/${E}))/${B}))) / ///
			(1+(((${lincombelow} + ${lincomabove})*(`denominator')*(1/${E}))/${B}))		, statname(WB_E)  dof(`dof') 					// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			
			nlcomestadd (((${lincombelow} + ${lincomabove})*(`denominator')*(1/${E}))/${B}) /  ///
			(((((${belowWB_full}+${aboveWB_full})*(`denominator')/$EWB ) - ///
			(((${lincombelow} + ${lincomabove})*(`denominator')*(1/${E}))/${B}))) / ///
			(1+(((${lincombelow} + ${lincomabove})*(`denominator')*(1/${E}))/${B})))		, statname(labordem_E)	 dof(`dof') 				// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.

			local belsh2 : di  %04.3f $B
			
			estadd local numobs = "`numberobs'"
			estadd local numevent = "`numberevent'"
			estadd local minwpc	= "`mwpc2'"
			estadd local belsh	= "`belsh2'"
			estadd local wagebinstate 		= "Y"			
			estadd local wagebinperiod 		= "Y"
			
			if `model' == 3{
			estadd local cons_comb 			= "Y"			
			}
			if `model' == 4{
			estadd local cons_comb 			= "Y"			
			estadd local otherdummied 		= "Y"			
			}
			
			eststo ta_overall_1979_stackmm`model'
			est save "${estimates}stackedevents_wagebins_clean_controls_overall_model`model'_pre`pre'_estadd", replace
			}
}
*
