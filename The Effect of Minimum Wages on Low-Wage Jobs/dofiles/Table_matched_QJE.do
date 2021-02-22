
*This do file is created by DC on 10/07/2016
********************************************************************************
***************************   Table matched      *******************************
********************************************************************************
		
**** DEFINE REGRESSION COMMAND AND CONTROLS *****

 
foreach s in 1 {
	global rtype`s' reghdfe
}
*

global a1  i.wagebinstate i.wagequarterdate  // distributional TWFE

global label1 TWFE

use "${data}qcew_state_overall_counts_2016.dta", clear
rename statefips statenum
gen quarterdate = yq(year, quarter)
tempfile qcew
save `qcew'



treatmentcontrolwindows

use "${data}state_panels_with3quant1979_matched.dta", clear



merge m:1 statenum quarterdate using `qcew', keep(1 3)
cap drop multiplier
gen multiplier = emp/mt_countall
replace multiplier = 1 if multiplier==0

foreach Y in   m_ mt_   { 
replace `Y'count = `Y'count*multiplier
replace `Y'overallcountpc = `Y'overallcountpc*multiplier
replace `Y'overallcountpcall = `Y'overallcountpcall *multiplier
replace `Y'countall = `Y'countall*multiplier
}


abovebelowbunch

abovebelowfull


********************************************************************************
***********************           Weights           ****************************
********************************************************************************

foreach Y in   overall { 
	foreach b of numlist  1979{
					global weightm_`Y'`b' "[aw=wt`Y'`b']" 
					global weightmt_`Y'`b' "[aw=wt`Y'`b']" 
					}
}  
*


********************************************************************************
**************     Sample and Control Specific Macros           ****************
********************************************************************************


local limittreat "_stateonly"


xtset wagebinstateprev quarterdate



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


	
********************************************************************************
***************          Useful to Find Below Share Wages               ********
********************************************************************************

cap drop statequarterdate
egen statequarterdate = group(state quarterdate)
sort emp_prev statequarterdate wagebins


foreach vv in mt_ m_ {

cap drop `vv'overallWB*
g `vv'overallWBFH = wagebins*`vv'count/(100)
g `vv'overallWBpcFH = wagebins*`vv'count/(100*population)

cap drop `vv'overallWBpcrsumFH
g `vv'overallWBpcrsumFH=0
replace `vv'overallWBpcrsumFH = `vv'overallWBpcFH if wagebins==100
replace `vv'overallWBpcrsumFH = `vv'overallWBpcrsumFH[_n-1] + `vv'overallWBpcFH if wagebins>100
	
cap drop `vv'overallcountpcrsum
g `vv'overallcountpcrsum=0
replace `vv'overallcountpcrsum = `vv'overallcountpc if wagebins==100
replace `vv'overallcountpcrsum = `vv'overallcountpcrsum[_n-1] + `vv'overallcountpc if wagebins>100
}	
*	

*Sanity check.

sort statequarterdate wagebins emp_prev 

assert mt_overallcountpcrsum == mt_overallcountpcrsum[_n-1] if statequarterdate== statequarterdate[_n-1] & wagebins == wagebins[_n-1] & emp_prev!=emp_prev[_n-1]

******************************************************************************** 
**********************       REGRESSIONS AND FIGURES        ********************
********************************************************************************
xtset wagebinstateprev quarterdate
compress

local if1 "& emp_prev==1"
local if2 "& emp_prev==0"
local if3 "& emp_prev==1" 														// In the third case, it does not matter whether emp_prev==1 or emp_prev==0.

local Ycounter = 0
foreach Y in   m_overall m_overall mt_overall  { 
local Ycounter = `Ycounter' + 1
foreach b in    1979 {
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
				| (F4.fedincrease != 1 & F4.overallcountgr>0 & F4.fedincrease != . & F4.overallcountgr!=. ) ) & year>=`b' & cleansample ==1  `if`Ycounter''  ///
				 ${weight`Y'`b'}		 // CHECK THIS  2/2/16

				local epop = r(mean)
				global E = `epop'
					
				global C = 1/(`epop'*`mwpc')
			
			
				*Below share
				
				sum `Y'countpcrsum if ( F.fedincrease!=1 & F.overallcountgr>0 & F.fedincrease!=. & F.overallcountgr!=.  & (wagebins+25)/100==F1MW_realM25) | ///
				 ( F2.fedincrease!=1 & F2.overallcountgr>0 & F2.fedincrease!=. & F2.overallcountgr!=. & (wagebins+25)/100==F2MW_realM25 ) | ///
				( F3.fedincrease!=1 & F3.overallcountgr>0 & F3.fedincrease!=. & F3.overallcountgr!=.  & (wagebins+25)/100==F3MW_realM25) | ///
				 ( F4.fedincrease!=1 & F4.overallcountgr>0 & F4.fedincrease!=. & F4.overallcountgr!=. & (wagebins+25)/100==F4MW_realM25 ) ///
				& year>=`b' & cleansample ==1		`if`Ycounter''     ${weight`Y'`b'}					
				
								
				global B = r(mean)/${E}
				local belsh2 : di  %04.3f $B

				*Affected share
				
				sum `Y'countpcrsum if (  F.fedincrease!=1 & F.overallcountgr>0 & F.fedincrease!=. & F.overallcountgr!=.  & (wagebins)/100==MW_realM25) | ///
				 ( F2.fedincrease!=1 & F2.overallcountgr>0 & F2.fedincrease!=. & F2.overallcountgr!=. & (wagebins)/100==MW_realM25 ) | ///
				( F3.fedincrease!=1 & F3.overallcountgr>0 & F3.fedincrease!=. & F3.overallcountgr!=.  & (wagebins)/100==MW_realM25) | ///
				 ( F4.fedincrease!=1 & F4.overallcountgr>0 & F4.fedincrease!=. & F4.overallcountgr!=. & (wagebins)/100==MW_realM25 ) ///
				& year>=1979 & cleansample ==1		`if`Ycounter''    ${weight`Y'`b'}			
				
				global AS`Y' = $B - (r(mean)/${E})
				local affsh`Y' : di  %04.3f ${AS`Y'}
				
				
				
				****************************************************************
				*************    Wagebill related globals       ****************
				****************************************************************
				*Think about this part.
				
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
				
			
				foreach s in 1 {		
				*** REGRESSION *** 
				est use "${estimates}`Y'`Ycounter'pl0after`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}" 
 				local dof = e(df_r)
				local numberobs = e(N)
				scalar temp1 = e(sample_beg)
				scalar temp2 = e(sample_end)

				count if fedincrease!=1 & overallcountgr>0 & quarterdate>=`=temp1' & quarterdate<=`=temp2' & wagebins==300
				local numberevent = r(N)
				
				
				local denominator = (1/(1+(${Tmax}/4)))

				
				
				****************************************************************
				**************     Figure 4      *******************************
				****************************************************************
				*Defining globals to get time averaged estimates. Need to define here.
				preserve															// For some reason, sometimes svmat clears the data in memory. To prevent that, we preserve here.
				local tmax=0
				local denominator = (1/(1+(`tmax'/4)))
				local wmax = 4
				local wmin = 4

				global PA_p0 "[treat_p0  ]*(4)*(1/${E})"		// Multiplying by 4 to get $ bin estimates. Dividing by E to get share of workforce.
				global PA_p0 "`denominator'*(${PA_p0})"

				forval j = 1/`wmax' {
					global PA_p`j' "[treat_p`j' ]*(4)*(1/${E})"
					global PA_p`j' "`denominator'*(${PA_p`j'})" 
				}
				
				forval j = 1/`wmin' {				
					global PA_m`j' "[treat_m`j' ]*(4)*(1/${E})"
					global PA_m`j' "`denominator'*(${PA_m`j'})"
				}

				
				global cumul "0"
				forval k = -`wmin'/`wmax' {	
					 di `k'
					if `k' < 0 {
						local j = -`k'
						local lincomline "${PA_m`j'}"
					}
					else if `k'==0 {
						local lincomline "${PA_p0}"
					}
					else if `k'>0 {
						local lincomline "${PA_p`k'}"
					}
					
 					qui lincom   "`lincomline'"
					local basic_b = r(estimate)
					local basic_se = r(se)
					global cumul "${cumul} + `lincomline'"
					qui lincom "${cumul}"
					local cumul_b = r(estimate)
					local cumul_se = r(se)
 					if `k' == -`wmin' {
						mat countmat = [ `k', `basic_b',  `basic_b'-1.96*`basic_se', `basic_b'+1.96*`basic_se', `cumul_b', `cumul_b'-1.96*`cumul_se', `cumul_b'+1.96*`cumul_se' ]
					}
					else {
						mat countmat = [countmat \ [ `k', `basic_b',  `basic_b'-1.96*`basic_se', `basic_b'+1.96*`basic_se', `cumul_b', `cumul_b'-1.96*`cumul_se', `cumul_b'+1.96*`cumul_se' ]]
					}
				}
				
				
			cap drop bin* 
			cap drop est* 
			cap drop low *
			cap drop high* 
			cap drop _est*
			cap drop totest*
			cap drop totlow
			cap drop tothigh
			cap drop countmat*
			
			svmat countmat
			rename countmat1 bin
			rename countmat2 est			
			rename countmat3 low
			rename countmat4 high
			rename countmat5 totest			
			rename countmat6 totlow
			rename countmat7 tothigh			
			
			qui sum totest if bin==`wmax' 
			local elastb: di %3.2f  r(mean)
			qui sum tothi if bin==`wmax' 
			local elasthi: di %3.2f  r(mean)
			qui sum totlow if bin==`wmax' 
			local elastlow: di %3.2f  r(mean)

			global elastest "MW elasticty=`elastb', CI=[`elastlow', `elasthi']"
 			
 			 
				twoway ( bar est bin  , fcolor(ltblue) lcolor(none) ) ///
				(rcap low  high bin  , lcolor(ltblue*1.25) lwidth(thick) ) ///
				(line totest bin, lcolor(red*.33) lpat(line) lwidth(thick) ) /* ///
				(line totlow  bin , color(red) lpat(dot)) ///
				(line tothigh  bin , color(red) lpat(dot))	*/			, ///
				/* xlabel(-4 "[-`=char(36)'4.25, -`=char(36)'3.25)" -2 "[-`=char(36)'2.25, -`=char(36)'1.25)" 0 "[-`=char(36)'0.25, `=char(36)'0.75)" ///
				 2 "[`=char(36)'1.75, `=char(36)'2.75)" 4 "[`=char(36)'3.75, `=char(36)'4.75)" , labsize(medsmall) ) ///
				 ylabel( -0.004  -0.002   0  0.002 0.004 0.006) */ ///
				 xlabel(-4 "-4" -3 "-3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "4"  ///
				, labsize(medsmall) ) ///
				ylabel(, labsize(medsmall)) scheme(s1color) /*xtitle("Wage Bin")*/ xtitle("Wage bins in `=char(36)' relative to new MW", size(medsmall)) xsc(titlegap(*10)) ytitle("", height(10))   ///
				/*title("${label`s'}", size(medium) )*/ legend(off) /*  ///
				note("${bunchest}" , size(small) color(blue)) */
				
				
				
				graphexportpdf ${figures}Figure4firstyear`limitsample'`limittreat'_${label`s'}_`Y'`Ycounter'_`temp1'_`temp2'.pdf, dropeps 
				restore

				
				
			
			est use "${estimates}`Y'`Ycounter'after`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
				
			
			
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

			
			estadd local wagebinperiod 		= "Y"

			*For name only
			if "`Y'"== "m_overall"{
			local zz "m`Ycounter'"
			}
			if "`Y'"== "mt_overall"{
			local zz "mt"
			}
			
			
			
			eststo ta_mat_`b'_`zz'

		
		}
			
	}
 }
*

