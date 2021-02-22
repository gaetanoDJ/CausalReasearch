
*This do file is created by DC on 10/07/2016
********************************************************************************
***************************   Table matched      *******************************
********************************************************************************

		
**** DEFINE REGRESSION COMMAND AND CONTROLS *****

 
foreach s in 1 {
	global rtype`s' reghdfe
}
*

local truewmin = 4
local truewmax = 4
local placebo1wmax = 13
local prevwmax = `placebo1wmax'
local placebo2wmax = 16


global a1  i.wagebinstate i.wagequarterdate  // distributional TWFE

global label1 TWFE

use "${data}qcew_state_overall_counts_2016.dta", clear
rename statefips statenum
gen quarterdate = yq(year, quarter)
tempfile qcew
save `qcew'



treatmentcontrolwindows


placebowindows1, truewmax(`truewmax') wmax(`placebo1wmax')

placebowindows2, prevwmax(`prevwmax') wmax(`placebo2wmax')


use "${data}state_panels_with3quant1979_matched.dta", clear
cap drop wagebinstate
gen wagebinstate =  wagebinstateprev

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
*****************          Placebo sample correction          ******************
********************************************************************************

placebosamplecorr1, wmax(`placebo1wmax') truewmax(`truewmax') 
placebosamplecorr2, wmax(`placebo2wmax') prevwmax(`placebo1wmax')


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
*Because the data is long.
replace sum_inf = sum_inf/2


compress

local if1 "& emp_prev==1"
local if2 "& emp_prev==0"
local if3 "& emp_prev==1" 														// In the third case, it does not matter whether emp_prev==1 or emp_prev==0.
local Ycounter = 0
foreach Y in   m_overall m_overall mt_overall  { 
local Ycounter = `Ycounter' + 1
foreach b in   /* 1995 */ 1979 {
					***  define MW change ***
				sum DMW_real if fedincrease!=1 & overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`Y'`b'}
				local mwc = r(mean)
				sum MW_real if  F.fedincrease!=1 & F.overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`Y'`b'}
				local mw = r(mean)
				local mwpc = `mwc'/`mw'
				local mwpc2 : di  %04.3f `mwpc'

				sum mt_overallcountpcall if ((F.fedincrease != 1 & F.fedincrease != . & F.overallcountgr>0 & F.overallcountgr!=. ) | ///
				(F2.fedincrease != 1 & F2.overallcountgr>0 & F2.fedincrease != . & F2.overallcountgr!=. ) ///
				| (F3.fedincrease != 1 & F3.overallcountgr>0 & F3.fedincrease != . & F3.overallcountgr!=. ) ///
				| (F4.fedincrease != 1 & F4.overallcountgr>0 & F4.fedincrease != . & F4.overallcountgr!=. ) ) & year>=`b' & cleansample ==1 	 ///
				 ${weight`Y'`b'} 	 // CHECK THIS  2/2/16

				local epop = r(mean)
				global E = `epop'
					
				global C = 1/(`epop'*`mwpc')
			
			
				*Below share
				
				sum `Y'countpcrsum if ( F.fedincrease!=1 & F.overallcountgr>0 & F.fedincrease!=. & F.overallcountgr!=.  & (wagebins+25)/100==F1MW_realM25) | ///
				 ( F2.fedincrease!=1 & F2.overallcountgr>0 & F2.fedincrease!=. & F2.overallcountgr!=. & (wagebins+25)/100==F2MW_realM25 ) | ///
				( F3.fedincrease!=1 & F3.overallcountgr>0 & F3.fedincrease!=. & F3.overallcountgr!=.  & (wagebins+25)/100==F3MW_realM25) | ///
				 ( F4.fedincrease!=1 & F4.overallcountgr>0 & F4.fedincrease!=. & F4.overallcountgr!=. & (wagebins+25)/100==F4MW_realM25 ) ///
				& year>=`b' & cleansample ==1	`if`Ycounter''     ${weight`Y'`b'}					
				
								
				global B = r(mean)/${E}
				local belsh2 : di  %04.3f $B

				*Affected share
				
				sum `Y'countpcrsum if (  F.fedincrease!=1 & F.overallcountgr>0 & F.fedincrease!=. & F.overallcountgr!=.  & (wagebins)/100==MW_realM25) | ///
				 ( F2.fedincrease!=1 & F2.overallcountgr>0 & F2.fedincrease!=. & F2.overallcountgr!=. & (wagebins)/100==MW_realM25 ) | ///
				( F3.fedincrease!=1 & F3.overallcountgr>0 & F3.fedincrease!=. & F3.overallcountgr!=.  & (wagebins)/100==MW_realM25) | ///
				 ( F4.fedincrease!=1 & F4.overallcountgr>0 & F4.fedincrease!=. & F4.overallcountgr!=. & (wagebins)/100==MW_realM25 ) ///
				& year>=1979 & cleansample ==1	`if`Ycounter''    ${weight`Y'`b'}			
				
				global AS`Y' = $B - (r(mean)/${E})
				local affsh`Y' : di  %04.3f ${AS`Y'}
				
				
				
				****************************************************************
				*************    Wagebill related globals       ****************
				****************************************************************
				
				sum `Y'WBpcrsumFH if (  F.fedincrease!=1 & F.overallcountgr>0 & F.fedincrease!=. & F.overallcountgr!=.   & (wagebins+25)/100==F1MW_realM25) | ///
				 (F2.fedincrease!=1 & F2.overallcountgr>0 & F2.fedincrease!=. & F2.overallcountgr!=. & (wagebins+25)/100==F2MW_realM25 ) | ///
				(F3.fedincrease!=1 & F3.overallcountgr>0 & F3.fedincrease!=. & F3.overallcountgr!=.  & (wagebins+25)/100==F3MW_realM25) | ///
				 (  F4.fedincrease!=1 & F4.overallcountgr>0 & F4.fedincrease!=. & F4.overallcountgr!=.  & (wagebins+25)/100==F4MW_realM25 ) ///
				& year>=`b' & cleansample ==1	`if`Ycounter''    ${weight`Y'`b'}			
					
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

				
				di "${rtype`s'} `Y'countpc  ${treatafter} one if  year>=`b'  & cleansample==1 `if`Ycounter''   ${weight`Y'`b'} , a(${a`s'} ${controlbefore}  ${window} ${placebocontafter1} ${placebobefore1} ${windowpl1} ${placebocontafter2} ${placebobefore2} ${windowpl2} ${controlf}  ${control}  )  cluster(statenum)"
				${rtype`s'} `Y'countpc  ${treatafter} one if  year>=`b'  & cleansample==1 `if`Ycounter''   ${weight`Y'`b'} , a(${a`s'} ${controlbefore}  ${window} ${placebocontafter1} ${placebobefore1} ${windowpl1} ${placebocontafter2} ${placebobefore2} ${windowpl2} ${controlf}  ${control}  )  cluster(statenum)
				est save "${estimates}`Y'`Ycounter'pl0after`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}", replace 

				di "${rtype`s'} `Y'countpc ${placeboafter1} one if  year>=`b'  & cleansample==1 `if`Ycounter''   ${weight`Y'`b'} , a(${a`s'} ${controlbefore}  ${controlafter}  ${window}  ${placebocontafter2} ${windowpl2}  ${placebobefore2} ${placebobefore1} ${windowpl1} ${controlf}  ${control}  )  cluster(statenum)"
				${rtype`s'} `Y'countpc ${placeboafter1} one if  year>=`b'  & cleansample==1 `if`Ycounter''   ${weight`Y'`b'} , a(${a`s'} ${controlbefore}  ${controlafter}  ${window}  ${placebocontafter2} ${windowpl2}  ${placebobefore2} ${placebobefore1} ${windowpl1} ${controlf}  ${control}  )  cluster(statenum)
				est save "${estimates}`Y'`Ycounter'pl1after`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}", replace 
 		
				di "${rtype`s'} `Y'countpc ${placeboafter2} one if  year>=`b'  & cleansample==1 `if`Ycounter''   ${weight`Y'`b'} , a(${a`s'} ${controlafter}  ${controlbefore}  ${window} ${placebocontafter1}  ${placebobefore1} ${windowpl1} ${placebobefore2} ${windowpl2} ${controlf}  ${control}  )  cluster(statenum)"
				${rtype`s'} `Y'countpc ${placeboafter2} one if  year>=`b'  & cleansample==1 `if`Ycounter''   ${weight`Y'`b'} , a(${a`s'} ${controlafter}  ${controlbefore}  ${window} ${placebocontafter1}  ${placebobefore1} ${windowpl1} ${placebobefore2} ${windowpl2} ${controlf}  ${control}  )  cluster(statenum)
				cap drop sample_indic
				gen sample_indic = e(sample)
				sum quarterdate if sample_indic==1, meanonly
				estadd scalar sample_beg=r(min)
				estadd scalar sample_end=r(max)
				cap drop sample_indic
				est save "${estimates}`Y'`Ycounter'pl2after`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}", replace 
		
		
		
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
				est use "${estimates}`Y'`Ycounter'pl0after`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
				local tmax=0
				local denominator = (1/(1+(`tmax'/4)))
				*Alternatively, we can use the original estimated values for the true window
				
				local wmax = `truewmax'
				local wmin = `truewmin'

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
 					if `k' == -`wmin' {
						mat countmat = [ `k', `basic_b',  `basic_b'-1.96*`basic_se', `basic_b'+1.96*`basic_se' ]
					}
					else {
						mat countmat = [countmat \ [ `k', `basic_b',  `basic_b'-1.96*`basic_se', `basic_b'+1.96*`basic_se'  ]]
					}
				}
				

				
				****************************************************************
				*************         First placebo group         **************
				****************************************************************
				
				
				est use "${estimates}`Y'`Ycounter'pl1after`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
				

				local denominator = (1/(1+(`tmax'/4)))
				local wmin = `truewmax' + 1
				local wmax = `placebo1wmax'

				forval j = `wmin'/`wmax' {
					global PA_p`j' "[treat_p`j' ]*(4)*(1/${E})"
					global PA_p`j' "`denominator'*(${PA_p`j'})" 
				}
				

				
				forval k = `wmin'/`wmax' {	
					 di `k'
						local lincomline "${PA_p`k'}"
					
 					qui lincom   "`lincomline'"
					local basic_b = r(estimate)
					local basic_se = r(se)
						mat countmat = [countmat \ [ `k', `basic_b',  `basic_b'-1.96*`basic_se', `basic_b'+1.96*`basic_se']]
				}
				

				****************************************************************
				************        Last placebo group         *****************
				****************************************************************
				
				est use "${estimates}`Y'`Ycounter'pl2after`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
				local denominator = (1/(1+(`tmax'/4)))

				local wmin = `placebo1wmax' + 1
				local wmax = `placebo2wmax' + 1
				
				sum sum_inf if overallcountgr>0 & overallcountgr!=. & fedincrease!=1   ${weight`Y'`b'}
				scalar numbins = r(mean)

				forval j = `wmin'/`wmax' {
				if `j' != `wmax'{
					global PA_p`j' "[treat_p`j' ]*(4)*(1/${E})"
					global PA_p`j' "`denominator'*(${PA_p`j'})" 
				}
				if `j' == `wmax'{
					sum sum_inf if overallcountgr>0 & overallcountgr!=. & fedincrease!=1   ${weight`Y'`b'}
					scalar numbins = r(mean)
					
					
					global PA_p`j' "[treat_p`j' ]*(`=numbins')*(1/${E})"
					global PA_p`j' "`denominator'*(${PA_p`j'})" 
					}
				}
				

				
				global cumul "0"
				forval k = `wmin'/`wmax' {	
						local lincomline "${PA_p`k'}"
					
 					qui lincom   "`lincomline'"
					local basic_b = r(estimate)
					local basic_se = r(se)
						mat countmat = [countmat \ [ `k', `basic_b',  `basic_b'-1.96*`basic_se', `basic_b'+1.96*`basic_se']]
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
			
 			gen totest = sum(est) if est!=.

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
				
				
				keep if bin!=.
				keep bin est low high totest
				foreach vv of varlist est low high totest{
				rename `vv' `vv'`Ycounter'
				}
				count 
				if `Ycounter'==1{
				save "${data}tempmatchlong_1.dta", replace
				}
				else{
				merge 1:1 bin using "${data}tempmatchlong_1.dta", assert(3) nogenerate
				save "${data}tempmatchlong_1.dta", replace
				
				}				
				restore


				
		}
			
	}
 }
*



local name "firstyear"
local num 1
use "${data}tempmatchlong_`num'.dta", clear

twoway ///
(bar est2 bin , barwidth(1) fcolor(ltblue) blcolor(blue))  ///
(rcap low2  high2 bin  , lcolor(ltblue*1.25) lwidth(thick) ) ///
(line totest2 bin, lcolor(ltblue*1.26) lpat(line) lwidth(thick) ) ///
, xlabel(-4(2)16 17 "17+"  ///
, labsize(medsmall) ) ///
ylabel(-0.012(0.004)0.012 , labsize(medsmall)) scheme(s1color) /*xtitle("Wage Bin")*/ xtitle("Wage bins in `=char(36)' relative to new MW", size(medsmall)) xsc(titlegap(*10))    ///
ytitle("Difference between actual and counterfactual new entrant employment count" "relative to the pre-treatment total employment", height(10) size(small))   ///
				text(0.0102 11.73  "{&Delta}a =  0.006 (0.001)" "{&Delta}b = -0.005 (0.001)" "%{&Delta} affected employment =  0.008 (0.034)" ///	
				"%{&Delta} affected wage =  0.019 (0.013)", box  fcolor(white) margin(0.2 0.2 1 1) justification(right)  ) ///
legend(off)
graph export "${figures}Figure4newentrant_`name'.pdf", replace 




twoway ///
(bar est1 bin , barwidth(1) fcolor(green*0.5) blcolor(green))  ///
(rcap low1  high1 bin  , lcolor(ltblue*1.25) lwidth(thick) ) ///
(line totest1 bin, lcolor(green*0.7) lpat(line) lwidth(thick) ) ///
, xlabel(-4(2)16 17 "17+"  ///
, labsize(medsmall) ) ///
ylabel( -0.012(0.004)0.012, labsize(medsmall)) scheme(s1color) /*xtitle("Wage Bin")*/ xtitle("Wage bins in `=char(36)' relative to new MW", size(medsmall)) xsc(titlegap(*10)) ytitle("", height(10))   ///
ytitle("Difference between actual and counterfactual incumbent employment count" "relative to the pre-treatment total employment", height(10) size(small))   ///
				text(0.0102 11.73  "{&Delta}a =  0.014 (0.002)" "{&Delta}b = -0.013 (0.002)" "%{&Delta} affected employment =  0.009 (0.068)" ///	
				"%{&Delta} affected wage =  0.095 (0.020)", box  fcolor(white) margin(0.2 0.2 1 1) justification(right)  ) ///
legend(off)
graph export "${figures}Figure4incumbent_`name'.pdf", replace 


