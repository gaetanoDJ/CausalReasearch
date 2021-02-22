
*This do file is created by DC on 03/30/2016
********************************************************************************
***************************   Figure 2      ************************************
********************************************************************************

set more off

********************************************************************************
********************************************************************************
********************************************************************************


		
**** DEFINE REGRESSION COMMAND AND CONTROLS *****

 
	global rtype1 reghdfe


global a1  i.wagebinstate i.wagequarterdate  // distributional TWFE


global label1 TWFE
local truewmin = 4
local truewmax = 4
local placebo1wmax = 13
local prevwmax = `placebo1wmax'
local placebo2wmax = 16


use "${data}qcew_multiplier.dta", clear
keep statenum quarterdate multiplier
tempfile qcew
save `qcew'


treatmentcontrolwindows




placebowindows1, truewmax(`truewmax') wmax(`placebo1wmax')

placebowindows2, prevwmax(`prevwmax') wmax(`placebo2wmax')

treatmentdefinition

merge m:1 statenum quarterdate using `qcew',assert(3) nogenerate
replace count = count*multiplier
replace overallcountpc=overallcountpc*multiplier
replace overallcountpcall = overallcountpcall * multiplier


********************************************************************************
********************      Dropping unnecessary variables        ****************
********************************************************************************

cap drop HSD*
cap drop HSL*
cap drop black*
cap drop female*
cap drop BH*
cap drop teen*	

********************************************************************************
*****************          Placebo sample correction          ******************
********************************************************************************

placebosamplecorr1, wmax(`placebo1wmax') truewmax(`truewmax') 
placebosamplecorr2, wmax(`placebo2wmax') prevwmax(`placebo1wmax')


********************************************************************************
***********************           Weights           ****************************
********************************************************************************
xtset wagebinstate quarterdate

foreach Y in   overall { 
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
foreach w of numlist 1  {
foreach Y in   overall { 
foreach b in    1979 {
					***  define MW change ***
				if `w'==0{
				global weight`Y'`b' " "
				}
				else{
				global weight`Y'`b' "[aw=wt`Y'`b']" 
				}
				sum DMW_real if fedincrease!=1 & overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`Y'`b'}
				local mwc = r(mean)
				sum MW_real if  F.fedincrease!=1 & F.overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`Y'`b'}
				local mw = r(mean)
				local mwpc = `mwc'/`mw'
				local mwpc2 : di  %04.3f `mwpc'

				sum `Y'countpcall if (( F.overallcountgr>0 & F.overallcountgr!=. & F.fedincrease!=1  & F.fedincrease!=.) | ///
				(F2.overallcountgr>0  & F2.overallcountgr!=. & F2.fedincrease!=1 & F2.fedincrease!=. ) ///
				| ( F3.overallcountgr>0 & F3.overallcountgr!=. & F3.fedincrease!=1 & F3.fedincrease!=. ) ///
				| (F4.overallcountgr>0 & F4.overallcountgr!=. & F4.fedincrease!=1  & F4.fedincrease!=. ) ) & year>=`b' & cleansample ==1 ///
				 ${weight`Y'`b'}		

				 local epop = r(mean)
				global E = `epop'
					
				global C = 1/(`epop'*`mwpc')
			
				
			
				foreach s in  1     {		
				*** REGRESSION *** 
				di "Weight is ${weight`Y'`b'}."
				
				${rtype`s'} `Y'countpc  ${treatafter} one if  year>=`b'  & cleansample==1 ${weight`Y'`b'} , a(${a`s'} ${controlbefore}  ${window} ${placebocontafter1} ${placebobefore1} ${windowpl1} ${placebocontafter2} ${placebobefore2} ${windowpl2} ${controlf}  ${control}  )  cluster(statenum)
				est save "${estimates}forplacebofig_true`limitsample'`limittreat'_`s'_`Y'_`b'_w`w'_`temp1'_`temp2'_${Tmax}${Tmin}", replace 

				${rtype`s'} `Y'countpc ${placeboafter1} one if  year>=`b'  & cleansample==1 ${weight`Y'`b'} , a(${a`s'} ${controlbefore}  ${controlafter}  ${window}  ${placebocontafter2} ${windowpl2}  ${placebobefore2} ${placebobefore1} ${windowpl1} ${controlf}  ${control}  )  cluster(statenum)
				est save "${estimates}forplacebofig_pl1`limitsample'`limittreat'_`s'_`Y'_`b'_w`w'_`temp1'_`temp2'_${Tmax}${Tmin}", replace 
				
				${rtype`s'} `Y'countpc ${placeboafter2} one if  year>=`b'  & cleansample==1 ${weight`Y'`b'} , a(${a`s'} ${controlafter}  ${controlbefore}  ${window} ${placebocontafter1}  ${placebobefore1} ${windowpl1} ${placebobefore2} ${windowpl2} ${controlf}  ${control}  )  cluster(statenum)
				est save "${estimates}forplacebofig_pl2`limitsample'`limittreat'_`s'_`Y'_`b'_w`w'_`temp1'_`temp2'_${Tmax}${Tmin}", replace 
				

				****************************************************************
				**************     Placebo figure      *************************
				****************************************************************
				*Defining globals to get time averaged estimates. Need to define here.
			
				
				local tmax = 16
				est use "${estimates}forplacebofig_true`limitsample'`limittreat'_`s'_`Y'_`b'_w`w'_`temp1'_`temp2'_${Tmax}${Tmin}"

				
				
				local denominator = (1/(1+(`tmax'/4)))
				local wmax = `truewmax'
				local wmin = `truewmin'

				global PA_p0 "[treat_p0  ]*(4)*(1/${E})"		// Multiplying by 4 to get $ bin estimates. Dividing by E to get share of workforce.
				foreach t of numlist 4(4)`tmax' {
					global PA_p0  "${PA_p0} + [ L`t'treat_p0 ]*(4)*(1/${E})"
				}
				global PA_p0 "`denominator'*(${PA_p0})"

				forval j = 1/`wmax' {
					global PA_p`j' "[treat_p`j' ]*(4)*(1/${E})"
						foreach t of numlist 4(4)`tmax' {
							global PA_p`j'    " ${PA_p`j'} + [L`t'treat_p`j' ]*(4)*(1/${E})"
						}
					global PA_p`j' "`denominator'*(${PA_p`j'})" 
				}
				
				forval j = 1/`wmin' {				
					global PA_m`j' "[treat_m`j' ]*(4)*(1/${E})"
						foreach t of numlist 4(4)`tmax' {
							global PA_m`j'    " ${PA_m`j'} + [L`t'treat_m`j' ]*(4)*(1/${E})"
						}
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
				
				
				est use "${estimates}forplacebofig_pl1`limitsample'`limittreat'_`s'_`Y'_`b'_w`w'_`temp1'_`temp2'_${Tmax}${Tmin}"
				

				local denominator = (1/(1+(`tmax'/4)))
				local wmin = `truewmax' + 1
				local wmax = `placebo1wmax'

				forval j = `wmin'/`wmax' {
					global PA_p`j' "[treat_p`j' ]*(4)*(1/${E})"
						foreach t of numlist 4(4)`tmax' {
							global PA_p`j'    " ${PA_p`j'} + [L`t'treat_p`j' ]*(4)*(1/${E})"
						}
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
				
				est use "${estimates}forplacebofig_pl2`limitsample'`limittreat'_`s'_`Y'_`b'_w`w'_`temp1'_`temp2'_${Tmax}${Tmin}"
				local denominator = (1/(1+(`tmax'/4)))

				local wmin = `placebo1wmax' + 1
				local wmax = `placebo2wmax' + 1
				
				sum sum_inf if overallcountgr>0 & overallcountgr!=. & fedincrease!=1   ${weight`Y'`b'}
				scalar numbins = r(mean)

				forval j = `wmin'/`wmax' {
				if `j' != `wmax'{
					global PA_p`j' "[treat_p`j' ]*(4)*(1/${E})"
						foreach t of numlist 4(4)`tmax' {
							global PA_p`j'    " ${PA_p`j'} + [L`t'treat_p`j' ]*(4)*(1/${E})"
						}
					global PA_p`j' "`denominator'*(${PA_p`j'})" 
				}
				if `j' == `wmax'{
					sum sum_inf if overallcountgr>0 & overallcountgr!=. & fedincrease!=1   ${weight`Y'`b'}
					scalar numbins = r(mean)
					
					
					global PA_p`j' "[treat_p`j' ]*(`=numbins')*(1/${E})"
						foreach t of numlist 4(4)`tmax' {
							global PA_p`j'    " ${PA_p`j'} + [L`t'treat_p`j' ]*(`=numbins')*(1/${E})"
						}
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
 			 
				twoway ( bar est bin  , fcolor(ltblue) lcolor(none) ) ///
				(rcap low  high bin  , lcolor(ltblue*1.25) lwidth(thick) )  ///
				(line totest bin, lcolor(red*.33) lpat(line) lwidth(thick) ) 		, ///
				  xlabel(-4 "-4"  -2 "-2"  0 "0"  2 "2"  4 "4" 6 "6" 8 "8" 10 "10" 12 "12" 14 "14" 17 "17+" ) ///
				  ///
				ylabel(, labsize(medsmall)) scheme(s1color) /*xtitle("Wage Bin")*/ xtitle("Wage bins in `=char(36)' relative to new MW", size(medsmall)) xsc(titlegap(*10)) ytitle("", height(10))   ///
				 legend(off) 
				
				graph export "${figures}Figure2.pdf", replace
			
		}
			
	}
 }
}
 *


