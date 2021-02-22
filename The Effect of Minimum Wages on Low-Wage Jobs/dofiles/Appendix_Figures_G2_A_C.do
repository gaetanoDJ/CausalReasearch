
*This do file is created by DC on 03/25/2016
********************************************************************************
***********          Alternate Specifications of Jobs Dist.      ***************
********************************************************************************
*This specification puts  leads and lags. Starts from 1995 and only clean sample
*Overall sample is used.


clear all
set more off

********************************************************************************
**************    Reshaping the original data    *******************************
********************************************************************************

use "${data}qcew_multiplier.dta", clear
keep statenum quarterdate multiplier 
assert multiplier!=0
tempfile qcew
save `qcew'




use "${data}VZmw_quarterly_lagsleads_1979_2016",clear
gen mw = exp(logmw)
gen year = year(dofq(quarterdate))
gen quarter = quarter(dofq(quarterdate))
collapse  mw, by(statenum year)


xtset statenum year
forvalues index = 1/4 {
	foreach lagorlead in L F {
		gen `lagorlead'`index'logmw = log(`lagorlead'`index'.mw)
	}
}
gen logmw = log(mw)
gen L0logmw = logmw
*
keep if year>=1979 
xtset statenum year
assert "`r(balanced)'" == "strongly balanced"
keep year statenum logmw L*mw F*mw

compress

tempfile mwdata
save `mwdata'


********************************************************************************
****************               Reshaping the data                  *************
********************************************************************************


*Groups are as follows:
*First: Top 10%.
*Second: Top 15%.
*Third: Top 20%.
*Fourth: Top 10%-50%.
*Alt_fourth: Top 20%-50%.
*Fifth: Bottom 50%.

use "${data}CK_groups.dta", clear
gen sixth_count = first_count + fourth_count
foreach vv in fifth_ sixth_ {
	cap drop `vv'countall
	bys statenum quarterdate: egen `vv'countall = total(`vv'count)
}	
gen countall = sixth_countall + fifth_countall

merge m:1 statenum quarterdate using `qcew', assert(3) nogenerate

foreach vv in fifth_ sixth_ {
		replace `vv'count = `vv'count * multiplier		
		replace `vv'countall = `vv'countall * multiplier
}	
*
replace countall = countall * multiplier

gen wagebindollar = floor(wagebin/100)
drop wagebins
rename wagebindollar wagebins

collapse (sum) fifth_count sixth_count , by(statenum quarterdate wagebins) fast

foreach ss in statenum quarterdate wagebins{
preserve
keep `ss'
duplicates drop
tempfile `ss'
save ``ss''
restore
}
*

preserve
use `statenum', clear
cross using `quarterdate'
cross using `wagebins'
tempfile for_merge
save `for_merge'
restore

merge 1:1 statenum quarterdate wagebins using `for_merge', assert(2 3) nogenerate


foreach vv of varlist fifth_count sixth_count {
replace `vv' = 0 if `vv' ==.
}
*

reshape wide fifth_count sixth_count , i(statenum quarterdate) j(wagebin)

tempfile CK_groups
save `CK_groups'



use statenum quarterdate  population using "${data}state_panels_cents_balanced_add_QJE.dta", clear
egen tagger = tag(statenum quarterdate)
keep if tagger
cap drop tagger
tempfile pop
save `pop'

use `CK_groups', clear
merge m:1 statenum quarterdate using `pop', assert(3) nogenerate
	
ds *count*
gen year = year(dofq(quarterdate))
collapse (mean) population `r(varlist)'  ,by(statenum year)

merge 1:1 statenum year using `mwdata',assert(3) nogenerate
	
	
	
	
********************************************************************************
*************            Preparing the data for regressions       **************
********************************************************************************	
	
	
rename population totalpopulation	

forval j = 1/30 { 
gen count`j' = sixth_count`j' + fifth_count`j'
}
*

foreach vv in fifth_ sixth_ {
	cap drop `vv'countall
	gen `vv'countall = `vv'count1
	forval j = 2/30 { 
	replace `vv'countall = `vv'count`j' + `vv'countall 
	}
}	
*
gen countall = sixth_countall + fifth_countall
g epop    = countall/totalpopulation

foreach vv in fifth_ sixth_ {
g `vv'epop    = `vv'countall/totalpopulation

}

foreach vv in fifth_ sixth_ {
	forval j = 1/30 { 
	g `vv'percap`j' = `vv'count`j'/totalpopulation
	}
}
*

forval j = 1/30 { 
	g percap`j' = count`j'/totalpopulation
}
*



g cleansample = 1
replace cleansample = 0 if year >=1994 & year <=1995

egen avetotalpopulation = mean(totalpopulation) , by(statenum)


*Note that we do not use most of these either, yet I keep this global in case it is needed.

global a1  i.statenum i.year  // TWFE


********************************************************************************
**************       Define Treatments         *********************************
********************************************************************************
*To keep all as flexible as possible, I am adding lincom too.

global treat   		F2logmw F1logmw logmw L1logmw L2logmw L3logmw L4logmw 
global lincom12    	((logmw) + (L1logmw+ logmw) + (L2logmw+L1logmw+logmw) + (L3logmw+L2logmw+L1logmw + logmw)+ (L4logmw+L3logmw+L2logmw+L1logmw + logmw))/5
	
global label1 TWFE
global grlabel1 TWFE


********************************************************************************
***************       The Regressions ******************************************
********************************************************************************		
		


gen all=1
xtset statenum year

tempfile temptemp
save `temptemp', replace	

local regsample ""
local graphname "o"
global weight2 [aw=altweight]
global weight1 [aw=avetotalpopulation]
global weight0 [aw=all]
local ylabel `"ylabel(-0.2 "-0.2" -0.1 "-0.1" 0 "0" 0.1 "0.1" , labsize(medium))"' 

local first_title 		"Top 10%"
local third_title 		"Top 20%"
local fourth_title 		"10%-50%"
local alt_fourth_title 	"20%-50%"
local fifth_title 		"Bottom 50%"
local sixth_title 		"Top 50%"


*To get semi elasticity, get rid of denominator.

foreach regsample in fifth_ sixth_ {
foreach b in   1979  {
	foreach w in 1   {
		
		sum epop if  year>=`b' & cleansample==1  ${weight`w'}
		local denominator = r(mean)
		foreach s in      1  {
		use `temptemp', clear
			forval k=1(1)30{

			 reghdfe `regsample'percap`k' $treat  if  year>=`b' & cleansample==1  ${weight`w'} , absorb(${a`s'}) cluster(statenum)
				
				local dof = e(df_r)
				
				foreach q in 12 /*F1*/ {
				lincom (${lincom`q'})/`denominator'
				local lowb	= r(estimate) - invttail(`dof', 0.025)*r(se) 
				local highb	= r(estimate) + invttail(`dof', 0.025)*r(se) 
				
				
				if `k' == 1 {
							mat countmat`q' = [`k', r(estimate), `lowb' , `highb']
				} // end of if
				else {
					mat countmat`q' = [countmat`q' \ [`k', r(estimate), `lowb' , `highb' ]]
				} // end of else
			} // end of q 
			} // end of k
			foreach q in 12 /*F1*/ {
				forval j = 31/34 {   // 32 to 31: couner changed here; but 34 kept same
					mat countmat`q' = [countmat`q' \ [`j', .,.,.] ]
				} // end of j
			} // end of q

			qui reghdfe `regsample'epop $treat  if  year>=`b' & cleansample==1 ${weight`w'} , absorb(${a`s'}) cluster(statenum)
			local dof = e(df_r)

			foreach q in 12 /*F1*/ {	
				qui	lincom (${lincom`q'})/`denominator'
				scalar ba_`w' = r(estimate)
				scalar sea_`w'= r(se)
				scalar ta_`w'= r(estimate)/r(se)				
				scalar pa_`w' = ttail(r(df), r(estimate)/r(se))
				local lowb	= r(estimate) - invttail(`dof', 0.025)*r(se) 
				local highb	= r(estimate) + invttail(`dof', 0.025)*r(se) 
				mat countmat`q' = [countmat`q' \ [35, r(estimate), `lowb' , `highb' ]]
				svmat countmat`q'
				cap drop wagebin est low high totest
				rename countmat`q'1 wagebin
				rename countmat`q'2 est
				rename countmat`q'3 low
				rename countmat`q'4 high

 				g totest = est if _n==1
				replace totest = totest[_n-1] + est if _n>1
				
				local ba: di %04.3f `=ba_`w''
				local se: di %04.3f `=sea_`w''

				local titlesixth_	"FigureG2_a"		
				local titlefifth_	"FigureG2_c"		
				
				
				twoway ( bar est wagebin if wagebin<32, fcolor(ltblue) ) ( bar est wagebin if wagebin==35, fcolor(purple) lcolor(purple) ) ///
				(rcap low  high wagebin if wagebin<32, lcolor(green) lwidth(thick) ) (line totest wagebin, lpat(dot) lwidth(thick) lcolor(purple)) , ///
				xlabel( 5 "$5"  10 "$10" 15 "$15" 20 "$20" 25 "$25" 31 ">$30" 35 "Total", labsize(medium)) ///
				ytitle("Employment elasticity") ///
				`ylabel' ///
				scheme(s1color) xtitle("Wage") legend(off)  ///
				text(-0.18 8  "Employment elas. = `ba' (`se')" , box  fcolor(white) margin(0.2 0.2 1 1) justification(left) ) ///
				name(jd_`graphname'_${label`s'}_w`w'_b`b'_`q', replace)
				graph export "${figures}`title`regsample''.pdf", replace 
				

				
			}
		}
	}
}
}
*
*		

