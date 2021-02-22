

use "${data}qcew_multiplier.dta", clear
keep statenum quarterdate multiplier
tempfile qcew
save `qcew'


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

merge 1:1 statenum quarterdate using "${data}VZmw_quarterly_lagsleads_1979_2016.dta"
drop _merge
merge 1:1 statenum quarterdate using "${data}eventclassification.dta", nogenerate assert(1 3)
replace overallcountgroup = 0 if overallcountgroup==.
assert overallcountgroup!=0 if alltreat==1 & toosmall==0
	
	
	

preserve
use "${data}cpiursai1977-2016.dta", clear
*Dropping an empty column and column that calculates yearly average (simple)
drop v14 avg
reshape long month, i(year) j(m)
rename month cpi
rename m month
keep if year >= 1979
*If base year needs to be changed, use here.
local baseyear=2016
sum cpi if year == `baseyear', meanonly
local cpibase = r(mean)
replace cpi = 100 * cpi / `cpibase'
gen monthdate = ym(year,month)
gen quarterdate = qofd(dofm(monthdate))
collapse cpi, by(quarterdate)
tempfile cpiquarter
save `cpiquarter'
restore


cap drop _merge
merge m:1 quarterdate using `cpiquarter', nogenerate assert(3)


xtset statenum quarterdate	


g MW = exp(logmw) 
g DMW = D.MW
g MW_real = exp(logmw)/(cpi/100) 
g DMW_real = D.MW_real 

g MW_realM25 = MW_real - mod(MW_real,0.25)
g DMW_realM25 = D.MW_realM25
	
	
	
compress
	
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

*

xtset statenum quarterdate

	cap drop treat 
	cap drop _treat
	g _treat_p=0
		replace _treat = 1 if overallcountgroup>0 & fedincrease!=1
	 g treat = ( _treat +  L._treat + L2._treat + L3._treat)
	cap drop _treat


cap drop  Dtreat
g Dtreat = D.treat

cap drop _cont
g _cont = 0
replace _cont = 1 if ($missedevents | toosmall==1) 
g tempcont = _cont + L._cont + L2._cont + L3._cont

local Tmax3 = 16 + 3
forval i = 0(1)`Tmax3'{
	if `i' == 0{
	g postcont = _cont
	}
	else {
	replace postcont = postcont + L`i'._cont
	sum postcont, meanonly
	assert r(mean)!=0
	}
}
cap drop _cont


cap drop _contf
cap drop postcontf

g _contf = 0
replace _contf = 1 if (fedincrease==1 &  overallcountgroup>0 )
g tempcontf = _contf + L._contf + L2._contf + L3._contf

local Tmax3 = 16 + 3
forval i = 0(1)`Tmax3'{
	if `i' == 0{
	g postcontf = _contf

	}
	else {
	replace postcontf = postcontf + L`i'._contf
	sum postcontf, meanonly
	assert r(mean)!=0
	}
}
cap drop _contf

g precontf = F4.tempcontf
g precont = F4.tempcont

replace precontf = 0 if precontf == .
replace precont = 0 if precont  == .


g earlycontf = F12.tempcontf + F8.tempcontf
g earlycont = F12.tempcont + F8.tempcont

replace earlycontf = 0 if earlycontf == .
replace earlycont = 0 if earlycont  == .



foreach j of numlist 4(4)16{
		cap drop F`j'treat
		cap drop L`j'treat
		
		g F`j'treat = F`j'.treat
 		g L`j'treat = L`j'.treat
		
		replace F`j'treat = 0 if F`j'treat ==.
		replace L`j'treat = 0 if L`j'treat ==.
		

	}
*

replace postcont = 0 if postcont == .
replace postcontf = 0 if postcontf == .


g cleansample = 1
replace cleansample = 0 if quarterdate >=136 & quarterdate <=142

egen avetotalpopulation = mean(totalpopulation) , by(statenum)


*Note that we do not use most of these either, yet I keep this global in case it is needed.

global a1  i.statenum i.quarterdate  // TWFE


********************************************************************************
**************       Define Treatments         *********************************
********************************************************************************
*To keep all as flexible as possible, I am adding lincom too.

global treat   		F12treat F8treat F4treat treat  L4treat L8treat L12treat L16treat 

*global minus 0
global minus (F4treat)
global lincomagg 	(treat-$minus)
forval i = 4(4)16{
global lincomagg "$lincomagg + (L`i'treat - $minus ) "
}
*

global label1 TWFE
global grlabel1 TWFE


********************************************************************************
***************       The Regressions ******************************************
********************************************************************************		
		


gen all=1
xtset statenum quarterdate

tempfile temptemp
save `temptemp', replace	

local regsample ""
local graphname "o"
global weight2 [aw=altweight]
global weight1 [aw=avetotalpopulation]
global weight0 [aw=all]
local ylabel `"ylabel(-0.2 "-0.2" -0.1 "-0.1" 0 "0" 0.1 "0.1" , labsize(medium))"' 

local fifth_title 		"Bottom 50%"
local sixth_title 		"Top 50%"



foreach regsample in sixth_ fifth_  {
foreach b in   1979   {
	foreach w in 1 /*0*/  {
		
				sum epop if  year>=`b' & cleansample==1  ${weight`w'},meanonly
				local epop = r(mean)
				sum DMW_real if fedincrease!=1 & overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`w'}
				local mwc = r(mean)
				sum MW_real if  (F.fedincrease != 1 & F.fedincrease != . & F.overallcountgr>0 & F.overallcountgr!=. )  & year>=`b' & cleansample ==1    ${weight`w'}
				local mw = r(mean)
				local mwpc = `mwc'/`mw'
				local mwpc2 : di  %04.3f `mwpc'

				global E = `epop'
					
				global C = 1/(`epop'*`mwpc')
		
		foreach s in      1 {
		use `temptemp', clear
			forval k=1(1)30{
			 reghdfe `regsample'percap`k' $treat  if  year>=`b' & cleansample==1  ${weight`w'} , absorb(${a`s'} postcont postcontf  precont precontf earlycont earlycontf ) cluster(statenum)
				
				local dof = e(df_r)
				
				foreach q in 12  {
				lincom (($lincomagg) * (1/5))*${C}
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
			foreach q in 12 {
				forval j = 31/34 {   // 32 to 31: couner changed here; but 34 kept same
					mat countmat`q' = [countmat`q' \ [`j', .,.,.] ]
				} // end of j
			} // end of q

			qui reghdfe `regsample'epop $treat  if  year>=`b' & cleansample==1 ${weight`w'} , absorb(${a`s'} postcont postcontf  precont precontf earlycont earlycontf ) cluster(statenum)
			local dof = e(df_r)

			foreach q in 12  {	
				lincom (($lincomagg) * (1/5))*${C}
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

				local titlesixth_	"FigureG2_b"		
				local titlefifth_	"FigureG2_d"		
				
				twoway ( bar est wagebin if wagebin<32, fcolor(ltblue) ) ( bar est wagebin if wagebin==35, fcolor(purple) lcolor(purple) ) ///
				(rcap low  high wagebin if wagebin<32, lcolor(green) lwidth(thick) ) (line totest wagebin, lpat(dot) lwidth(thick) lcolor(purple)) , ///
				xlabel( 5 "$5"  10 "$10" 15 "$15" 20 "$20" 25 "$25" 31 ">$30" 35 "Total", labsize(medium)) ///
				ytitle("Employment elasticity") ///
				`ylabel' ///
				scheme(s1color) xtitle("Wage") legend(off) title(`"``regsample'title'"') ///
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

