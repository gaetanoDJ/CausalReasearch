
*This do file is created by DC on 05/11/2018.
********************************************************************************
****************         QCEW vs. CPS-ORG Employment              **************
********************************************************************************


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


use "${data}VZmw_quarterly_lagsleads_1979_2016.dta",clear
merge m:1 quarterdate using `cpiquarter', nogenerate assert(3)

cap drop _merge


xtset statenum quarterdate	


g MW = exp(logmw) 
g DMW = D.MW
g MW_real = exp(logmw)/(cpi/100) 
g DMW_real = D.MW_real 

g MW_realM25 = MW_real - mod(MW_real,0.25)
g DMW_realM25 = D.MW_realM25

tempfile mw_data
save `mw_data'



use statenum quarterdate population using "${data}state_panels_cents_balanced_add_QJE.dta", clear
egen tagger = tag(statenum quarterdate)
keep if tagger
drop tagger
tempfile pop
save `pop'


use "${data}totransfer.dta", clear

keep month state age marital race sex esr ethnic uhours earnhr uearnwk earnwt ///
uhourse paidhre earnhre earnwke I25a I25b I25c I25d year lfsr89 lfsr94 statenum ///
orgwt monthdate quarterdate quarter division region censusnum stfips gradeat gradecp ihigrdc ///
grade92 unioncov unionmme hourslw class class94 smsa80 smsa93 smsa04 smsastat

 
cap drop mlr
gen mlr = .
replace mlr = esr if year >= 1979 & year <= 1988
replace mlr = lfsr89 if year >= 1989 & year <= 1993
replace mlr = lfsr94 if year >= 1994

gen emp = mlr == 1 | mlr == 2 if mlr ~= .

replace earnwt = earnwt/3
collapse (sum) emp [iw=earnwt], by(statenum quarterdate)

rename emp CPS_emp
merge 1:1 statenum quarterdate using "${data}qcew_multiplier.dta", assert(3) nogenerate
rename emp qcew_emp
merge 1:1 statenum quarterdate using "`pop'", assert(3) nogenerate
rename population totalpopulation
merge 1:1 statenum quarterdate using "${data}eventclassification.dta", assert(1 3) nogenerate
merge 1:1 statenum quarterdate using `mw_data', assert(2 3) nogenerate keep(3)
replace overallcountgroup = 0 if overallcountgroup==.
assert overallcountgroup!=0 if alltreat==1 & toosmall==0
sum overallcountgroup,meanonly
local upperquant = r(max)


gen qcew_cps_epop = (qcew_emp-CPS_emp)/totalpopulation

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
replace postcontf = 1 if postcontf == .


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

tempfile temptemp
save `temptemp', replace	

local regsample ""
local graphname "o"
global weight2 [aw=totalpopulation]
global weight1 [aw=countall]
global weight0 [aw=all] 
local ylabel "ylabel( , labsize(medium))" 


*To get semi elasticity, get rid of denominator.
			local counter = 0
foreach b in   1979   {
	foreach w in 2   {
		foreach s in  1 {
		use `temptemp', clear
				*Directly from the main tables.
				sum DMW_real if fedincrease!=1 & overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`w'}
				local mwc = r(mean)
				sum MW_real if  F.fedincrease!=1 & F.overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`w'}
				local mw = r(mean)
				local mwpc = `mwc'/`mw'
				local mwpc2 : di  %04.3f `mwpc'

				global E = 1
				
				
			qui reghdfe qcew_cps_epop $treat if  year>=`b' & cleansample==1 ${weight`w'} , absorb( ${a`s'} postcont postcontf  precont precontf earlycont earlycontf ) cluster(statenum)
			sum qcew_cps_epop if e(sample)==1 ${weight`w'}
			local mean_depvar: di %04.3f r(mean)
			sum quarterdate if e(sample)==1
			estadd local begquarter = r(min)
			estadd local endquarter = r(max)

			local dof = e(df_r)

			lincom (($lincomagg) * (1/5))/${E}
			scalar b_`w'=r(estimate)
			scalar sest_`w'=r(se)
			scalar pst_`w' = ttail(r(df), r(estimate)/r(se))

			
*Figure			
			
local figcounter= 0
			foreach vv in F12treat F8treat F4treat treat  L4treat L8treat L12treat L16treat {
			local figcounter = `figcounter' +1
			local counter = `counter' + 1
			lincom (`vv' - F4treat) / $E
			if `counter' ==1{
			mat estimates = ( r(estimate), r(estimate) + 2*r(se), r(estimate) - 2*r(se), `w', `s', 4*`figcounter' - 16 )
			}
			else{
			mat estimates = ( estimates \ r(estimate), r(estimate) + 2*r(se), r(estimate) - 2*r(se), `w', `s', 4*`figcounter' - 16 )
			}
			}
			
			
			
			
			
			
			
			}
		}
	}	
*	
svmat estimates
tempfile temp
save `temp'

	
global figlab1 TWFE	
global figlab2 ST
global figlab3 DP
global figlab4 ST_DP

	foreach w in 2    {
		foreach s in  1 {


use `temp', clear
rename estimates1 estA
rename estimates2 highA
rename estimates3 lowA
rename estimates4 weight
rename estimates5 spec
rename estimates6 time



keep if weight == `w'
keep if spec == `s'

	

				twoway ( line estA time  , fcolor(blue) lcolor(blue)  lwidth(thick) yline(0, lcolor(gs10) lwidth(vvthin)) xline(-4, lpattern(dash) lcolor(black) ) ) ///
				(rcap lowA  highA time  , lwidth(thick) lcolor(blue*1.25) )  ///
				, scheme(s1color) xtitle("Years relative to the minimum wage change", size(medsmall)) ///
				ytitle("Employment rate difference between QCEW and CPS-ORG", height(5) axis(1) size(medsmall))  ///
				 ylabel(-0.04(0.01)0.04 0 "0", axis(1) labsize(medsmall) format(%03.2f)) yscale(titlegap(0) axis(1))  ///
				/*xlabel(-12 "[-12,-9]" -8 "[-8,-5]" -4 "[-4,-1]" 0 "[0,3]" 4 "[4,7]" 8 "[8,11]" 12 "[12,15]" 16 "[16,19]", labsize(medsmall)) */ ///
				xlabel(-12 "-3" -8 "-2" -4 "-1" 0 "0" 4 "1" 8 "2" 12 "3" 16 "4", labsize(medsmall)) legend(off) /* ///
				title("${label`s'}", size(medium) )  note("$bunchch" "$bunchelas" "$belowmasssh" "$abovemasssh"  /*"$elastest"*/, size(small) color(blue)) ///
				*/

			gr export "${figures}FigureF5.pdf", replace
	}	
}
*


	

