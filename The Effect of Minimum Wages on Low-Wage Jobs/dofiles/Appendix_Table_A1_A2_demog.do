
*This do file is created by DC on 03/25/2016
********************************************************************************
***********          Alternate Specifications of Jobs Dist.      ***************
********************************************************************************

* CPI
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

* QCEW
use "${data}qcew_multiplier.dta", clear
keep statenum quarterdate multiplier
tempfile qcew
save `qcew'

* wage-bin emp counts
use "${data}state_panels_cents_balanced_add_QJE.dta", clear
* normalize counts to overall QCEW
merge m:1 statenum quarterdate using `qcew', nogenerate assert(3)
rename count overallcount
rename countall overallcountall
rename population overallpop
gen femalecount = (overallcount-gendercount)
gen femalecountall = (overallcountall-gendercountall)
gen femalepop = (overallpop - genderpop)
gen BHcount = (blackcount + hispaniccount)
gen BHcountall = (blackcountall + hispaniccountall)
gen BHpop = (blackpop + hispanicpop)
foreach group in hsl hsl40 hsd hsd40 {
	local uppergroup = upper("`group'")
	foreach stat in pop count countall {
		rename `group'`stat' `uppergroup'`stat'
	}
}
foreach group in overall teen female BH HSL HSL40 HSD HSD40 {
	replace `group'count = `group'count * multiplier
	replace `group'countall = `group'countall * multiplier
}

* this is not what we should be doing to calculate average wages
* but I was unable to find code that uses the microdata that I could run
foreach group in overall teen female BH HSL HSL40 HSD HSD40 {
	gen logwagesum = log(wagebins)*`group'count
	egen totallogwagesum = total(logwagesum), by(statenum quarterdate)
	gen `group'logwage = totallogwagesum / `group'countall
	drop logwagesum totallogwagesum
}

* convert to $ wage bins and sum counts
gen wagebindollar = floor(wagebin/100)
drop wagebins
rename wagebindollar wagebins
preserve
	collapse (sum) overallcount teencount femalecount BHcount HSLcount HSL40count HSDcount HSD40count, by(statenum quarterdate wagebins)
	tempfile tempsum
	save `tempsum'
restore

* grab stateXquarter total counts
preserve
	egen tagger=tag(statenum quarterdate)
	keep if tagger==1
	keep statenum year quarterdate overallcountall overallpop teencountall teenpop femalecountall femalepop BHcountall BHpop HSLcountall HSLpop HSL40countall HSL40pop HSDcountall HSDpop HSD40countall HSD40pop overalllogwage teenlogwage femalelogwage BHlogwage HSLlogwage HSL40logwage HSDlogwage HSD40logwage
	tempfile temptag
	save `temptag'
restore

* reshape state-quarter-bin counts and merge total counts
use `tempsum', clear
reshape wide overallcount teencount femalecount BHcount HSLcount HSL40count HSDcount HSD40count, i(statenum quarterdate) j(wagebin)
merge 1:1 statenum quarterdate using `temptag', nogenerate assert(3)

* merge mw and events
merge 1:1 statenum quarterdate using "${data}VZmw_quarterly_lagsleads_1979_2016.dta", nogenerate assert(3)
merge 1:1 statenum quarterdate using "${data}eventclassification.dta", nogenerate assert(1 3)
replace overallcountgroup = 0 if overallcountgroup==.
assert overallcountgroup!=0 if alltreat==1 & toosmall==0
cap drop _merge
merge m:1 quarterdate using `cpiquarter', nogenerate assert(3)

* create min wage variables
xtset statenum quarterdate
g MW = exp(logmw)
g DMW = D.MW
g MW_real = exp(logmw)/(cpi/100)
g DMW_real = D.MW_real
g MW_realM25 = MW_real - mod(MW_real,0.25)
g DMW_realM25 = D.MW_realM25

* construct and group-specific epops and group-wagebin-specific epops
compress
foreach group in overall teen female BH HSL HSL40 HSD HSD40 {
	forval j = 1/30 {
		g ln`group'count`j' = ln(`group'count`j')
		g `group'percap`j' = `group'count`j'/`group'pop
	}
	gen `group'epop = `group'countall / `group'pop
	gen limited_`group'epop = `group'percap1
	forval j = 2/14 {
		replace limited_`group'epop = limited_`group'epop  + `group'percap`j'
	}
	gen upper_`group'epop  = `group'epop - limited_`group'epop

	egen ave`group'pop = mean(`group'pop), by(statenum)
}

* define treatment variables
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

replace postcont = 0 if postcont == .
replace postcontf = 0 if postcontf == .

global treat   		F12treat F8treat F4treat treat L4treat L8treat L12treat L16treat
global minus (F4treat)
global lincomagg 	(treat-$minus)
forval i = 4(4)16{
global lincomagg "$lincomagg + (L`i'treat - $minus ) "
}

* clean sample
g cleansample = 1
replace cleansample = 0 if quarterdate >=136 & quarterdate <=142

* incorporate base period routine occupation data
merge m:1 statenum using "${data}state_rocc_scores.dta", assert(3) nogenerate
* incorporate base period industry shares
merge m:1 statenum using "${data}state_majind_shares.dta", assert(3) nogenerate

* regression controls
global a1 i.statenum i.quarterdate  // TWFE
global a4 i.statenum##c.quarterdate i.quarterdate // state-trends
global a101 i.statenum i.quarterdate c.roccshare#i.quarterdate
global a111 i.statenum i.quarterdate c.roccindex#i.quarterdate
global a121 i.statenum i.quarterdate i.quarterdate#(c.occmajor_*)
global a221 i.statenum i.quarterdate i.quarterdate#(c.indmajor_*)
global a321 i.statenum i.quarterdate i.quarterdate#(c.indmajor_* c.occmajor_*)
global a421 i.statenum i.quarterdate i.quarterdate#(c.union19831984)



global label1 TWFE
global grlabel1 TWFE

********************************************************************************
***************       The Regressions ******************************************
********************************************************************************
gen all=1
xtset statenum quarterdate
tempfile temptemp
save `temptemp'

local regsample ""
local graphname "o"
global weight1 [aw=avetotalpopulation]
global weight0 [aw=all]
local ylabel "ylabel( , labsize(medium))"



foreach b in 1979   {
	foreach w in 1  {
		foreach s in 321 {
			foreach group in overall HSD HSL teen female BH {
				use `temptemp', clear

				if `w' == 1 local weight ave`group'pop
				else local weight all

				sum `group'epop if  year>=`b' & cleansample==1 [aw=`weight'], meanonly
				local epop = r(mean)
				sum DMW_real if fedincrease!=1 & overallcountgr>0 & year>=`b' & cleansample ==1 [aw=`weight']
				local mwc = r(mean)
				sum MW_real if  F.fedincrease!=1 & F.overallcountgr>0 & year>=`b' & cleansample ==1 [aw=`weight']
				local mw = r(mean)
				local mwpc = `mwc'/`mw'
				local C = 1/(`epop'*`mwpc')

				reghdfe `group'epop $treat if  year>=`b' & cleansample==1 [aw=`weight'], absorb(${a`s'} postcont postcontf precont precontf earlycont earlycontf) cluster(statenum)
				lincomestadd (($lincomagg) * (1/5))*`C', statname(allemp)
				local allempb = e(allempb)
				local allempse = e(allempse)
				local allempse_num = e(allempse_num)
				local allempt = e(allempt)
				reghdfe `group'logwage $treat if year>=`b' & cleansample==1 [aw=`weight'], a(${a`s'} i.postcont i.postcont i.postcontf i.precont i.precontf i.earlycont i.earlycontf) cluster(statenum)
				lincomestadd ((${lincomagg}) * (1/5)) * 1/(`mwpc'), statname(allwagemw)
				local allwageb = e(allwagemwb)
				local allwagese = e(allwagemwse)
				local allwagese_num = e(allwagemwse_num)
				local allwaget = e(allwagemwt)

				reghdfe limited_`group'epop $treat if  year>=`b' & cleansample==1 [aw=`weight'], absorb(${a`s'} postcont postcontf precont precontf earlycont earlycontf) cluster(statenum)
				lincomestadd (($lincomagg) * (1/5))*`C', statname(below15emp)
				local below15empb = e(below15empb)
				local below15empse = e(below15empse)
				local below15empse_num = e(below15empse_num)
				local below15empt = e(below15empt)

				reghdfe upper_`group'epop $treat if  year>=`b' & cleansample==1 [aw=`weight'], absorb(${a`s'} postcont postcontf precont precontf earlycont earlycontf) cluster(statenum)
				lincomestadd (($lincomagg) * (1/5))*`C', statname(over15emp)
				estadd local below15empb "`below15empb'"
				estadd local below15empse "`below15empse'"
				estadd scalar below15empse_num = `below15empse_num'
				estadd local below15empt "`below15empt'"
				estadd local allempb "`allempb'"
				estadd local allempse "`allempse'"
				estadd scalar allempse_num = `allempse_num'
				estadd local allempt "`allempt'"
				estadd local allwageb "`allwageb'"
				estadd local allwagese "`allwagese'"
				estadd scalar allwagese_num = `allwagese_num'
				estadd local allwaget "`allwaget'"

				estimates save "${estimates}eventbased_aggregate_`b'_`w'_`s'_`group'epop_estadd.ster", replace
			}
		}
	}
}
