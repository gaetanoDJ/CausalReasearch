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
use "${data}statequarter_counts_maxupperw.dta", clear
rename totalpopulation overallpop
* normalize counts to overall QCEW
merge m:1 statenum quarterdate using `qcew', nogenerate assert(3)
replace emp = emp * multiplier
replace wage = wage * multiplier
gen epop = emp / overallpop
gen wagepc = wage / overallpop

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

* clean up
keep statenum quarterdate F*treat L*treat treat postcont postcontf precont precontf earlycont earlycontf *MW* fedincrease overallcountgr epop wagepc overallpop

* prep data to be stacked
* create wage outcome data
preserve
drop epop
rename wagepc outcome
gen byte epopoutcome = 0
gen byte wageoutcome = 1
tempfile wages
save `wages'
restore
* create employment outcome data
drop wagepc
rename epop outcome
gen byte epopoutcome = 1
gen byte wageoutcome = 0
* stack data
append using `wages'
egen stateXoutcome = group(statenum epopoutcome)

* create stacked treatment variables
gen treatepop = treat * (epopoutcome == 1)
gen treatwage = treat * (wageoutcome == 1)
foreach j of numlist 4(4)16 {
	foreach lf in F L {
		gen `lf'`j'treatwage = `lf'`j'treat * (wageoutcome == 1)
		gen `lf'`j'treatepop = `lf'`j'treat * (epopoutcome == 1)
	}
}

global treatwage F12treatwage F8treatwage F4treatwage treatwage L4treatwage L8treatwage L12treatwage L16treatwage
global treatepop F12treatepop F8treatepop F4treatepop treatepop L4treatepop L8treatepop L12treatepop L16treatepop

foreach outcome in epop wage {
	global minus`outcome' (F4treat`outcome')
	global lincomagg`outcome' 	(treat`outcome'-${minus`outcome'})
	forval i = 4(4)16 {
		global lincomagg`outcome' "${lincomagg`outcome'} + (L`i'treat`outcome' - ${minus`outcome'}) "
	}
}

foreach outcome in epop wage {
	global bminus`outcome' (_b[F4treat`outcome'])
	global blincomagg`outcome' 	(_b[treat`outcome']-${bminus`outcome'})
	forval i = 4(4)16 {
		global blincomagg`outcome' "${blincomagg`outcome'} + (_b[L`i'treat`outcome'] - ${bminus`outcome'}) "
	}
}

* clean sample
g cleansample = 1
replace cleansample = 0 if quarterdate >= 136 & quarterdate <= 142

* regression controls
global a1 i.statenum#i.epopoutcome i.quarterdate#i.epopoutcome  // TWFE

global label1 TWFE
global grlabel1 TWFE

********************************************************************************
***************       The Regressions ******************************************
********************************************************************************
gen all=1
xtset stateXoutcome quarterdate
gen year = year(dofq(quarterdate))

gen aggemp_treat =  treatepop + L4treatepop + L8treatepop + L12treatepop + L16treatepop
gen aggwage_treat =  treatwage + L4treatwage + L8treatwage + L12treatwage + L16treatwage

gen aggemp_window = F4treatepop +  treatepop + L4treatepop + L8treatepop + L12treatepop + L16treatepop
gen aggwage_window =F4treatwage +  treatwage + L4treatwage + L8treatwage + L12treatwage + L16treatwage


tempfile temptemp
save `temptemp'
compress
save "${data}regready_simpler_model.dta", replace

*
foreach b in 1979 {
	foreach w in 1 /*0*/ {
		foreach s in 1 {
			foreach group in belowupperw {
				use `temptemp', clear

				if `w' == 1 local weight overallpop
				else local weight all

				local Bepop = 1/($E*$B)
				local Bwage = 1/($EWB)
				local C = 1/($E * $mwpc )
				reghdfe outcome $treatwage $treatepop if year>=`b' & cleansample==1 [aw=`weight'], absorb(${a`s'} i.postcont#i.epopoutcome i.postcontf#i.epopoutcome i.precont#i.epopoutcome i.precontf#i.epopoutcome i.earlycont#i.epopoutcome i.earlycontf#i.epopoutcome) cluster(statenum)
				
				lincomestadd ((${lincomaggepop}) * (1/5))*`Bepop', statname(bunching_E)
				lincomestadd ((${lincomaggepop}) * (1/5))*`C', statname(bunchelas_E)
				
				local dof = e(df_r)
				nlcomestadd [[((${blincomaggwage}) * (1/5))*`Bwage']-[((${blincomaggepop}) * (1/5))*`Bepop']]/[1+((${blincomaggepop}) * (1/5))*`Bepop'], statname(WB_E) dof(`dof')

				nlcomestadd [((${blincomaggepop}) * (1/5))*`Bepop']/[[[((${blincomaggwage}) * (1/5))*`Bwage']-[((${blincomaggepop}) * (1/5))*`Bepop']]/[1+((${blincomaggepop}) * (1/5))*`Bepop']],	statname(labordem_E) dof(`dof')

				estimates save "${estimates}simplermethod_`group'.ster", replace
			}
		}
	}
}

