use "${data}state_panels_cents_QJE.dta" , clear
keep if year!=.
sum quarterdate, meanonly
scalar numberqrt=r(max) - r(min) + 1
scalar minqrt=r(min)
*g all=1
egen tagger= tag(wagebins statenum)
keep if tagger
keep wagebins statenum
*collapse , by(wagebins statenum)
*drop all

tempfile wagebinstateshell

expand numberqrt
bys wagebins statenum: gen quarterdate = _n+ minqrt - 1

sort wagebins statenum quarterdate
save "`wagebinstateshell'"

use "${data}state_panels_cents_QJE.dta" , clear

cap drop _merge
merge 1:1 wagebins statenum quarterdate using "`wagebinstateshell'", keep(2 3) nogenerate

***  fill in state-by-period level variables ****

foreach x in year cpi blackcountall hispaniccountall ///
			  teencountall  hslcountall hsl40countall hsdcountall hsd40countall  ///
			 gendercountall totalpopulation whitecountall countall {
	cap drop _`x'
	egen _`x' = mean(`x'), by(statenum quarterdate)
	replace `x' = _`x' if `x'==.
	drop _`x'
}
*
foreach x in blackcount ///
	///
	dmarriedcount gendercount hispaniccount  ///
	hslcount hsl40count hsdcount hsd40count teencount  totpopcount whitecount FTE_orig {
		replace `x' = 0 if `x'==.
	}
*	
	
cap drop _merge
*To assert that the panel is balanced.
egen wagebinstate = group(wagebins statenum)
xtset wagebinstate quarterdate
assert "`r(balanced)'" == "strongly balanced"
cap drop wagebinstate
save "${data}state_panels_cents_balanced_QJE.dta" , replace


