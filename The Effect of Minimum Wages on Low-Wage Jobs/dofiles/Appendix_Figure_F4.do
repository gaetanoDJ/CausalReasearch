
*This do file is created by DC on 08/14/2018.
********************************************************************************
************       Comparison between ME CPS and CPS                  **********
********************************************************************************

use "${data}me_corrected_logwages.dta", clear

foreach vv in statenum quarterdate wagebins{
preserve
keep `vv'
duplicates drop
tempfile `vv'
save ``vv''
restore
}

use `statenum', clear
cross using `quarterdate'
cross using `wagebins'

tempfile for_balance
save `for_balance'


use "${data}me_corrected_logwages.dta", clear
merge 1:1 statenum quarterdate wagebins using `for_balance', nogenerate assert(2 3) 
replace density = 0 if density==.
bys statenum quarterdate: egen mec_countall = total(density)
drop if mec_countall==0
merge 1:1 statenum quarterdate wagebins using "${data}state_panels_with3quant1979.dta", assert(3) nogenerate keepusing(count countall *MW*)

drop if quarterdate >=136 & quarterdate <=142


gen rel_wagebins = wagebins - MW_realM25*100
assert rel_wagebins != .
replace rel_wagebins = -500 if rel_wagebins<-500
replace rel_wagebins = 1700 if rel_wagebins>1700
replace rel_wagebins = round(rel_wagebins/100)
*collapse (firstnm) mec_countall countall (sum) count density, by(statenum quarterdate rel_wagebins)
collapse (sum) count density, by(rel_wagebins)
egen countall = total(count)
egen mec_countall = total(density)

gen countsh = count/countall
gen mec_countsh = density/mec_countall

*Jobs below current MW decreases from 0.0267 to 0.0157.

twoway ///
(scatter countsh rel_wagebins ,  msize(large) msymbol(O) mcolor(blue) ) ///				
(scatter mec_countsh rel_wagebins ,  msize(large) msymbol(Sh) mcolor(red) ) ///				
, graphregion(color(white))  xtitle("Wage bins in $ relative to the MW") ylabel( ,format(%03.2f)) ///
 legend(order(1 "CPS" 2 "Measurement error corrected CPS"))  xlabel(-4(2)16 17 "17+")

gr export "${figures}FigureF4.pdf" ,replace

