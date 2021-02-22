
*This do file is created by DC on 10/05/2016.
********************************************************************************
***************     State panels with matched CPS                ***************
********************************************************************************

local bigcounter = 0
foreach empornonemp of numlist 1 0 {
*

local bigcounter = `bigcounter' + 1
use "${data}matchedCPS_1979_2016.dta", clear

keep if emp_1==`empornonemp'
drop *_1
gen logwage = log(wage)

drop if logwage == .

cap drop wagebins
gen wagebins=0
replace wagebins=100 if wage<125
replace wagebins=floor(wage/25)*25 if wage>=125 & wage<3000
replace wagebins=3000 if wage>=3000

assert wagebins!=0
recode sex (2=0)

assert emp==1


preserve
replace earnwt=earnwt/3          												//Since when earnwt summed over month gives totalpopulation, we need to divide it to 3.
collapse  (sum) m_count=earnwt, by(statenum quarterdate wagebins) fast
tempfile qtrwagebincounts
save `qtrwagebincounts', replace
restore




use "${data}state_panels_with3quant1979.dta", clear
cap drop _merge
merge 1:1 statenum quarterdate wagebins using `qtrwagebincounts'	, assert(1 3)

*Sample starts from 1979, so we do not observe first period for the observations in 1979. We drop this year.
*Additionally, as noted in NBER's documentation, matching is impossible for some quarters in some years. We drop them as well.
drop if year==1979
drop if (quarterdate>=102 & quarterdate <=106 ) | (quarterdate>=141 & quarterdate<= 146 )


ds m_*
foreach vv of varlist `r(varlist)'{
bys statenum quarterdate: egen `vv'all = total(`vv')
replace `vv'=0 if missing(`vv')
assert `vv'all!=0
assert `vv'all!=.
}

cap drop *_q*
cap drop HSD* 
cap drop HSL*
cap drop BH*
cap drop teen*
cap drop female*


g m_overallcountpc 		= m_count/pop
g m_overallcountpcall 	= m_countall/pop

*Create variables for the previous employment/wage bin status.
gen emp_prev= `empornonemp'


tempfile sample`bigcounter'
save `sample`bigcounter''
}
*
use `sample1'
foreach num of numlist 2{
append using `sample`bigcounter''
}
*
compress
bys statenum quarterdate wagebins: egen double mt_count 		= total(m_count)
bys statenum quarterdate wagebins: egen double mt_countall 		= total(m_countall)

g double mt_overallcountpc 		= mt_count/pop
g double mt_overallcountpcall 		= mt_countall/pop


egen wagebinstateprev = group(wagebins statenum emp_prev)
xtset wagebinstateprev quarterdate

assert mt_count>=m_count
assert mt_countall>=m_countall
assert mt_overallcountpc + 0.000001 >= m_overallcountpc
assert mt_overallcountpcall + 0.000001 >= m_overallcountpcall					// Adding a small number so that assertion is not false due to some extremely small recording error.

cap drop _merge
compress
save "${data}state_panels_with3quant1979_matched.dta", replace
