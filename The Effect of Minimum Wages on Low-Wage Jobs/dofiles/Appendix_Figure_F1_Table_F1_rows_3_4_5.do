clear

local timevar quarterdate

use  statenum quarterdate pop using "${data}state_panels_with3quant1979.dta", clear
cap drop tagger
egen tagger = tag(statenum quarterdate)
keep if tagger
keep statenum quarterdate pop 
tempfile pop
save `pop'

local k 15
use "${data}data_appendix_real.dta", clear
merge m:1 statenum quarterdate using `pop', nogenerate keep(3) assert(2 3)

	g under`k' = wagebins<`k'*100
	bys statenum `timevar' under`k':   g firstob_`k' = _n==1
 
foreach j in count rw_count admincount {
		egen `j'_under`k' = total(`j') if under`k'==1, by(under`k' statenum `timevar')
}
keep if wagebins>(`k'*100)-26 &  wagebins<(`k'*100)-24 


xtset statenum quarterdate
foreach j in count_under`k' rw_count_under`k' admincount_under`k' pop {
	forval num = 1(1)4{
		gen L`num'`j' = L`num'.`j' 
	}
}
*
foreach j in count_under`k' rw_count_under`k' admincount_under`k' pop  {
	forval num = 0(1)19{
		gen F`num'`j' = F`num'.`j' 
	}
}
*

foreach j in count_under`k' rw_count_under`k' admincount_under`k' pop{
gen post4qtr`j' = F0`j'/4 
gen post20qtr`j' = F0`j'/20 
gen pre4qtr`j' = L1`j' /4

foreach num of numlist 1(1)3{
replace post4qtr`j' =  post4qtr`j' + ( F`num'`j' / 4)
}
foreach num of numlist 1(1)19{
replace post20qtr`j' =  post20qtr`j' + ( F`num'`j' / 20)
}
foreach num of numlist 2(1)4{
replace pre4qtr`j' =  pre4qtr`j'  + ( L`num'`j' / 4)
}

}
*

foreach j in count rw_count admincount {
gen post4qtr`j'_pop = post4qtr`j'_under`k' / post4qtrpop 
gen post20qtr`j'_pop = post20qtr`j'_under`k' / post20qtrpop 
gen pre4qtr`j'_pop =  pre4qtr`j'_under`k' / pre4qtrpop 

}

foreach j in count rw_count admincount {
	g post20mpre4`j'_pop = post20qtr`j'_pop - pre4qtr`j'_pop
}
*




*20 post-4pre

reg post20mpre4admincount_pop post20mpre4count_pop 
local text1: di %04.3f _b[post20mpre4count_pop]
di "`text1'"
local text2: di %04.3f `e(r2_a)'
di "`text2'"

cap drop diff
gen diff = (post20mpre4admincount_pop-post20mpre4count_pop)^2
sum diff, meanonly
local text3: di %04.3fc r(mean)
di "`text3'"

local text4: di %04.3f e(rmse)^2
di "`text4'"

twoway (scatter post20mpre4admincount_pop post20mpre4count_pop ) (lfit post20mpre4admincount_pop post20mpre4count_pop , lwidth(thick) ) (function y=x, range(-0.1 0.1) lcolor(black) lwidth(medthick) lpattern(dash)) , graphregion(color(white)) ///
ytitle("Administrative data") xtitle("CPS-Raw") xlabel(-0.1(0.05)0.1 0 "0", format(%03.2fc))  ylabel(-0.1(0.05)0.1 0 "0", format(%03.2fc))  text(0.09 -0.085  "R{superscript:2} = `text2'" "Slope = `text1'", justification(right) box  fcolor(white) margin(0.2 0.2 1 1)) ///
/*note("The slope of the line is `text1'. Mean of the squared difference between CPS-Raw and Admin is `text3'." "Observations with hourly wages equal to or larger than $15 are excluded." ///
/*"Regression MSE is `text4'."*/)  title("20postqtr-4preqtr, 2016$")*/ legend(order(2 "Fitted line" 3 "45-degree line")) 
gr export "${figures}FigureF1_b.pdf", replace

reg  post20mpre4admincount_pop post20mpre4rw_count_pop
local text1: di %04.3f _b[post20mpre4rw_count_pop]
di "`text1'"
local text2: di %04.3f `e(r2_a)'
di "`text2'"


cap drop diff
gen diff = (post20mpre4admincount_pop-post20mpre4rw_count_pop)^2
sum diff, meanonly
local text3: di %04.3fc r(mean)
di "`text3'"

local text4: di %04.3f e(rmse)^2
di "`text4'"


twoway (scatter  post20mpre4admincount_pop post20mpre4rw_count_pop) (lfit  post20mpre4admincount_pop post20mpre4rw_count_pop, lwidth(thick) ) (function y=x, range(-0.1 0.1) lcolor(black) lwidth(medthick) lpattern(dash))  , graphregion(color(white))  ///
ytitle("Admin data") xtitle("QCEW-benchmarked CPS") xlabel(-0.1(0.05)0.1 0 "0", format(%03.2fc))  ylabel(-0.1(0.05)0.1 0 "0", format(%03.2fc)) text(0.09 -0.085  "R{superscript:2} = `text2'" "Slope = `text1'", justification(right)  box  fcolor(white) margin(0.2 0.2 1 1)) ///
/*note("The slope of the line is `text1'. Mean of the squared difference between QCEW-adj. CPS and Admin is `text3'." "Observations with hourly wages equal to or larger than $15 are excluded."  ///
/*"Regression MSE is `text4'."*/) title("20postqtr-4preqtr, 2016$")*/ legend(order(2 "Fitted line" 3 "45-degree line")) 
gr export "${figures}FigureF1_a.pdf", replace








*For table
cap drop one
gen one = 1
reg one
cap drop diff
gen diff = (post4qtradmincount_pop-post4qtrcount_pop)^2
sum diff, meanonly
local CPSraw_post4=  r(mean)

cap drop diff
gen diff = (post4qtradmincount_pop-post4qtrrw_count_pop)^2
sum diff, meanonly
local CPSadj_post4=  r(mean)

local temp: di %04.3f `CPSraw_post4'/`CPSadj_post4'
estadd local _4qtr "`temp'"
di "`temp'"

cap drop diff
gen diff = (post20qtradmincount_pop-post20qtrcount_pop)^2
sum diff, meanonly
local CPSraw_post20=  r(mean)

cap drop diff
gen diff = (post20qtradmincount_pop-post20qtrrw_count_pop)^2
sum diff, meanonly
local CPSadj_post20=  r(mean)

local temp: di %04.3f `CPSraw_post20'/`CPSadj_post20'
estadd local _20qtr "`temp'"

cap drop diff
gen diff = (post20mpre4admincount_pop-post20mpre4count_pop)^2
sum diff, meanonly
local CPSraw_post20_4=  r(mean)

cap drop diff
gen diff = (post20mpre4admincount_pop-post20mpre4rw_count_pop)^2
sum diff, meanonly
local CPSadj_post20_4=  r(mean)

local temp: di %04.3f `CPSraw_post20_4'/`CPSadj_post20_4'
estadd local _20_4qtr "`temp'"
eststo foo


local appendorreplace append
local stats stats(_4qtr blankspace _20qtr blankspace _20_4qtr , labels("4 quarters, Aggregated, $<$ \\\$ 15" " " "20 quarters, Aggregated, $<$ \\\$ 15" " "  "20 post-quarters \ - 4 pre- quarters, Aggregated, $<$ \\\$ 15"  ))


esttab foo ///
using "${tables}TableF1_cols3_4_5.tex", ///
`appendorreplace' `stats' `mgroups' ///
nodep nogaps cells(none) noobs nonumbers fragment booktabs nodepvar nomtitles  nolines

