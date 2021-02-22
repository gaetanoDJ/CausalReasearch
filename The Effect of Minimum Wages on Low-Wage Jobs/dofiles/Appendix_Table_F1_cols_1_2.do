set more off
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

keep if wagebins<(`k'*100) 
xtset wagebinstate quarterdate
foreach j in count rw_count admincount pop{
	forval num = 1(1)4{
		gen L`num'`j' = L`num'.`j' 
	}
}
*
foreach j in count rw_count admincount  pop{
	forval num = 0(1)19{
		gen F`num'`j' = F`num'.`j' 
	}
}
*

foreach j in count rw_count admincount pop{
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


foreach j in count rw_count admincount pop{
gen post4qtr`j'_pop = post4qtr`j' / post4qtrpop 
gen post20qtr`j'_pop = post20qtr`j' / post20qtrpop 
gen pre4qtr`j'_pop =  pre4qtr`j' / pre4qtrpop 

}


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
eststo foo

local appendorreplace replace
local mgroups mgroups("\specialcell{MSPE ratio: \\ Raw/Benchmarked}", lhs(\bigcell{c}{Data structure})) 
local stats stats(_4qtr blankspace _20qtr blankspace, labels("4 quarters, \\\$ 0.25, $<$ \\\$ 15" " " "20 quarters, \\\$ 0.25, $<$ \\\$ 15" " " ))


esttab foo ///
using "${tables}TableF1_cols1_2.tex", ///
`appendorreplace' `stats' `mgroups' ///
nodep nogaps cells(none) noobs nonumbers fragment booktabs nodepvar nomtitles


