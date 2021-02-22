


use "${data}Admin_CPS_workingdata_in2016dollars_allind_new.dta", clear


 global treatdata  "CPS"


local hiw = 17
local loww = 5

tempfile origdata
save `origdata'

foreach vv in statenum quarterdate wagebins{
preserve
keep `vv'
duplicates drop
tempfile `vv'
save ``vv''
restore
}
*

global firstyear 1992

global pre "( year>=1996 & year<=1998)"

local firstyear = ${firstyear}


use `statenum', clear
cross using `quarterdate'
cross using `wagebins'
tempfile balance
save `balance'
use `origdata',clear
cap drop _merge
merge 1:1 statenum quarterdate wagebins using `balance', assert(2 3) nogenerate
replace year = year(dofq(quarterdate)) if year==.
replace count = 0 if count==.
replace acount = 0 if acount==.
replace MNcount = 0 if MNcount==.
bys statenum quarterdate: egen _pop = max(pop)
replace pop = _pop if pop==.

g treatstate = statenum==53
g _control = 1  if year>=1996 & year<=2004
replace _control = 0 if DMW>0 & DMW<. & (year>=1998 & year<=2004)
egen control = min(_control), by(statenum)
keep if year>=$firstyear & year<=2004
keep if treatstate==1 | control==1
keep if wagebins!=.

g wagebin100 = wagebins - mod(wagebins, 100)
replace wagebin100 = int(wagebin100/100)
replace count = 0 if count==.
assert acount!=. if statenum==53
assert MNcount!=. if statenum==27
cap drop countall
cap drop acountall
cap drop MNcountall

bys statenum quarterdate: egen countall = total(count) 
bys statenum quarterdate: egen acountall = total(acount)  
bys statenum quarterdate: egen MNcountall = total(MNcount) 
 
*Get annual average
collapse (mean)  count acount pop MNcount countall acountall MNcountall MW_real, by(year wagebins statenum control treatstate  )
g wagebin100 = wagebins - mod(wagebins, 100)
replace wagebin100 = int(wagebin100/100)
replace wagebin100 = 26 if wagebin100>26
*Collapse by wagebin100
collapse (sum) MNcount count acount (mean) pop  countall acountall MNcountall MW_real, by(year wagebin100 control treatstate statenum  )
*Collapse by treat/control state


gen countpc = count/pop
gen countallpc = countall/pop
gen acountpc = acount/pop
gen acountallpc = acountall/pop

gen adminCPScount = count if treat==0
gen adminCPScountpc = countpc if treat==0
gen adminCPScountall = countall if treat==0
gen adminCPScountallpc = countallpc if treat==0


if "${treatdata}" == "Admin" {
di "Admin data from treated state used"
replace adminCPScount = acount if treat==1
replace adminCPScountpc = acountpc if treat==1
replace adminCPScountall = acountall if treat==1
replace adminCPScountallpc = acountallpc if treat==1
}
else if "${treatdata}" == "CPS" {
di "CPS data from treated state used"
replace adminCPScount = count if treat==1
replace adminCPScountpc = countpc if treat==1
replace adminCPScountall = countall if treat==1
replace adminCPScountallpc = countallpc if treat==1
}
else {
di "Treated state data source unspecified"
STOP
}


********************************************************************************
****************       Normalizing constants                      **************
********************************************************************************

sum adminCPScountallpc [aw=population] if treatstate==1 & $pre
global E =  r(mean)

g wagebins=wagebin100

cap drop stateyear
egen stateyear = group(statenum year)
sort stateyear wagebins

cap drop overallWB*
g overallWBFH = wagebins*adminCPScount/(1)
g overallWBpcFH = wagebins*adminCPScount/(1*pop)

cap drop overallWBpcrsumFH
g overallWBpcrsumFH=0
replace overallWBpcrsumFH = overallWBpcFH if wagebins==0
replace overallWBpcrsumFH = overallWBpcrsumFH[_n-1] + overallWBpcFH if wagebins>0
	
cap drop overallcountpcrsum
g overallcountpcrsum=0
replace overallcountpcrsum = adminCPScountpc if wagebins==0
replace overallcountpcrsum = overallcountpcrsum[_n-1] + adminCPScountpc if wagebins>0


sum overallcountpcrsum if $pre & (wagebins==8) & treatstate==1 [aw=population]
global B = r(mean)/${E}

sum overallWBpcrsumFH if (  $pre & treatstate==1 & wagebins==8 )  [aw=population]
global EWB = r(mean)


forval num1 = 0(1)`hiw'{
	foreach num2 of numlist     2000(1)2004 {
		gen treat_p`num1'_`num2' = treatstate==1 & year==`num2' & wagebin100==9 + `num1'
	}
}
*
forval num1 = 1(1)`loww'{
	foreach num2 of numlist     2000(1)2004{
		gen treat_m`num1'_`num2' = treatstate==1 & year==`num2' & wagebin100==9 - `num1'
	}
}
*

*

egen wagebinstate = group(wagebin100 statenum)
egen wagebinyear = group(wagebin100 year)

global a1 wagebinstate wagebinyear

global lab1 TWFE
xtset wagebinstate year

 

********************************************************************************
**************                Figure 5                       *******************
********************************************************************************

 
set more off
local firstyear = ${firstyear}
local hiw = 4
local loww = 4


forval num1 = 0(1)`hiw'{
	foreach num2 of numlist      $firstyear (1)2004{
		cap drop treat_p`num1'_`num2' 
		gen treat_p`num1'_`num2' = treatstate==1 & year==`num2' & wagebin100==9 + `num1'
	}
}
*
forval num1 = 1(1)`loww'{
	foreach num2 of numlist      $firstyear (1)2004{
		cap drop treat_m`num1'_`num2' 
		gen treat_m`num1'_`num2' = treatstate==1 & year==`num2' & wagebin100==9 - `num1'
	}
}
*





global treatvec " "

forval num1 = 0(1)`hiw'{
	foreach num2 of numlist      $firstyear (1)2004{
		global treatvec " ${treatvec}  treat_p`num1'_`num2' "
	}
}
*
forval num1 = 1(1)`loww'{
	foreach num2 of numlist      $firstyear (1)2004{
		global treatvec " ${treatvec}  treat_m`num1'_`num2' "
	}
}
*



local num = 1
reghdfe adminCPScountpc ${treatvec}  if  year!=1999 [aw=population] , a(${a`num'} )
est sto spec`num'

*
local spec  = 1


preserve

est restore spec`spec'
forval num = 1(1)`loww'{
global precoeffsum_m = "(treat_m`num'_1996 + treat_m`num'_1997 + treat_m`num'_1998)/3"
global CSBW_m`num' "(treat_m`num'_2000+ treat_m`num'_2001+ treat_m`num'_2002+ treat_m`num'_2003+ treat_m`num'_2004)/5 - ${precoeffsum_m}"
}
forval num = 0(1)`hiw'{
global precoeffsum_p = "(treat_p`num'_1996 + treat_p`num'_1997 + treat_p`num'_1998)/3"
global CSBW_p`num' "(treat_p`num'_2000+ treat_p`num'_2001+ treat_p`num'_2002+ treat_p`num'_2003+ treat_p`num'_2004)/5 - ${precoeffsum_p} "
}
*
local counter = 0
forval num2 = 1(1)`loww'{
qui lincom ${CSBW_m`num2'}
local counter= `counter' + 1
if `counter' == 1{
mat estimates = ( r(estimate), -`num2' )
}
else{
mat estimates = ( estimates  \ r(estimate), -`num2' )
}
}
*
forval num2 = 0(1)`hiw'{
qui lincom ${CSBW_p`num2'}
mat estimates = ( estimates  \ r(estimate), `num2' )
}
*
cap drop estimates*
cap drop est_beta
cap drop bin
cap drop alt_bin
cap drop cum_est
svmat estimates
rename estimates1 est_beta
rename estimates2 bin
sum adminCPScountallpc [aw=population] if treatstate==1 & $pre
replace est_beta = est_beta/r(mean)

keep bin est_beta
rename bin wagebin
keep if  wagebin!=.


restore


preserve
est restore spec`spec'

foreach num of numlist  1996(1)1998  {
global ATPm_`num' "0 "
forval num1 = 0(1)`hiw'{
global ATPm_`num' "(${ATPm_`num'} + (treat_p`num1'_`num') )"
}
}
*
foreach num of numlist  1996(1)1998  {
global BTPm_`num' "0 "
forval num1 = 1(1)`loww'{
global BTPm_`num' "(${BTPm_`num'} + (treat_m`num1'_`num') )"
}
}
*
foreach num of numlist $firstyear (1)1998  2000(1)2004{
global ATP_`num' "0 "
forval num1 = 0(1)`hiw'{
global ATP_`num' "(${ATP_`num'} + (treat_p`num1'_`num') )"
}
global ATP_`num' " (${ATP_`num'}) - (${ATPm_1996} + ${ATPm_1997} + ${ATPm_1998})/3 "
}
*
foreach num of numlist $firstyear (1)1998  2000(1)2004{
global BTP_`num' "0 "
forval num1 = 1(1)`loww'{
global BTP_`num' "(${BTP_`num'} + (treat_m`num1'_`num') )"
}
global BTP_`num' " (${BTP_`num'}) -  (${BTPm_1996} + ${BTPm_1997} + ${BTPm_1998})/3"
}
*

foreach num2 of numlist $firstyear (1)1998 2000(1)2004{
lincom ${BTP_`num2'}
if `num2' == $firstyear {
mat estimates_low = ( r(estimate), `num2' )
}
else{
mat estimates_low = ( estimates_low  \ r(estimate), `num2' )
}
}
*
foreach num2 of numlist   $firstyear (1)1998 2000(1)2004{
qui lincom ${ATP_`num2'}
if `num2' == $firstyear {
mat estimates_up = ( r(estimate), `num2' )
}
else{
mat estimates_up = ( estimates_up  \ r(estimate), `num2' )
}
}
*
cap drop estimates*
cap drop est_beta_*
cap drop bin
svmat estimates_low
rename estimates_low1 est_beta_low
rename estimates_low2 bin
svmat estimates_up
rename estimates_up1 est_beta_up

sum adminCPScountallpc if treatstate==1 & $pre
replace est_beta_low = est_beta_low/r(mean)
replace est_beta_up = est_beta_up/r(mean)
sort bin


sum est_beta_low if bin>1998 & bin!=.
scalar est_beta_below = r(mean)

sum est_beta_up if bin>1998 & bin!=.
scalar est_beta_above = r(mean)

local excesscolor ltblue*1.25
local missingcolor red

sum est_beta_up
gen high = r(max) if bin >= 1998 & bin <= 2000
sum est_beta_low
gen low = -high if bin >= 1998 & bin <= 2000

* minimum wage values in WA
egen MWmean = mean(MW_real), by(year statenum)
egen stateyeartag = tag(statenum year)



gen emp_est = est_beta_low + est_beta_up
sum emp_est if bin>1998 & bin!=.
scalar emp_est = r(mean)



gen emp_est_aff = emp_est/${B}
sum emp_est_aff if bin>1998 & bin!=.
local emp_est_aff = r(mean)
scalar emp_est_aff = r(mean)



foreach num of numlist  1996(1)1998  {
global ATPWBm_`num' "0 "
forval num1 = 0(1)`hiw'{
global ATPWBm_`num' "(${ATPWBm_`num'} + ((9+ `num1' )*treat_p`num1'_`num') )"
}
}
*
foreach num of numlist  1996(1)1998  {
global BTPWBm_`num' "0 "
forval num1 = 1(1)`loww'{
global BTPWBm_`num' "(${BTPWBm_`num'} + ((9-`num1') * treat_m`num1'_`num') )"
}
}
*
foreach num of numlist $firstyear (1)1998  2000(1)2004{
global ATPWB_`num' "0 "
forval num1 = 0(1)`hiw'{
global ATPWB_`num' "(${ATPWB_`num'} + (treat_p`num1'_`num')*(9+`num1') )"
}
global ATPWB_`num' " (${ATPWB_`num'}) - (${ATPWBm_1996} + ${ATPWBm_1997} + ${ATPWBm_1998})/3 "
}
*
foreach num of numlist $firstyear (1)1998  2000(1)2004{
global BTPWB_`num' "0 "
forval num1 = 1(1)`loww'{
global BTPWB_`num' "(${BTPWB_`num'} + (treat_m`num1'_`num')*(9-`num1') )"
}
global BTPWB_`num' " (${BTPWB_`num'}) -  (${BTPWBm_1996} + ${BTPWBm_1997} + ${BTPWBm_1998})/3"
}
*

foreach num of numlist $firstyear (1)1997 1998 2000(1)2004{
lincom ((${BTPWB_`num'} +  ${ATPWB_`num'})/$EWB ) - (( ${BTP_`num'} +  ${ATP_`num'} )*(1/${E})*(1/${B}))
if `num'==`firstyear'{
mat estimates_wage = (r(estimate) , `num')

}
else{
mat estimates_wage = (estimates_wage \ r(estimate) , `num')
}
}
*
svmat estimates_wage
rename estimates_wage1 wage_est
rename estimates_wage2 year_bin
sort year_bin

sum wage_est if year_bin>1998 & year_bin!=.
scalar wage_est = r(mean)/(1+`emp_est_aff')

restore






********************************************************************************
******************          Counterfactual Jobs Dist.           ****************
********************************************************************************


drop if year<1996

local hiw 17
local loww 4

global treatvec " "

forval num1 = 0(1)`hiw'{
	foreach num2 of numlist      2000(1)2004{
		global treatvec " ${treatvec}  treat_p`num1'_`num2' "
	}
}
*
forval num1 = 1(1)`loww'{
	foreach num2 of numlist     2000(1)2004{
		global treatvec " ${treatvec}  treat_m`num1'_`num2' "
	}
}
*
preserve
keep if wagebin100>=9-4 & wagebin100<9+5 
gen below = wagebin100<9
collapse adminCPScountpc [aw=pop] , by(treatstate year below)
replace adminCPScountpc = adminCPScountpc*4/$E if below==1
replace adminCPScountpc = adminCPScountpc*5/$E if below==0
sum adminCPScountpc if below==0 & treatstate==1 & year<=1998
sum adminCPScountpc if below==0 & treatstate==0 & year<=1998
sum adminCPScountpc if below==0 & treatstate==1 & year>1999
sum adminCPScountpc if below==0 & treatstate==0 & year>1999

sum adminCPScountpc if below==1 & treatstate==1 & year<=1998
sum adminCPScountpc if below==1 & treatstate==0 & year<=1998
sum adminCPScountpc if below==1 & treatstate==1 & year>1999
sum adminCPScountpc if below==1 & treatstate==0 & year>1999
restore



local num = 1
reghdfe adminCPScountpc ${treatvec}  if  year!=1999 [aw=population], a(${a`num'} )
est store spec`num'

*forval spec = 1(1)2{
local spec = 1
preserve
est restore spec`spec'
forval num = 1(1)`loww'{
global precoeffsum_m = "(treat_m`num'_1996 + treat_m`num'_1997 + treat_m`num'_1998)/3"
global CSBW_m`num' "(treat_m`num'_2000+ treat_m`num'_2001+ treat_m`num'_2002+ treat_m`num'_2003+ treat_m`num'_2004)/5"
}
forval num = 0(1)`hiw'{
global precoeffsum_p = "(treat_p`num'_1996 + treat_p`num'_1997 + treat_p`num'_1998)/3"
global CSBW_p`num' "(treat_p`num'_2000+ treat_p`num'_2001+ treat_p`num'_2002+ treat_p`num'_2003+ treat_p`num'_2004)/5"
}
*
local counter = 0
forval num2 = 1(1)`loww'{
qui lincom ${CSBW_m`num2'}
local counter= `counter' + 1
if `counter' == 1{
mat estimates = ( r(estimate), -`num2' )
}
else{
mat estimates = ( estimates  \ r(estimate), -`num2' )
}
}
*
forval num2 = 0(1)`hiw'{
qui lincom ${CSBW_p`num2'}
mat estimates = ( estimates  \ r(estimate), `num2' )
}
*
cap drop estimates*
cap drop est_beta
cap drop bin
cap drop alt_bin
cap drop cum_est
svmat estimates
rename estimates1 est_beta
rename estimates2 bin
sum adminCPScountallpc [aw=population] if treatstate==1 & $pre
replace est_beta = est_beta/r(mean)
sort bin
gen cum_est = sum(est_beta) if bin!=.

foreach vv in est_beta_above est_beta_below emp_est emp_est_aff wage_est{
local _`vv': di %04.3f `=`vv''
di "`_`vv''"
}
*

global xlabel2 " "
foreach num of numlist -`loww'(2)16{
local num2 = `num' + 9
global xlabel2 `" ${xlabel2} `num' "`num2'" "'
}

global xlabel2 `" ${xlabel2} 17 "26+" "'


*

gen alt_bin = bin+9

gen relMW = bin
gen fits =  est_beta * ${E}


twoway (bar est_beta bin if bin!=., color(blue) fcolor(ltblue) xaxis(1 2)   xlabel(-4(2)16 17 "17+") xlabel(  ${xlabel2} , axis(2))  ) ///
(line cum_est bin  if bin!=. , lwidth(thick) lcolor(red*.33)  ) , ///
graphregion(color(white)) legend(off) xtitle("Wage bins relative to $9 in 2016$", size(medsmall)) xtitle("Wage bins in 2016$", size(medsmall) axis(2)) ///
ylabel( ,format(%03.2f)) ytitle("Difference between actual and counterfactual employment count" "relative to the pre-treatment total employment", height(10) size(small))   ///
text(0.03 11.5  "{&Delta}a =  `_est_beta_above'" "{&Delta}b = `_est_beta_below'" "%{&Delta} affected employment =  `_emp_est_aff'" ///
"%{&Delta} affected wage =  `_wage_est'", box  fcolor(white) margin(0.2 0.2 1 1) justification(right) )


gr export "${figures}FigureC4_b.pdf", replace

 
sum est_beta if bin==17


g _post = 1 if year>=2000 & year<=2004
egen adminCPScountpc_post = mean(_post*adminCPScountpc), by(treatstate wagebin100)


cap drop adminCPScountpc_cf
g adminCPScountpc_cf = .

forval k = -4/17 {
sum est_beta if bin == `k'
replace adminCPScountpc_cf = adminCPScountpc_post - r(mean) if treatstate==1 & wagebin100==9+`k'
}
replace wagebin100 = 27 if wagebin100==26
 replace wagebin100 = wagebin100 - 9
 gen alt_wage = wagebin100 + 9


 sum adminCPScountpc_post if wagebin100==18 & year==2004 & treatstate==1
 sum adminCPScountpc_cf if wagebin100==18 & year==2004 & treatstate==1
 
twoway (bar adminCPScountpc_post  wagebin100 if wagebin100<=17 & wagebin100>=-4 & year==2004 & treatstate==1 , barwidth(1) color(blue*.3) yaxis(1) ) ///
(bar adminCPScountpc_post  wagebin100 if wagebin100==18 & year==2004 & treatstate==1 , color(blue*.3) yaxis(2)  barwidth(1) ) ///
(bar adminCPScountpc_cf  alt_wage if alt_wage<=26 &  alt_wage>=5 & year==2004 &  treatstate==1, xaxis(2)  lcolor(red*.8)  barwidth(1) yaxis(1) fcolor(none) lw(thick) ) ///
  (bar adminCPScountpc_cf  alt_wage if alt_wage==27 & year==2004 &  treatstate==1, xaxis(2) yaxis(2) lcolor(red*.8) fcolor(none)  barwidth(1) lw(thick) )  ///
,  xlabel(5(2)25 27 "26+", axis(2))  xlabel(-4(2)16 18 "17+", axis(1)) ylabel(0.19(0.01)0.25, axis(2)) ylabel(0(0.01)0.06, axis(1))  ///
 legend(order(1 "Actual distribution" 3 "Counterfactual distribution") cols(1)) graphregion(color(white)) ///
 xtitle("Wage bins relative to $9 in 2016$")  xtitle("Wage bins in 2016$", size(medsmall) axis(2))  ///
ytitle("Actual and counterfactual employment count" "relative to the pre-treatment total employment", height(10) size(small))   ///
ytitle("Actual and counterfactual employment count" "relative to the pre-treatment total employment ($26+ bin)", axis(2) height(10) size(small))

gr export "${figures}FigureC4_a.pdf", replace 

