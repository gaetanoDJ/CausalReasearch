
*This do file is created by DC on 09/16/2018.
********************************************************************************
**  Finding events that occurred during the post-treatment window of another  **
********************************************************************************

set more off
use "${data}eventclassification.dta", clear
gen temp = (belowsharegr!=.)
gen eventno = sum(temp) if temp==1
cap drop temp
xtset statenum quarterdate
tsfill, full

forval eventno = 1(1)138{
sum quarterdate if eventno == `eventno', meanonly
local eventdate = r(mean)
sum statenum if eventno == `eventno', meanonly
local eventstate = r(mean)

assert r(N)==1
qui gen event`eventno'window = (quarterdate>=`eventdate' & quarterdate<=`eventdate'+19 & statenum == `eventstate')
qui count if event`eventno'window == 1
assert r(N)<=20

qui gen event`eventno'window_pre = (quarterdate<`eventdate' & quarterdate>=`eventdate'-20 & statenum == `eventstate')
qui count if event`eventno'window_pre == 1
assert r(N)<=20

}
*
*There is an event in nth event's pre-window.
forval eventno = 1(1)138{
di "`eventno'"
count if  event`eventno'window_pre==1 & eventno!=. & eventno!=`eventno'
if `eventno'==1{
mat asd = (r(N), `eventno')
}
else{
mat asd = ( asd \ r(N), `eventno')
}

}
*
preserve
clear
svmat asd
rename asd2 eventno
rename asd1 unclean
tempfile temp
save `temp'
restore

keep if eventno!=.
merge 1:1 eventno using `temp', nogenerate assert(1 3)
keep if unclean==0
keep statenum quarterdate
rename statenum treatedstatenum
rename quarterdate treatdate
compress

save "${data}to_combine_consecutive_events.dta", replace




