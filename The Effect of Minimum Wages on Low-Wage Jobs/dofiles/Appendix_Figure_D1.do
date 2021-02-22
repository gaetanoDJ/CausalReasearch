
clear *
*Run this first to create the necessary omega^2 values.
*do "${dofiles}calculating_omega2_sum_of_squared_weights.do"

set seed 9856840
clear *
use "${data}omega2_sum_of_squared_weights.dta"
keep statenum quarterdate omega2
duplicates drop 
tempfile omega2
save `omega2'

foreach num of numlist 1(1)138{
di "Event no `num'."
qui {
use if event==`num' using "${data}stackedevents_regready_pre12.dta",clear

sum quarterdate if F4treat==1
local F4qmin=r(min)
local F4qmax=r(max)
sum treatdate , meanonly
local treatquarterdate  = r(mean)
sum treatedstate, meanonly
local treatstatenum  = r(mean)

gen T=quarterdate>=treatdate
gen D=statenum==treatedstate
gen P_jt = overallpop

tsset eventstate time
rename overallexcessepop above_epop
rename overallmissingepop below_epop
rename overallupperepop upper_epop
rename overalltotalepop all_epop
gen bunching_epop = above_epop + below_epop
rename aveoverallpop avetotalpopulation
		sum all_epop if cleansample==1  [aw=avetotalpopulation]
		local epop = r(mean)
		sum avetotalpopulation if treatedstate==statenum, meanonly
		local population = r(mean)

bys statenum event: egen states_to_drop = max(control_origeventpost)
drop if states_to_drop!=0 & treatedstatenum!=statenum
assert control_origeventpost==0 if treatedstatenum!=statenum
sum control_origeventpost,meanonly
		
		

gen DD = L0treat + L4treat + L8treat + L12treat + L16treat
gen window = F4treat + L0treat + L4treat + L8treat + L12treat + L16treat

tempfile event`num'data
save 	`event`num'data'
* Run DID regression and store eta_jt residuals and subtract F4eta_jt from eta_jt
local cat_num=0 
foreach cat in above below upper all bunching{
use `event`num'data',clear
local cat_num = `cat_num'+1
reghdfe `cat'_epop DD [aw=avetotalpopulation] if  cleansample==1 , a(i.event#i.window i.event#i.statenum i.event#i.time i.event#i.control_origeventpost i.event#control_fedeventpost i.event#control_othereventpost) 
local alpha_hat = _b[DD]

reghdfe `cat'_epop [aw=avetotalpopulation] if  cleansample==1 , a(i.event#i.statenum i.event#i.time i.event#i.control_origeventpost i.event#control_fedeventpost i.event#control_othereventpost) residuals(eta_jt)  
gen eta_jt_ref = eta_jt if quarterdate>=`F4qmin' & quarterdate<=`F4qmax' 
levelsof statenum, local(control_states)
foreach ll of local control_states{
sum eta_jt_ref [aw=avetotalpopulation], meanonly, if statenum == `ll'
qui replace eta_jt = eta_jt - r(mean)  if statenum == `ll'
}
sum  eta_jt [aw= avetotalpopulation] if F4treat==1
assert r(mean)<0.000001
* Define parameters to estimate linear combination Wj

egen P_post=sum(overallpop  ) if DD==1 	
egen P_pre=sum(overallpop  ) if DD==0 & treatedstate==statenum

gen temp=(overallpop/P_post) if DD==1 	
replace temp=(overallpop/P_pre) if DD==0 & treatedstate==statenum

egen a_1t=mean(temp), by(quarterdate) // this command sets the coefficients of the linear combination Wj derived for j=1 for all other j  
drop temp P_post P_pre

* This variable will generate the linear combination Wj when we summ it for each j
gen W=a_1t*eta_jt  if T==1
replace W=-a_1t*eta_jt  if T==0

* Generates the variable to estimate the var(W|M) function.
merge 1:1 statenum quarterdate using `omega2', nogenerate keep(3) assert(2 3)
gen q=(a_1t^2)*omega2/(P_jt)^2



collapse (mean) D (sum) W q P_jt, by(statenum)
	
	
gen W2 = (W)^2
	
reg W2 q [pw=P_jt] 
predict var_M
local A=_b[_cons]
local B=_b[q]


* Correction for finite N_0 (when the var(W|M) estimate gets negative values)	
summ var_M 
local min=r(min)

replace var_M=1 if `min'<0 & `B'<0
replace var_M=q if `min'<0 & `A'<0
	

* Generate normalized W_j (var=1)
gen W_normalized = W/sqrt(var_M)


* Bootstrap: runs the boostrap with 1000 repetitions. To have more repetitions, change the number in the definition of matrix B and in the forvalues loop.
mat B=J(1000,1,.)

	summ state
	local N=r(N)
		
	forvalues Round=1(1)1000 {    
	
	
		gen h1=uniform()  
		gen h2=1 + int((`N'-1+1)*h1)
			
		gen W_tilde_corrected = W_normalized[h2] * sqrt(var_M) 	
					
		
		
		summ W_tilde_corrected if D==1
		local treated=r(mean)
	
		summ W_tilde_corrected if D==0 [aw=P_jt]
		local control=r(mean)
		
		mat B[`Round',1]=`treated' - `control'


		drop h1 h2 *tilde* 
	
	}


	set more off
	clear 
	svmat B
	
	** p-value without correction
		
	** Correction, unknown variance
		
	gen p_FP=B1^2>(`alpha_hat')^2  if B1!=.

	summ p_FP
	local p_FP = r(mean)


	di "DID estimate: `alpha_hat'"

	di "p-value with FP correction = `p_FP'"

	_pctile B1, nq(200)
	if `num' ==1 & `cat_num'==1{
	mat CIs = (`alpha_hat',  r(r5),  r(r195), `num' ,`cat_num', `treatstatenum', `treatquarterdate', `epop' , `population')
	}
	else{
	mat CIs = (CIs \  `alpha_hat',  r(r5),  r(r195), `num' ,`cat_num', `treatstatenum', `treatquarterdate', `epop' , `population')
	
	}
	
	}
	
}
}
*

clear 
svmat CIs
rename CIs1 estimate
rename CIs2 low_estimate
rename CIs3 hi_estimate
rename CIs4 event_no
rename CIs5 cat
rename CIs6 statenum
rename CIs7 quarterdate
rename CIs8 epop
rename CIs9 population

gen 	cat_str = "_above" 		if cat == 1
replace cat_str= "_below" 		if cat == 2
replace cat_str = "_upper" 		if cat == 3
replace cat_str = "_all" 		if cat == 4
replace cat_str = "_bunching" 	if cat == 5
cap drop cat

reshape wide estimate low_estimate hi_estimate, i(event_no  statenum quarterdate epop population) j(cat_str) string

foreach vv of varlist estimate* low_estimate* hi_estimate*{
replace `vv' = `vv' / epop
}
*

*Shifting confidence intervals

foreach cat in above below bunching  upper{
gen low_estimate_`cat'2 = -low_estimate_`cat' +  estimate_`cat'
gen hi_estimate_`cat'2 = -hi_estimate_`cat' + estimate_`cat'
}

local titlebelow FigureD1_a
local titleabove FigureD1_b
local titlebunching FigureD1_c
local titleupper FigureD2_b


foreach cat in above below bunching  upper{
preserve
drop if statenum==11
drop if quarterdate<81
count 
local tot_event = r(N)
cap drop _event
egen _event = rank(estimate_`cat') 

count if (estimate_`cat' <= low_estimate_`cat' | estimate_`cat' >=hi_estimate_`cat') 
local sig = r(N)

cap drop sig
g sig = (estimate_`cat' <= low_estimate_`cat' | estimate_`cat' >=hi_estimate_`cat') 

twoway ///
rbar low_estimate_`cat'2 hi_estimate_`cat'2 _event , horizontal color(gs12)   ///
|| function y=0 , range(0 130) horizontal color(gs6) lpattern(dash)  ///
 || scatter _event estimate_`cat'  if sig==0, mcolor(maroon) mlwidth(vvthin)   msymbol(Sh)  msize(vsmall)   ///
  || scatter _event estimate_`cat' if sig==1, mcolor(red)  msymbol(S) msize(vsmall) graphregion(color(white)) ///
legend(off)  ylabel(0(10)130,angle(horizontal)) xlabel(-0.2(0.1)0.2)  ///
 ytitle(Event) xtitle("Actual estimate and the 95% CI")
gr export "${figures}`title`cat''.pdf", replace
restore
}
*

