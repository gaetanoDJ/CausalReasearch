
*This do file is created by DC on 10/26/2017 
********************************************************************************
****************          Measurement Error                   ******************
********************************************************************************
clear
********************************************************************************
****************     AMS type measurement error correction          ************
********************************************************************************

*Minimum wage data
use "${data}VZ_mw_state_quarterly_new.dta", clear 
rename statefips statenum 
rename quarterly_date quarterdate
gen year = year(dofq(quarterdate))
gen quarter = quarter(dofq(quarterdate))
rename mean_mw mw
replace mw = round(mw*100, 1)
tempfile mw_quarter
save `mw_quarter'
sort year statenum mw

by year statenum : egen newmin=mode(mw)
sort statenum year quarterdate
by statenum year: replace newmin=(mw+mw[_n-1])/2 if quarter==3 & quarter[_n-1]==2 & newmin==.
by statenum year: egen _newmin = max(newmin)
replace newmin = _newmin if newmin==.

sort statenum year quarterdate

collapse (max) newmin, by(statenum year)
rename newmin mw
sort state year
tempfile minw
save "${data}yearly_mw_data_for_ME.dta", replace


*Commented out for the moment, since data is created and creating it takes time.
*Quarterly CPS-ORG data
use "${data}CPS_nominal_1979onwards.dta", clear

*To do AMS-type measurement error correction, we need to drop tipped workers and workers younger than 18.
keep if age>=18 & age<=64
keep if tipocc==0
keep if wage>0
*We need nominal wages
gen hr_wage = wage
*Then we need state-by-year percentiles.
local weight_var count


  # delimit ;
  collapse 
       (p1) p1=hr_wage 
       (p2) p2=hr_wage 
       (p3) p3=hr_wage   
       (p4) p4=hr_wage 
       (p5) p5=hr_wage 
       (p6) p6=hr_wage 
       (p7) p7=hr_wage 
       (p8) p8=hr_wage 
       (p9) p9=hr_wage 
       (p10) p10=hr_wage 
       (p11) p11=hr_wage 
       (p12) p12=hr_wage 
       (p13) p13=hr_wage 
       (p14) p14=hr_wage 
       (p15) p15=hr_wage 
       (p16) p16=hr_wage 
       (p17) p17=hr_wage 
       (p18) p18=hr_wage 
       (p19) p19=hr_wage 
       (p20) p20=hr_wage 
       (p21) p21=hr_wage 
       (p22) p22=hr_wage 
       (p23) p23=hr_wage 
       (p24) p24=hr_wage 
       (p25) p25=hr_wage 
       (p26) p26=hr_wage 
       (p27) p27=hr_wage 
       (p28) p28=hr_wage 
       (p29) p29=hr_wage 
       (p30) p30=hr_wage 
       (p31) p31=hr_wage 
       (p32) p32=hr_wage 
         (p33) p33=hr_wage 
         (p34) p34=hr_wage 
         (p35) p35=hr_wage 
         (p36) p36=hr_wage 
         (p37) p37=hr_wage 
         (p38) p38=hr_wage 
         (p39) p39=hr_wage 
         (p40) p40=hr_wage 
         (p41) p41=hr_wage 
         (p42) p42=hr_wage 
         (p43) p43=hr_wage 
         (p44) p44=hr_wage 
         (p45) p45=hr_wage 
         (p46) p46=hr_wage 
         (p47) p47=hr_wage 
         (p48) p48=hr_wage 
         (p49) p49=hr_wage 
         (p50) p50=hr_wage 
         (p51) p51=hr_wage 
         (p52) p52=hr_wage 
         (p53) p53=hr_wage 
         (p54) p54=hr_wage 
         (p55) p55=hr_wage 
         (p56) p56=hr_wage 
         (p57) p57=hr_wage 
         (p58) p58=hr_wage 
         (p59) p59=hr_wage 
         (p60) p60=hr_wage 
         (p61) p61=hr_wage 
         (p62) p62=hr_wage 
         (p63) p63=hr_wage 
         (p64) p64=hr_wage 
         (p65) p65=hr_wage 
         (p66) p66=hr_wage 
         (p67) p67=hr_wage 
         (p68) p68=hr_wage 
         (p69) p69=hr_wage 
         (p70) p70=hr_wage 
         (p71) p71=hr_wage 
         (p72) p72=hr_wage 
         (p73) p73=hr_wage 
         (p74) p74=hr_wage 
         (p75) p75=hr_wage 
         (p76) p76=hr_wage 
         (p77) p77=hr_wage 
         (p78) p78=hr_wage 
         (p79) p79=hr_wage 
         (p80) p80=hr_wage 
         (p81) p81=hr_wage 
         (p82) p82=hr_wage 
         (p83) p83=hr_wage 
         (p84) p84=hr_wage 
         (p85) p85=hr_wage 
         (p86) p86=hr_wage 
         (p87) p87=hr_wage 
         (p88) p88=hr_wage 
         (p89) p89=hr_wage 
         (p90) p90=hr_wage 
         (p91) p91=hr_wage 
         (p92) p92=hr_wage 
         (p93) p93=hr_wage 
         (p94) p94=hr_wage 
         (p95) p95=hr_wage 
         (p96) p96=hr_wage 
         (p97) p97=hr_wage
         (p98) p98=hr_wage 
         (p99) p99=hr_wage
         (rawsum) sumwt=`weight_var'
         [aw=count], by(year state) fast;
		 # delimit cr

save "${data}cps_state_year_percentiles.dta", replace



use "${data}cps_state_year_percentiles.dta", clear

reshape long p, i(state year sumwt) j(f)
rename p w_all
egen sumwt_all=max(sumwt), by(state year)


sort state year f
merge m:1 state year using "${data}yearly_mw_data_for_ME.dta", nogenerate keep(3) assert(2 3)

replace mw=round(mw,1)
g lw_all=ln(round(w_all),1)


compress
g lmin=ln(mw)

g invf=invnorm(f/100)
g invf_y=invf*(year/10)
keep if f>=50&f<=75

xi: reg lw_all invf i.year i.state i.state*year i.year*invf i.state*invf i.state*invf_y [aw=sumwt_all] if f>=50&f<=75 

predict lmed_all if e(sample) & f==50
predict z_all if e(sample) & f==75
sort state year
egen sig_all=max(z_all), by(state year)
replace sig_all=(sig_all-lmed_all)/invnorm(0.75)

foreach year of numlist 1979(1)2016{
	sum sig_all, meanonly, if year==`year' [aw=sumwt_all]
	if `year'==1979{
	mat sig_all = ( r(mean), `year' )
	}
	else{
	mat sig_all = ( sig_all \ r(mean), `year' )	
	}
}
*
	
	
collapse lmin lmed* sig*, by(state year)
sort state year

tempfile latent_notipped
save `latent_notipped'


clear
set obs 1
g year=.

save "${data}ML_results_pooled_all_CPS.dta", replace

/* this section pulls in the compressed morg dataset, which includes a dummy for whether the individ is in a "tipped occupation" */
use "${data}CPS_nominal_1979onwards.dta", clear
cap drop mw
*To do AMS-type measurement error correction, we need to drop tipped workers and workers younger than 18.
keep if age>=18 & age<=64
keep if tipocc==0
keep if wage>0
gen hr_wage = wage

sort state year quarterdate
merge m:1 state year quarterdate using `mw_quarter', keepusing(mw) nogenerate assert(2 3) keep(3)
rename mw mw_quarter
sort state year
merge m:1 state year using "${data}yearly_mw_data_for_ME.dta", keepusing(mw) nogenerate assert(2 3) keep(3)

*this keeps only observations where the monthly minwage equals the value we use for yearly
keep if mw_quarter==mw
rename mw mw_y

g lw=ln(hr_wage)
gen lmin=ln(mw_y)

tempfile tipped_full_all
save `tipped_full_all', replace


/* this section does the actual ML estimation, separately by year/gender */
disp "ML est"

cap program drop mlerr2

program mlerr2
	   version 8.0
         args lnf c b
         tempvar p eta se rho rho2
         quietly gen `p'=exp(`c')/(1+exp(`c'))
         quietly gen `rho2'=1/(1+exp(2*`b'))
         quietly gen `rho'=sqrt(`rho2')
         quietly gen `eta'=`p'+(1-`p')*`rho'
         quietly gen `se'=`eta'*exp(`b')

quietly replace `lnf' = ln(`p')+ln(normal(zm/`eta')) if z==zm

// quietly replace `lnf' = ln(`p'*(1-normal(zm/`eta'))+0.5*(1-`p')*normal(zm/`eta')+  /*
//   */ (1-`p')*binormal(zm/`eta',`rho'*zm/`eta',`rho')) if z>zm
    
quietly replace `lnf' = ln(`p'*(1-normal(zm/`eta'))+0.5*(1-`p')*normal(zm/`eta')+  /*
  */ (1-`p')*(1-normal(zm/`eta')-normal(`rho'*zm/`eta')+binormal(zm/`eta',`rho'*zm/`eta',`rho'))) if z>zm
  
quietly replace `lnf' = ln(1-`p')+ln(normalden((z-zm)/`se' ,`se')*normal(zm/`eta') /*
  */ +normalden(`rho'*z/`eta',`eta'/`rho')*  /*
  */ (1-normal((zm-`rho2'*z)/(`eta'*sqrt(1-`rho2'))))) if z<zm

end

*mat bz=0.5,-0.5
mat bz=0.5,0.5

local s _all
forvalues y=1979/2016 {
disp "year=`y' & sex=`s'"

use `tipped_full_all' if year==`y', clear
gen earnwt = count

sort state year
merge m:1 state year using `latent_notipped', keepusing(lmed`1' sig`1') nogenerate

g zmin=(lmin-lmed`s')/sig`s'
g z=(lw-lmed`s')/sig`s'

drop if z==.

keep z zmin state year earnwt

disp "1"

egen swgt=mean(earnwt)
replace earnwt=earnwt/swgt
drop swgt

ml model lf mlerr2 () () [aw=earnwt]
ml init bz, copy
ml max, iter(100)

matrix b=e(b)
svmat b, name(b)
matrix ic=e(ic)
svmat ic, name(ic)
keep if b1<.
g sample = "pool"

keep b1 b2 sample year ic
append using "${data}ML_results_pooled_all_CPS.dta"
save "${data}ML_results_pooled_all_CPS.dta", replace

}
*

use "${data}ML_results_pooled_all_CPS.dta", clear

gen p=exp(b1)/(1+exp(b1))
gen rho2=1/(1+exp(2*b2))
gen rho=sqrt(rho2)
gen eta=p+(1-p)*rho
gen se_w=exp(b2)

preserve
clear
svmat sig_all
rename sig_all2 year
rename sig_all1 sig_all
tempfile temp
save `temp'
restore

merge 1:1 year using `temp'


save "${data}ML_results_pooled_all_CPS.dta", replace

