
*This do file is created by DC on 10/26/2017 
********************************************************************************
****************          Measurement Error                   ******************
********************************************************************************
clear 

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




*Minimum wage data
use "${data}VZ_mw_state_quarterly_new.dta", clear 
rename statefips statenum 
rename quarterly_date quarterdate
gen year = year(dofq(quarterdate))
gen quarter = quarter(dofq(quarterdate))
rename mean_mw mw
replace mw = round(mw*100, 1)

gen int _mw = floor(mw/5)*5
replace mw=_mw
cap drop _mw
tempfile for_correction
save `for_correction'

use "${data}VZ_mw_state_quarterly_new.dta", clear 
rename statefips statenum 
rename quarterly_date quarterdate
gen year = year(dofq(quarterdate))
gen quarter = quarter(dofq(quarterdate))
rename mean_mw mw
replace mw = round(mw*100, 1)

gen int _mw = floor(mw/25)*25
replace mw=_mw
cap drop _mw
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
save "${data}yearly_mw_data_for_ME.dta", replace


********************************************************************************
******                   Correction for mismeasurement in hours       **********
********************************************************************************

use "${data}Admin_CPS_workingdata_nominal_allind_5cents_new.dta", clear
keep if statenum==27| statenum==41 | statenum == 53
merge m:1 statenum quarterdate using `for_correction', keepusing(mw) keep(3) nogenerate
gen mod = mod(mw, 25)
gen _wagebins = floor((wagebins-mod+10)/25)*25
drop mod mw wagebins
gen wagebins = _wagebins
replace wagebins = 0 if wagebins<0
replace wagebins = 3475 if wagebins==3500
drop _wagebins
bys statenum quarterdate: egen countall = total(count)
collapse  (firstnm)  population MW DMW acountall MNcountall ORcountall countall (sum) count MNcount ORcount acount, by(statenum quarterdate year wagebins)
tempfile admin_cps_data
save `admin_cps_data'


*Quarterly CPS-ORG data

foreach vv in CPS Admin{
use `admin_cps_data', clear
keep if statenum==27| statenum==41 | statenum == 53
`alt'
keep if year>=1998 & year<=2015

if "`vv'" == "CPS"{
replace count = 0 if count==.
local weight_var count
}
if "`vv'" == "Admin"{
gen admincount = acount if statenum==53
replace admincount = MNcount if statenum==27
replace admincount = ORcount if statenum==41
local weight_var admincount
}

  # delimit ;
  collapse 
       (p1) p1=wagebins 
       (p2) p2=wagebins 
       (p3) p3=wagebins   
       (p4) p4=wagebins 
       (p5) p5=wagebins 
       (p6) p6=wagebins 
       (p7) p7=wagebins 
       (p8) p8=wagebins 
       (p9) p9=wagebins 
       (p10) p10=wagebins 
       (p11) p11=wagebins 
       (p12) p12=wagebins 
       (p13) p13=wagebins 
       (p14) p14=wagebins 
       (p15) p15=wagebins 
       (p16) p16=wagebins 
       (p17) p17=wagebins 
       (p18) p18=wagebins 
       (p19) p19=wagebins 
       (p20) p20=wagebins 
       (p21) p21=wagebins 
       (p22) p22=wagebins 
       (p23) p23=wagebins 
       (p24) p24=wagebins 
       (p25) p25=wagebins 
       (p26) p26=wagebins 
       (p27) p27=wagebins 
       (p28) p28=wagebins 
       (p29) p29=wagebins 
       (p30) p30=wagebins 
       (p31) p31=wagebins 
       (p32) p32=wagebins 
         (p33) p33=wagebins 
         (p34) p34=wagebins 
         (p35) p35=wagebins 
         (p36) p36=wagebins 
         (p37) p37=wagebins 
         (p38) p38=wagebins 
         (p39) p39=wagebins 
         (p40) p40=wagebins 
         (p41) p41=wagebins 
         (p42) p42=wagebins 
         (p43) p43=wagebins 
         (p44) p44=wagebins 
         (p45) p45=wagebins 
         (p46) p46=wagebins 
         (p47) p47=wagebins 
         (p48) p48=wagebins 
         (p49) p49=wagebins 
         (p50) p50=wagebins 
         (p51) p51=wagebins 
         (p52) p52=wagebins 
         (p53) p53=wagebins 
         (p54) p54=wagebins 
         (p55) p55=wagebins 
         (p56) p56=wagebins 
         (p57) p57=wagebins 
         (p58) p58=wagebins 
         (p59) p59=wagebins 
         (p60) p60=wagebins 
         (p61) p61=wagebins 
         (p62) p62=wagebins 
         (p63) p63=wagebins 
         (p64) p64=wagebins 
         (p65) p65=wagebins 
         (p66) p66=wagebins 
         (p67) p67=wagebins 
         (p68) p68=wagebins 
         (p69) p69=wagebins 
         (p70) p70=wagebins 
         (p71) p71=wagebins 
         (p72) p72=wagebins 
         (p73) p73=wagebins 
         (p74) p74=wagebins 
         (p75) p75=wagebins 
         (p76) p76=wagebins 
         (p77) p77=wagebins 
         (p78) p78=wagebins 
         (p79) p79=wagebins 
         (p80) p80=wagebins 
         (p81) p81=wagebins 
         (p82) p82=wagebins 
         (p83) p83=wagebins 
         (p84) p84=wagebins 
         (p85) p85=wagebins 
         (p86) p86=wagebins 
         (p87) p87=wagebins 
         (p88) p88=wagebins 
         (p89) p89=wagebins 
         (p90) p90=wagebins 
         (p91) p91=wagebins 
         (p92) p92=wagebins 
         (p93) p93=wagebins 
         (p94) p94=wagebins 
         (p95) p95=wagebins 
         (p96) p96=wagebins 
         (p97) p97=wagebins
         (p98) p98=wagebins 
         (p99) p99=wagebins
         (rawsum) sumwt`vv'=`weight_var'
         [aw=`weight_var'], by(statenum year) fast;
		 # delimit cr

save "${data}annual_percentiles_`vv'_MNWAOR_`altname'.dta", replace
}
*


********************************************************************************
**************              Fully pooled                             ***********
********************************************************************************


foreach vv in Admin CPS {

if "`vv'" == "CPS"{
local weight_var count
}
if "`vv'" == "Admin"{
local weight_var admincount
}

use "${data}annual_percentiles_`vv'_MNWAOR_`altname'.dta", clear

reshape long p, i(state year sumwt) j(f)
rename p w_all
egen sumwt_all=max(sumwt), by(state year)


sort state year f
merge m:1 state year using "${data}yearly_mw_data_for_ME.dta", nogenerate keep(3) assert(2 3)

cap drop lw_all
g lw_all=ln(round(w_all))


compress
g lmin=ln(mw)

g invf=invnorm(f/100)
g invf_y=invf*(year/10)
keep if f>=50&f<=75
*keep if f>=50&f<=60

xi: reg lw_all invf i.year i.state i.state*year i.year*invf i.state*invf i.state*invf_y [aw=sumwt_all] if f>=50&f<=75 
*xi: reg lw_all invf i.year i.state i.state*year i.year*invf i.state*invf i.state*invf_y [aw=sumwt_all] if f>=50&f<=60 

predict lmed_all if e(sample) & f==50
predict z_all if e(sample) & f==75
*predict z_all if e(sample) & f==60
sort state year
egen sig_all=max(z_all), by(state year)
replace sig_all=(sig_all-lmed_all)/invnorm(0.75)
*replace sig_all=(sig_all-lmed_all)/invnorm(0.60)

collapse lmin lmed* sig*, by(state year)
sort state year

tempfile latent_notipped
save `latent_notipped'


clear
set obs 1
g year=.

*save "${data}ML_results_pooled_`vv'_MNWAOR_`altname'.dta", replace

/* this section pulls in the compressed morg dataset, which includes a dummy for whether the individ is in a "tipped occupation" */
use `admin_cps_data', clear
keep if statenum==27| statenum==41 | statenum == 53
keep if year>=1998 & year<=2015

replace count = 0 if count==.
gen admincount = acount if statenum==53
replace admincount = MNcount if statenum==27
replace admincount = ORcount if statenum==41

sort state year quarterdate
merge m:1 state year quarterdate using `mw_quarter', keepusing(mw) nogenerate assert(2 3) keep(3)
rename mw mw_quarter
sort state year
merge m:1 state year using "${data}yearly_mw_data_for_ME.dta", keepusing(mw) nogenerate assert(2 3) keep(3)

*this keeps only observations where the monthly minwage equals the value we use for yearly
keep if mw_quarter==mw
rename mw mw_y

g lw=ln(wagebins)
gen lmin=ln(mw_y)

tempfile tipped_full_all
save `tipped_full_all', replace


/* this section does the actual ML estimation, separately by year/gender */
disp "ML est"


*mat bz=0.5,-0.5
mat bz=0.5,0.5

local s _all

use `tipped_full_all' , clear


sort state year
merge m:1 state year using `latent_notipped', keepusing(lmed`1' sig`1') nogenerate

g zmin=(lmin-lmed`s')/sig`s'
g z=(lw-lmed`s')/sig`s'

drop if z==.

keep z zmin state year `weight_var'

disp "1"
egen swgt=mean(`weight_var')
replace `weight_var'=`weight_var'/swgt
drop swgt
ml model lf mlerr2 () () [aw=`weight_var']
ml init bz, copy
ml max, iter(100)

matrix b=e(b)
svmat b, name(b)
matrix ic=e(ic)
svmat ic, name(ic)
keep if b1<.
g sample = "pool"

keep b1 b2 sample  ic
save "${data}ML_results_pooled_`vv'_MNWAOR_`altname'_fullypool.dta", replace


use "${data}ML_results_pooled_`vv'_MNWAOR_`altname'_fullypool.dta", clear
gen p=exp(b1)/(1+exp(b1))
gen rho2=1/(1+exp(2*b2))
gen rho=sqrt(rho2)
gen eta=p+(1-p)*rho
gen se_w=exp(b2)
save "${data}ML_results_pooled_`vv'_MNWAOR_`altname'_fullypool.dta", replace

}
*






