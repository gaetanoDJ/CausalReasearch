
*This do file is created by DC on 05/01/2016.
********************************************************************************
***************************   Working dataset            ***********************
********************************************************************************
*This working dataset is created for baseline regressions and tercile regressions.


use 						"${data}state_panels_cents_balanced_add_QJE.dta" , clear
g all=1

cap drop _merge
merge m:1 statenum quarterdate using "${data}VZmw_quarterly_lagsleads_1979_2016.dta"
 
g lnpop = ln(population)


* cap drop _merge
* merge 1:1 statenum quarterdate using "${Dropbox}/NSW response/Final Files Aug 2015 Submission ILRR/ReplicationPackage/Data/macrocps_org_teens.dta"

*** SET PANEL VARS ***

egen wagebinstate = group(wagebins statenum)
xtset wagebinstate quarterdate   

egen wagequarterdate = group(wagebins quarterdate)


********** Define outcomes - *****


g cleansample = 1
replace cleansample = 0 if quarterdate >=136 & quarterdate <=142

g femalepop = pop-genderpop
g overallpop = pop
rename hslpop HSLpop
rename hsl40pop HSL40pop
rename hsdpop HSDpop
rename hsd40pop HSD40pop

g BHpop = blackpop + hispanicpop

rename hslcount* HSLcount*
rename hsl40count* HSL40count*
rename hsdcount* HSDcount*
rename hsd40count* HSD40count*
 
g overallcountpc = count/pop
g teencountpc = teencount/teenpop
g femalecountpc = (count-gendercount)/(pop-genderpop)
g HSLcountpc = HSLcount/HSLpop
g HSDcountpc = HSDcount/HSDpop
g BHcountpc = (blackcount + hispaniccount)/(blackpop + hispanicpop)
g HSL40countpc = HSL40count/HSL40pop
g HSD40countpc = HSD40count/HSD40pop


g overallcountpcall = countall/pop
g teencountpcall = teencountall/teenpop
g femalecountpcall = (countall-gendercountall)/(pop-genderpop)
g HSLcountpcall = HSLcountall/HSLpop
g HSDcountpcall = HSDcountall/HSDpop
g BHcountpcall = (blackcountall + hispaniccountall)/(blackpop + hispanicpop)
g HSL40countpcall = HSL40countall/HSLpop
g HSD40countpcall = HSD40countall/HSDpop


g overallFTEcountpc = count*avehours/(pop*40)

foreach x in overallFTEcountpc {
	cap drop `x'all
	egen `x'all = sum(`x'), by(statenum quarterdate)
}


*** SHOULD ALSO COMPUTE overallFTEpcall, etc. ***

* g lnEPOP = ln(EPOP)

cap drop emp
* g emp = EPOP
 
sort statenum quarterdate wagebins
/* 
foreach j in overall teen female HSL BH {
	g `j'countpc_cum = `j'countpc if wagebins==300
	replace `j'countpc_cum = `j'countpc_cum[_n-1] + `j'countpc if wagebins>300 & wagebins<=3000 & statenum==statenum[_n-1] & quarterdate==quarterdate[_n-1]
 }
 */

/********  Real Minimum Wages, Wage Increases, Treatment  ******/

global Wmax 4
global Wmin 4
 

xtset wagebinstate quarterdate   


g wageM25 = wagebins/100

cap drop *MW* 

g MW = exp(logmw) 
g DMW = D.MW
g MW_real = exp(logmw)/(cpi/100) 
g DMW_real = D.MW_real 

g MW_realM25 = MW_real - mod(MW_real,0.25)
g DMW_realM25 = D.MW_realM25

cap drop quarterdatesq 
g quarterdatesq = quarterdate^2


********************************************************************************
****************        Find Quintiles by Below Share        *******************
********************************************************************************

foreach num of numlist 1(1)4{
	gen F`num'MW_realM25 = F`num'.MW_realM25
}

cap drop tagger
egen tagger =tag(statenum quarterdate)

	****************************************************************************
	*******         Find treatments & Federal & State Increases      ***********
	****************************************************************************
	
	
	
		preserve
			gen alltreat = (${ifclause})
			assert alltreat == 1 | alltreat==0
			count if alltreat==1
			scalar asd=r(N)
			keep if tagger==1
			keep statenum quarterdate alltreat year
			tempfile widedata
			save `widedata', replace
		restore

	****************************************************************************
	************        Find Effective below bins          *********************
	****************************************************************************

*Get below shares & workforce
preserve
forval num=1(1)4{
bys statenum quarterdate: egen _below`num'count = total(count) if (wagebins)/100<F`num'MW_realM25 & wagebins/100>=MW_realM25
bys statenum quarterdate: egen _below`num'count2 = total(count) if (wagebins)/100<F`num'MW_realM25
by statenum quarterdate:  egen below`num'count = max(_below`num'count)
by statenum quarterdate:  egen below`num'count2 = max(_below`num'count2)
assert _below`num'count==below`num'count if _below`num'count!=.
assert _below`num'count2==below`num'count2 if _below`num'count2!=.
}
keep if tagger==1
keep statenum quarterdate below* pop _below* countall MW
xtset statenum quarterdate
forval num=1(1)4{
gen L`num'below`num'count = L`num'.below`num'count
gen L`num'below`num'count2 = L`num'.below`num'count2
gen L`num'below`num'countall = L`num'.countall
}
merge 1:1 statenum quarterdate using `widedata', assert(3) nogenerate

********************************************************************************
***********************      Federal Increases           ***********************
********************************************************************************

			cap drop fedincrease
			gen fedincrease=0
			replace fedincrease = 1 if quarterdate == tq(1980q1) & alltreat==1 & (MW<=3.10001 & MW>=3.0999)
			replace fedincrease = 1 if quarterdate == tq(1981q1) & alltreat==1 & (MW<=3.35001 & MW>=3.3499)
			replace fedincrease = 1 if quarterdate == tq(1990q2) & alltreat==1 & (MW<=3.80001 & MW>=3.7999)
			replace fedincrease = 1 if quarterdate == tq(1991q2) & alltreat==1 & (MW<=4.25001 & MW>=4.2499)
			replace fedincrease = 1 if quarterdate == tq(1996q4) & alltreat==1 & (MW<=4.75001 & MW>=4.7499)
			replace fedincrease = 1 if quarterdate == tq(1997q3) & alltreat==1 & (MW<=5.15001 & MW>=5.1499)
			replace fedincrease = 1 if quarterdate == tq(2007q3) & alltreat==1 & (MW<=5.85001 & MW>=5.8499)
			replace fedincrease = 1 if quarterdate == tq(2008q3) & alltreat==1 & (MW<=6.55001 & MW>=6.5499)
			replace fedincrease = 1 if quarterdate == tq(2009q3) & alltreat==1 & (MW<=7.25001 & MW>=7.2499)

save `widedata',replace
restore

preserve
use `widedata',clear
keep if alltreat==1
forval num=1(1)4{
replace L`num'below`num'count = 0 if  L`num'below`num'count==. 					// In some cases L`i'belowshare is "." because size of MW increase is not very large.
assert  L`num'below`num'count2!=. if year>1981
}
gen belowshare = (L1below1count + L2below2count + L3below3count + L4below4count)/(L1below1countall + L2below2countall + L3below3countall + L4below4countall)
gen belowshare2 = (L1below1count2 + L2below2count2 + L3below3count2 + L4below4count2)/(L1below1countall + L2below2countall + L3below3countall + L4below4countall)

assert belowshare!=. 
assert belowshare2!=. if year>1981 

cap drop toosmall
gen toosmall = (belowshare<0.02 & belowshare!=.)
if "$numberquant" == ""{
global numberquant = 3
}
xtile overallcountgroup=belowshare if toosmall==0 , n($numberquant)
xtile belowsharegroup = belowshare if overallcountgr>0 & overallcountgr!=. & fedincrease!=1 , n($numberquant)
assert overallcountgroup!=0 & overallcountgroup!=. if year>=1981 & toosmall==0
assert overallcountgroup<=$numberquant if overallcountgr!=.
keep statenum quarterdate overallcountgroup alltreat toosmall belowshare /*belowsharegroup*/ fedincrease /*balancedgroup*/ belowsharegroup
save `widedata',replace
save "${data}eventclassification.dta", replace

use "${data}eventclassification.dta", clear

tsfill, full

replace overallcountgr = 0 if overallcountgr==.
replace belowsharegroup = 0 if belowsharegroup==.

foreach vv in L {
	forval num=1(1)12{
		gen `vv'`num'quarterdate = (`vv'`num'.quarterdate!=. & (`vv'`num'.quarterdate<136 |  `vv'`num'.quarterdate>142) )
	}
}
*

foreach vv in F {
	forval num=1(1)19{
		gen `vv'`num'quarterdate = (`vv'`num'.quarterdate!=. & (`vv'`num'.quarterdate<136 |  `vv'`num'.quarterdate>142) )
	}
}
*




gen fullbelowsharegr = overallcountgr

local vv F
forval num=1(1)19{
		replace fullbelowsharegr = fullbelowsharegr * `vv'`num'quarterdate
	}
*

local vv L
forval num=1(1)12{
		replace fullbelowsharegr = fullbelowsharegr * `vv'`num'quarterdate
	}
*

drop F* L*
count if fullbelowshare>0
xtile fullbelowshare=belowshare if fullbelowshare>0 , n($numberquant)
replace fullbelowshare = 0 if fullbelowshare==.
keep statenum quarterdate fullbelowshare
save "${data}fullybalancedevents.dta", replace


restore

cap drop _merge
merge m:1 statenum quarterdate using "${data}eventclassification.dta", nogenerate
merge m:1 statenum quarterdate using "${data}fullybalancedevents.dta", nogenerate
replace overallcountgroup = 0 if overallcountgroup==.
replace belowsharegroup = 0 if belowsharegroup==.
replace fullbelowshare = 0 if fullbelowshare==.
assert overallcountgroup!=0 if alltreat==1 & toosmall==0
sum overallcountgroup,meanonly
local upperquant = r(max)





********************************************************************************
******************     Finding Federal Increases           *********************
********************************************************************************
xtset wagebinstate quarterdate

compress




********************************************************************************
************************      Correcting Treatments       **********************
********************************************************************************
xtset wagebinstate quarterdate
forval k = 0/7  {
	cap drop treat_p`k' 
	cap drop _treat_p`k'
	g _treat_p`k'=0
		replace _treat_p`k' = 1 if $ifclause &  (wageM25>=(MW_realM25+`k' ) &  wageM25 < (MW_realM25 +1 +`k' )) & overallcountgroup>0 & fedincrease!=1
	 g byte treat_p`k' = ( _treat_p`k' +  L._treat_p`k' + L2._treat_p`k' + L3._treat_p`k')
	cap drop _treat_p`k'

}

forval k = 1/7  {
	cap drop treat_m`k'
	cap drop _treat_m`k'
	g _treat_m`k'=0
		replace _treat_m`k' = 1 if $ifclause & (wageM25<(MW_realM25 - `k' + 1 )  &  wageM25 >= (MW_realM25 -`k'  ))  & overallcountgroup>0 & fedincrease!=1
	g byte treat_m`k' = ( _treat_m`k' + L._treat_m`k' + L2._treat_m`k' + L3._treat_m`k' )
cap drop _treat_m`k'
}  


 cap drop  Dtreat_p0
 g byte Dtreat_p0 = D.treat_p0 

 
 
 


********************************************************************************
*****************     Control for Federal Increases             ****************
********************************************************************************   
*Aggregated Controls

cap drop _contf_p
cap drop postcontf_p
cap drop postcontf_m

g _contf_p = 0
replace _contf_p = 1 if (fedincrease==1 &  overallcountgroup>0 & $ifclause ) &  (wageM25>=(MW_realM25) &  wageM25 < (MW_realM25 +1 +${Wmax} ))
g tempcontf_p = _contf_p + L._contf_p + L2._contf_p + L3._contf_p

local Tmax3 = ${Tmax} + 3
forval i = 0(1)`Tmax3'{
	if `i' == 0{
	g postcontf_p = _contf_p

	}
	else {
	replace postcontf_p = postcontf_p + L`i'._contf_p
	sum postcontf_p, meanonly
	assert r(mean)!=0
	}
}
cap drop _contf_p

cap drop _contf_m
g _contf_m = 0
replace _contf_m = 1 if (fedincrease==1 &  overallcountgroup>0 & $ifclause ) &  (wageM25<(MW_realM25 )  &  wageM25 >= (MW_realM25 -  ${Wmin}  ))	
g tempcontf_m = _contf_m + L._contf_m + L2._contf_m + L3._contf_m
forval i = 0(1)`Tmax3'{
	if `i' == 0{
	g postcontf_m = _contf_m
	}
	else {
	replace postcontf_m = postcontf_m + L`i'._contf_m
	sum postcontf_m, meanonly
	assert r(mean)!=0

	}
}
cap drop _contf_m



*Leads
if $Tmin == 12{
	foreach k in m p {
		g precontf_`k' = F4.tempcontf_`k'
		replace precontf_`k' = 0 if precontf_`k' == .
	}
*

foreach k in m p {
		g earlycontf_`k' = F12.tempcontf_`k' + F8.tempcontf_`k'
		replace earlycontf_`k' = 0 if earlycontf_`k' == .
	}
}
*


if $Tmin == 8{
	foreach k in m p {
		g precontf_`k' = F4.tempcontf_`k'
	}
*

foreach k in m p {
		g earlycontf_`k' = F8.tempcontf_`k'
	}
}

********************************************************************************
****************     Treatments for Quantile Analysis      *********************
********************************************************************************


forval k = 0/5  {
	cap drop treatq_p`k' 
	cap drop _treatq_p`k'
	g _treatq_p`k'=0
		replace _treatq_p`k' = 1 if $ifclause &  (wageM25>=(MW_realM25+`k' ) &  wageM25 < (MW_realM25 +1 +`k' )) & belowsharegroup == `upperquant' & fedincrease!=1
	 g treatq_p`k' = ( _treatq_p`k' +  L._treatq_p`k' + L2._treatq_p`k' + L3._treatq_p`k')
	cap drop _treatq_p`k'

}

forval k = 1/5  {
	cap drop treatq_m`k'
	cap drop _treatq_m`k'
	g _treatq_m`k'=0
		replace _treatq_m`k' = 1 if $ifclause & (wageM25<(MW_realM25 - `k' + 1 )  &  wageM25 >= (MW_realM25 -`k'  ))  & belowsharegroup == `upperquant' & fedincrease!=1
	g treatq_m`k' = ( _treatq_m`k' + L._treatq_m`k' + L2._treatq_m`k' + L3._treatq_m`k' )
cap drop _treatq_m`k'
}  


  
g Dtreatq_p0 = D.treatq_p0 


********************************************************************************
****************          Controls         *************************************
********************************************************************************


********************************************************************************
****************       Disaggregated Controls        ***************************
********************************************************************************



	forval k = 0/5  {
		cap drop cont_p`k' 
		cap drop _cont_p`k'
		g _cont_p`k'=0
		replace _cont_p`k' = 1 if ($missedevents | toosmall==1) &  (wageM25>=(MW_realM25+`k' ) &  wageM25 < (MW_realM25 +1 +`k' ))
		g cont_p`k' = ( _cont_p`k' +  L._cont_p`k' + L2._cont_p`k' + L3._cont_p`k')
		cap drop _cont_p`k'

	}

	forval k = 1/5  {
		cap drop cont_m`k'
		cap drop _cont_m`k'
		g _cont_m`k'=0
		replace _cont_m`k' = 1 if ($missedevents | toosmall==1) & (wageM25<(MW_realM25 - `k' + 1)  &  wageM25 >= (MW_realM25 -`k'  ))
		g cont_m`k' = ( _cont_m`k' + L._cont_m`k' + L2._cont_m`k' + L3._cont_m`k' )
	cap drop _cont_m`k'
	}	  

********************************************************************************
******************     Fully Aggregated Controls        ************************
********************************************************************************	
	
	
	
cap drop _cont_p
g _cont_p = 0
replace _cont_p = 1 if ($missedevents | toosmall==1) &  (wageM25>=(MW_realM25 ) &  wageM25 < (MW_realM25 +1 +${Wmax} ))
g tempcont_p = _cont_p + L._cont_p + L2._cont_p + L3._cont_p

local Tmax3 = ${Tmaxcont} + 3
forval i = 0(1)`Tmax3'{
	if `i' == 0{
	g postcont_p = _cont_p
	}
	else {
	replace postcont_p = postcont_p + L`i'._cont_p
	}
}
cap drop _cont_p

cap drop _cont_m
g _cont_m = 0
replace _cont_m = 1 if ($missedevents | toosmall==1) &  (wageM25<(MW_realM25 - 0)  &  wageM25 >= (MW_realM25 -  ${Wmin} - 0 ))	
g tempcont_m = _cont_m + L._cont_m + L2._cont_m + L3._cont_m
forval i = 0(1)`Tmax3'{
	if `i' == 0{
	g postcont_m = _cont_m
	}
	else {
	replace postcont_m = postcont_m + L`i'._cont_m
	}
}
cap drop _cont_m

*

********************************************************************************
*****************       Controls for Quantiles         *************************
********************************************************************************
*Disaggregated


forval k = 0/5  {
	cap drop contq_p`k' 
	cap drop _contq_p`k'
	g _contq_p`k'=0
		replace _contq_p`k' = 1 if $ifclause &  (wageM25>=(MW_realM25+`k' - 0) &  wageM25 < (MW_realM25 +1 +`k' - 0)) & (belowsharegroup != `upperquant' & overallcountgroup>0 & overallcountgroup!=.)  & fedincrease!=1
	 g contq_p`k' = ( _contq_p`k' +  L._contq_p`k' + L2._contq_p`k' + L3._contq_p`k')
	cap drop _contq_p`k'

}

forval k = 1/5  {
	cap drop contq_m`k'
	cap drop _contq_m`k'
	g _contq_m`k'=0
		replace _contq_m`k' = 1 if $ifclause & (wageM25<(MW_realM25 - `k' + 1 -0)  &  wageM25 >= (MW_realM25 -`k' -0 )) & (belowsharegroup != `upperquant' & overallcountgroup>0 & overallcountgroup!=.)  & fedincrease!=1
	g contq_m`k' = ( _contq_m`k' + L._contq_m`k' + L2._contq_m`k' + L3._contq_m`k' )
cap drop _contq_m`k'
}  
*


*************     Aggregated

g _contq_p = 0
replace _contq_p = 1 if  $ifclause & (belowsharegroup != `upperquant' & overallcountgroup>0 & overallcountgroup!=.)  & fedincrease!=1 &  (wageM25>=(MW_realM25 - 0) &  wageM25 < (MW_realM25 +1 +${Wmax} - 0))
g tempcontq_p = _contq_p + L._contq_p + L2._contq_p + L3._contq_p

local Tmax3 = ${Tmaxcont} + 3
forval i = 0(1)`Tmax3'{
	if `i' == 0{
	g postcontq_p = _contq_p
	}
	else {
	replace postcontq_p = postcontq_p + L`i'._contq_p
	}
}
cap drop _contq_p

cap drop _contq_m
g _contq_m = 0
replace _contq_m = 1 if $ifclause & (belowsharegroup != `upperquant' & overallcountgroup>0 & overallcountgroup!=.)  & fedincrease!=1 & (wageM25<(MW_realM25 - 0)  &  wageM25 >= (MW_realM25 -  ${Wmin} - 0 ))	
g tempcontq_m = _contq_m + L._contq_m + L2._contq_m + L3._contq_m
forval i = 0(1)`Tmax3'{
	if `i' == 0{
	g postcontq_m = _contq_m
	}
	else {
	replace postcontq_m = postcontq_m + L`i'._contq_m
	}
}
cap drop _contq_m


compress


**** DEFINE TREATMENT VARIABLES AND LAGS/LEADS *****
*No need for lags but we need to define leads 

xtset wagebinstate quarterdate   

********************************************************************************
*************************      Leads and Lags           ************************
********************************************************************************



 
foreach j in  4 8 12 16 {
	foreach k in m5 m4 m3 m2 m1 p0 p1 p2 p3 p4 p5 {
		cap drop F`j'treat_`k'
		cap drop L`j'treat_`k'
		cap drop F`j'treatq_`k'
		cap drop L`j'treatq_`k'
		cap drop F`j'contq_`k'
		cap drop L`j'contq_`k'
		cap drop F`j'cont_`k'
		cap drop L`j'cont_`k'
		
		g F`j'treat_`k' = F`j'.treat_`k'
 		g L`j'treat_`k' = L`j'.treat_`k'
		g F`j'treatq_`k' = F`j'.treatq_`k'
 		g L`j'treatq_`k' = L`j'.treatq_`k'
		g F`j'contq_`k' = F`j'.contq_`k'
 		g L`j'contq_`k' = L`j'.contq_`k'
		g F`j'cont_`k' = F`j'.cont_`k'
 		g L`j'cont_`k' = L`j'.cont_`k'

		
		replace F`j'treat_`k' = 0 if F`j'treat_`k' == .
 		replace L`j'treat_`k' = 0 if L`j'treat_`k' == .
		replace F`j'treatq_`k' = 0 if  F`j'treatq_`k'== .
 		replace L`j'treatq_`k' = 0 if L`j'treatq_`k' == .
		replace F`j'contq_`k' = 0 if F`j'contq_`k'== .
 		replace L`j'contq_`k' = 0 if  L`j'contq_`k'== .
		replace F`j'cont_`k' = 0 if  F`j'cont_`k'== .
 		replace L`j'cont_`k' = 0 if L`j'cont_`k' == .
		
		
		
	}
} 

*

foreach j in  4 8 12 16 {
	foreach k in m7 m6 p6 p7 {
		cap drop F`j'treat_`k'
		cap drop L`j'treat_`k'

		g F`j'treat_`k' = F`j'.treat_`k'
 		g L`j'treat_`k' = L`j'.treat_`k'

		
		replace F`j'treat_`k' = 0 if F`j'treat_`k' == .
 		replace L`j'treat_`k' = 0 if L`j'treat_`k' == .
		
		
	}
}
*
********************************************************************************
******************      Leads for Agg. Controls          ***********************
********************************************************************************

	foreach k in m p {
		g precont_`k' = F4.tempcont_`k'
		g precontq_`k' = F4.tempcontq_`k'

		replace precont_`k' =  0 if precont_`k' == .
		replace precontq_`k' = 0 if precontq_`k' == .
		
		
	}
*

foreach k in m p {
		cap drop earlycont_`k'
		cap drop earlycontq_`k'
		g earlycont_`k' = F12.tempcont_`k' + F8.tempcont_`k'
		g earlycontq_`k' = F12.tempcontq_`k' + F8.tempcontq_`k'

		replace earlycont_`k' = 0 if earlycont_`k' == .
		replace earlycontq_`k' = 0 if earlycontq_`k' == .
		}
*


replace postcont_p = 0 if postcont_p==.
replace postcont_m = 0 if postcont_m==.

replace postcontf_p = 0 if postcontf_p==.
replace postcontf_m = 0 if postcontf_m==.

replace postcontq_p = 0 if postcontq_p==.
replace postcontq_m = 0 if postcontq_m==.

********************************************************************************
********************           Window Variable             *********************
********************************************************************************


foreach pm in p m{
	foreach num of numlist 1(1)7{
		gen window_`pm'`num' =  F12treat_`pm'`num' +  F8treat_`pm'`num' +  F4treat_`pm'`num' + treat_`pm'`num' + L4treat_`pm'`num'  + L8treat_`pm'`num'  + L12treat_`pm'`num'  + L16treat_`pm'`num'  
	}
}

local pm p
local num 0
gen window_`pm'`num' =  F12treat_`pm'`num' +  F8treat_`pm'`num' +  F4treat_`pm'`num' + treat_`pm'`num' + L4treat_`pm'`num'  + L8treat_`pm'`num'  + L12treat_`pm'`num'  + L16treat_`pm'`num'  


foreach name in q {
foreach pm in p m{
	foreach num of numlist 1(1)5{
		gen window`name'_`pm'`num' =  F12treat`name'_`pm'`num' +  F8treat`name'_`pm'`num' +  F4treat`name'_`pm'`num' + treat`name'_`pm'`num' + L4treat`name'_`pm'`num'  + L8treat`name'_`pm'`num'  + L12treat`name'_`pm'`num'  + L16treat`name'_`pm'`num'  
	}
}

local pm p
local num 0
gen window`name'_`pm'`num' =  F12treat`name'_`pm'`num' +  F8treat`name'_`pm'`num' +  F4treat`name'_`pm'`num' + treat`name'_`pm'`num' + L4treat`name'_`pm'`num'  + L8treat`name'_`pm'`num'  + L12treat`name'_`pm'`num'  + L16treat`name'_`pm'`num'  
}
*	





	
********************************************************************************
*****************              Weights             *****************************
********************************************************************************

cap drop wt 
foreach Y in   overall teen HSD HSD40 HSL female BH HSL40 { 
	foreach b of numlist  1979{
					egen wt`Y'`b' = mean(`Y'pop) if year>=`b' & cleansample ==1 , by(statenum quarterdate)
					}
}  
	
compress

*	
save "${data}state_panels_with${numberquant}quant1979.dta", replace	
