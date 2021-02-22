**outline of file.
*1) County-Full sample
*2) Cross-state metro analysis
*3) census division effects
*4) county-Full sample w. census division & state linear trends

clear 

use "${data}QCEWindustry_minwage_all.dta", clear
compress
gen all=1

egen stperiod = group(state_fips period)

gen sample_1 = (all==1)
gen sample_2 = (all==1)
gen sample_3 = (all==1)
gen sample_4 = (all==1)

gen absorb_1 = period
egen absorb_2 = group (period cbmsa) 
egen absorb_3= group (period censusdiv)
egen absorb_4= group (period censusdiv)
 

egen countyg = group(county)

xtset countyreal period


global lincomglobal "lnMW"
forval j = 1/16 {
	global lincomglobal "${lincomglobal} + L`j'.lnMW"
}



g dlnMW = D.lnMW

global dlincomglobal "dlnMW"
forval j = 1/16 {
	global dlincomglobal "${dlincomglobal} + L`j'.dlnMW"
}


*Add event-based stuff
*From other calculations (if needed)

global E = .5628805218130546
global C = 17.30151444236615

gen statenum = state_fips
gen quarterdate = yq(year, quarter)
merge m:1 statenum quarterdate using "${data}quarterly_state_panel.dta", keepusing(	F12treat F8treat F4treat treat  L4treat L8treat L12treat L16treat ///
																					postcont postcontf  precont precontf earlycont earlycontf  cleansample ///
																					DMW_real MW_real fedincrease overallcountgr avetotalpopulation totalpopulation)
global treat   		F12treat F8treat F4treat treat  L4treat L8treat L12treat L16treat 
*global minus 0
global minus (F4treat)
global lincomagg 	(treat-$minus)
forval i = 4(4)16{
global lincomagg "$lincomagg + (L`i'treat - $minus ) "
}
*
egen avetotalpopulation2 = mean(totalpopulation), by(statenum)

**** REST *****

xtset countyreal period


reghdfe lnemp_rest_both F(8/1).lnMW L(0/16).lnMW   lnpop  if nonmissing_rest_both ==66   , a(countyreal period) cluster(state_fips)

lincom "${lincomglobal}"
lincom .25*[F3.lnMW + 2*F2.lnMW + 3*F1.lnMW + 4*(${lincomglobal})]
lincomestadd .25*[F3.lnMW + 2*F2.lnMW + 3*F1.lnMW + 4*(${lincomglobal})], statname(estimated_effect)
eststo rest_TWFE

				sum DMW_real if fedincrease!=1 & overallcountgr>0  & cleansample ==1 & year>=1990 & year<=2006   [aw=avetotalpopulation2] 
				local mwc = r(mean)
				sum MW_real if  F.fedincrease!=1 & F.overallcountgr>0 & cleansample ==1  & year>=1990 & year<=2006 [aw=avetotalpopulation2] 
				local mw = r(mean)
				local mwpc = `mwc'/`mw'

reghdfe lnemp_rest_both ${treat} lnpop if nonmissing_rest_both ==66 , a(statenum quarterdate postcont postcontf  precont precontf earlycont earlycontf ) cluster(state_fips)
lincom (($lincomagg) * (1/5))/`mwpc'
lincomestadd (($lincomagg) * (1/5))/`mwpc', statname(estimated_effect)
eststo rest_EBD





reghdfe D.lnemp_rest_both F(8/1).dlnMW L(0/16).dlnMW D.lnpop  if nonmissing_rest_both==66  , a(countyreal period) cluster(state_fips)

lincom "${dlincomglobal}"
lincom .25*[F3.dlnMW + 2*F2.dlnMW + 3*F1.dlnMW + 4*(${dlincomglobal})]
lincomestadd .25*[F3.dlnMW + 2*F2.dlnMW + 3*F1.dlnMW + 4*(${dlincomglobal})], statname(estimated_effect)
eststo rest_FD


*** TOT ***

g lnepop = lnemp_TOT - lnpop

reghdfe lnemp_TOT F(8/1).lnMW L(0/16).lnMW  lnpop    if nonmissing_TOT==66    , a(countyreal period) cluster(state_fips)

lincom "${lincomglobal}"
lincom .25*[F3.lnMW + 2*F2.lnMW + 3*F1.lnMW + 4*(${lincomglobal})]
lincomestadd .25*[F3.lnMW + 2*F2.lnMW + 3*F1.lnMW + 4*(${lincomglobal})], statname(estimated_effect)
eststo tot_TWFE

reghdfe lnemp_TOT ${treat} lnpop if nonmissing_TOT ==66  , a(statenum quarterdate postcont postcontf  precont precontf earlycont earlycontf ) cluster(state_fips)
lincom (($lincomagg) * (1/5))/`mwpc'
lincomestadd (($lincomagg) * (1/5))/`mwpc', statname(estimated_effect)
eststo tot_EBD

reghdfe D.lnemp_TOT F(8/1).dlnMW L(0/16).dlnMW D.lnpop  if nonmissing_TOT==66  , a(countyreal period) cluster(state_fips)

lincom "${dlincomglobal}"
lincom .25*[F3.dlnMW + 2*F2.dlnMW + 3*F1.dlnMW + 4*(${dlincomglobal})]
lincomestadd .25*[F3.dlnMW + 2*F2.dlnMW + 3*F1.dlnMW + 4*(${dlincomglobal})], statname(estimated_effect)
eststo tot_FD


*********************** CONTIGUOUS COUNTIES *****************************************
drop _all 

use "${data}QCEWindustry_minwage_contig.dta", clear

**

egen pair_id_num = group(pair_id)
gen sample_5 = 1
gen absorb_5 = period
gen sample_6 = 1
egen absorb_6 = group(pair_id period)

sort  pair_id period
 
gen state_a = real(substr(pair_id, 1,2))
gen state_b = real(substr(pair_id, 7,2))

gen st_min = min( state_a, state_b)
gen st_max = max(state_a, state_b)
egen bordersegment = group(st_min st_max)
 

egen county_pair_id = group(countyreal pair_id)



global lincomglobal "lnMW"
forval j = 1/16 {
	global lincomglobal "${lincomglobal} + L`j'.lnMW"
}

 
global dlincomglobal "dlnMW"
forval j = 1/15 {
	global dlincomglobal "${dlincomglobal} + L`j'.dlnMW"
}

xtset pair_id_county period

g dlnMW = D.lnMW


*Add event-based stuff
*From other calculations (if needed)

global E = .5628805218130546
global C = 17.30151444236615

gen statenum = state_fips
gen quarterdate = yq(year, quarter)
merge m:1 statenum quarterdate using "${data}quarterly_state_panel.dta", keepusing(	F12treat F8treat F4treat treat  L4treat L8treat L12treat L16treat ///
																					postcont postcontf  precont precontf earlycont earlycontf  cleansample ///
																					DMW_real MW_real fedincrease overallcountgr avetotalpopulation totalpopulation)
global treat   		F12treat F8treat F4treat treat  L4treat L8treat L12treat L16treat 
*global minus 0
global minus (F4treat)
global lincomagg 	(treat-$minus)
forval i = 4(4)16{
global lincomagg "$lincomagg + (L`i'treat - $minus ) "
}
*
egen avetotalpopulation2 = mean(totalpopulation), by(statenum)

**** REST *****

xtset pair_id_county period


**** REST *****

 
reghdfe lnemp_rest_both F(8/1).lnMW L(0/16).lnMW lnpop  if nonmissing_rest_both==66   , a(countyreal absorb_6) cluster(state_fips bordersegment)

lincom "${lincomglobal}"
lincom .25*[F3.lnMW + 2*F2.lnMW + 3*F1.lnMW + 4*(${lincomglobal})]
lincomestadd .25*[F3.lnMW + 2*F2.lnMW + 3*F1.lnMW + 4*(${lincomglobal})], statname(estimated_effect)
eststo rest_BCP



				sum DMW_real if fedincrease!=1 & overallcountgr>0  & cleansample ==1  & year>=1990 & year<=2006 [aw=avetotalpopulation2] 
				local mwc = r(mean)
				sum MW_real if  F.fedincrease!=1 & F.overallcountgr>0 & cleansample ==1  & year>=1990 & year<=2006 [aw=avetotalpopulation2] 
				local mw = r(mean)
				local mwpc = `mwc'/`mw'


reghdfe lnemp_rest_both ${treat} lnpop  if nonmissing_rest_both==66   , a(countyreal absorb_6) cluster(state_fips bordersegment)

lincom (($lincomagg) * (1/5))/`mwpc'
lincomestadd (($lincomagg) * (1/5))/`mwpc', statname(estimated_effect)
eststo rest_EBD_BCP


				
			


*** TOT ***

g lnepop = lnemp_TOT - lnpop

reghdfe lnemp_TOT F(8/1).lnMW L(0/16).lnMW  lnpop     if nonmissing_TOT==66    , a(countyreal absorb_6) cluster(state_fips bordersegment)

lincom "${lincomglobal}"
lincom .25*[F3.lnMW + 2*F2.lnMW + 3*F1.lnMW + 4*(${lincomglobal})]
lincomestadd .25*[F3.lnMW + 2*F2.lnMW + 3*F1.lnMW + 4*(${lincomglobal})], statname(estimated_effect)
eststo tot_BCP 

reghdfe lnemp_TOT ${treat}  lnpop     if nonmissing_TOT==66    , a(countyreal absorb_6) cluster(state_fips bordersegment)
lincom (($lincomagg) * (1/5))/`mwpc'
lincomestadd (($lincomagg) * (1/5))/`mwpc', statname(estimated_effect)
eststo tot_EBD_BCP


esttab rest_TWFE rest_EBD rest_FD rest_BCP rest_EBD_BCP using "${tables}TableG8.tex", ///
replace stats(estimated_effectb estimated_effectse blankspace N , labels("Restaurant emp. elasticity" " " " " "Number of observations" )) ///
mgroups("TWFE-logMW" "Event-based" "First-differences" "BCP" "BCP \& EB", pattern(1 1 1 1 1) prefix(\multicolumn{1}{c}{) suffix(}) span end(\cmidrule(l{0.2cm}r{0.2cm}){1-1}\cmidrule(l{0.2cm}r{0.2cm}){2-2}\cmidrule(l{0.2cm}r{0.2cm}){3-3}\cmidrule(l{0.2cm}r{0.2cm}){4-4}) lhs(\hfil \bigcell{c}{Specification: }) )  ///
mlabels("(1)" "(2)" "(3)" "(4)", prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(l{0.2cm}r{0.2cm}){@span})) ///
booktabs fragment nonotes nolines nomtitles nonumbers nogaps compress cells(none) nodepvar


esttab tot_TWFE tot_EBD tot_FD tot_BCP tot_EBD_BCP using "${tables}TableG8.tex", ///
append stats(blankspace estimated_effectb estimated_effectse blankspace N , labels(" " "Overall emp. elasticity" " " " " "Number of observations")) ///
booktabs fragment nonotes nolines nomtitles nonumbers nogaps compress cells(none) nodepvar


