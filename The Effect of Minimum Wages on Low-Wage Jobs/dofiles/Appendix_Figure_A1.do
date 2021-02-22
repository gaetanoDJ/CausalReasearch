
*This do file is created by DC on 04/20/2016.
********************************************************************************
***************     Depicting MW Events (new Definition)        ****************
********************************************************************************

use "${data}state_names.dta",clear
keep statenum stateabb
tempfile stateabb
save `stateabb', replace

global numberquant=3
use 						"${data}state_panels_cents_balanced_add_QJE.dta" , clear
g all=1

cap drop _merge
merge m:1 statenum quarterdate using "${data}VZmw_quarterly_lagsleads_1979_2016.dta"
 
g lnpop = ln(population)

 


*** SET PANEL VARS ***

egen wagebinstate = group(wagebins statenum)
xtset wagebinstate quarterdate   

egen wagequarterdate = group(wagebins quarterdate)


cap drop *MW* 

g MW = exp(logmw) 
g DMW = D.MW
g MW_real = exp(logmw)/(cpi/100) 
g DMW_real = D.MW_real 

g MW_realM25 = MW_real - mod(MW_real,0.25)
g DMW_realM25 = D.MW_realM25




********************************************************************************
****************        Find Quintiles by Below Share        *******************
********************************************************************************
*The main issue we have here is that we need to calculate the weighted average of 4 period shares. 

foreach num of numlist 1(1)4{
	gen F`num'MW_realM25 = F`num'.MW_realM25
}

cap drop tagger
egen tagger =tag(statenum quarterdate)




	****************************************************************************
	*******         Find treatments & Federal & State Increases      ***********
	****************************************************************************
	
	
	

cap drop _merge
merge m:1 statenum quarterdate using "${data}eventclassification.dta", nogenerate assert(1 3)
replace overallcountgroup = 0 if overallcountgroup==.
keep if tagger
assert overallcountgroup!=0 if alltreat==1 & toosmall==0

cap drop _merge
merge m:1 statenum using `stateabb'

xtset statenum quarterdate

gen stateonly 	= (overallcountgroup>0 & overallcountgroup!=.) & fedincrease!=1
gen statefed 	= (overallcountgroup>0 & overallcountgroup!=.) & stateonly==0
gen allothers	= ($missedevents | toosmall==1 ) 




gen date= quarterdate
labmask date,val(year)
*save "${data}temp.dta", replace

set more off
*use "${data}temp.dta", clear

cap drop stateonly
gen stateonly 			= (overallcountgroup>0 & overallcountgroup!=.) & fedincrease!=1


levelsof date if fedincrease == 1 & date ~= tq(1997q4), local(fedincreases)

local mlabgap mlabgap(-2.9)
local xlines xline(`fedincreases', lpattern(l) lcolor(gs14))


encode stateabb, gen(stateindex)

sum stateindex,meanonly
local stateindexmax = r(max)

gen fedmw = .
replace fedmw = 2.90 if date == tq(1979q1)
replace fedmw = 3.10 if date == tq(1980q1)
replace fedmw = 3.35 if date == tq(1981q1)
replace fedmw = 3.80 if date == tq(1990q2)
replace fedmw = 4.25 if date == tq(1991q2)
replace fedmw = 4.75 if date == tq(1996q4)
replace fedmw = 5.15 if date == tq(1997q3)
replace fedmw = 5.85 if date == tq(2007q3)
replace fedmw = 6.55 if date == tq(2008q3)
replace fedmw = 7.25 if date == tq(2009q3)


gen balancedpanel= 1


local stateindexplus = `stateindexmax' + 3
local fedmwlabels ""
foreach date of numlist `fedincreases' {
	sum fedmw if date == `date'
	local xpos = `date'
	if `date' == tq(1980q1){
	local xpos = `date' + 1
	}
	
	if `date' == tq(1980q1) | `date' == tq(1990q2) | `date' == tq(1996q4) | `date' == tq(2007q3) {
		local placement placement(c)
	}
	else if `date' == tq(1981q1) | `date' == tq(1991q2) | `date' == tq(1997q3) | `date' == tq(2009q3) {
		local placement placement(e)
	}
	else local placement placement(c)
	if `date' == tq(2007q3) local xpos = `date' - 1
	if `date' == tq(2008q3) local xpos = `date' + 0.75
	local fedmw: di %3.2f r(mean)
	local fedmwlabels `fedmwlabels' text(`stateindexplus' `xpos' `"$`fedmw'"', size(vsmall) color(gs9) `placement')
}
local postfedmw = tq(2009q3) + 5
local fedmwlabels `fedmwlabels' text(`stateindexplus' `postfedmw' "= Federal Minimum", size(vsmall) color(gs9) placement(e))

sum date,meanonly
local datemin = r(min)
local datemax = r(max)


local graphoptions yscale(off) ylabel(,nogrid) xlabel(`datemin'(12)220 227, valuelabel labsize(small)) ysize(4) xsize(6.5) graphregion(color(white) /*margin(t=3 b=0 r=3 l=2)*/) plotregion(color(white) margin(t=6)) ytitle("") xtitle("") legend(off) 

levelsof statenum	, local(temploc1)
levelsof quarterdate, local(temploc2)

gen stateonly_high = .
gen stateonly_low = .

foreach ll1 of local temploc1{
	foreach ll2 of local temploc2{
	sum stateonly if statenum==`ll1' & quarterdate == `ll2', meanonly
	if r(mean)>0{
	replace stateonly_high = `ll2' + 19 if statenum==`ll1' & quarterdate == `ll2'
	replace stateonly_low  = `ll2' - 12 if statenum==`ll1' & quarterdate == `ll2'
		}
	}
}
*

*The next part does not matter in regression, but matters in this figure.
gen belowshare_corr = belowshare*4/3 if alltreat==1 & year==1980
replace allothers = 1 if belowshare_corr<0.02 & belowshare_corr!=. &  alltreat==1 & year==1980 
replace allothers = 0 if belowshare_corr>=.02 & belowshare_corr!=. &  alltreat==1 & year==1980 
replace statefed = 1 if belowshare_corr>=0.02 & belowshare_corr!=. & alltreat==1 & year==1980




twoway (scatter stateindex date if stateindex>0 & stateindex!=. & stateonly==1 & balancedpanel==1 , msymbol(O) mlab(stateabb) mlabsize(2.75) mcolor(blue) mlabcolor(black) `mlabgap'  mlabsize(tiny) ) ///
(scatter stateindex date if stateindex>0 & stateindex!=. & statefed==1  & balancedpanel==1, msymbol(O) mlab(stateabb) mlabsize(2.75) mlabcolor(black) mcolor(green*0.2) `mlabgap'  mlabsize(tiny) ) ///
(scatter stateindex date if stateindex>0 & stateindex!=. & allothers==1  & balancedpanel==1, msymbol(T) mlab(stateabb) mlabsize(2.75) mlabcolor(black) mcolor(orange*0.3) `mlabgap' mlabsize(tiny)  `xlines' ) ///
(scatter stateindex date if stateindex>0 & stateindex!=. & (allothers==1 | statefed==1 | stateonly==1)  & balancedpanel==0, msymbol(O) mlab(stateabb) mlabsize(2.75) mlabcolor(black) mcolor(gray) `mlabgap' mlabsize(tiny)  `xlines' ) ///
, `graphoptions'   
graph export "${figures}FigureA1.pdf", replace
