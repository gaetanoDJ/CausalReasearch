
clear

use "${data}qcew_multiplier.dta", clear
keep statenum quarterdate multiplier
assert multiplier!=0
tempfile qcew
save `qcew'




use "${data}VZmw_quarterly_lagsleads_1979_2016",clear
gen mw = exp(logmw)
gen year = year(dofq(quarterdate))
gen quarter = quarter(dofq(quarterdate))
collapse  mw, by(statenum year)


xtset statenum year
forvalues index = 1/5 {
	foreach lagorlead in L F {
		gen `lagorlead'`index'logmw = log(`lagorlead'`index'.mw)
	}
}
gen logmw = log(mw)
*
keep if year>=1979 & year<=2016
xtset statenum year
assert "`r(balanced)'" == "strongly balanced"
keep year statenum logmw L*mw F*mw

compress

tempfile mwdata
save `mwdata'

use "${data}state_panels_cents_balanced_add_QJE.dta", clear

		merge m:1 statenum quarterdate using `qcew', assert(3) nogenerate
		replace count = count * multiplier		
		replace countall = countall * multiplier
		
		

gen wagebindollar = floor(wagebin/100)
drop wagebins
rename wagebindollar wagebins

		************************************************************************
		**************          Ones that should be summed       ***************
		************************************************************************

		preserve 
			collapse (sum)  count, by(statenum quarterdate wagebins)
			tempfile tempsum
			save `tempsum'
		restore

		************************************************************************
		******        Ones that are already at statenumXquarterdate   **********
		************************************************************************
		
		preserve
			egen tagger=tag(statenum quarterdate)
			keep if tagger==1
			keep statenum quarterdate countall population year 
			tempfile temptag
			save `temptag'
		restore
		
		
		************************************************************************
		*********    Merge ave's and sum's      ********************************
		************************************************************************
		
		use `tempsum', clear

		reshape wide  count , i(statenum quarterdate) j(wagebin)
		merge 1:1 statenum quarterdate using `temptag', nogenerate assert(3)


		

********************************************************************************
******************       Replicating previous results     **********************
********************************************************************************


ds count*
collapse (mean) population `r(varlist)'  ,by(statenum year)

merge 1:1 statenum year using `mwdata'
rename population totalpopulation
g epop    = countall/totalpopulation

g cleansample = 1
replace cleansample = 0 if year >=1994 & year <=1995

egen avetotalpopulation = mean(totalpopulation) , by(statenum)

gen all=1
xtset statenum year


g mw = exp(logmw)

egen fedmw = min(mw), by(year)

g fedonly = mw == fedmw

g pre1995 = year<1995
 
g statemin = mw > fedmw if year>=1996
egen Statemin = max(statemin), by(statenum)
 
cap drop _merge
merge m:1 statenum using "${data}pvi_7616.dta"

g pvi = (pvi2000 + pvi2004 + pvi2008 + pvi2012 + pvi2016)/5 

cap drop pvi_preshock
g pvi_preshock = (pvi1980 + pvi1984 + pvi1988 )/3

cap drop _merge
merge m:1 statenum using "${data}state_ideologyshares.dta"

cap drop _merge


 
sort statenum year

merge 1:1 statenum year using "${data}unionization_state_year.dta" 
 
 
cap drop _merge
 
sort statenum year

merge 1:1 statenum year using "${data}state_demog.dta" 
 
 
foreach j in avewage  union  hslshare  {
	
	cap drop *`j'80s
	g _`j'80s = `j' if year <1990

	egen `j'80s = mean(_`j'80s), by(statenum) 
 
 }
 *
xtset statenum year


g Dem = pvi<0

  
cap drop *group*
egen pop_group = sum(totalpopulation), by(Statemin year)	



cap drop midwage* 
  
 
foreach j in epop logmw  { 
 
 	egen `j'pop_group = sum(`j'*totalpopulation), by(Statemin year) 
	g `j'_group =  `j'pop_group/pop_group
	egen `j'_group_unwtd = mean(`j') , by(statemin year)
	
	
	egen `j'_group_1 = mean(`j'_group*Statemin) , by(year)
	egen `j'_group_0 = mean(`j'_group*(1-Statemin)) , by(year)
	
	gen dif_`j'_group =  `j'_group_1-`j'_group_0
	
	

}

twoway line logmw_group  year if Statemin==0 & statenum ==1 , lcolor(red)  lwidth(thick) ||  ///
line logmw_group  year if Statemin==1 &  statenum==4 , lcolor(blue)  lwidth(thick) lpattern(dash) graphregion(color(white)) ///
xtitle("")  ytitle("log of Minimum Wage") legend(off)  name(g1, replace) xlabel(1980(4)2016) ylabel(1(0.25)2.25)

  
twoway line epop_group  year if Statemin==0 & statenum ==1  , lcolor(red) lwidth(thick) ||  ///
line epop_group  year if Statemin==1 &  statenum==4 , lcolor(blue)  lwidth(thick)   lpattern(dash) graphregion(color(white))  ///
xtitle(Year)  ytitle("Employment rate") legend(order(1 "States with only federal MW in 1996-2016" 2 "States with MW higher than the federal at least once in 1996-2016") rows(2)) name(g2, replace) xlabel(1980(4)2016) ylabel(0.5(0.05)0.65, format(%04.3f))
grc1leg g1 g2,  legendfrom(g2) graphregion(color(white)) col(1) name(g3, replace)
graph display g3, xsize(6) ysize(5)
 
gr export "${figures}FigureG4.pdf", replace



cap drop *shock*


foreach y in epop {

	g _shock91_`y' = S3.L24.`y' if year==2016
	egen shock91_`y' = mean(_shock91_`y'), by(statenum)
	
	
  }
*
  

 
reg  Statemin shock91_epop    if year==2016
reg  Statemin shock91_epop    if year==2016  [aw=totalpop], robust  
lincomestadd -shock91_epop    , statname(shock)
eststo e1

reg  Statemin union80s  avewage80s  hslshare80s if year==2016  [aw=totalpop], robust  
reg  Statemin shock91_epop     union80s  avewage80s  hslshare80s if year==2016  [aw=totalpop], robust  
lincomestadd -shock91_epop    , statname(shock)
lincomestadd union80s    , statname(union)
lincomestadd avewage80s   , statname(wage)  
lincomestadd hslshare80s  , statname(hslshare)
eststo e2

reg  Statemin shock91_epop pvi   if year==2016  [aw=totalpop], robust 
ivreg2  Statemin shock91_epop (pvi  = pvi1988) if year==2016  [aw=totalpop], robust ffirst first small
lincomestadd -shock91_epop    , statname(shock)
lincomestadd pvi    , statname(pvi)

local temp =  e(widstat)
local Fstat1: di %05.3f `temp'
di "`Fstat1'"
estadd local Fstat1 "`Fstat1'"
estadd local IVspec "Y"
eststo e3


esttab e1 e2 e3 using "${tables}TableG6.tex", replace ///
	 stats(shockb shockse blankspace unionb unionse blankspace wageb wagese blankspace hslshareb hslsharese blankspace pvib pvise blankspace IVspec Fstat1  N, label("Severity of the 1990s shock" " " " " "Unionization rate in the 1980s" " " " " "Average wage in the 1980s" " " " " "HSL share in the 1980s" " " " "  "PVI: Rep-Dem Vote Share in 2000s" " " " "   "IV specification" "First stage F-statistic" "Number of observations"  )) ///
	nodep addnotes("`footnotes'") nogaps cells(none) nomtitles fragment booktabs
 
