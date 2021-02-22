
*This do file is created by DC on 03/30/2016
********************************************************************************
********************************************************************************
********************************************************************************



 
foreach s in 0a 0 1 1b 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17{
	global rtype`s' reghdfe
}


global a1  i.wagebinstate i.wagequarterdate  // distributional TWFE

global label1 TWFE


		
*Consecutive events

use statenum quarterdate MW_realM25 wagebins wagebinstate overallcountgr fedincrease using "${data}state_panels_with3quant1979.dta", clear
xtset wagebinstate quarterdate
gen int MW_realM25_cents = MW_realM25 * 100

gen indic_event = (overallcountgr>0 & fedincrease!=1 & MW_realM25_cents == wagebins)

foreach num of numlist 1(1)19{
gen F`num'indic_event = F`num'.indic_event
replace F`num'indic_event =0 if F`num'indic_event ==.
}
*asd =  1.6521739
matrix multiplier  = J(17,20,0) 
sort statenum quarterdate wagebins
*
foreach num2 of numlist 1(1)8{
foreach num of numlist 1(1)19{
local row = 9 + `num2'
local col = `num'+1
sum F`num'indic_event if indic_event[_n-`num2']==1, meanonly
*scalar asd = asd + r(mean)
mat multiplier[`row', `col'] = (r(mean))
}
}
*
foreach num of numlist 1(1)19{
sum F`num'indic_event if indic_event==1, meanonly
local col = `num'+1
mat multiplier[9,  `col'] = (r(mean))
}
foreach num of numlist 1(1)19{
sum indic_event if indic_event==1, meanonly
mat multiplier[9, 1] = (r(mean))
}
*
foreach num2 of numlist 1(1)8{
foreach num of numlist 1(1)19{
sum F`num'indic_event if indic_event[_n+`num2']==1, meanonly
local row = 9 - `num2'  
local col = `num'+1
mat multiplier[`row',  `col'] = (r(mean))
}
}
*

mat multiplier = multiplier'
clear 
svmat multiplier
cap drop sorter
gen sorter = floor((_n-1)/4)
ds multiplier*
collapse (sum) `r(varlist)', by(sorter)
xpose, clear
drop if _n==1
foreach vv of numlist 1(1)5{
local vv2 = `vv' - 1
rename v`vv' event_year`vv2'
}
gen sorter = floor((_n-9)/4)
ds event*
collapse (sum) `r(varlist)', by(sorter)

save "${data}consecutiveevents_bin_by_year.dta", replace



*Globals for aggregation of events

use "${data}consecutiveevents_bin_by_year.dta", replace


*0th wage bin
global asd00  "_b[treat_p0]"
global asd04  "_b[L4treat_p0]"
global asd08  "_b[L8treat_p0]"
global asd012 "_b[L12treat_p0]"
global asd016 "_b[L16treat_p0]"
foreach num of numlist 0(1)4{
sum event_year`num' if sorter==0
local mult0`num' = r(mean)
}
*
local mult0 = `mult00'+`mult01'+`mult02'+`mult03'+`mult04'
local mult4 = `mult00'+`mult01'+`mult02'+`mult03'
local mult8 = `mult00'+`mult01'+`mult02'
local mult12 = `mult00'+`mult01'
local mult16 = `mult00'

foreach num of numlist 0(4)16{
global asd0`num' = "`mult`num''*${asd0`num'}"
}
*

global masd00  "_b[treat_m1]"
global masd04  "_b[L4treat_m1]"
global masd08  "_b[L8treat_m1]"
global masd012 "_b[L12treat_m1]"
global masd016 "_b[L16treat_m1]"

foreach num of numlist 0(1)4{
sum event_year`num' if sorter==1
local mult0`num' = r(mean)
}
*
local mult0 = `mult00'+`mult01'+`mult02'+`mult03'+`mult04'
local mult4 = `mult00'+`mult01'+`mult02'+`mult03'
local mult8 = `mult00'+`mult01'+`mult02'
local mult12 = `mult00'+`mult01'
local mult16 = `mult00'

foreach num of numlist 0(4)16{
global masd0`num' = "`mult`num''*${masd0`num'}"
}
*

global pasd00  "_b[treat_p1]"
global pasd04  "_b[L4treat_p1]"
global pasd08  "_b[L8treat_p1]"
global pasd012 "_b[L12treat_p1]"
global pasd016 "_b[L16treat_p1]"

foreach num of numlist 0(1)4{
sum event_year`num' if sorter==-1
local mult0`num' = r(mean)
}
*
local mult0 = `mult00'+`mult01'+`mult02'+`mult03'+`mult04'
local mult4 = `mult00'+`mult01'+`mult02'+`mult03'
local mult8 = `mult00'+`mult01'+`mult02'
local mult12 = `mult00'+`mult01'
local mult16 = `mult00'

foreach num of numlist 0(4)16{
global pasd0`num' = "`mult`num''*${pasd0`num'}"
}
*

global wbp0 "${asd00} + ${masd00} + ${pasd00}"

foreach num of numlist 4(4)16{
global wbp0 "$wbp0 + ${asd0`num'} + ${masd0`num'} + ${pasd0`num'}"

}
*



*-1 wage bin

global asd00  "_b[treat_m1]"
global asd04  "_b[L4treat_m1]"
global asd08  "_b[L8treat_m1]"
global asd012 "_b[L12treat_m1]"
global asd016 "_b[L16treat_m1]"
foreach num of numlist 0(1)4{
sum event_year`num' if sorter==0
local mult0`num' = r(mean)
}
*
local mult0 = `mult00'+`mult01'+`mult02'+`mult03'+`mult04'
local mult4 = `mult00'+`mult01'+`mult02'+`mult03'
local mult8 = `mult00'+`mult01'+`mult02'
local mult12 = `mult00'+`mult01'
local mult16 = `mult00'

foreach num of numlist 0(4)16{
global asd0`num' = "`mult`num''*${asd0`num'}"
}
*

global masd00  "_b[treat_m2]"
global masd04  "_b[L4treat_m2]"
global masd08  "_b[L8treat_m2]"
global masd012 "_b[L12treat_m2]"
global masd016 "_b[L16treat_m2]"

foreach num of numlist 0(1)4{
sum event_year`num' if sorter==1
local mult0`num' = r(mean)
}
*
local mult0 = `mult00'+`mult01'+`mult02'+`mult03'+`mult04'
local mult4 = `mult00'+`mult01'+`mult02'+`mult03'
local mult8 = `mult00'+`mult01'+`mult02'
local mult12 = `mult00'+`mult01'
local mult16 = `mult00'

foreach num of numlist 0(4)16{
global masd0`num' = "`mult`num''*${masd0`num'}"
}
*

global pasd00  "_b[treat_p0]"
global pasd04  "_b[L4treat_p0]"
global pasd08  "_b[L8treat_p0]"
global pasd012 "_b[L12treat_p0]"
global pasd016 "_b[L16treat_p0]"

foreach num of numlist 0(1)4{
sum event_year`num' if sorter==-1
local mult0`num' = r(mean)
}
*
local mult0 = `mult00'+`mult01'+`mult02'+`mult03'+`mult04'
local mult4 = `mult00'+`mult01'+`mult02'+`mult03'
local mult8 = `mult00'+`mult01'+`mult02'
local mult12 = `mult00'+`mult01'
local mult16 = `mult00'

foreach num of numlist 0(4)16{
global pasd0`num' = "`mult`num''*${pasd0`num'}"
}
*

global wbm1 "${asd00} + ${masd00} + ${pasd00}"

foreach num of numlist 4(4)16{
global wbm1 "${wbm1} + ${asd0`num'} + ${masd0`num'} + ${pasd0`num'}"

}
*



*-2 to -3 wage bins
*Note the minus becomes plus
foreach wagebinno of numlist 2(1)3{
local wagebinno_m = `wagebinno' + 1
local wagebinno_p = `wagebinno' - 1
global asd00  "_b[treat_m`wagebinno']"
global asd04  "_b[L4treat_m`wagebinno']"
global asd08  "_b[L8treat_m`wagebinno']"
global asd012 "_b[L12treat_m`wagebinno']"
global asd016 "_b[L16treat_m`wagebinno']"
foreach num of numlist 0(1)4{
sum event_year`num' if sorter==0
local mult0`num' = r(mean)
}
*
local mult0 = `mult00'+`mult01'+`mult02'+`mult03'+`mult04'
local mult4 = `mult00'+`mult01'+`mult02'+`mult03'
local mult8 = `mult00'+`mult01'+`mult02'
local mult12 = `mult00'+`mult01'
local mult16 = `mult00'

foreach num of numlist 0(4)16{
global asd0`num' = "`mult`num''*${asd0`num'}"
}
*

global masd00  "_b[treat_m`wagebinno_m']"
global masd04  "_b[L4treat_m`wagebinno_m']"
global masd08  "_b[L8treat_m`wagebinno_m']"
global masd012 "_b[L12treat_m`wagebinno_m']"
global masd016 "_b[L16treat_m`wagebinno_m']"

foreach num of numlist 0(1)4{
sum event_year`num' if sorter==1
local mult0`num' = r(mean)
}
*
local mult0 = `mult00'+`mult01'+`mult02'+`mult03'+`mult04'
local mult4 = `mult00'+`mult01'+`mult02'+`mult03'
local mult8 = `mult00'+`mult01'+`mult02'
local mult12 = `mult00'+`mult01'
local mult16 = `mult00'

foreach num of numlist 0(4)16{
global masd0`num' = "`mult`num''*${masd0`num'}"
}
*

global pasd00  "_b[treat_m`wagebinno_p']"
global pasd04  "_b[L4treat_m`wagebinno_p']"
global pasd08  "_b[L8treat_m`wagebinno_p']"
global pasd012 "_b[L12treat_m`wagebinno_p']"
global pasd016 "_b[L16treat_m`wagebinno_p']"

foreach num of numlist 0(1)4{
sum event_year`num' if sorter==-1
local mult0`num' = r(mean)
}
*
local mult0 = `mult00'+`mult01'+`mult02'+`mult03'+`mult04'
local mult4 = `mult00'+`mult01'+`mult02'+`mult03'
local mult8 = `mult00'+`mult01'+`mult02'
local mult12 = `mult00'+`mult01'
local mult16 = `mult00'

foreach num of numlist 0(4)16{
global pasd0`num' = "`mult`num''*${pasd0`num'}"
}
*

global wbm`wagebinno' "${asd00} + ${masd00} + ${pasd00}"

foreach num of numlist 4(4)16{
global wbm`wagebinno' "${wbm`wagebinno'} + ${asd0`num'} + ${masd0`num'} + ${pasd0`num'}"

}

}
*

*-4 wage bin
foreach wagebinno of numlist 4{
local wagebinno_p = `wagebinno' - 1
global asd00  "_b[treat_m`wagebinno']"
global asd04  "_b[L4treat_m`wagebinno']"
global asd08  "_b[L8treat_m`wagebinno']"
global asd012 "_b[L12treat_m`wagebinno']"
global asd016 "_b[L16treat_m`wagebinno']"
foreach num of numlist 0(1)4{
sum event_year`num' if sorter==0
local mult0`num' = r(mean)
}
*
local mult0 = `mult00'+`mult01'+`mult02'+`mult03'+`mult04'
local mult4 = `mult00'+`mult01'+`mult02'+`mult03'
local mult8 = `mult00'+`mult01'+`mult02'
local mult12 = `mult00'+`mult01'
local mult16 = `mult00'

foreach num of numlist 0(4)16{
global asd0`num' = "`mult`num''*${asd0`num'}"
}
*


global pasd00  "_b[treat_m`wagebinno_p']"
global pasd04  "_b[L4treat_m`wagebinno_p']"
global pasd08  "_b[L8treat_m`wagebinno_p']"
global pasd012 "_b[L12treat_m`wagebinno_p']"
global pasd016 "_b[L16treat_m`wagebinno_p']"

foreach num of numlist 0(1)4{
sum event_year`num' if sorter==-1
local mult0`num' = r(mean)
}
*
local mult0 = `mult00'+`mult01'+`mult02'+`mult03'+`mult04'
local mult4 = `mult00'+`mult01'+`mult02'+`mult03'
local mult8 = `mult00'+`mult01'+`mult02'
local mult12 = `mult00'+`mult01'
local mult16 = `mult00'

foreach num of numlist 0(4)16{
global pasd0`num' = "`mult`num''*${pasd0`num'}"
}
*

global wbm`wagebinno' "${asd00} + ${pasd00}"

foreach num of numlist 4(4)16{
global wbm`wagebinno' "${wbm`wagebinno'} + ${asd0`num'} + ${pasd0`num'}"

}

}
*








*+1 to +16 wage bins

foreach wagebinno of numlist 1(1)16{
local wagebinno_m = `wagebinno' - 1
local wagebinno_p = `wagebinno' + 1
global asd00  "_b[treat_p`wagebinno']"
global asd04  "_b[L4treat_p`wagebinno']"
global asd08  "_b[L8treat_p`wagebinno']"
global asd012 "_b[L12treat_p`wagebinno']"
global asd016 "_b[L16treat_p`wagebinno']"
foreach num of numlist 0(1)4{
sum event_year`num' if sorter==0
local mult0`num' = r(mean)
}
*
local mult0 = `mult00'+`mult01'+`mult02'+`mult03'+`mult04'
local mult4 = `mult00'+`mult01'+`mult02'+`mult03'
local mult8 = `mult00'+`mult01'+`mult02'
local mult12 = `mult00'+`mult01'
local mult16 = `mult00'

foreach num of numlist 0(4)16{
global asd0`num' = "`mult`num''*${asd0`num'}"
}
*

global masd00  "_b[treat_p`wagebinno_m']"
global masd04  "_b[L4treat_p`wagebinno_m']"
global masd08  "_b[L8treat_p`wagebinno_m']"
global masd012 "_b[L12treat_p`wagebinno_m']"
global masd016 "_b[L16treat_p`wagebinno_m']"

foreach num of numlist 0(1)4{
sum event_year`num' if sorter==1
local mult0`num' = r(mean)
}
*
local mult0 = `mult00'+`mult01'+`mult02'+`mult03'+`mult04'
local mult4 = `mult00'+`mult01'+`mult02'+`mult03'
local mult8 = `mult00'+`mult01'+`mult02'
local mult12 = `mult00'+`mult01'
local mult16 = `mult00'

foreach num of numlist 0(4)16{
global masd0`num' = "`mult`num''*${masd0`num'}"
}
*

global pasd00  "_b[treat_p`wagebinno_p']"
global pasd04  "_b[L4treat_p`wagebinno_p']"
global pasd08  "_b[L8treat_p`wagebinno_p']"
global pasd012 "_b[L12treat_p`wagebinno_p']"
global pasd016 "_b[L16treat_p`wagebinno_p']"

foreach num of numlist 0(1)4{
sum event_year`num' if sorter==-1
local mult0`num' = r(mean)
}
*
local mult0 = `mult00'+`mult01'+`mult02'+`mult03'+`mult04'
local mult4 = `mult00'+`mult01'+`mult02'+`mult03'
local mult8 = `mult00'+`mult01'+`mult02'
local mult12 = `mult00'+`mult01'
local mult16 = `mult00'

foreach num of numlist 0(4)16{
global pasd0`num' = "`mult`num''*${pasd0`num'}"
}
*
if `wagebinno'!=4 &  `wagebinno'!=5 &  `wagebinno'!=13 &  `wagebinno'!=14{  
global wbp`wagebinno' "${asd00} + ${masd00} + ${pasd00}"



foreach num of numlist 4(4)16{
global wbp`wagebinno' "${wbp`wagebinno'} + ${asd0`num'} + ${masd0`num'} + ${pasd0`num'}"

}
}
if `wagebinno'==4 |  `wagebinno'==13 {  
global wbp`wagebinno' "${asd00} + ${masd00} "

foreach num of numlist 4(4)16{
global wbp`wagebinno' "${wbp`wagebinno'} + ${asd0`num'} + ${masd0`num'} "
}
}

if `wagebinno'==5 |  `wagebinno'==14 {  
global wbp`wagebinno' "${asd00} + ${pasd00}"

foreach num of numlist 4(4)16{
global wbp`wagebinno' "${wbp`wagebinno'} + ${asd0`num'} + ${pasd0`num'}"
}
}


}
*



*+17 wage bins

local wagebinno 17
local wagebinno_m = `wagebinno' - 1
global asd00  "_b[treat_p`wagebinno']"
global asd04  "_b[L4treat_p`wagebinno']"
global asd08  "_b[L8treat_p`wagebinno']"
global asd012 "_b[L12treat_p`wagebinno']"
global asd016 "_b[L16treat_p`wagebinno']"
foreach num of numlist 0(1)4{
sum event_year`num' if sorter==0
local mult0`num' = r(mean)
}
*
local mult0 = `mult00'+`mult01'+`mult02'+`mult03'+`mult04'
local mult4 = `mult00'+`mult01'+`mult02'+`mult03'
local mult8 = `mult00'+`mult01'+`mult02'
local mult12 = `mult00'+`mult01'
local mult16 = `mult00'

foreach num of numlist 0(4)16{
global asd0`num' = "`mult`num''*${asd0`num'}"
}
*

global masd00  "_b[treat_p`wagebinno_m']"
global masd04  "_b[L4treat_p`wagebinno_m']"
global masd08  "_b[L8treat_p`wagebinno_m']"
global masd012 "_b[L12treat_p`wagebinno_m']"
global masd016 "_b[L16treat_p`wagebinno_m']"

foreach num of numlist 0(1)4{
sum event_year`num' if sorter==1
local mult0`num' = r(mean)
}
*
local mult0 = `mult00'+`mult01'+`mult02'+`mult03'+`mult04'
local mult4 = `mult00'+`mult01'+`mult02'+`mult03'
local mult8 = `mult00'+`mult01'+`mult02'
local mult12 = `mult00'+`mult01'
local mult16 = `mult00'

foreach num of numlist 0(4)16{
global masd0`num' = "`mult`num''*${masd0`num'}"
}
*


global wbp`wagebinno' "${asd00} + ${masd00} "

foreach num of numlist 4(4)16{
global wbp`wagebinno' "${wbp`wagebinno'} + ${asd0`num'} + ${masd0`num'}"

}
*




use "${data}qcew_state_overall_counts_2016.dta", clear
rename statefips statenum
gen quarterdate = yq(year, quarter)
tempfile qcew
save `qcew'

treatmentdefinition

merge m:1 statenum quarterdate using `qcew',assert(3) nogenerate
replace count = count*emp/countall if emp!=0
cap drop overallcountpc
gen overallcountpc=count/population 
replace overallcountpcall = emp/population  if emp!=0


placebosamplecorr2, wmax(16) prevwmax(15)

********************************************************************************
********************      Dropping unnecessary variables        ****************
********************************************************************************

cap drop HSD*
cap drop HSL*
cap drop black*
cap drop female*
cap drop BH*
cap drop teen*	


********************************************************************************
***********************           Weights           ****************************
********************************************************************************

cap drop wt 
foreach Y in   overall { 
	foreach b of numlist 1979{
					global weight`Y'`b' "[aw=wt`Y'`b']" 
					}
}  
*



********************************************************************************
**************     Sample and Control Specific Macros           ****************
********************************************************************************


local limittreat "_stateonly"


xtset wagebinstate quarterdate



********************************************************************************
**************      To name the graph              *****************************
********************************************************************************

if "${disagg}" == "Y"{
local temp1 "disaggcont"
}
else{
local temp1 "aggcont"
}

if "$controlmethod" == "Linear"{
local temp2 "L"
}
else{
if "$controlmethod" == "None"{
local temp1 "nocont"
}
else{
local temp2 "A"
}
}
cap drop one
cap drop counter
g one =1   	
gen counter = ((_n-1)*4) - $Tmin if _n<=8

foreach s in  0 1  4  5  6 10  11 12 {
	cap drop be_timeb
	cap drop be_timese
	cap drop be_timeupper
	cap drop be_timelower
	
	g be_timeb=.
	g be_timese=.
	g be_timeupper=.
	g be_timelower=.

	replace be_timeb=0 if counter==-4
	replace be_timese=0 if counter==-4
	replace be_timeupper=0 if counter==-4
	replace be_timelower=0 if counter==-4
	
	}
*


	
********************************************************************************
***************          Useful to Find Below Share Wages               ********
********************************************************************************


cap drop statequarterdate
egen statequarterdate = group(state quarterdate)
sort statequarterdate wagebins

cap drop overallWB*
g overallWBFH = wagebins*count/(100)
g overallWBpcFH = wagebins*count/(100*population)

cap drop overallWBpcrsumFH
g overallWBpcrsumFH=0
replace overallWBpcrsumFH = overallWBpcFH if wagebins==100
replace overallWBpcrsumFH = overallWBpcrsumFH[_n-1] + overallWBpcFH if wagebins>100
	
cap drop overallcountpcrsum
g overallcountpcrsum=0
replace overallcountpcrsum = overallcountpc if wagebins==100
replace overallcountpcrsum = overallcountpcrsum[_n-1] + overallcountpc if wagebins>100
	
	

******************************************************************************** 
**********************       REGRESSIONS AND FIGURES        ********************
********************************************************************************
xtset wagebinstate quarterdate
compress



foreach Y in   overall  { 
foreach b in    1979 {
					***  define MW change ***
				sum DMW_real if fedincrease!=1 & overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`Y'`b'}
				local mwc = r(mean)
				sum MW_real if  F.fedincrease!=1 & F.overallcountgr>0 & year>=`b' & cleansample ==1   ${weight`Y'`b'}
				local mw = r(mean)
				local mwpc = `mwc'/`mw'
				local mwpc2 : di  %04.3f `mwpc'

				sum `Y'countpcall if ((F.fedincrease != 1 & F.fedincrease != . & F.overallcountgr>0 & F.overallcountgr!=. ) | ///
				(F2.fedincrease != 1 & F2.overallcountgr>0 & F2.fedincrease != . & F2.overallcountgr!=. ) ///
				| (F3.fedincrease != 1 & F3.overallcountgr>0 & F3.fedincrease != . & F3.overallcountgr!=. ) ///
				| (F4.fedincrease != 1 & F4.overallcountgr>0 & F4.fedincrease != . & F4.overallcountgr!=. ) ) & year>=`b' & cleansample ==1 ///
				 ${weight`Y'`b'}		 // CHECK THIS  2/2/16
				
				local epop = r(mean)
				global E = `epop'
					
				global C = 1/(`epop'*`mwpc')
			
			
				*Below share

				
				sum overallcountpcrsum if ( F.fedincrease!=1 & F.overallcountgr>0 & F.fedincrease!=. & F.overallcountgr!=.  & (wagebins+25)/100==F1MW_realM25) | ///
				 ( F2.fedincrease!=1 & F2.overallcountgr>0 & F2.fedincrease!=. & F2.overallcountgr!=. & (wagebins+25)/100==F2MW_realM25 ) | ///
				( F3.fedincrease!=1 & F3.overallcountgr>0 & F3.fedincrease!=. & F3.overallcountgr!=.  & (wagebins+25)/100==F3MW_realM25) | ///
				 ( F4.fedincrease!=1 & F4.overallcountgr>0 & F4.fedincrease!=. & F4.overallcountgr!=. & (wagebins+25)/100==F4MW_realM25 ) ///
				& year>=`b' & cleansample ==1	   ${weight`Y'`b'}					
				
				global B = r(mean)/${E}
				local belsh2 : di  %04.3f $B

				*Affected share
				
				sum `Y'countpcrsum if (  F.fedincrease!=1 & F.overallcountgr>0 & F.fedincrease!=. & F.overallcountgr!=.  & (wagebins)/100==MW_realM25) | ///
				 ( F2.fedincrease!=1 & F2.overallcountgr>0 & F2.fedincrease!=. & F2.overallcountgr!=. & (wagebins)/100==MW_realM25 ) | ///
				( F3.fedincrease!=1 & F3.overallcountgr>0 & F3.fedincrease!=. & F3.overallcountgr!=.  & (wagebins)/100==MW_realM25) | ///
				 ( F4.fedincrease!=1 & F4.overallcountgr>0 & F4.fedincrease!=. & F4.overallcountgr!=. & (wagebins)/100==MW_realM25 ) ///
				& year>=1979 & cleansample ==1	  ${weight`Y'`b'}			
				
				global AS`Y' = $B - (r(mean)/${E})
				local affsh`Y' : di  %04.3f ${AS`Y'}
				
				
				****************************************************************
				*************    Wagebill related globals       ****************
				****************************************************************
				sum overallWBpcrsumFH if (  F.fedincrease!=1 & F.overallcountgr>0 & F.fedincrease!=. & F.overallcountgr!=.   & (wagebins+25)/100==F1MW_realM25) | ///
				 (F2.fedincrease!=1 & F2.overallcountgr>0 & F2.fedincrease!=. & F2.overallcountgr!=. & (wagebins+25)/100==F2MW_realM25 ) | ///
				(F3.fedincrease!=1 & F3.overallcountgr>0 & F3.fedincrease!=. & F3.overallcountgr!=.  & (wagebins+25)/100==F3MW_realM25) | ///
				 (  F4.fedincrease!=1 & F4.overallcountgr>0 & F4.fedincrease!=. & F4.overallcountgr!=.  & (wagebins+25)/100==F4MW_realM25 ) ///
				& year>=`b' & cleansample ==1	  ${weight`Y'`b'}			
	
				global EWB = r(mean)
				local EWB2 : di  %04.3f $EWB
				cap drop tempweight	
				global CWB = 1/(${EWB}*`mwpc')


				sum wagebins if treat_p0 == 1  & year>=`b' & cleansample ==1   ${weight`Y'`b'}, meanonly
				global wagemult = r(mean)/100
				
				*We have to do this here because multipliers are calculated in the loop.
				
				abovebelowWBbunch,wagemult(${wagemult})
				abovebelowWBfull
				
			
				foreach s in 1    {		
				est use  "${estimates}Table1aafterqcew`limitsample'`limittreat'_`s'_`Y'_`b'_`temp1'_`temp2'_${Tmax}${Tmin}"
 				local dof = e(df_r)
				local numberobs = e(N)
				count if fedincrease!=1 & overallcountgr>0 & wagebins==300		// The wage bin does not matter.
				local numberevent = r(N)
				
				
				local denominator = (1/(1+(${Tmax}/4)))

			
			
				****************************************************************
				**************     Figure 4      *******************************
				****************************************************************
				*Defining globals to get time averaged estimates. Need to define here.

				
				local tmax = 16
				preserve															// For some reason, sometimes svmat clears the data in memory. To prevent that, we preserve here.

				est use "${estimates}forplacebofig_true`limitsample'`limittreat'_`s'_`Y'_`b'_w1_`temp1'_`temp2'_${Tmax}${Tmin}"

				
				local wmax = 4
				local wmin = 4

				global PA_p0 "`denominator'*(${wbp0})*(4)*(1/${E})"

				forval j = 1/`wmax' {
					global PA_p`j' "`denominator'*(${wbp`j'})*(4)*(1/${E})" 
				}
				
				forval j = 1/`wmin' {				
					global PA_m`j' "`denominator'*(${wbm`j'})*(4)*(1/${E})"
				}

				
				global cumul "0"
				forval k = -`wmin'/`wmax' {	
					 di `k'
					if `k' < 0 {
						local j = -`k'
						local lincomline "${PA_m`j'}"
					}
					else if `k'==0 {
						local lincomline "${PA_p0}"
					}
					else if `k'>0 {
						local lincomline "${PA_p`k'}"
					}
					
 					qui lincom   "`lincomline'"
					local basic_b = r(estimate)
					local basic_se = r(se)
 					if `k' == -`wmin' {
						mat countmat = [ `k', `basic_b',  `basic_b'-1.96*`basic_se', `basic_b'+1.96*`basic_se' ]
					}
					else {
						mat countmat = [countmat \ [ `k', `basic_b',  `basic_b'-1.96*`basic_se', `basic_b'+1.96*`basic_se'  ]]
					}
				}
				

				
				****************************************************************
				*************         First placebo group         **************
				****************************************************************
				
				
				est use "${estimates}forplacebofig_pl1`limitsample'`limittreat'_`s'_`Y'_`b'_w1_`temp1'_`temp2'_${Tmax}${Tmin}"
				

				local wmin = 4 + 1
				local wmax = 13

				forval j = `wmin'/`wmax' {
					global PA_p`j' "`denominator'*(${wbp`j'})*(4)*(1/${E})" 
				}
				

				
				forval k = `wmin'/`wmax' {	
					 di `k'
						local lincomline "${PA_p`k'}"
					
 					qui lincom   "`lincomline'"
					local basic_b = r(estimate)
					local basic_se = r(se)
						mat countmat = [countmat \ [ `k', `basic_b',  `basic_b'-1.96*`basic_se', `basic_b'+1.96*`basic_se']]
				}
				

				****************************************************************
				************        Last placebo group         *****************
				****************************************************************
				
				est use "${estimates}forplacebofig_pl2`limitsample'`limittreat'_`s'_`Y'_`b'_w1_`temp1'_`temp2'_${Tmax}${Tmin}"

				local wmin = 13 + 1
				local wmax = 16 + 1
				

				forval j = `wmin'/`wmax' {
				if `j' != `wmax'{
					global PA_p`j' "`denominator'*(${wbp`j'})*(4)*(1/${E})" 
				}
				if `j' == `wmax'{
					sum sum_inf if overallcountgr>0 & overallcountgr!=. & fedincrease!=1   ${weight`Y'`b'}
					scalar numbins = r(mean)
					
					
					global PA_p`j' "`denominator'*(${wbp`j'})*(`=numbins')*(1/${E})" 
					}
				}
				

				
				global cumul "0"
				forval k = `wmin'/`wmax' {	
						local lincomline "${PA_p`k'}"
					
 					qui lincom   "`lincomline'"
					local basic_b = r(estimate)
					local basic_se = r(se)
						mat countmat = [countmat \ [ `k', `basic_b',  `basic_b'-1.96*`basic_se', `basic_b'+1.96*`basic_se']]
				}
				
				
								
			cap drop bin* 
			cap drop est* 
			cap drop low *
			cap drop high* 
			cap drop _est*
			cap drop totest*
			cap drop totlow
			cap drop tothigh
			cap drop countmat*
			
			svmat countmat
			rename countmat1 bin
			rename countmat2 est			
			rename countmat3 low
			rename countmat4 high
			
     			keep bin est low high
				gen wage = $wagemult
  				keep if bin!=.
				gen totest = sum(est)

				est use "${estimates}forplacebofig_true`limitsample'`limittreat'_`s'_`Y'_`b'_w1_`temp1'_`temp2'_${Tmax}${Tmin}"

				
				
*Globals to calculate estimates				
global below_full " ${wbm4} +  ${wbm3} +  ${wbm2} +  ${wbm1}"
global above_full " ${wbp0} +  ${wbp1} +  ${wbp2} +  ${wbp3} +  ${wbp4}"

global belowWB_full " (${wbm4})*(${wagemult}-4) +  (${wbm3})*(${wagemult}-3) +  (${wbm2})*(${wagemult}-2) +  (${wbm1})*(${wagemult}-1)"
global aboveWB_full " (${wbp0})*(${wagemult}) +  (${wbp1})*(${wagemult}+1) +  (${wbp2})*(${wagemult}+2) +  (${wbp3})*(${wagemult}+3) +  (${wbp4})*(${wagemult}+4)"



				
			lincomestadd (${below_full})*(4*`denominator')*(1/${E})							, statname(below_E) 				// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			gen below_b = r(estimate)
			gen below_se = r(se)
			lincomestadd (${above_full})*(4*`denominator')*(1/${E})							, statname(above_E)					// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5.
			gen above_b = r(estimate)
			gen above_se = r(se)
			lincomestadd ((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B} 	, statname(bunching_E)				// Multiplying by 4 because bins are in 25 cent precision. To average by time, dividing by 5. 			
			gen ch_aff_emp_b = r(estimate)
			gen ch_aff_emp_se = r(se)

			nlcom (a1:((((${belowWB_full}+${aboveWB_full})*(4*`denominator')/$EWB ) - ///
			(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B})))) ///
			(a2: (1+(((${below_full} + ${above_full})*(4*`denominator')*(1/${E}))/${B}))) , post
			
			nlcomestadd  (_b[a1]/_b[a2]) , statname(WB_E) dof(`dof') 			
			mat temp1 = r(b)
			local temp1 = temp1[1,1]
			mat temp2 = r(V)
			local temp2 = sqrt(temp2[1,1])
			
			gen ch_aff_wage_b = `temp1'
			gen ch_aff_wage_se = `temp2'
	
				save "${data}longer_fig4_agg.dta", replace
				
			restore
			}
			
			
		}
			
	}
*

use "${data}longer_fig4_agg.dta", clear
foreach vv in below_b below_se above_b above_se ch_aff_emp_b ch_aff_emp_se ch_aff_wage_b ch_aff_wage_se {
sum `vv', meanonly
assert r(min)==r(max)
local `vv': di %04.3fc r(mean) 
}
*

				twoway ( bar est bin  , fcolor(ltblue) lcolor(none) ) ///
				(rcap low  high bin  , lcolor(ltblue*1.25) lwidth(thick) )  ///
				(line totest bin, lcolor(red*.33) lpat(line) lwidth(thick) ) /* ///
				(line totlow  bin , color(red) lpat(dot)) ///
				(line tothigh  bin , color(red) lpat(dot))	*/			, ///
				/* xlabel(-4 "[-`=char(36)'4.25, -`=char(36)'3.25)" -2 "[-`=char(36)'2.25, -`=char(36)'1.25)" 0 "[-`=char(36)'0.25, `=char(36)'0.75)" ///
				 2 "[`=char(36)'1.75, `=char(36)'2.75)" 4 "[`=char(36)'3.75, `=char(36)'4.75)" , labsize(medsmall) ) */ ///
				  ///
				  xlabel(-4 "-4"  -2 "-2"  0 "0"  2 "2"  4 "4" 6 "6" 8 "8" 10 "10" 12 "12" 14 "14" 16 "16" 17 "17+" ) ///
				  ///
				ylabel(-0.03(0.01)-0.01 0 "0" 0.01(0.01)0.03 , format(%03.2f) labsize(medsmall)) ysc(range(-0.03 0.03)) scheme(s1color) /*xtitle("Wage Bin")*/ xtitle("Wage bins in `=char(36)' relative to new MW", size(medsmall)) xsc(titlegap(*10)) ///
				ytitle("Difference between actual and counterfactual employment count" "relative to the pre-treatment total employment", height(10) size(small))   ///
				text(0.0255 11.73  "{&Delta}a =  `above_b' (`above_se')" "{&Delta}b = `below_b' (`below_se')" "%{&Delta} affected employment =  `ch_aff_emp_b' (`ch_aff_emp_se')" ///	
				"%{&Delta} affected wage =  `ch_aff_wage_b' (`ch_aff_wage_se')", box  fcolor(white) margin(0.2 0.2 1 1) justification(right) ) ///
				/*title("${label`s'}", size(medium) )*/ legend(off) /*  ///
				note("${bunchest}" , size(small) color(blue)) */

gr export "${figures}FigureA6.pdf", replace





