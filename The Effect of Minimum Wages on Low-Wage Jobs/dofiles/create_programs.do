
*This do file is created by DC on 06/28/2016.
********************************************************************************
************      Program do file              *********************************
********************************************************************************
*This do file creates necessary programs to run do files. 
*INCOMPLETE! 
*Requires a readme file to explain what options can/need be specified and what is the default.
*The order of the programs is as follows:
*1) nlcomestadd
*2) lincomestadd
*3) treatmentcontrolwindows
*4) quantperswindows
*5) treatmentdefinition
*6) abovebelowbunch
*7) abovebelowfull
*8) abovebelowWBbunch (all three methods)
*9) abovebelowWBfull
*10) makefigure_massbywagebin
*11) makefigure_massovertime
*12) makefigure_chovertime														// This is employment related change (Figure 7).
*13) makefigure_chovertimeWB													// This is wage related change (Figure 6).
*14) abovebelowqpersbunch
*15) abovebelowqpersfull
*16) abovebelowWBqpersbunch
*17) abovebelowWBqpersfull
*18) correctaggcont
*19) placebowindows1
*20) placebowindows2
*NEW!!
*21) finertreatmentwindow
*22) finertreatmentdefinition
*23) finerabovebelowbunch
*24) finerabovebelowWBbunch
*25) data_reweight
********************************************************************************
*********************      Do files for Tables              ********************
********************************************************************************


capture program drop nlcomestadd
program define nlcomestadd
syntax anything, statname(name) dof(real)

nlcom `anything' 
mat temp1 = r(b)
scalar temp1 = temp1[1,1]
mat temp2 = r(V)
scalar temp2 = sqrt(temp2[1,1])
local pvalue = 2*ttail(`dof', abs((temp1)/temp2))
assert `pvalue' ~= .
local b : di  %04.3f `=temp1'
local se : di %04.3f `=temp2'
scalar drop temp1 temp2

local stars ""
if `pvalue' < 0.10 local stars *
if `pvalue' < 0.05 local stars **
if `pvalue' < 0.01 local stars ***
local bstring `b'`stars'
local sestring (`se')

estadd local `statname'b "`bstring'"
estadd local `statname'se "`sestring'"

end


********************************************************************************
********************************************************************************
********************************************************************************

capture program drop lincomestadd
program define lincomestadd
syntax anything, statname(name)

lincom `anything'
local pvalue = 2*ttail(r(df), abs(r(estimate)/r(se)))
assert `pvalue' ~= .

local b : di  %04.3f `r(estimate)'
local se : di %04.3f `r(se)'

local stars ""
if `pvalue' < 0.10 local stars *
if `pvalue' < 0.05 local stars **
if `pvalue' < 0.01 local stars ***
local bstring `b'`stars'
local sestring (`se')

estadd local `statname'b "`bstring'"
estadd local `statname'se "`sestring'"

end


********************************************************************************
************     Do file to define treatment globals         *******************
********************************************************************************

cap program drop treatmentcontrolwindows
program define treatmentcontrolwindows
syntax  [, Tmax(real 16) Tmin(real 12) Wmax(real 4) Wmin(real 4)  CONTROLMethod(string) CONTLead(string) CONTROLFmethod(string) Disagg(string)]  

			********************************************************************
			********		Defining defaults for strings			 ***********
			********************************************************************

			if "`disagg'" == "" local disagg "N"
			if "`controlmethod'" == "" local controlmethod "Absorb"
			if "`contlead'" == "" local contlead "Y"
			


			********************************************************************
			***********************         Treatment Vector           *********
			********************************************************************

 global treatafter ""
 global treatbefore ""

forval k = 0(4)`tmax'   {
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmin'(1)1       {
		global treatafter = "$treatafter `K'treat_m`j'"
	}
	foreach j of numlist  0(1)`wmax' {
		global treatafter = "$treatafter `K'treat_p`j'"
	}
	}
* 
forval k = -`tmin'(4)-8   {
if `k'!=-4{
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmin'(1)1       {
		global treatbefore = "$treatbefore `K'treat_m`j'"
	}
	foreach j of numlist  0(1)`wmax' {
		global treatbefore = "$treatbefore `K'treat_p`j'"
	}
	}
	
}
* 
 
global controlbefore " "
global controlafter " " 

forval k = 0(4)`tmax'   {
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmin'(1)1       {
		global controlafter = "$controlafter i.one#c.`K'treat_m`j'"
	}
	foreach j of numlist  0(1)`wmax' {
		global controlafter = "$controlafter i.one#c.`K'treat_p`j'"
	}
	}
* 
forval k = -`tmin'(4)-8   {
if `k'!=-4{
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmin'(1)1       {
		global controlbefore = "$controlbefore i.one#c.`K'treat_m`j'"
	}
	foreach j of numlist  0(1)`wmax' {
		global controlbefore = "$controlbefore i.one#c.`K'treat_p`j'"
	}
	}
	
}
*
********************************************************************************
*****************         Control Vector         *******************************
********************************************************************************
*Depends on what is specified in the master do file.


cap drop one
g one =1  

global control = " "

if "`contlead'"=="Nolead"{
if "`disagg'" != "Y" {
	if "`controlmethod'" == "Linear"{

		global control = "${control} i.one#c.postcont_m   "
		global control = "${control} i.one#c.postcont_p  "

		}
	else{

		global control = "${control} postcont_m  "
		global control = "${control} postcont_p  "
		}
	}
else{
	if "`controlmethod'" == "Linear"{

	 forval k = 0(4)`tmax'   {
 
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmin'(1)1       {
		global control = "$control i.one#c.`K'cont_m`j' "
	}
	foreach j of numlist  0(1)`wmax' {
		global control = "$control i.one#c.`K'cont_p`j' "
	}
	
}

		} 
	else{
	 forval k = 0(4)`tmax'   {
 
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmin'(1)1       {
		global control = "$control `K'cont_m`j' "
	}
	foreach j of numlist  0(1)`wmax' {
		global control = "$control `K'cont_p`j' "
	}
	
}

		}
	}

}
else{
if "`disagg'" != "Y" {
	if "`controlmethod'" == "Linear"{

		global control = "${control} i.one#c.postcont_m  i.one#c.precont_m i.one#c.earlycont_m "
		global control = "${control} i.one#c.postcont_p  i.one#c.precont_p  i.one#c.earlycont_p "

		}
	else{

		global control = "${control} postcont_m  precont_m  earlycont_m "
		global control = "${control} postcont_p  precont_p  earlycont_p "
		}
	}
else{
	if "`controlmethod'" == "Linear"{

	 forval k = -`tmin'(4)`tmax'   {
 
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmin'(1)1       {
		global control = "$control i.one#c.`K'cont_m`j' "
	}
	foreach j of numlist  0(1)`wmax' {
		global control = "$control i.one#c.`K'cont_p`j' "
	}
	
}

		} 
	else{
	 forval k = -`tmin'(4)`tmax'   {
 
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmin'(1)1       {
		global control = "$control `K'cont_m`j' "
	}
	foreach j of numlist  0(1)`wmax' {
		global control = "$control `K'cont_p`j' "
	}
	
}

		}
	}
}
*

if "`controlmethod'" == "None"{
global control = " "
}
*

	
	****************************************************************************
	***************      Define Federal Controls         ***********************
	****************************************************************************

global controlf " "	
		if "`contlead'"=="Nolead"{
			global controlf = "${controlf} postcontf_m  "
			global controlf = "${controlf} postcontf_p  "
		}
		else{
			global controlf = "${controlf} postcontf_m  precontf_m  earlycontf_m "
			global controlf = "${controlf} postcontf_p  precontf_p  earlycontf_p "
		}
		
		if "`controlfmethod'" == "None"{
			global controlf = " "
		}
*

********************************************************************************
*****************        Window         ****************************************
********************************************************************************

global window " "
	
	foreach j of numlist `wmin'(1)1       {
		global window = "$window i.one#c.window_m`j' "
	}
	foreach j of numlist  0(1)`wmax' {
		global window = "$window i.one#c.window_p`j' "
	}



end





********************************************************************************
**********       Program for Quantile and persistent Events            *********
********************************************************************************

cap program drop quantperswindows
program define quantperswindows
syntax  [, Tmax(real 16) Tmin(real 12) Wmax(real 4) Wmin(real 4)  Controlpersmethod(string) Controlqmethod(string) Disaggpers(string) Disaggq(string)] 


			********************************************************************
			********		Defining defaults for strings			 ***********
			********************************************************************

			if "`disaggpers'" == "" local disaggpers "Y"
			if "`disaggq'" == "" local disaggq "Y"			
			if "`contlead'" == "" local contlead "Y"
			if "`controlpersmethod'" == "" local controlpersmethod "Linear"
			if "`controlqmethod'" == "" local controlqmethod "Absorb"

			
			
			


 global treatpersafter ""
 global treatpersbefore ""
 global treatqafter ""
 global treatqbefore ""
 
forval k = 0(4)`tmax'   {
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	
	foreach j of numlist `wmin'(1)1       {
		global treatqafter = "$treatqafter `K'treatq_m`j'"
	}
	foreach j of numlist  0(1)`wmax' {
		global treatqafter = "$treatqafter `K'treatq_p`j'"
	}
	foreach j of numlist `wmin'(1)1       {
		global treatpersafter = "$treatpersafter `K'treatpers_m`j'"
	}
	foreach j of numlist  0(1)`wmax' {
		global treatpersafter = "$treatpersafter `K'treatpers_p`j'"
	}
	}
* 
forval k = -`tmin'(4)-8   {
if `k'!=-4{
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmin'(1)1       {
		global treatqbefore = "$treatqbefore `K'treatq_m`j'"
	}
	foreach j of numlist  0(1)`wmax' {
		global treatqbefore = "$treatqbefore `K'treatq_p`j'"
	}
	foreach j of numlist `wmin'(1)1       {
		global treatpersbefore = "$treatpersbefore `K'treatpers_m`j'"
	}
	foreach j of numlist  0(1)`wmax' {
		global treatpersbefore = "$treatpersbefore `K'treatpers_p`j'"
	}

	}
	
}
*

	****************************************************************************
	*************      Control Treatments            ***************************
	****************************************************************************

	
	
global controlqbefore " "
global controlqafter " " 
global controlpersbefore " "
global controlpersafter " " 

forval k = 0(4)`tmax'   {
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmin'(1)1       {
		global controlqafter = "$controlqafter i.one#c.`K'treatq_m`j'"
	}
	foreach j of numlist  0(1)`wmax' {
		global controlqafter = "$controlqafter i.one#c.`K'treatq_p`j'"
	}
	foreach j of numlist `wmin'(1)1       {
		global controlpersafter = "$controlpersafter i.one#c.`K'treatpers_m`j'"
	}
	foreach j of numlist  0(1)`wmax' {
		global controlpersafter = "$controlpersafter i.one#c.`K'treatpers_p`j'"
	}
	}
* 
forval k = -`tmin'(4)-8   {
if `k'!=-4{
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmin'(1)1       {
		global controlqbefore = "$controlqbefore i.one#c.`K'treatq_m`j'"
	}
	foreach j of numlist  0(1)`wmax' {
		global controlqbefore = "$controlqbefore i.one#c.`K'treatq_p`j'"
	}
	foreach j of numlist `wmin'(1)1       {
		global controlpersbefore = "$controlpersbefore i.one#c.`K'treatpers_m`j'"
	}
	foreach j of numlist  0(1)`wmax' {
		global controlpersbefore = "$controlpersbefore i.one#c.`K'treatpers_p`j'"
	}
	}
	
}
*	

********************************************************************************
***************    Event specific controls           ***************************
********************************************************************************




global controlpers ""
	
	if "`controlpersmethod'" != "Absorb"{

	 forval k = -`tmin'(4)`tmax'   {
 
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmin'(1)1       {
		global controlpers = "$controlpers i.one#c.`K'contpers_m`j' "
	}
	foreach j of numlist  0(1)`wmax' {
		global controlpers = "$controlpers i.one#c.`K'contpers_p`j' "
	}
	
}

		} 
if "`controlpersmethod'" == "Absorb"{
		if "`disaggpers'"!="Y"{

		global controlpers = "$controlpers earlycontpers_m precontpers_m  postcontpers_m"
		global controlpers = "$controlpers earlycontpers_p precontpers_p  postcontpers_p"
		}
		else{

		forval k = -`tmin'(4)`tmax'   {
 
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmin'(1)1       {
		global controlpers = "$controlpers `K'contpers_m`j' "
	}
	foreach j of numlist  0(1)`wmax' {
		global controlpers = "$controlpers `K'contpers_p`j' "
	}
	
}
		
		
		}
}
*

global controlq " "

if "`disaggq'"=="Y"{
if "`controlqmethod'" == "Absorb"{
	 forval k = -`tmin'(4)`tmax'   {
 
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmin'(1)1       {
		global controlq = "$controlq `K'contq_m`j' "
	}
	foreach j of numlist  0(1)`wmax' {
		global controlq = "$controlq `K'contq_p`j' "
	}
	
	}
}
else{
	 forval k = -`tmin'(4)`tmax'   {
 
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmin'(1)1       {
		global controlq = "$controlq i.one#c.`K'contq_m`j' "
	}
	foreach j of numlist  0(1)`wmax' {
		global controlq = "$controlq i.one#c.`K'contq_p`j' "
	}
	
}


}
}
else{


		global controlq = "$controlq earlycontq_m precontq_m  postcontq_m"
		global controlq = "$controlq earlycontq_p precontq_p  postcontq_p"


}
*

end


********************************************************************************
***********     Define Table 1 treatments other than WD			****************
********************************************************************************
*Also calls the data
cap program drop treatmentdefinition
program define treatmentdefinition
syntax  [, Federal(string) Small(string) Restriction(string)]



if "`federal'"=="" & "`small'"=="" & "`restriction'" == ""{
di "You are using the primary working data."
use "${data}state_panels_with3quant1979.dta",clear
}
********************************************************************************
if  "`restriction'" == "hourly"{
di "You are using the primary treatments and limiting the sample to the hourly earners."
use "${data}state_panels_hourly_with3quant1979.dta",clear
}
********************************************************************************
if  "`restriction'" == "nontip"{
di "You are using the primary treatments and limiting the sample to the non-tipped workers."
use "${data}state_panels_notip_with3quant1979.dta",clear
}
********************************************************************************

if  "`restriction'" == "hourlynontip"{
di "You are using the primary treatments and limiting the sample to the non-tipped workers."
use "${data}state_panels_hourly_notip_with3quant1979.dta",clear
}
********************************************************************************

if "`federal'"=="Y" & "`small'"==""  & "`restriction'" == ""{
di "You are using the working data with federal MW changes counted as events."
use "${data}state_panels_with3quant1979_statefed.dta",clear
global controlf " "
}
********************************************************************************
if "`federal'"=="Y" & "`small'"=="Y"  & "`restriction'" == ""{
di "You are counting all MW increases as events."

use "${data}state_panels_with3quant1979.dta",clear
local treatdef1 "D.MW_real> 0  & D.MW_real<.& D.MW>0"							// Every real increase in MW is considered as treatment.
global controlf " "
global control " "


forval k = 0/5  {
	cap drop treat_p`k' 
	cap drop _treat_p`k'
	g _treat_p`k'=0
	* replace _treat_p`k' = 1 if (  D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))	
	*	replace _treat_p`k' = 1 if (D.MW_real > 0  & D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))
		replace _treat_p`k' = 1 if `treatdef1' &  (wageM25>=(MW_realM25+`k' - 0.25) &  wageM25 < (MW_realM25 +1 +`k' - 0.25)) 
	*	replace _treat_p`k' = 1 if (D.MW_real>=0.75  & D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))
	 g treat_p`k' = ( _treat_p`k' +  L._treat_p`k' + L2._treat_p`k' + L3._treat_p`k')
	cap drop _treat_p`k'

}

forval k = 1/5  {
	cap drop treat_m`k'
	cap drop _treat_m`k'
	g _treat_m`k'=0
	*	replace _treat_m`k' = 1 if ( D.MW_real<.& D.MW>0) & (wageM25<(MW_realM25 - `k' + 1)  &  wageM25 >= (MW_realM25 -`k' ))
	*	replace _treat_m`k' = 1 if(D.MW_real> 0  & D.MW_real<.& D.MW>0) & (wageM25<(MW_realM25 - `k' + 1)  &  wageM25 >= (MW_realM25 -`k' ))
		replace _treat_m`k' = 1 if `treatdef1' & (wageM25<(MW_realM25 - `k' + 1 -0.25)  &  wageM25 >= (MW_realM25 -`k' -0.25 )) 
	*	replace _treat_m`k' = 1 if(D.MW_real>=0.75  & D.MW_real<.& D.MW>0) & (wageM25<(MW_realM25 - `k' + 1)  &  wageM25 >= (MW_realM25 -`k' ))
	g treat_m`k' = ( _treat_m`k' + L._treat_m`k' + L2._treat_m`k' + L3._treat_m`k' )
cap drop _treat_m`k'
}  
*

 cap drop  Dtreat_p0
 g Dtreat_p0 = D.treat_p0 

*Leads and Lags
 
foreach j in  4 8 12 16 {
	foreach k in m5 m4 m3 m2 m1 p0 p1 p2 p3 p4 p5 {
		cap drop F`j'treat_`k'
		cap drop L`j'treat_`k'
		
		g F`j'treat_`k' = F`j'.treat_`k'
 		g L`j'treat_`k' = L`j'.treat_`k'

	}
} 

cap drop window_*
foreach pm in p m{
	foreach num of numlist 1(1)5{
		gen window_`pm'`num' =  F12treat_`pm'`num' +  F8treat_`pm'`num' +  F4treat_`pm'`num' + treat_`pm'`num' + L4treat_`pm'`num'  + L8treat_`pm'`num'  + L12treat_`pm'`num'  + L16treat_`pm'`num'  
	}
}

local pm p
local num 0
gen window_`pm'`num' =  F12treat_`pm'`num' +  F8treat_`pm'`num' +  F4treat_`pm'`num' + treat_`pm'`num' + L4treat_`pm'`num'  + L8treat_`pm'`num'  + L12treat_`pm'`num'  + L16treat_`pm'`num'  


}
*

xtset wagebinstate quarterdate



end

********************************************************************************
*****************    Program defining above and below masses          **********
********************************************************************************


cap program drop abovebelowbunch
program define abovebelowbunch
syntax [,Tmin(real 12) Tmax(real 16) Wmax(real 4) Wmin(real 4)]

foreach t of numlist -`tmin'(4)`tmax' {

	if `t' < -4  {	
		local nt = -`t' 
		global aboveF`nt' "(_b[F`nt'treat_p0] )"
		forval j = 1/`wmax' {
			global aboveF`nt' "${aboveF`nt'} + (_b[F`nt'treat_p`j'] )"
		}
		global belowF`nt' "(_b[F`nt'treat_m1] )"
		forval j = 2/`wmin' {
			global belowF`nt' "${belowF`nt'} + (_b[F`nt'treat_m`j'] )"
		}

    }
 
	if `t' == -4 {
		global aboveF4 "0"
		global belowF4 "0"
	}
 
	if `t' == 0 {	
		 
		global aboveL`t' "(_b[treat_p0])"
		forval j = 1/`wmax' {
			global aboveL`t' "${aboveL`t'} + (_b[treat_p`j'] )"
		}
		global belowL`t' "(_b[treat_m1] )"
		forval j = 2/`wmin' {
			global belowL`t' "${belowL`t'} + (_b[treat_m`j'] )"
		}

    }
	
   if `t' > 0 {	
		 
		global aboveL`t' "(_b[L`t'treat_p0])"
		forval j = 1/`wmax' {
			global aboveL`t' "${aboveL`t'} + (_b[L`t'treat_p`j'])"
		}
		global belowL`t' "(_b[L`t'treat_m1])"
		forval j = 2/`wmin' {
			global belowL`t' "${belowL`t'} + (_b[L`t'treat_m`j'])"
		}

    }
	
 
 } 
 *
end


cap program drop abovebelowfull
program define abovebelowfull
syntax [, Tmax(real 16)]
di "Make sure that belowL`tmax' and aboveL`tmax' exists. Otherwise the program does not work. Assertion will be false."
assert  "${belowL`tmax'}"!=""
assert  "${aboveL`tmax'}"!=""
 
global above_full "${aboveL0}"
global below_full "${belowL0}"

forval t = 4(4)`tmax' {
			
				global below_full "${below_full} + ${belowL`t'}"
				global above_full "${above_full} + ${aboveL`t'}"
 }

 end



********************************************************************************
**********     Wagebill related globals          *******************************
********************************************************************************

cap program drop abovebelowWBbunch
program define abovebelowWBbunch
syntax , Wagemult(real) [Tmin(real 12) Tmax(real 16)  Wmax(real 4) Wmin(real 4)]




				foreach t of numlist -`tmin'(4)`tmax' {
				
						if `t' < -4  {	
							local nt = -`t' 
							global aboveWBF`nt' "((_b[F`nt'treat_p0])*(`wagemult'+0) )"
						forval j = 1/`wmax' {
							global aboveWBF`nt' "${aboveWBF`nt'} + ((_b[F`nt'treat_p`j'])*(`wagemult'+`j') )"
						}
						global belowWBF`nt' "((_b[F`nt'treat_m1])*(`wagemult'-1) )"
						forval j = 2/`wmin' {
							global belowWBF`nt' "${belowWBF`nt'} + ((_b[F`nt'treat_m`j'])*(`wagemult'-`j') )"
						}

					}

				

					if `t' == 0 {	
		 
						global aboveWBL`t' "((_b[treat_p0])*(`wagemult'+0))"
						forval j = 1/`wmax' {
							global aboveWBL`t' "${aboveWBL`t'} + ((_b[treat_p`j'])*(`wagemult'+`j') )"
						}
						global belowWBL`t' "((_b[treat_m1])*(`wagemult'-1) )"
						forval j = 2/`wmin'{
							global belowWBL`t' "${belowWBL`t'} + ((_b[treat_m`j'])*(`wagemult'-`j') )"
						}

					}
	
					if `t' > 0 {	
		 
						global aboveWBL`t' "((_b[L`t'treat_p0])*(`wagemult'+0))"
						forval j = 1/`wmax' {
							global aboveWBL`t' "${aboveWBL`t'} + ((_b[L`t'treat_p`j'])*(`wagemult'+`j'))"
						}
						global belowWBL`t' "((_b[L`t'treat_m1])*(`wagemult'-1) )"
						forval j = 2/`wmin' {
							global belowWBL`t' "${belowWBL`t'} + ((_b[L`t'treat_m`j'])*(`wagemult'-`j'))"
						}

					}
	
 
			} 
 			global aboveWBF4 = 0
 			global belowWBF4 = 0
			
			
*
end


********************************************************************************
********************************************************************************
********************************************************************************


cap program drop abovebelowWBfull
program define abovebelowWBfull
syntax [, Tmax(real 16)]
di "Make sure that belowL`tmax' and aboveL`tmax' exists. Otherwise the program does not work. Assertion will be false."
assert  "${belowWBL`tmax'}"!=""
assert  "${aboveWBL`tmax'}"!=""
 
			global aboveWB_full "${aboveWBL0}"
			global belowWB_full "${belowWBL0}"

forval t = 4(4)`tmax' {
			
				global belowWB_full "${belowWB_full} + ${belowWBL`t'}"
				global aboveWB_full "${aboveWB_full} + ${aboveWBL`t'}"
			}

 end
 

 
 
********************************************************************************
*****************           Alternative Wage bill                      *********
********************************************************************************

cap program drop abovebelowWBbunch_alt
program define abovebelowWBbunch_alt
syntax ,  [Tmax(real 16) SECONDmethod(string) ]







			foreach t of numlist -12(4)`tmax' {

			if `t'>=-12 & `t'<-4{
			local nt = -`t'
				foreach nn2 of numlist 0(1)4{
				
				if "`secondmethod'" != "Y"{
				global coefficient`nn2'  "-(_b[F`nt'treat_p`nn2'])/(_b[F`nt'treat_p0] + _b[F`nt'treat_p1] + _b[F`nt'treat_p2] + _b[F`nt'treat_p3] + _b[F`nt'treat_p4])"
				}
				
				if "`secondmethod'" == "Y"{
				if `nn2'==0{
				global coefficient`nn2'  "-1"
				}
				else{
				global coefficient`nn2'  "0"				
				}
				}
				global aboveWBF`nt'_`nn2'`secondmethod' "0"
				foreach nn of numlist 1(1)4{
						local nn3 = `nn'+`nn2'
						global aboveWBF`nt'_`nn2'`secondmethod' "  ${aboveWBF`nt'_`nn2'`secondmethod'} + (`nn3'*_b[F`nt'treat_m`nn'])"
					}
					global aboveWBF`nt'_`nn2'`secondmethod' " ( ${aboveWBF`nt'_`nn2'`secondmethod'})* (${coefficient`nn2'})"
				}

			}
			
			
			
			if `t'==0{
				foreach nn2 of numlist 0(1)4{
				
				if "`secondmethod'" != "Y"{
				global coefficient`nn2'  "-(_b[treat_p`nn2'])/(_b[treat_p0] + _b[treat_p1] + _b[treat_p2] + _b[treat_p3] + _b[treat_p4])"
				}
				
				if "`secondmethod'" == "Y"{
				if `nn2'==0{
				global coefficient`nn2'  "-1"
				}
				else{
				global coefficient`nn2'  "0"				
				}
				}
				global aboveWBL`t'_`nn2'`secondmethod' "0"
				foreach nn of numlist 1(1)4{
						local nn3 = `nn'+`nn2'
						global aboveWBL`t'_`nn2'`secondmethod' "  ${aboveWBL`t'_`nn2'`secondmethod'} + (`nn3'*_b[treat_m`nn'])"
					}
					global aboveWBL`t'_`nn2'`secondmethod' " ( ${aboveWBL`t'_`nn2'`secondmethod'})* (${coefficient`nn2'})"
				}


			}
			
			if `t'>0{
				global aboveWBL`t'`secondmethod' " 0 "
				foreach nn2 of numlist 0(1)4{
				/*
				if "`secondmethod'" != "Y" & `emploss' >= 0 {
				global coefficient`nn2'  "-(_b[L`t'treat_p`nn2'])/(_b[L`t'treat_p0] + _b[L`t'treat_p1] + _b[L`t'treat_p2] + _b[L`t'treat_p3] + _b[L`t'treat_p4])"
				}
				*if "`secondmethod'" != "Y" & `emploss' < 0 {
				if "`secondmethod'" != "Y" {
				global coefficient`nn2'  "-((_b[L`t'treat_p`nn2'])/(_b[L`t'treat_p0] + _b[L`t'treat_p1] + _b[L`t'treat_p2] + _b[L`t'treat_p3] + _b[L`t'treat_p4]))*(1+`emploss')"
				if `nn2'==0{				
				global coefficient`nn2'  " (${coefficient`nn2'}) +  (`emploss')"
				}
				}*/			
				
				
				if "`secondmethod'" != "Y"{
				global coefficient`nn2'  "-(_b[L`t'treat_p`nn2'])/(_b[L`t'treat_p0] + _b[L`t'treat_p1] + _b[L`t'treat_p2] + _b[L`t'treat_p3] + _b[L`t'treat_p4])"
				}
				
				if "`secondmethod'" == "Y"{
				if `nn2'==0{
				global coefficient`nn2'  "-1"
				}
				else{
				global coefficient`nn2'  "0"				
				}
				}
				global aboveWBL`t'_`nn2'`secondmethod' "0"
					foreach nn of numlist 1(1)4{
						local nn3 = `nn'+`nn2'
						global aboveWBL`t'_`nn2'`secondmethod' "  ${aboveWBL`t'_`nn2'`secondmethod'} + (`nn3'*_b[L`t'treat_m`nn'])"
					}
					global aboveWBL`t'_`nn2'`secondmethod' " ( ${aboveWBL`t'_`nn2'`secondmethod'})* (${coefficient`nn2'})"

				}
			
			}
			}
			
			
 			global aboveWBF4 = 0
 			global belowWBF4 = 0
 			global aboveWBF4Y = 0
 			global belowWBF4Y = 0
			
			
*
end


cap program drop abovebelowWBbunch_altq
program define abovebelowWBbunch_altq
syntax ,  [Tmax(real 16) SECONDmethod(string) ]



			foreach t of numlist 0(4)`tmax' {

			if `t'==0{
				foreach nn2 of numlist 0(1)4{
				/*
				if "`secondmethod'" != "Y" & `emploss' >= 0 {
				global coefficient`nn2'  "-(_b[treat_p`nn2'])/(_b[treat_p0] + _b[treat_p1] + _b[treat_p2] + _b[treat_p3] + _b[treat_p4])"
				}
				*if "`secondmethod'" != "Y" & `emploss' < 0 {
				if "`secondmethod'" != "Y" {
				global coefficient`nn2'  "-((_b[treat_p`nn2'])/(_b[treat_p0] + _b[treat_p1] + _b[treat_p2] + _b[treat_p3] + _b[treat_p4]))*(1+`emploss')"
				if `nn2'==0{				
				global coefficient`nn2'  " (${coefficient`nn2'}) +  (`emploss')"
				}
				}*/			
				
				if "`secondmethod'" != "Y"{
				global coefficientq`nn2'  "-(_b[treatq_p`nn2'])/(_b[treatq_p0] + _b[treatq_p1] + _b[treatq_p2] + _b[treatq_p3] + _b[treatq_p4])"
				}
				
				if "`secondmethod'" == "Y"{
				if `nn2'==0{
				global coefficientq`nn2'  "-1"
				}
				else{
				global coefficientq`nn2'  "0"				
				}
				}
				global aboveWBL`t'_`nn2'`secondmethod'q "0"
				foreach nn of numlist 1(1)4{
						local nn3 = `nn'+`nn2'
						global aboveWBL`t'_`nn2'`secondmethod'q "  ${aboveWBL`t'_`nn2'`secondmethod'q} + (`nn3'*_b[treatq_m`nn'])"
					}
					global aboveWBL`t'_`nn2'`secondmethod'q " ( ${aboveWBL`t'_`nn2'`secondmethod'q})* (${coefficientq`nn2'})"
				}


			}
			
			if `t'>0{
				global aboveWBL`t'`secondmethod'q " 0 "
				foreach nn2 of numlist 0(1)4{
				/*
				if "`secondmethod'" != "Y" & `emploss' >= 0 {
				global coefficient`nn2'  "-(_b[L`t'treat_p`nn2'])/(_b[L`t'treat_p0] + _b[L`t'treat_p1] + _b[L`t'treat_p2] + _b[L`t'treat_p3] + _b[L`t'treat_p4])"
				}
				*if "`secondmethod'" != "Y" & `emploss' < 0 {
				if "`secondmethod'" != "Y" {
				global coefficient`nn2'  "-((_b[L`t'treat_p`nn2'])/(_b[L`t'treat_p0] + _b[L`t'treat_p1] + _b[L`t'treat_p2] + _b[L`t'treat_p3] + _b[L`t'treat_p4]))*(1+`emploss')"
				if `nn2'==0{				
				global coefficient`nn2'  " (${coefficient`nn2'}) +  (`emploss')"
				}
				}*/			
				
				
				if "`secondmethod'" != "Y"{
				global coefficientq`nn2'  "-(_b[L`t'treatq_p`nn2'])/(_b[L`t'treatq_p0] + _b[L`t'treatq_p1] + _b[L`t'treatq_p2] + _b[L`t'treatq_p3] + _b[L`t'treatq_p4])"
				}
				
				if "`secondmethod'" == "Y"{
				if `nn2'==0{
				global coefficientq`nn2'  "-1"
				}
				else{
				global coefficientq`nn2'  "0"				
				}
				}
				global aboveWBL`t'_`nn2'`secondmethod'q "0"
					foreach nn of numlist 1(1)4{
						local nn3 = `nn'+`nn2'
						global aboveWBL`t'_`nn2'`secondmethod'q "  ${aboveWBL`t'_`nn2'`secondmethod'q} + (`nn3'*_b[L`t'treatq_m`nn'])"
					}
					global aboveWBL`t'_`nn2'`secondmethod'q " ( ${aboveWBL`t'_`nn2'`secondmethod'q})* (${coefficientq`nn2'})"

				}
			
			}
			}
			
			
 			global aboveWBF4 = 0
 			global belowWBF4 = 0
			
			
*
end
 

 

cap program drop abovebelowWBbunch_alt4a
program define abovebelowWBbunch_alt4a
syntax ,  [Tmax(real 16) SECONDmethod(string) ]

local denom = (1/(1+(`tmax'/4)))
lincom ((${below_full} + ${above_full})*(4*`denom')*(1/${E}))/${B}
local emploss = r(estimate)



			foreach t of numlist 0(4)`tmax' {

			if `t'==0{
				foreach nn2 of numlist 0(1)4{
				
				if "`secondmethod'" != "Y" & `emploss' >= 0 {
				global coefficient`nn2'  "-(_b[treat_p`nn2'])/(_b[treat_p0] + _b[treat_p1] + _b[treat_p2] + _b[treat_p3] + _b[treat_p4])"
				}
				if "`secondmethod'" != "Y" & `emploss' < 0 {
				*if "`secondmethod'" != "Y" {
				global coefficient`nn2'  "-((_b[treat_p`nn2'])/(_b[treat_p0] + _b[treat_p1] + _b[treat_p2] + _b[treat_p3] + _b[treat_p4]))*(1+`emploss')"
				if `nn2'==0{				
				global coefficient`nn2'  " (${coefficient`nn2'}) +  (`emploss')"
				}
				}			
								
				if "`secondmethod'" == "Y"{
				if `nn2'==0{
				global coefficient`nn2'  "-1"
				}
				else{
				global coefficient`nn2'  "0"				
				}
				}
				global aboveWBL`t'_`nn2'`secondmethod' "0"
				foreach nn of numlist 1(1)4{
						local nn3 = `nn'+`nn2'
						global aboveWBL`t'_`nn2'`secondmethod' "  ${aboveWBL`t'_`nn2'`secondmethod'} + (`nn3'*_b[treat_m`nn'])"
					}
					global aboveWBL`t'_`nn2'`secondmethod' " ( ${aboveWBL`t'_`nn2'`secondmethod'})* (${coefficient`nn2'})"
				}


			}
			
			if `t'>0{
				global aboveWBL`t'`secondmethod' " 0 "
				foreach nn2 of numlist 0(1)4{
				
				if "`secondmethod'" != "Y" & `emploss' >= 0 {
				global coefficient`nn2'  "-(_b[L`t'treat_p`nn2'])/(_b[L`t'treat_p0] + _b[L`t'treat_p1] + _b[L`t'treat_p2] + _b[L`t'treat_p3] + _b[L`t'treat_p4])"
				}
				if "`secondmethod'" != "Y" & `emploss' < 0 {
				*if "`secondmethod'" != "Y" {
				global coefficient`nn2'  "-((_b[L`t'treat_p`nn2'])/(_b[L`t'treat_p0] + _b[L`t'treat_p1] + _b[L`t'treat_p2] + _b[L`t'treat_p3] + _b[L`t'treat_p4]))*(1+`emploss')"
				if `nn2'==0{				
				global coefficient`nn2'  " (${coefficient`nn2'}) +  (`emploss')"
				}
				}		
								
				if "`secondmethod'" == "Y"{
				if `nn2'==0{
				global coefficient`nn2'  "-1"
				}
				else{
				global coefficient`nn2'  "0"				
				}
				}
				global aboveWBL`t'_`nn2'`secondmethod' "0"
					foreach nn of numlist 1(1)4{
						local nn3 = `nn'+`nn2'
						global aboveWBL`t'_`nn2'`secondmethod' "  ${aboveWBL`t'_`nn2'`secondmethod'} + (`nn3'*_b[L`t'treat_m`nn'])"
					}
					global aboveWBL`t'_`nn2'`secondmethod' " ( ${aboveWBL`t'_`nn2'`secondmethod'})* (${coefficient`nn2'})"

				}
			
			}
			}
			
			
 			global aboveWBF4 = 0
 			global belowWBF4 = 0
			
			
*
end
 

cap program drop abovebelowWBbunch_alt4b
program define abovebelowWBbunch_alt4b
syntax ,  [Tmax(real 16) SECONDmethod(string) ]

local denom = (1/(1+(`tmax'/4)))
lincom ((${below_full} + ${above_full})*(4*`denom')*(1/${E}))/${B}
local emploss = r(estimate)



			foreach t of numlist 0(4)`tmax' {

			if `t'==0{
				foreach nn2 of numlist 0(1)4{
				
				if "`secondmethod'" != "Y" {
				global coefficient`nn2'  "-((_b[treat_p`nn2'])/(_b[treat_p0] + _b[treat_p1] + _b[treat_p2] + _b[treat_p3] + _b[treat_p4]))*(1+`emploss')"
				if `nn2'==0{				
				global coefficient`nn2'  " (${coefficient`nn2'}) +  (`emploss')"
				}
				}		
				
				if "`secondmethod'" == "Y"{
				if `nn2'==0{
				global coefficient`nn2'  "-1"
				}
				else{
				global coefficient`nn2'  "0"				
				}
				}
				global aboveWBL`t'_`nn2'`secondmethod' "0"
				foreach nn of numlist 1(1)4{
						local nn3 = `nn'+`nn2'
						global aboveWBL`t'_`nn2'`secondmethod' "  ${aboveWBL`t'_`nn2'`secondmethod'} + (`nn3'*_b[treat_m`nn'])"
					}
					global aboveWBL`t'_`nn2'`secondmethod' " ( ${aboveWBL`t'_`nn2'`secondmethod'})* (${coefficient`nn2'})"
				}


			}
			
			if `t'>0{
				global aboveWBL`t'`secondmethod' " 0 "
				foreach nn2 of numlist 0(1)4{
				
				if "`secondmethod'" != "Y" {
				global coefficient`nn2'  "-((_b[L`t'treat_p`nn2'])/(_b[L`t'treat_p0] + _b[L`t'treat_p1] + _b[L`t'treat_p2] + _b[L`t'treat_p3] + _b[L`t'treat_p4]))*(1+`emploss')"
				if `nn2'==0{				
				global coefficient`nn2'  " (${coefficient`nn2'}) +  (`emploss')"
				}
				}			
				
				if "`secondmethod'" == "Y"{
				if `nn2'==0{
				global coefficient`nn2'  "-1"
				}
				else{
				global coefficient`nn2'  "0"				
				}
				}
				global aboveWBL`t'_`nn2'`secondmethod' "0"
					foreach nn of numlist 1(1)4{
						local nn3 = `nn'+`nn2'
						global aboveWBL`t'_`nn2'`secondmethod' "  ${aboveWBL`t'_`nn2'`secondmethod'} + (`nn3'*_b[L`t'treat_m`nn'])"
					}
					global aboveWBL`t'_`nn2'`secondmethod' " ( ${aboveWBL`t'_`nn2'`secondmethod'})* (${coefficient`nn2'})"

				}
			
			}
			}
			
			
 			global aboveWBF4 = 0
 			global belowWBF4 = 0
			
			
*
end
 
******************************************************************************** 

cap program drop abovebelowWBfull_alt
program define abovebelowWBfull_alt
syntax [, Tmax(real 16) SECONDmethod(string)]
 
 
foreach t of numlist -12(4)`tmax' {
if `t'<-4{
local nt = -`t'
global aboveWBF`nt'`secondmethod' "${aboveWBF`nt'_0`secondmethod'} + ${aboveWBF`nt'_1`secondmethod'} + ${aboveWBF`nt'_2`secondmethod'} + ${aboveWBF`nt'_3`secondmethod'} + ${aboveWBF`nt'_4`secondmethod'}"
}
if `t'>=0{
global aboveWBL`t'`secondmethod' "${aboveWBL`t'_0`secondmethod'} + ${aboveWBL`t'_1`secondmethod'} + ${aboveWBL`t'_2`secondmethod'} + ${aboveWBL`t'_3`secondmethod'} + ${aboveWBL`t'_4`secondmethod'}"
}
}
			global aboveWB_full`secondmethod' "${aboveWBL0`secondmethod'}"

forval t = 4(4)`tmax' {
			
				global aboveWB_full`secondmethod' "${aboveWB_full`secondmethod'} + ${aboveWBL`t'`secondmethod'}"
			}

 end
 
 
 
cap program drop abovebelowWBfull_altq
program define abovebelowWBfull_altq
syntax [, Tmax(real 16) SECONDmethod(string)]
 
 
foreach t of numlist 0(4)`tmax' {
global aboveWBL`t'`secondmethod'q "${aboveWBL`t'_0`secondmethod'q} + ${aboveWBL`t'_1`secondmethod'q} + ${aboveWBL`t'_2`secondmethod'q} + ${aboveWBL`t'_3`secondmethod'q} + ${aboveWBL`t'_4`secondmethod'q}"
}
			global aboveWB_full`secondmethod'q "${aboveWBL0`secondmethod'q}"

forval t = 4(4)`tmax' {
			
				global aboveWB_full`secondmethod'q "${aboveWB_full`secondmethod'q} + ${aboveWBL`t'`secondmethod'q}"
			}

 end
 
 
 
 
 
********************************************************************************
*************       Mass by wagebin figure         *****************************
********************************************************************************

cap program drop makefigure_massbywagebin
program define makefigure_massbywagebin
syntax , [Tmin(real 12) Tmax(real 16)  Wmax(real 4) Wmin(real 4)]

				local denominator = (1/(1+(`tmax'/4)))


				global PA_p0 "[treat_p0  ]*(4)*(1/${E})"		// Multiplying by 4 to get $ bin estimates. Dividing by E to get share of workforce.
				forval t = 4(4)`tmax' {
					global PA_p0  "${PA_p0} + [ L`t'treat_p0 ]*(4)*(1/${E})"
				}
				global PA_p0 "`denominator'*(${PA_p0})"

				forval j = 1/`wmax' {
					global PA_p`j' "[treat_p`j' ]*(4)*(1/${E})"
				forval t = 4(4)`tmax' {
							global PA_p`j'    " ${PA_p`j'} + [L`t'treat_p`j' ]*(4)*(1/${E})"
						}
					global PA_p`j' "`denominator'*(${PA_p`j'})" 
				}
				
				forval j = 1/`wmin' {				
					global PA_m`j' "[treat_m`j' ]*(4)*(1/${E})"
				forval t = 4(4)`tmax' {
							global PA_m`j'    " ${PA_m`j'} + [L`t'treat_m`j' ]*(4)*(1/${E})"
						}
					global PA_m`j' "`denominator'*(${PA_m`j'})"
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
					global cumul "${cumul} + `lincomline'"
					qui lincom "${cumul}"
					local cumul_b = r(estimate)
					local cumul_se = r(se)
 					if `k' == -`wmin' {
						mat countmat = [ `k', `basic_b',  `basic_b'-1.96*`basic_se', `basic_b'+1.96*`basic_se', `cumul_b', `cumul_b'-1.96*`cumul_se', `cumul_b'+1.96*`cumul_se' ]
					}
					else {
						mat countmat = [countmat \ [ `k', `basic_b',  `basic_b'-1.96*`basic_se', `basic_b'+1.96*`basic_se', `cumul_b', `cumul_b'-1.96*`cumul_se', `cumul_b'+1.96*`cumul_se' ]]
					}
				}
			 	
		/*		 								
			nlcom "-(${above_full})/(${below_full})"
			mat b = r(b)
			mat V = r(V)
 
			local bunchestb: di %3.2f  b[1,1]
			local bunchestse: di %3.2f sqrt(V[1,1])
			global bunchest "Bunching estimate=`bunchestb'(`bunchestse')"
			
		*/	
 			 
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
			rename countmat5 totest			
			rename countmat6 totlow
			rename countmat7 tothigh			
			
			qui sum totest if bin==`wmax' 
			local elastb: di %3.2f  r(mean)
			qui sum tothi if bin==`wmax' 
			local elasthi: di %3.2f  r(mean)
			qui sum totlow if bin==`wmax' 
			local elastlow: di %3.2f  r(mean)

			global elastest "MW elasticty=`elastb', CI=[`elastlow', `elasthi']"
 			
 			 
				twoway ( bar est bin  , fcolor(ltblue) lcolor(none) ) ///
				(rcap low  high bin  , lcolor(ltblue*1.25) lwidth(thick) ) ///
				(line totest bin, lcolor(red*.33) lpat(line) lwidth(thick) ) /* ///
				(line totlow  bin , color(red) lpat(dot)) ///
				(line tothigh  bin , color(red) lpat(dot))	*/			, ///
				/* xlabel(-4 "[-`=char(36)'4.25, -`=char(36)'3.25)" -2 "[-`=char(36)'2.25, -`=char(36)'1.25)" 0 "[-`=char(36)'0.25, `=char(36)'0.75)" ///
				 2 "[`=char(36)'1.75, `=char(36)'2.75)" 4 "[`=char(36)'3.75, `=char(36)'4.75)" , labsize(medsmall) ) ///
				 ylabel( -0.004  -0.002   0  0.002 0.004 0.006) */ ///
				 xlabel(-4 "-4" -3 "-3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "4"  ///
				, labsize(medsmall) ) ///
				ylabel(, labsize(medsmall)) scheme(s1color) /*xtitle("Wage Bin")*/ xtitle("Wage bins in `=char(36)' relative to new MW", size(medsmall)) xsc(titlegap(*10)) ytitle("", height(10))   ///
				/*title("${label`s'}", size(medium) )*/ legend(off) /*  ///
				note("${bunchest}" , size(small) color(blue)) */
 


end


cap program drop makefigure_massbywagebin_lab
program define makefigure_massbywagebin_lab
syntax , [Tmin(real 12) Tmax(real 16)  Wmax(real 4) Wmin(real 4)]

				local denominator = (1/(1+(`tmax'/4)))


				global PA_p0 "[treat_p0  ]*(4)*(1/${E})"		// Multiplying by 4 to get $ bin estimates. Dividing by E to get share of workforce.
				foreach t of numlist 4(4)`tmax' {
					global PA_p0  "${PA_p0} + [ L`t'treat_p0 ]*(4)*(1/${E})"
				}
				global PA_p0 "`denominator'*(${PA_p0})"

				forval j = 1/`wmax' {
					global PA_p`j' "[treat_p`j' ]*(4)*(1/${E})"
						foreach t of numlist 4(4)`tmax' {
							global PA_p`j'    " ${PA_p`j'} + [L`t'treat_p`j' ]*(4)*(1/${E})"
						}
					global PA_p`j' "`denominator'*(${PA_p`j'})" 
				}
				
				forval j = 1/`wmin' {				
					global PA_m`j' "[treat_m`j' ]*(4)*(1/${E})"
						foreach t of numlist 4(4)`tmax' {
							global PA_m`j'    " ${PA_m`j'} + [L`t'treat_m`j' ]*(4)*(1/${E})"
						}
					global PA_m`j' "`denominator'*(${PA_m`j'})"
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
					global cumul "${cumul} + `lincomline'"
					qui lincom "${cumul}"
					local cumul_b = r(estimate)
					local cumul_se = r(se)
 					if `k' == -`wmin' {
						mat countmat = [ `k', `basic_b',  `basic_b'-1.96*`basic_se', `basic_b'+1.96*`basic_se', `cumul_b', `cumul_b'-1.96*`cumul_se', `cumul_b'+1.96*`cumul_se' ]
					}
					else {
						mat countmat = [countmat \ [ `k', `basic_b',  `basic_b'-1.96*`basic_se', `basic_b'+1.96*`basic_se', `cumul_b', `cumul_b'-1.96*`cumul_se', `cumul_b'+1.96*`cumul_se' ]]
					}
				}
			 	
		/*		 								
			nlcom "-(${above_full})/(${below_full})"
			mat b = r(b)
			mat V = r(V)
 
			local bunchestb: di %3.2f  b[1,1]
			local bunchestse: di %3.2f sqrt(V[1,1])
			global bunchest "Bunching estimate=`bunchestb'(`bunchestse')"
			
		*/	
 			 
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
			rename countmat5 totest			
			rename countmat6 totlow
			rename countmat7 tothigh			
			
			qui sum totest if bin==`wmax' 
			local elastb: di %3.2f  r(mean)
			qui sum tothi if bin==`wmax' 
			local elasthi: di %3.2f  r(mean)
			qui sum totlow if bin==`wmax' 
			local elastlow: di %3.2f  r(mean)

			global elastest "MW elasticty=`elastb', CI=[`elastlow', `elasthi']"
 			
 			 
				twoway ( bar est bin  , fcolor(ltblue) lcolor(none) ) ///
				(rcap low  high bin  , lcolor(ltblue*1.25) lwidth(thick) ) ///
				(line totest bin, lcolor(red*.33) lpat(line) lwidth(thick) ) /* ///
				(line totlow  bin , color(red) lpat(dot)) ///
				(line tothigh  bin , color(red) lpat(dot))	*/			, ///
				/* xlabel(-4 "[-`=char(36)'4.25, -`=char(36)'3.25)" -2 "[-`=char(36)'2.25, -`=char(36)'1.25)" 0 "[-`=char(36)'0.25, `=char(36)'0.75)" ///
				 2 "[`=char(36)'1.75, `=char(36)'2.75)" 4 "[`=char(36)'3.75, `=char(36)'4.75)" , labsize(medsmall) ) */ ///
				 ylabel(-0.03(0.01)0.02) ysc(range(-0.03 0.023))  ///
				 xlabel(-4 "-4" -3 "-3" -2 "-2" -1 "-1" 0 "0" 1 "1" 2 "2" 3 "3" 4 "4"  ///
				, labsize(medsmall) ) ///
				ylabel(, labsize(medsmall)) scheme(s1color) /*xtitle("Wage Bin")*/ xtitle("Wage bins in `=char(36)' relative to new MW", size(medsmall)) xsc(titlegap(*10)) ytitle("", height(10))   ///
				/*title("${label`s'}", size(medium) )*/ legend(off) /*  ///
				note("${bunchest}" , size(small) color(blue)) */
 


end

********************************************************************************
*************      Make Figure, mass over time         *************************
********************************************************************************
*The following two program requires stored estimation results to be called.
 
 
			********************************************************************
			****************      Before               *************************
			********************************************************************
 
cap program drop makefigure_massovertime_before
program define makefigure_massovertime_before
syntax , [Tmin(real 12) Tmax(real 16)]

				
				foreach t of numlist -`tmin'(4)-4 {
			
			
					local nt = -`t'
					nlcom ("${aboveF`nt'}") 
					mat b = r(b)
					mat V = r(V)
					local ab = b[1,1]*(4)*(1/${E})
					local ase = sqrt(V[1,1])*(4)*(1/${E})
					nlcom ("${belowF`nt'}") 
					mat b = r(b)
					mat V = r(V)
					local bb =  (b[1,1]*(4))*(1/${E})
					local bse = sqrt(V[1,1])*(4)*(1/${E})

					if `nt' == `tmin' {
						mat eventmat = [`t', `ab', `ab'-1.96*`ase', `ab'+1.96*`ase',  `bb', `bb'-1.96*`bse', `bb'+1.96*`bse'] 
					}
					else {
						mat eventmat = [eventmat \ [`t', `ab', `ab'-1.96*`ase', `ab'+1.96*`ase',  `bb', `bb'-1.96*`bse', `bb'+1.96*`bse']] 
					}
}
*

end
					
					

					************************************************************
					*******************     After             ******************
					************************************************************
					
					
cap program drop makefigure_massovertime_after
program define makefigure_massovertime_after
syntax , [Tmin(real 12) Tmax(real 16)]
					

				foreach t of numlist 0(4)`tmax' {
					nlcom ("${aboveL`t'}") 
					mat b = r(b)
					mat V = r(V)
					local ab =  b[1,1]*(4)*(1/${E})
					local ase = sqrt(V[1,1])*(4)*(1/${E})
					nlcom ("${belowL`t'}") 
					mat b = r(b)
					mat V = r(V)
					local bb =  (b[1,1]*(4))*(1/${E})
					local bse = sqrt(V[1,1])*(4)*(1/${E})
					mat eventmat = [eventmat \ [`t', `ab', `ab'-1.96*`ase', `ab'+1.96*`ase',  `bb', `bb'-1.96*`bse', `bb'+1.96*`bse'] ]

					}
			
			cap drop time* 
			cap drop est* 
			cap drop low* high* 
			cap drop _est*
			
			cap drop eventmat*
			cap drop bunchest
			
			svmat eventmat
			rename eventmat1 time
			rename eventmat2 estA			
			rename eventmat3 lowA
			rename eventmat4 highA
			rename eventmat5 estB			
			rename eventmat6 lowB
			rename eventmat7 highB
 
				twoway ( line estA time  , fcolor(ltblue) lcolor(ltblue)  lwidth(thick)  ) ///
				(rcap lowA  highA time  , lwidth(thick) lcolor(ltblue*1.25) )  ///
				( line estB time  , fcolor(red) lcolor(red) lwidth(thick) yline(0, lcolor(gs10) lwidth(vvthin)) xline(0, lpattern(dash) lcolor(black) ) ) ///
				(rcap lowB  highB time  , lwidth(thick)  lcolor(red*1.25) ) , ///
				scheme(s1color) xtitle("Annualized event time", size(medsmall)) ///
				/*ytitle("Excess mass", height(5) axis(1) size(medsmall))*/   ///
				 ylabel(, axis(1) labsize(medsmall)) yscale(titlegap(0) axis(1))  ///
				/*xlabel(-12 "[-12,-9]" -8 "[-8,-5]" -4 "[-4,-1]" 0 "[0,3]" 4 "[4,7]" 8 "[8,11]" 12 "[12,15]" 16 "[16,19]", labsize(medsmall)) */ ///
				xlabel(-12 "-3" -8 "-2" -4 "-1" 0 "0" 4 "1" 8 "2" 12 "3" 16 "4", labsize(medsmall)) legend(off) /* ///
				title("${label`s'}", size(medium) )  note("$bunchch" "$bunchelas" "$belowmasssh" "$abovemasssh"  /*"$elastest"*/, size(small) color(blue)) ///
				*/
 
end 
 
********************************************************************************
****************      Make Figure: Change over time          *******************
********************************************************************************

*The following two program requires stored estimation results to be called.
 
 
			********************************************************************
			****************      Before               *************************
			********************************************************************
 
 
cap program drop makefigure_chovertime_before
program define makefigure_chovertime_before
syntax , [Tmin(real 12) Tmax(real 16)]

				foreach t of numlist -`tmin'(4)-4 {
				
						local nt = -`t' 
						lincom ((${aboveF`nt'}) + (${belowF`nt'}))*(1/${E})*(1/${B})*4							// Multiplying by 4 because of the number of bins (treatments at 1$ increments, bins at 25 cents).
						replace be_timeb 		= r(estimate) 	if counter==`t'
						replace be_timese 		= r(se) 		if counter==`t'
						replace be_timeupper 		= r(estimate) + invttail(r(df), 0.025)*r(se) 	if counter==`t'
						replace be_timelower 		= r(estimate) - invttail(r(df), 0.025)*r(se) 	if counter==`t'
						
						
						
					}  //end of t<-4
 
 end

 
			********************************************************************
			****************      After               **************************
			********************************************************************
 
 
cap program drop makefigure_chovertime_after
program define makefigure_chovertime_after
syntax , [Tmin(real 12) Tmax(real 16)]
 
 
				foreach t of numlist 0(4)`tmax' {
 
						lincom ((${aboveL`t'}) + (${belowL`t'}))*(1/${E})*(1/${B})*4
						replace be_timeb 		= r(estimate) 	if counter==`t'
						replace be_timese 		= r(se) 		if counter==`t'
						replace be_timeupper 		= r(estimate) + invttail(r(df), 0.025)*r(se) 	if counter==`t'
						replace be_timelower 		= r(estimate) - invttail(r(df), 0.025)*r(se) 	if counter==`t'

						}  
*
 						twoway (scatter be_timeb counter  , msymbol(D) mcolor(blue) yline(0, lcolor(gs10) lwidth(vvthin)) xline(0, lpattern(dash) lcolor(black) )) ///
						(line be_timeb counter ,  lwidth(thick) lpattern(dash)  lcolor(blue)) ///
						(rcap be_timeupper be_timelower counter,  lwidth(thick) lcolor(blue) ), graphregion(color(white)) ///
						xlabel(-12 "-3" -8 "-2" -4 "-1" 0 "0" 4 "1" 8 "2" 12 "3" 16 "4", labsize(medsmall))	ylabel(-0.2(0.1)0.2, format(%02.1fc)) ///
						ylabel(, labsize(medsmall)) legend(off) xtitle("Annualized event time", size(medsmall))  

end
 
 
********************************************************************************
****************      Make Figure: Change over time (wages)         ************
********************************************************************************

*The following two program requires stored estimation results to be called.
 
 
			********************************************************************
			****************      Before               *************************
			********************************************************************

 
 cap program drop makefigure_chovertimeWB_before
 program define makefigure_chovertimeWB_before
 syntax , [Tmin(real 12) Tmax(real 16)]

 				foreach t of numlist -`tmin'(4)-4 {
				
						local nt = -`t' 
						nlcom ((((${belowWBF`nt'}+${aboveWBF`nt'})*4/$EWB ) - ///
						(((${belowF`nt'} + ${aboveF`nt'})*(4)*(1/${E}))/${B}))) /(1+(((${belowF`nt'} + ${aboveF`nt'})*(4)*(1/${E}))/${B}))								// Multiplying by 4 because of the number of bins (treatments at 1$ increments, bins at 25 cents).
						mat temp1 = r(b)
						local temp1 = temp1[1,1]
						mat temp2 = r(V)
						local temp2 = sqrt(temp2[1,1])
						
						replace be_timeb 		= `temp1' 	if counter==`t'
						replace be_timese 		= `temp2' 		if counter==`t'
						replace be_timeupper 		= `temp1' + invttail(e(df_r), 0.025)*`temp2' 	if counter==`t'
						replace be_timelower 		= `temp1' - invttail(e(df_r), 0.025)*`temp2' 	if counter==`t'
						
		/*				
						lincom ((((${belowWBF`nt'}+${aboveWBF`nt'})*4/$EWB ) - ///
						(((${belowF`nt'} + ${aboveF`nt'})*(4)*(1/${E}))/${B})*(${MW_real}/(${EWB}/(${B}*${E})))   ))	
					
					
						replace be_timeb 		= r(estimate) 	if counter==`t'
						replace be_timese 		= r(se) 		if counter==`t'
						replace be_timeupper 		= r(estimate) + invttail(r(df), 0.025)*r(se) 	if counter==`t'
						replace be_timelower 		= r(estimate) - invttail(r(df), 0.025)*r(se) 	if counter==`t'
						
		*/				
						
					}  //end of t<-4

					
					
					
end

 cap program drop makefigure_chovertimeWB_after
 program define makefigure_chovertimeWB_after
 syntax , [Tmin(real 12) Tmax(real 16)]
 
 
 
	foreach t of numlist 0(4)`tmax' {
 						nlcom ((((${belowWBL`t'}+${aboveWBL`t'})*4/$EWB ) - ///
						(((${belowL`t'} + ${aboveL`t'})*(4)*(1/${E}))/${B}))) /(1+(((${belowL`t'} + ${aboveL`t'})*(4)*(1/${E}))/${B}))								// Multiplying by 4 because of the number of bins (treatments at 1$ increments, bins at 25 cents).
						mat temp1 = r(b)
						local temp1 = temp1[1,1]
						mat temp2 = r(V)
						local temp2 = sqrt(temp2[1,1])

						replace be_timeb 		= `temp1' 	if counter==`t'
						replace be_timese 		= `temp2' 		if counter==`t'
						replace be_timeupper 		= `temp1' + invttail(e(df_r), 0.025)*`temp2' 	if counter==`t'
						replace be_timelower 		= `temp1' - invttail(e(df_r), 0.025)*`temp2' 	if counter==`t'

/*
 						lincom ((((${belowWBL`t'}+${aboveWBL`t'})*4/$EWB ) - ///
						(((${belowL`t'} + ${aboveL`t'})*(4)*(1/${E}))/${B})*(${MW_real}/(${EWB}/(${B}*${E}))) ))								// Multiplying by 4 because of the number of bins (treatments at 1$ increments, bins at 25 cents).

						replace be_timeb 		= r(estimate) 	if counter==`t'
						replace be_timese 		= r(se) 		if counter==`t'
						replace be_timeupper 		= r(estimate) + invttail(r(df), 0.025)*r(se) 	if counter==`t'
						replace be_timelower 		= r(estimate) - invttail(r(df), 0.025)*r(se) 	if counter==`t'

*/
						}   //end of t==0	
*
						twoway (scatter be_timeb counter  , msymbol(D) mcolor(blue) yline(0, lcolor(gs10) lwidth(vvthin)) xline(0, lpattern(dash) lcolor(black) )) ///
						(line be_timeb counter, lcolor(blue) lpattern(dash)  lwidth(thick) ) ///
						(rcap be_timeupper be_timelower counter, lcolor(blue) lwidth(thick) ), graphregion(color(white)) ///
						xlabel(-12 "-3" -8 "-2" -4 "-1" 0 "0" 4 "1" 8 "2" 12 "3" 16 "4", labsize(medsmall))	///
						ylabel(, labsize(medsmall)) legend(off) xtitle("Annualized event time", size(medsmall)) 

end


*Alternative

			********************************************************************
			****************      Before               *************************
			********************************************************************

 
 cap program drop makefigure_chovertimeWB_b_alt
 program define makefigure_chovertimeWB_b_alt
 syntax , [Tmin(real 12) Tmax(real 16) SECONDmethod(string)]

 				foreach t of numlist -`tmin'(4)-4 {
				
						local nt = -`t' 
						nlcom ((${aboveWBF`nt'`secondmethod'})*4/$EWB ) // Multiplying by 4 because of the number of bins (treatments at 1$ increments, bins at 25 cents).
						mat temp1 = r(b)
						local temp1 = temp1[1,1]
						mat temp2 = r(V)
						local temp2 = sqrt(temp2[1,1])

						replace be_timeb 		= `temp1' 	if counter==`t'
						replace be_timese 		= `temp2' 		if counter==`t'
						replace be_timeupper 		= `temp1' + invttail(e(df_r), 0.025)*`temp2' 	if counter==`t'
						replace be_timelower 		= `temp1' - invttail(e(df_r), 0.025)*`temp2' 	if counter==`t'
						
						
						
					}  //end of t<-4

end

 cap program drop makefigure_chovertimeWB_a_alt
 program define makefigure_chovertimeWB_a_alt
 syntax , [Tmin(real 12) Tmax(real 16) SECONDmethod(string)]
 
 
 
	foreach t of numlist 0(4)`tmax' {
 						nlcom ((${aboveWBL`t'`secondmethod'})*4/$EWB ) 								// Multiplying by 4 because of the number of bins (treatments at 1$ increments, bins at 25 cents).
						mat temp1 = r(b)
						local temp1 = temp1[1,1]
						mat temp2 = r(V)
						local temp2 = sqrt(temp2[1,1])

						replace be_timeb 		= `temp1' 	if counter==`t'
						replace be_timese 		= `temp2' 		if counter==`t'
						replace be_timeupper 		= `temp1' + invttail(e(df_r), 0.025)*`temp2' 	if counter==`t'
						replace be_timelower 		= `temp1' - invttail(e(df_r), 0.025)*`temp2' 	if counter==`t'
						

						}   //end of t==0	
*
						twoway (scatter be_timeb counter  , msymbol(D) mcolor(blue) yline(0, lcolor(gs10) lwidth(vvthin)) xline(0, lpattern(dash) lcolor(black) )) ///
						(line be_timeb counter, lcolor(blue) lpattern(dash)  lwidth(thick) ) ///
						(rcap be_timeupper be_timelower counter, lcolor(blue) lwidth(thick) ), graphregion(color(white)) ///
						xlabel(-12 "-3" -8 "-2" -4 "-1" 0 "0" 4 "1" 8 "2" 12 "3" 16 "4", labsize(medsmall))	///
						ylabel(, labsize(medsmall)) legend(off) xtitle("Annualized event time", size(medsmall)) 

end







********************************************************************************
******************      Program for creating tables          *******************
******************************************************************************** 
 
 
cap program drop Table_create
program define Table_create
syntax namelist , Tablename(string) [Group(string) Startyear(real 1979) Mgroup(string) MPattern(string) Footnotes(string)]

	tokenize `namelist'
	local i: word count `namelist'
	di "`i'"
	forval num=1(1)`i'{
	local temp "ta_`group'_`startyear'_``num'' "
	local full_list: list full_list | temp 
	}	

	if "`mgroup'" == ""{
	esttab  `full_list' using "${tables}`tablename'.tex",	/// 
	replace stats(below_Eb below_Ese above_Eb above_Ese  blankspace WB_Eb WB_Ese  bunching_Eb bunching_Ese blankspace bunchelas_Eb bunchelas_Ese labordem_Eb labordem_Ese blankspace  belsh  minwpc  numevent numobs blankspace blankspace wagebinstate /*periodfe*/ wagebinperiod trend qtrend divtime stp, label("Missing jobs below new MW (`=char(36)' \Delta `=char(36)' b)" " " "Excess jobs above new MW (`=char(36)' \Delta `=char(36)' a)"  " " " "   "\%`=char(36)'\Delta`=char(36)' affected wages" " " "\%`=char(36)'\Delta`=char(36)' affected employment " " " " "  "Employment elasticity w.r.t. MW" " " "Emp. elasticity w.r.t. affected wage" " " " " "Jobs below new MW (`=char(36)'\overline{b}\ _{-1}`=char(36)')" "\%`=char(36)'\Delta`=char(36)' MW" "\# Event " "N " " " "\underline{\textit{Controls}}" "Bin-state FE" /*"Period FE"*/ "Bin-period FE " "Bin-state linear trends" "Bin-state quadratic trends" "Bin-division-period FE" "State-period FE" )) ///
	nodep addnotes("`footnotes'") nogaps cells(none) nomtitles fragment booktabs
	}
	else{
	local numberword : word count `mgroup'
		forval ii = 1/`numberword' {
			local mgroup`ii' : word `ii' of `mgroup'
		}
	
	esttab  `full_list' using "${tables}`tablename'.tex",	/// 
	replace stats(below_Eb below_Ese above_Eb above_Ese  blankspace WB_Eb WB_Ese  bunching_Eb bunching_Ese blankspace bunchelas_Eb bunchelas_Ese labordem_Eb labordem_Ese blankspace  belsh  minwpc  numevent numobs blankspace blankspace wagebinstate /*periodfe*/ wagebinperiod trend qtrend divtime stp, label("Missing jobs below new MW (`=char(36)' \Delta `=char(36)' b)" " " "Excess jobs above new MW (`=char(36)' \Delta `=char(36)' a)" " " " "   "\%`=char(36)'\Delta`=char(36)' affected wages" " " "\%`=char(36)'\Delta`=char(36)' affected employment " " " " "  "Employment elasticity w.r.t. MW" " " "Emp. elasticity w.r.t. affected wage" " " " " "Jobs below new MW (`=char(36)'\overline{b}\ _{-1}`=char(36)')" "\%`=char(36)'\Delta`=char(36)' MW" "\# Event " "N " " " "\underline{\textit{Controls}}" "Bin-state FE" /*"Period FE"*/ "Bin-period FE " "Bin-state linear trends" "Bin-state quadratic trends" "Bin-division-period FE" "State-period FE" )) ///
	nodep  addnotes("`footnotes'") nogaps cells(none) nomtitles mgroups("`mgroup1'" "`mgroup2'", pattern("`mpattern'") prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) fragment booktabs	
	}
end


********************************************************************************
******    Above and Below Bunches for Quantiles and Persistent Events     ******
********************************************************************************


cap program drop abovebelowqpersbunch
program define abovebelowqpersbunch
syntax ,  [Tmin(real 12) Tmax(real 16) Wmax(real 4) Wmin(real 4)]

foreach y in pers q{
foreach t of numlist -`tmin'(4)`tmax' {

	if `t' < -4  {	
		local nt = -`t' 
		global aboveF`nt'`y' "(_b[F`nt'treat`y'_p0] )"
		forval j = 1/`wmax' {
			global aboveF`nt'`y' "${aboveF`nt'`y'} + (_b[F`nt'treat`y'_p`j'] )"
		}
		global belowF`nt'`y' "(_b[F`nt'treat`y'_m1] )"
		forval j = 2/`wmin' {
			global belowF`nt'`y' "${belowF`nt'`y'} + (_b[F`nt'treat`y'_m`j'] )"
		}

    }
 
	if `t' == -4 {
		global aboveF4`y' "0"
		global belowF4`y' "0"
	}
 
	if `t' == 0 {	
		 
		global aboveL`t'`y' "(_b[treat`y'_p0])"
		forval j = 1/`wmax' {
			global aboveL`t'`y' "${aboveL`t'`y'} + (_b[treat`y'_p`j'] )"
		}
		global belowL`t'`y' "(_b[treat`y'_m1] )"
		forval j = 2/`wmin' {
			global belowL`t'`y' "${belowL`t'`y'} + (_b[treat`y'_m`j'])"
		}

    }
	
   if `t' > 0 {	
		 
		global aboveL`t'`y' "(_b[L`t'treat`y'_p0])"
		forval j = 1/`wmax' {
			global aboveL`t'`y' "${aboveL`t'`y'} + (_b[L`t'treat`y'_p`j'])"
		}
		global belowL`t'`y' "(_b[L`t'treat`y'_m1] )"
		forval j = 2/`wmin' {
			global belowL`t'`y' "${belowL`t'`y'} + (_b[L`t'treat`y'_m`j'] )"
		}

    }
	
 
 } 
 }
 *
end 

 
cap program drop abovebelowqpersfull
program define abovebelowqpersfull
syntax ,  [Tmin(real 12) Tmax(real 16) Wmax(real 4) Wmin(real 4)]

 
 foreach y in pers q{
global above_full`y' "${aboveL0`y'}"
global below_full`y' "${belowL0`y'}"

foreach t of numlist 4(4)`tmax' {
			
				global below_full`y' "${below_full`y'} + ${belowL`t'`y'}"
				global above_full`y' "${above_full`y'} + ${aboveL`t'`y'}"
 }
 }
*

end

********************************************************************************
*************     Above and below bunches of wages           *******************
********************************************************************************



cap program drop abovebelowWBqpersbunch
program define abovebelowWBqpersbunch
syntax , Y(string) [Tmin(real 12) Tmax(real 16) Wmax(real 4) Wmin(real 4)]




				foreach t of numlist -`tmin'(4)`tmax' {
				
						if `t' < -4  {	
							local nt = -`t' 
							global aboveWBF`nt'`y' "((_b[F`nt'treat`y'_p0])*(${wagemult`y'}+0) )"
						forval j = 1/`wmax' {
							global aboveWBF`nt'`y' "${aboveWBF`nt'`y'} + ((_b[F`nt'treat`y'_p`j'])*(${wagemult`y'}+`j') )"
						}
						global belowWBF`nt'`y' "((_b[F`nt'treat`y'_m1])*(${wagemult`y'}-1) )"
						forval j = 2/`wmin' {
							global belowWBF`nt'`y' "${belowWBF`nt'`y'} + ((_b[F`nt'treat`y'_m`j'])*(${wagemult`y'}-`j') )"
						}

					}

				

					if `t' == 0 {	
		 
						global aboveWBL`t'`y' "((_b[treat`y'_p0])*(${wagemult`y'}+0))"
						forval j = 1/`wmax' {
							global aboveWBL`t'`y' "${aboveWBL`t'`y'} + ((_b[treat`y'_p`j'])*(${wagemult`y'}+`j') )"
						}
						global belowWBL`t'`y' "((_b[treat`y'_m1])*(${wagemult`y'}-1) )"
						forval j = 2/`wmin' {
							global belowWBL`t'`y' "${belowWBL`t'`y'} + ((_b[treat`y'_m`j'])*(${wagemult`y'}-`j') )"
						}

					}
	
					if `t' > 0 {	
		 
						global aboveWBL`t'`y' "((_b[L`t'treat`y'_p0])*(${wagemult`y'}+0))"
						forval j = 1/`wmax' {
							global aboveWBL`t'`y' "${aboveWBL`t'`y'} + ((_b[L`t'treat`y'_p`j'])*(${wagemult`y'}+`j'))"
						}
						global belowWBL`t'`y' "((_b[L`t'treat`y'_m1])*(${wagemult`y'}-1) )"
						forval j = 2/`wmin' {
							global belowWBL`t'`y' "${belowWBL`t'`y'} + ((_b[L`t'treat`y'_m`j'])*(${wagemult`y'}-`j'))"
						}

					}
	
 
			} 
 			global aboveWBF4`y' = 0
 			global belowWBF4`y' = 0

			
end			
 
cap program drop abovebelowWBqpersfull
program define abovebelowWBqpersfull
syntax , Y(string) [Tmin(real 12) Tmax(real 16) Wmax(real 4) Wmin(real 4)]

 
			global aboveWB_full`y' "${aboveWBL0`y'}"
			global belowWB_full`y' "${belowWBL0`y'}"

			foreach t of numlist 4(4)`tmax' {
			
				global belowWB_full`y' "${belowWB_full`y'} + ${belowWBL`t'`y'}"
				global aboveWB_full`y' "${aboveWB_full`y'} + ${aboveWBL`t'`y'}"
			}
*
			
end		


********************************************************************************
********************************************************************************
********************************************************************************

cap program drop correctaggcont
program define correctaggcont
syntax , [TMIn(real 12) TMAx(real 16) WMAx(real 4) WMIn(real 4)]


if `tmin'!=12 | `tmax'!=16 | `wmax'!=4 | `wmin'!=4{
*
cap drop _contf_p
cap drop postcontf_p
cap drop postcontf_m
cap drop tempcontf_p
cap drop tempcontf_m
g _contf_p = 0
replace _contf_p = 1 if (fedincrease==1 &  overallcountgroup>0 & $ifclause ) &  (wageM25>=(MW_realM25 - 0) &  wageM25 < (MW_realM25 +1 + `wmax' - 0))
g tempcontf_p = _contf_p + L._contf_p + L2._contf_p + L3._contf_p

local Tmax3 = `tmax' + 3
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
replace _contf_m = 1 if (fedincrease==1 &  overallcountgroup>0 & $ifclause ) &  (wageM25<(MW_realM25 - 0)  &  wageM25 >= (MW_realM25 -  `wmin' - 0 ))	
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
cap drop precontf_*
cap drop earlycontf_*
if `tmin' == 12{
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
if `tmin'==8{
	foreach k in m p {
		g precontf_`k' = F4.tempcontf_`k'
		replace precontf_`k' = 0 if precontf_`k' == .

	}
*

foreach k in m p {
		g earlycontf_`k' = F8.tempcontf_`k'
		replace earlycontf_`k' = 0 if earlycontf_`k' == .
		
	}



}
*

*************   Other    

cap drop tempcont_*
cap drop postcont_*
cap drop _cont_p
g _cont_p = 0
replace _cont_p = 1 if ($missedevents | toosmall==1) &  (wageM25>=(MW_realM25 - 0) &  wageM25 < (MW_realM25 +1 + `wmax' - 0))
g tempcont_p = _cont_p + L._cont_p + L2._cont_p + L3._cont_p

local Tmax3 = `tmax' + 3
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
replace _cont_m = 1 if ($missedevents | toosmall==1) &  (wageM25<(MW_realM25 - 0)  &  wageM25 >= (MW_realM25 -  `wmin' - 0 ))	
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

cap drop precont_*
cap drop earlycont_*
if `tmin'==12{
	foreach k in m p {
		g precont_`k' = F4.tempcont_`k'
		replace precont_`k' =  0 if precont_`k' == .
		
	}
*

foreach k in m p {
		g earlycont_`k' = F12.tempcont_`k' + F8.tempcont_`k'
		replace earlycont_`k' = 0 if earlycont_`k' == .
		
	}
}
if `tmin'==8{
	foreach k in m p {
		g precont_`k' = F4.tempcont_`k'
		replace precont_`k' =  0 if precont_`k' == .		
		
	}
*

foreach k in m p {
		g earlycont_`k' =  F8.tempcont_`k'
		replace earlycont_`k' = 0 if earlycont_`k' == .
		
	}

}

replace postcont_p = 0 if postcont_p==.
replace postcont_m = 0 if postcont_m==.

replace postcontf_p = 0 if postcontf_p==.
replace postcontf_m = 0 if postcontf_m==.

}

else{
di "No need to correct anything. You are using the default settings."
}
end

			
********************************************************************************
************     Do file to define placebo treatment globals         ***********
********************************************************************************

cap program drop placebowindows1
program define placebowindows1
syntax  , Wmax(real) TRUEwmax(real) [Tmax(real 16) Tmin(real 12)]  


			********************************************************************
			***************         Placebo Treatment Vector           *********
			********************************************************************

global placeboafter1 " "
global placebobefore1 " "
global placebocontafter1 " "

 local wmin = `truewmax'  + 1
di "Placebo wmin and wmax should not be too far apart." 
di "`wmax' - `wmin'"
assert `wmax' - `wmin' < 9
 
forval k = 0(4)`tmax'   {
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmin'(1)`wmax'       {
		global placeboafter1 = "$placeboafter1 `K'treat_p`j'"
		global placebocontafter1 = "$placebocontafter1  i.one#c.`K'treat_p`j'"
		}
	}
* 
 
global placebobefore1 " " 


forval k = -`tmin'(4)-8   {
if `k'!=-4{
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmin'(1)`wmax'  {
		global placebobefore1 = "$placebobefore1 i.one#c.`K'treat_p`j'"
	}
	}
	
}
*



global windowpl1 " "
	
	foreach j of numlist  `wmin'(1)`wmax' {
		global windowpl1 = "$windowpl1 i.one#c.windowpl1_p`j' "
	}


*




end




********************************************************************************
************     Do file to define treatment globals         *******************
********************************************************************************

cap program drop placebowindows2
program define placebowindows2
syntax  , Wmax(real) PREVwmax(real) [Tmax(real 16) Tmin(real 12)]  


			********************************************************************
			***********************         Treatment Vector           *********
			********************************************************************

global placeboafter2 " "
global placebobefore2 " "
global placebocontafter2 " "

 local wmin = `prevwmax'  + 1
di "Placebo wmin and wmax should not be too far apart." 
di "`wmax' - `wmin'"
assert `wmax' - `wmin' < 9
local winf = `wmax' + 1
 
forval k = 0(4)`tmax'   {
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmin'(1)`winf'       {
		global placeboafter2 = "$placeboafter2 `K'treat_p`j'"
		global placebocontafter2 = "$placebocontafter2  i.one#c.`K'treat_p`j'"
		}
	}
* 


forval k = -`tmin'(4)-8   {
if `k'!=-4{
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmin'(1)`winf'  {
		global placebobefore2 = "$placebobefore2 i.one#c.`K'treat_p`j'"
	}
	}
	
}
*


global windowpl2 " "
	
	foreach j of numlist  `wmin'(1)`winf' {
		global windowpl2 = "$windowpl2 i.one#c.windowpl2_p`j' "
	}




end

********************************************************************************
*********       Sample correction for placebo estimates        *****************
********************************************************************************			
*We need to create multiple for window variables.			
			
cap program drop placebosamplecorr1
program define placebosamplecorr1
syntax  , WMAx(real) TRuewmax(real)  
			
local wmin = `truewmax' + 1
xtset wagebinstate quarterdate
foreach k of numlist 8(1)`wmax'  {
	cap drop treat_p`k' 
	cap drop _treat_p`k'
	g _treat_p`k'=0
	* replace _treat_p`k' = 1 if (  D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))	
	*	replace _treat_p`k' = 1 if (D.MW_real > 0  & D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))
		replace _treat_p`k' = 1 if $ifclause &  (wageM25>=(MW_realM25+`k' - 0) &  wageM25 < (MW_realM25 +1 +`k' - 0)) & overallcountgroup>0 & fedincrease!=1
	*	replace _treat_p`k' = 1 if (D.MW_real>=0.75  & D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))
	 g byte treat_p`k' = ( _treat_p`k' +  L._treat_p`k' + L2._treat_p`k' + L3._treat_p`k')
	cap drop _treat_p`k'

}
*


foreach j in  4 8 12 16 {
	foreach k of numlist 8(1)`wmax' {
		cap drop F`j'treat_p`k'
		cap drop L`j'treat_p`k'
		
		g byte F`j'treat_p`k' = F`j'.treat_p`k'
 		g byte L`j'treat_p`k' = L`j'.treat_p`k'
		
		replace F`j'treat_p`k' = 0 if  F`j'treat_p`k'==. 
 		replace L`j'treat_p`k' = 0 if  L`j'treat_p`k'==.		

	}
} 
*
foreach pm in p{
	foreach num of numlist `wmin'(1)`wmax'{
		gen windowpl1_`pm'`num' =  F12treat_`pm'`num' +  F8treat_`pm'`num' +  F4treat_`pm'`num' + treat_`pm'`num' + L4treat_`pm'`num'  + L8treat_`pm'`num'  + L12treat_`pm'`num'  + L16treat_`pm'`num'  
	}
}
*

compress

end



cap program drop placebosamplecorr2
program define placebosamplecorr2
syntax  , Wmax(real)  PREVwmax(real)   

local wmin = `prevwmax'  + 1

xtset wagebinstate quarterdate
foreach k of numlist `wmin'(1)`wmax'  {
	cap drop treat_p`k' 
	cap drop _treat_p`k'
	g _treat_p`k'=0
	* replace _treat_p`k' = 1 if (  D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))	
	*	replace _treat_p`k' = 1 if (D.MW_real > 0  & D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))
		replace _treat_p`k' = 1 if $ifclause &  (wageM25>=(MW_realM25+`k' - 0) &  wageM25 < (MW_realM25 +1 +`k' - 0)) & overallcountgroup>0 & fedincrease!=1
	*	replace _treat_p`k' = 1 if (D.MW_real>=0.75  & D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))
	 g byte treat_p`k' = ( _treat_p`k' +  L._treat_p`k' + L2._treat_p`k' + L3._treat_p`k')
	cap drop _treat_p`k'

}
*
foreach j in  4 8 12 16 {
	foreach k of numlist `wmin'(1)`wmax' {
		cap drop F`j'treat_p`k'
		cap drop L`j'treat_p`k'
		
		g byte F`j'treat_p`k' = F`j'.treat_p`k'
 		g byte L`j'treat_p`k' = L`j'.treat_p`k'
		
		replace F`j'treat_p`k' = 0 if  F`j'treat_p`k'==. 
 		replace L`j'treat_p`k' = 0 if  L`j'treat_p`k'==.		
		

	}
} 

foreach pm in p {
	foreach num of numlist `wmin'(1)`wmax'{
		gen windowpl2_`pm'`num' =  F12treat_`pm'`num' +  F8treat_`pm'`num' +  F4treat_`pm'`num' + treat_`pm'`num' + L4treat_`pm'`num'  + L8treat_`pm'`num'  + L12treat_`pm'`num'  + L16treat_`pm'`num'  
	}
}
*Final bin
local winf = `wmax' + 1
foreach k of numlist `winf'  {
	cap drop treat_p`k' 
	cap drop _treat_p`k'
	g byte _treat_p`k'=0
	* replace _treat_p`k' = 1 if (  D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))	
	*	replace _treat_p`k' = 1 if (D.MW_real > 0  & D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))
		replace _treat_p`k' = 1 if $ifclause &  (wageM25>=(MW_realM25+`k' - 0)) & overallcountgroup>0 & fedincrease!=1
	*	replace _treat_p`k' = 1 if (D.MW_real>=0.75  & D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))
	 g byte treat_p`k' = ( _treat_p`k' +  L._treat_p`k' + L2._treat_p`k' + L3._treat_p`k')

}
*
foreach j in  4 8 12 16 {
	foreach k of numlist `winf' {
		cap drop F`j'treat_p`k'
		cap drop L`j'treat_p`k'
		
		g byte F`j'treat_p`k' = F`j'.treat_p`k'
 		g byte L`j'treat_p`k' = L`j'.treat_p`k'
		
		replace F`j'treat_p`k' = 0 if  F`j'treat_p`k'==. 
 		replace L`j'treat_p`k' = 0 if  L`j'treat_p`k'==.		
		

	}
} 

foreach pm in p {
	foreach num of numlist `winf'{
		gen windowpl2_`pm'`num' =  F12treat_`pm'`num' +  F8treat_`pm'`num' +  F4treat_`pm'`num' + treat_`pm'`num' + L4treat_`pm'`num'  + L8treat_`pm'`num'  + L12treat_`pm'`num'  + L16treat_`pm'`num'  
	}
}


*Check here!
bys statenum quarterdate: egen sum_inf = total(_treat_p`winf')
compress


end



********************************************************************************
*******************          Finer treatment window            *****************
********************************************************************************

cap program drop finertreatmentwindow
program define finertreatmentwindow
syntax  [, Tmax(real 16) Tmin(real 12) WMAXcent(real 500) WMINcent(real 400) WINCrement(real 25) ]  

local wmaxcent2 = `wmaxcent' - `wincrement'
local wincrement2 = `wincrement' * 2
			


			********************************************************************
			***********************         Treatment Vector           *********
			********************************************************************

 global treatafterm400 ""
 global treatafterm200 ""
 global treatafterp0 ""
 global treatafterp250 ""

foreach vv in 400 200{
forval k = 0(4)`tmax'   {
	local temp = `vv' -200 + 25
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `vv'(`wincrement')`temp'      {
		global treatafterm`vv' = "${treatafterm`vv'} `K'treat_m`j'"
	}
	}
}
* 

foreach vv in 0 250{
forval k = 0(4)`tmax'   {
	local temp = `vv' +250 - 25
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `vv'(`wincrement')`temp'      {
		global treatafterp`vv' = "${treatafterp`vv'} `K'treat_p`j'"
	}
	}
}
* 

 
global controlafterm400 " " 
global controlafterm200 " " 
global controlafterp0 " " 
global controlafterp250 " " 

foreach vv in 400 200{
forval k = 0(4)`tmax'   {
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	local temp = `vv' -200 + 25
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `vv'(`wincrement')`temp'       {
		global controlafterm`vv' = "${controlafterm`vv'} i.one#c.`K'treat_m`j'"
	}
	}
}
*
foreach vv in 0 250{
forval k = 0(4)`tmax'   {
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	local temp = `vv' + 250 - 25
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `vv'(`wincrement')`temp'       {
		global controlafterp`vv' = "${controlafterp`vv'} i.one#c.`K'treat_p`j'"
	}
	}
}
* 
global controlbefore " "
forval k = -`tmin'(4)-8   {
if `k'!=-4{
	if `k' < 0 {
		local nk = -`k'
		local K  "F`nk'"
	}	
	
	
	if `k' == 0 {
		local K  ""
	}	
	
	if `k' > 0 {
		local K  "L`k'"
	}	
	
	
	foreach j of numlist `wmincent'(`wincrement')`wincrement'       {
		global controlbefore = "$controlbefore i.one#c.`K'treat_m`j'"
	}
	foreach j of numlist  0(`wincrement')`wmaxcent2' {
		global controlbefore = "$controlbefore i.one#c.`K'treat_p`j'"
	}
	}
	
}


global window " "
	
	foreach j of numlist `wmincent'(`wincrement')`wincrement'       {
		global window = "$window i.one##c.window_m`j' "
	}
	foreach j of numlist  0(`wincrement')`wmaxcent2' {
		global window = "$window i.one##c.window_p`j' "
	}



end


********************************************************************************
********************************************************************************
********************************************************************************


cap program drop finertreatmentdefinition
program define finertreatmentdefinition
syntax  [, WMAXcent(real 500) WMINcent(real 400) WINCrement(real 25) ]  

cap drop *treat_*
cap drop window_*
local wmaxcent2 = `wmaxcent' - `wincrement'

xtset wagebinstate quarterdate
forval k = 0(`wincrement')`wmaxcent2'  {
	cap drop treat_p`k' 
	cap drop _treat_p`k'
	g _treat_p`k'=0
	local num = `k'/100
	* replace _treat_p`k' = 1 if (  D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))	
	*	replace _treat_p`k' = 1 if (D.MW_real > 0  & D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))
		replace _treat_p`k' = 1 if $ifclause &  (wageM25>=(MW_realM25+`num' - 0.25) &  wageM25 < (MW_realM25 + (`wincrement'/100) +`num' - 0.25)) & overallcountgroup>0 & fedincrease!=1
	*	replace _treat_p`k' = 1 if (D.MW_real>=0.75  & D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))
	 g byte treat_p`k' = ( _treat_p`k' +  L._treat_p`k' + L2._treat_p`k' + L3._treat_p`k')
	cap drop _treat_p`k'

}

forval k = `wincrement'(`wincrement')`wmincent'  {
	cap drop treat_m`k'
	cap drop _treat_m`k'
	g _treat_m`k'=0
		local num = `k'/100
	*	replace _treat_m`k' = 1 if ( D.MW_real<.& D.MW>0) & (wageM25<(MW_realM25 - `k' + 1)  &  wageM25 >= (MW_realM25 -`k' ))
	*	replace _treat_m`k' = 1 if(D.MW_real> 0  & D.MW_real<.& D.MW>0) & (wageM25<(MW_realM25 - `k' + 1)  &  wageM25 >= (MW_realM25 -`k' ))
		replace _treat_m`k' = 1 if $ifclause & (wageM25<(MW_realM25 - `num' + (`wincrement'/100) -0.25)  &  wageM25 >= (MW_realM25 -`num' -0.25 ))  & overallcountgroup>0 & fedincrease!=1
	*	replace _treat_m`k' = 1 if(D.MW_real>=0.75  & D.MW_real<.& D.MW>0) & (wageM25<(MW_realM25 - `k' + 1)  &  wageM25 >= (MW_realM25 -`k' ))
	g byte treat_m`k' = ( _treat_m`k' + L._treat_m`k' + L2._treat_m`k' + L3._treat_m`k' )
cap drop _treat_m`k'
}  
*

 cap drop  Dtreat_p0
 g byte Dtreat_p0 = D.treat_p0 

 
 foreach j in  4 8 12 16 {
	foreach k in  p {
		forval l =0(`wincrement')`wmaxcent2'{ 

		cap drop F`j'treat_`k'`l'
		cap drop L`j'treat_`k'`l'
		
		g byte F`j'treat_`k'`l' = F`j'.treat_`k'`l'
 		g byte L`j'treat_`k'`l' = L`j'.treat_`k'`l'
		}
	}
} 
*
 foreach j in  4 8 12 16 {
	foreach k in  m {
		forval l = `wincrement'(`wincrement')`wmincent'{ 

		cap drop F`j'treat_`k'`l'
		cap drop L`j'treat_`k'`l'
		
		g byte F`j'treat_`k'`l' = F`j'.treat_`k'`l'
 		g byte L`j'treat_`k'`l' = L`j'.treat_`k'`l'
		}
	}
} 
* 


foreach pm in p {
		forval num =0(`wincrement')`wmaxcent2'{ 
		cap drop  window_`pm'`num' 
		gen window_`pm'`num' =  F12treat_`pm'`num' +  F8treat_`pm'`num' +  F4treat_`pm'`num' + treat_`pm'`num' + L4treat_`pm'`num'  + L8treat_`pm'`num'  + L12treat_`pm'`num'  + L16treat_`pm'`num'  
	}
}
foreach pm in m {
		forval num = `wincrement'(`wincrement')`wmincent'{ 
		cap drop  window_`pm'`num' 
		gen window_`pm'`num' =  F12treat_`pm'`num' +  F8treat_`pm'`num' +  F4treat_`pm'`num' + treat_`pm'`num' + L4treat_`pm'`num'  + L8treat_`pm'`num'  + L12treat_`pm'`num'  + L16treat_`pm'`num'  
	}
}
*
compress

end



********************************************************************************
********************************************************************************
********************************************************************************


cap program drop finertreatmentdefinitionexact
program define finertreatmentdefinitionexact
syntax  [, WMAXcent(real 500) WMINcent(real 400) WINCrement(real 25) ]  

cap drop *treat_*
cap drop window_*
local wmaxcent2 = `wmaxcent' - `wincrement'

xtset wagebinstate quarterdate
forval k = 0(`wincrement')`wmaxcent2'  {
	cap drop treat_p`k' 
	cap drop _treat_p`k'
	g _treat_p`k'=0
	local num = `k'/100
	* replace _treat_p`k' = 1 if (  D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))	
	*	replace _treat_p`k' = 1 if (D.MW_real > 0  & D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))
		replace _treat_p`k' = 1 if $ifclause &  (wageM25>=(MW_realM25+`num' ) &  wageM25 < (MW_realM25 + (`wincrement'/100) +`num' )) & overallcountgroup>0 & fedincrease!=1
	*	replace _treat_p`k' = 1 if (D.MW_real>=0.75  & D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))
	 g byte treat_p`k' = ( _treat_p`k' +  L._treat_p`k' + L2._treat_p`k' + L3._treat_p`k')
	cap drop _treat_p`k'

}

forval k = `wincrement'(`wincrement')`wmincent'  {
	cap drop treat_m`k'
	cap drop _treat_m`k'
	g _treat_m`k'=0
		local num = `k'/100
	*	replace _treat_m`k' = 1 if ( D.MW_real<.& D.MW>0) & (wageM25<(MW_realM25 - `k' + 1)  &  wageM25 >= (MW_realM25 -`k' ))
	*	replace _treat_m`k' = 1 if(D.MW_real> 0  & D.MW_real<.& D.MW>0) & (wageM25<(MW_realM25 - `k' + 1)  &  wageM25 >= (MW_realM25 -`k' ))
		replace _treat_m`k' = 1 if $ifclause & (wageM25<(MW_realM25 - `num' + (`wincrement'/100))  &  wageM25 >= (MW_realM25 -`num' ))  & overallcountgroup>0 & fedincrease!=1
	*	replace _treat_m`k' = 1 if(D.MW_real>=0.75  & D.MW_real<.& D.MW>0) & (wageM25<(MW_realM25 - `k' + 1)  &  wageM25 >= (MW_realM25 -`k' ))
	g byte treat_m`k' = ( _treat_m`k' + L._treat_m`k' + L2._treat_m`k' + L3._treat_m`k' )
cap drop _treat_m`k'
}  
*

 cap drop  Dtreat_p0
 g byte Dtreat_p0 = D.treat_p0 

 
 foreach j in  4 8 12 16 {
	foreach k in  p {
		forval l =0(`wincrement')`wmaxcent2'{ 

		cap drop F`j'treat_`k'`l'
		cap drop L`j'treat_`k'`l'
		
		g byte F`j'treat_`k'`l' = F`j'.treat_`k'`l'
 		g byte L`j'treat_`k'`l' = L`j'.treat_`k'`l'
		replace F`j'treat_`k'`l' = 0 if F`j'treat_`k'`l'  == .
 		replace L`j'treat_`k'`l' = 0 if  L`j'treat_`k'`l' == .
		
		}
	}
} 
*
 foreach j in  4 8 12 16 {
	foreach k in  m {
		forval l = `wincrement'(`wincrement')`wmincent'{ 

		cap drop F`j'treat_`k'`l'
		cap drop L`j'treat_`k'`l'
		
		g byte F`j'treat_`k'`l' = F`j'.treat_`k'`l'
 		g byte L`j'treat_`k'`l' = L`j'.treat_`k'`l'
		replace F`j'treat_`k'`l' = 0 if F`j'treat_`k'`l'  == .
 		replace L`j'treat_`k'`l' = 0 if  L`j'treat_`k'`l' == .
		
		}
	}
} 
* 


foreach pm in p {
		forval num =0(`wincrement')`wmaxcent2'{ 
		cap drop  window_`pm'`num' 
		gen window_`pm'`num' =  F12treat_`pm'`num' +  F8treat_`pm'`num' +  F4treat_`pm'`num' + treat_`pm'`num' + L4treat_`pm'`num'  + L8treat_`pm'`num'  + L12treat_`pm'`num'  + L16treat_`pm'`num'  
	}
}
foreach pm in m {
		forval num = `wincrement'(`wincrement')`wmincent'{ 
		cap drop  window_`pm'`num' 
		gen window_`pm'`num' =  F12treat_`pm'`num' +  F8treat_`pm'`num' +  F4treat_`pm'`num' + treat_`pm'`num' + L4treat_`pm'`num'  + L8treat_`pm'`num'  + L12treat_`pm'`num'  + L16treat_`pm'`num'  
	}
}
*
compress

end



********************************************************************************
********************************************************************************
********************************************************************************


cap program drop finertreatmentdefexact_long
program define finertreatmentdefexact_long
syntax  [, WMINcent(real 400) WINCrement(real 25) ]  

cap drop *treat_*
cap drop window_*
local wmaxcent2 = 1675

xtset wagebinstate quarterdate
forval k = 0(`wincrement')`wmaxcent2'  {
	cap drop treat_p`k' 
	cap drop _treat_p`k'
	g _treat_p`k'=0
	local num = `k'/100
	* replace _treat_p`k' = 1 if (  D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))	
	*	replace _treat_p`k' = 1 if (D.MW_real > 0  & D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))
		replace _treat_p`k' = 1 if $ifclause &  (wageM25>=(MW_realM25+`num' ) &  wageM25 < (MW_realM25 + (`wincrement'/100) +`num' )) & overallcountgroup>0 & fedincrease!=1
	*	replace _treat_p`k' = 1 if (D.MW_real>=0.75  & D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))
	 g byte treat_p`k' = ( _treat_p`k' +  L._treat_p`k' + L2._treat_p`k' + L3._treat_p`k')
	cap drop _treat_p`k'

}

forval k = `wincrement'(`wincrement')`wmincent'  {
	cap drop treat_m`k'
	cap drop _treat_m`k'
	g _treat_m`k'=0
		local num = `k'/100
	*	replace _treat_m`k' = 1 if ( D.MW_real<.& D.MW>0) & (wageM25<(MW_realM25 - `k' + 1)  &  wageM25 >= (MW_realM25 -`k' ))
	*	replace _treat_m`k' = 1 if(D.MW_real> 0  & D.MW_real<.& D.MW>0) & (wageM25<(MW_realM25 - `k' + 1)  &  wageM25 >= (MW_realM25 -`k' ))
		replace _treat_m`k' = 1 if $ifclause & (wageM25<(MW_realM25 - `num' + (`wincrement'/100))  &  wageM25 >= (MW_realM25 -`num' ))  & overallcountgroup>0 & fedincrease!=1
	*	replace _treat_m`k' = 1 if(D.MW_real>=0.75  & D.MW_real<.& D.MW>0) & (wageM25<(MW_realM25 - `k' + 1)  &  wageM25 >= (MW_realM25 -`k' ))
	g byte treat_m`k' = ( _treat_m`k' + L._treat_m`k' + L2._treat_m`k' + L3._treat_m`k' )
cap drop _treat_m`k'
}  
*

 cap drop  Dtreat_p0
 g byte Dtreat_p0 = D.treat_p0 

 
 
 
 
 foreach j in  4 8 12 16 {
	foreach k in  p {
		forval l =0(`wincrement')`wmaxcent2'{ 

		cap drop F`j'treat_`k'`l'
		cap drop L`j'treat_`k'`l'
		
		g byte F`j'treat_`k'`l' = F`j'.treat_`k'`l'
 		g byte L`j'treat_`k'`l' = L`j'.treat_`k'`l'
		replace F`j'treat_`k'`l' = 0 if F`j'treat_`k'`l'  == .
 		replace L`j'treat_`k'`l' = 0 if  L`j'treat_`k'`l' == .
		}
	}
} 
*

 foreach j in  4 8 12 16 {
	foreach k in  m {
		forval l = `wincrement'(`wincrement')`wmincent'{ 

		cap drop F`j'treat_`k'`l'
		cap drop L`j'treat_`k'`l'
		
		g byte F`j'treat_`k'`l' = F`j'.treat_`k'`l'
 		g byte L`j'treat_`k'`l' = L`j'.treat_`k'`l'
		replace F`j'treat_`k'`l' = 0 if F`j'treat_`k'`l' == .
 		replace L`j'treat_`k'`l' = 0 if L`j'treat_`k'`l' == .

		}
	}
} 
* 


foreach pm in p {
		forval num =0(`wincrement')`wmaxcent2'{ 
		cap drop  window_`pm'`num' 
		gen byte window_`pm'`num' =  F12treat_`pm'`num' +  F8treat_`pm'`num' +  F4treat_`pm'`num' + treat_`pm'`num' + L4treat_`pm'`num'  + L8treat_`pm'`num'  + L12treat_`pm'`num'  + L16treat_`pm'`num'  
	}
}
foreach pm in m {
		forval num = `wincrement'(`wincrement')`wmincent'{ 
		cap drop  window_`pm'`num' 
		gen byte window_`pm'`num' =  F12treat_`pm'`num' +  F8treat_`pm'`num' +  F4treat_`pm'`num' + treat_`pm'`num' + L4treat_`pm'`num'  + L8treat_`pm'`num'  + L12treat_`pm'`num'  + L16treat_`pm'`num'  
	}
}
*



*Final bin
local winf = 1700
foreach k of numlist `winf'  {
	cap drop treat_p`k' 
	cap drop _treat_p`k'
	g byte _treat_p`k'=0
	* replace _treat_p`k' = 1 if (  D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))	
	*	replace _treat_p`k' = 1 if (D.MW_real > 0  & D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))
		replace _treat_p`k' = 1 if $ifclause &  (wageM25>=(MW_realM25+(`k'/100) - 0)) & overallcountgroup>0 & fedincrease!=1
	*	replace _treat_p`k' = 1 if (D.MW_real>=0.75  & D.MW_real<. & D.MW>0) &  (wageM25>=(MW_realM25+`k') &  wageM25 < (MW_realM25 +1 +`k'))
	 g byte treat_p`k' = ( _treat_p`k' +  L._treat_p`k' + L2._treat_p`k' + L3._treat_p`k')

}
*
foreach j in  4 8 12 16 {
	foreach k of numlist `winf' {
		cap drop F`j'treat_p`k'
		cap drop L`j'treat_p`k'
		
		g byte F`j'treat_p`k' = F`j'.treat_p`k'
 		g byte L`j'treat_p`k' = L`j'.treat_p`k'
		
		replace F`j'treat_p`k' = 0 if  F`j'treat_p`k'==. 
 		replace L`j'treat_p`k' = 0 if  L`j'treat_p`k'==.		
		

	}
} 

foreach pm in p {
	foreach num of numlist `winf'{
		gen window_`pm'`num' =  F12treat_`pm'`num' +  F8treat_`pm'`num' +  F4treat_`pm'`num' + treat_`pm'`num' + L4treat_`pm'`num'  + L8treat_`pm'`num'  + L12treat_`pm'`num'  + L16treat_`pm'`num'  
	}
}



bys statenum quarterdate: egen sum_inf = total(_treat_p`winf')
compress


end



********************************************************************************
********************************************************************************
********************************************************************************

cap program drop finerabovebelowbunch
program define finerabovebelowbunch
syntax [,Tmin(real 12) Tmax(real 16) WMAXcent(real 500) WMINcent(real 400) WINCrement(real 25) ]

local wmaxcent2 = `wmaxcent' - `wincrement'
local wincrement2 = `wincrement' * 2

foreach t of numlist -`tmin'(4)`tmax' {

	if `t' < -4  {	
		local nt = -`t' 
		global aboveF`nt' "(_b[F`nt'treat_p0] )"
		forval j = `wincrement'(`wincrement')`wmaxcent2' {
			global aboveF`nt' "${aboveF`nt'} + (_b[F`nt'treat_p`j'] )"
		}
		global belowF`nt' "(_b[F`nt'treat_m`wincrement'] )"
		forval j = `wincrement2'(`wincrement')`wmincent' {
			global belowF`nt' "${belowF`nt'} + (_b[F`nt'treat_m`j'] )"
		}

    }
 
	if `t' == -4 {
		global aboveF4 "0"
		global belowF4 "0"
	}
 
	if `t' == 0 {	
		 
		global aboveL`t' "(_b[treat_p0])"
		forval j = `wincrement'(`wincrement')`wmaxcent2'  {
			global aboveL`t' "${aboveL`t'} + (_b[treat_p`j'] )"
		}
		global belowL`t' "(_b[treat_m`wincrement'] )"
		forval j = `wincrement2'(`wincrement')`wmincent'  {
			global belowL`t' "${belowL`t'} + (_b[treat_m`j'] )"
		}

    }
	
   if `t' > 0 {	
		 
		global aboveL`t' "(_b[L`t'treat_p0])"
		forval j = `wincrement'(`wincrement')`wmaxcent2'  {
			global aboveL`t' "${aboveL`t'} + (_b[L`t'treat_p`j'])"
		}
		global belowL`t' "(_b[L`t'treat_m`wincrement'])"
		forval j = `wincrement2'(`wincrement')`wmincent' {
			global belowL`t' "${belowL`t'} + (_b[L`t'treat_m`j'])"
		}

    }
	
 
 } 
 *
end



cap program drop finerabovebelowWBbunch
program define finerabovebelowWBbunch
syntax , Wagemult(real) [Tmin(real 12) Tmax(real 16)  Wmaxcent(real 500) Wmincent(real 400) WINCrement(real 25)]

local wmaxcent2 = `wmaxcent' - `wincrement'
local wincrement2 = `wincrement' * 2



				foreach t of numlist -`tmin'(4)`tmax' {
				
						if `t' < -4  {	
							local nt = -`t' 
							global aboveWBF`nt' "((_b[F`nt'treat_p0])*(`wagemult'+0) )"
						forval j = `wincrement'(`wincrement')`wmaxcent2' {
						local k = `j'/100
							global aboveWBF`nt' "${aboveWBF`nt'} + ((_b[F`nt'treat_p`j'])*(`wagemult'+`k') )"
						}
						global belowWBF`nt' "((_b[F`nt'treat_m`wincrement'])*(`wagemult'-(`wincrement'/100)) )"
						forval j = `wincrement2'(`wincrement')`wmincent' {
							local k = `j'/100
							global belowWBF`nt' "${belowWBF`nt'} + ((_b[F`nt'treat_m`j'])*(`wagemult'-`k') )"
						}

					}

				

					if `t' == 0 {	
		 
						global aboveWBL`t' "((_b[treat_p0])*(`wagemult'+0))"
						forval j = `wincrement'(`wincrement')`wmaxcent2' {
						local k = `j'/100
							global aboveWBL`t' "${aboveWBL`t'} + ((_b[treat_p`j'])*(`wagemult'+`k') )"
						}
						global belowWBL`t' "((_b[treat_m`wincrement'])*(`wagemult'-(`wincrement'/100)) )"
						forval j = `wincrement2'(`wincrement')`wmincent'  {
							local k = `j'/100
							global belowWBL`t' "${belowWBL`t'} + ((_b[treat_m`j'])*(`wagemult'-`k') )"
						}

					}
	
					if `t' > 0 {	
		 
						global aboveWBL`t' "((_b[L`t'treat_p0])*(`wagemult'+0))"
						forval j = `wincrement'(`wincrement')`wmaxcent2' {
						local k = `j'/100
							global aboveWBL`t' "${aboveWBL`t'} + ((_b[L`t'treat_p`j'])*(`wagemult'+`k'))"
						}
						global belowWBL`t' "((_b[L`t'treat_m`wincrement'])*(`wagemult'-(`wincrement'/100)) )"
						forval j = `wincrement2'(`wincrement')`wmincent'  {
							local k = `j'/100
							global belowWBL`t' "${belowWBL`t'} + ((_b[L`t'treat_m`j'])*(`wagemult'-`k'))"
						}

					}
	
 
			} 
 			global aboveWBF4 = 0
 			global belowWBF4 = 0
			
			
*
end


********************************************************************************
********************************************************************************
********************************************************************************

cap program drop create_data_reweight
program define create_data_reweight
syntax  [,  WMAXcent(real 825) WMINcent(real 400) BELOWerror(string)]  

local QQtipnotipQQ1 "real"
local QQtipnotipQQ2 "real_notip"

foreach num of numlist 1 2 {

use "${data}cps77_`QQtipnotipQQ`num''.dta", clear

local realmw_bin = 850
gen wagebins_relMW = wagebins - `realmw_bin'

collapse (sum) w_count er_count, by(wagebins_relMW wagebins)

foreach vv of varlist *count{
egen `vv'all = total(`vv') if wagebins_relMW>=-`wmincent' & wagebins_relMW<`wmaxcent'
gen `vv'_sh = `vv' / `vv'all
}
*


lowess  er_count_sh wagebins_relMW if wagebins_relMW<0 & wagebins_relMW>=-`wmincent' , nograph gen(er_1) 
lowess  er_count_sh wagebins_relMW if wagebins_relMW>0 & wagebins_relMW< `wmaxcent' , nograph gen(er_2)
sum er_count_sh   if wagebins_relMW==0  
assert r(N)==1

gen lowess_er_count_sh = er_1
replace lowess_er_count_sh = er_2 		if lowess_er_count_sh== . 
replace lowess_er_count_sh = r(mean)  	if wagebins_relMW == 0



lowess  w_count_sh wagebins_relMW if wagebins_relMW<0  & wagebins_relMW>=-`wmincent'  , nograph gen(w_1) 
lowess  w_count_sh wagebins_relMW if wagebins_relMW>0  & wagebins_relMW< `wmaxcent'   , nograph gen(w_2)
sum w_count_sh   if wagebins_relMW==0  
assert r(N)==1

gen lowess_w_count_sh = w_1
replace lowess_w_count_sh = w_2 		if lowess_w_count_sh== . 
replace lowess_w_count_sh = r(mean)  	if wagebins_relMW == 0

*They both should sum up to 1 (now, they are very close to 1, but not exactly 1).
foreach vv of varlist lowess_er_count_sh lowess_w_count_sh{
sum `vv', meanonly
replace `vv'=`vv'/r(sum)
}
*

gen cum_lowess_er_count_sh= sum(lowess_er_count_sh) if lowess_er_count_sh!=.
gen cum_lowess_w_count_sh = sum(lowess_w_count_sh)	if lowess_w_count_sh!=.
gen rho = lowess_er_count_sh / lowess_w_count_sh
gen cum_rho = cum_lowess_er_count_sh/cum_lowess_w_count_sh

keep wagebins_relMW rho cum_rho

if `num' == 2{
rename rho rho_notip 
rename cum_rho cum_rho_notip
}
tempfile reweight_`QQtipnotipQQ`num''
save `reweight_`QQtipnotipQQ`num'''
}

use `reweight_real', clear
merge 1:1 wagebins_relMW using `reweight_real_notip', assert(3) nogenerate

save "${data}data_reweight.dta", replace

end



********************************************************************************
********************************************************************************
********************************************************************************


cap program drop reweight_data
program define reweight_data
syntax  [,  WMAXcent(real 825) WMINcent(real 400) BELOWerror(string)]  
cap drop wagebins_relMW
qui gen wagebins_relMW = wagebins - (MW_realM25*100)
qui merge m:1 wagebins_relMW using "${data}data_reweight.dta", keep(1 3) nogenerate
qui levelsof statenum, local(temploc1)
qui levelsof quarterdate, local(temploc2)
cap drop temp
qui gen temp=.
*if "`belowerror'" == ""{
foreach ll1 of local temploc1{
	foreach ll2 of local temploc2{
	sum wagebins_relMW, meanonly, if statenum==`ll1' & quarterdate==`ll2' 
	if `ll1' == 2 |  `ll1' == 30 |  `ll1' == 6 |  `ll1' == 27 |  `ll1' == 32 |  `ll1' == 41 |  `ll1' == 53 {
		if r(min)>-`wmincent'{
			qui replace rho_notip = cum_rho_notip if  statenum==`ll1' & quarterdate==`ll2'  & wagebins_relMW==r(min)
		}
		sum count if statenum==`ll1' & quarterdate==`ll2' & wagebins_relMW>=-`wmincent' & wagebins_relMW<`wmaxcent', meanonly
		scalar countall_window = r(sum)
		qui replace temp =count*rho_notip if statenum==`ll1' & quarterdate==`ll2' & wagebins_relMW>=-`wmincent' & wagebins_relMW<`wmaxcent'
		sum temp if statenum==`ll1' & quarterdate==`ll2' & wagebins_relMW>=-`wmincent' & wagebins_relMW<`wmaxcent', meanonly
		qui replace temp=temp*(`=countall_window' / r(sum) ) if statenum==`ll1' & quarterdate==`ll2' & wagebins_relMW>=-`wmincent' & wagebins_relMW<`wmaxcent'	

	*Sanity check
	assert `ll1'!=1
	
	}
	else{
		if r(min)>-`wmincent'{
			qui replace rho = cum_rho if  statenum==`ll1' & quarterdate==`ll2'  & wagebins_relMW==r(min)
		}
		sum count if statenum==`ll1' & quarterdate==`ll2' & wagebins_relMW>=-`wmincent' & wagebins_relMW<`wmaxcent', meanonly
		scalar countall_window = r(sum)
		qui replace temp =count*rho if statenum==`ll1' & quarterdate==`ll2' & wagebins_relMW>=-`wmincent' & wagebins_relMW<`wmaxcent'
		sum temp if statenum==`ll1' & quarterdate==`ll2' & wagebins_relMW>=-`wmincent' & wagebins_relMW<`wmaxcent', meanonly
		qui replace temp=temp*(`=countall_window' / r(sum) ) if statenum==`ll1' & quarterdate==`ll2' & wagebins_relMW>=-`wmincent' & wagebins_relMW<`wmaxcent'	

	*Sanity check
	assert `ll1'!=2
		
		
		}
	
	
	}
}
*}
/*
else{
foreach ll1 of local temploc1{
	foreach ll2 of local temploc2{

sum count if statenum==`ll1' & quarterdate==`ll2' & wagebins_relMW<0, meanonly
qui replace count = r(sum) if statenum==`ll1' & quarterdate==`ll2' & wagebins_relMW==0
qui replace count = 0 if statenum==`ll1' & quarterdate==`ll2' & wagebins_relMW<0

sum count if statenum==`ll1' & quarterdate==`ll2' & wagebins_relMW>=-`wmincent' & wagebins_relMW<`wmaxcent', meanonly
qui replace temp=r(sum)*shares_relMW if statenum==`ll1' & quarterdate==`ll2' & wagebins_relMW>=-`wmincent' & wagebins_relMW<`wmaxcent'
}
}
}
*
*/
cap drop temp1_count_all
cap drop temp2_count_all

qui bys statenum quarterdate:egen temp1_count_all=total(count) if wagebins_relMW>=-`wmincent' & wagebins_relMW<`wmaxcent'
qui bys statenum quarterdate:egen temp2_count_all=total(temp) if wagebins_relMW>=-`wmincent' & wagebins_relMW<`wmaxcent'
qui gen sanitycheck = abs(temp1_count_all - temp2_count_all)
qui assert sanitycheck<1  if sanitycheck!=.
qui replace count = temp if temp!=.
cap drop temp1_count_all
cap drop temp2_count_all
cap drop temp
cap drop wagebins_relMW
cap drop shares_relMW
cap drop cum_shares_relMW
cap drop sanitycheck
cap drop rho
cap drop cum_rho
cap drop rho_notip
cap drop cum_rho_notip

end 
