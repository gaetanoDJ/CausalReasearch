
*This do file is created by DC on 09/07/2017.
********************************************************************************
************        Wage estimates using alternative methods         ***********
********************************************************************************

do "${dofiles}wage_estimate_incum_new_sr.do"
do "${dofiles}wage_estimate_base.do"
do "${dofiles}wage_estimate_demog_groups.do"
do "${dofiles}wage_estimate_ind_groups.do"


 
			local mgroups mgroups("\% $ \Delta $ affected wage" "Spillover share of wage increase" , pattern(1 0 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(l{0.3cm}r{0.3cm}){@span}) ) ///
			mlabels("\% $ \Delta $ W" " $ \% \Delta W_{\text{No spillover}} $ " " $ \frac{\%\Delta W - \% \Delta W_{\text{No spillover}}}{\% \Delta W} $ "  , ///
			 prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat( \cmidrule(l{0.2cm}r{0.2cm}){@span}))


esttab all_2 all_3 all_spill ///
using "${tables}Table4.tex", ///
replace stats(WB_Eb WB_Ese blankspace  blankspace , label("Overall" " " " " " ")) ///
nodep nogaps cells(none) nomtitles fragment booktabs nonumbers nolines ///
`mgroups'

foreach vv in HSD teen  HSL female BH {
if "`vv'"!="BH"{
local stats WB_Eb WB_Ese 
}
else{
local stats WB_Eb WB_Ese blankspace blankspace
}
esttab `vv'_2 `vv'_3 `vv'_spill ///
using "${tables}Table4.tex", ///
append stats(`stats' , label("`vv'" " " " " " ")) ///
nodep nogaps cells(none) nomtitles fragment booktabs nonumbers nolines 
}
*


foreach vv in m1 m2  {
if "`vv'"!="m2"{
local stats WB_Eb WB_Ese 
}
else{
local stats WB_Eb WB_Ese blankspace blankspace
}
esttab `vv'_2 `vv'_3 `vv'_spill ///
using "${tables}Table4.tex", ///
append stats(`stats' , label("`vv'" " " " " " ")) ///
nodep nogaps cells(none) nomtitles fragment booktabs nonumbers nolines 
}
*

