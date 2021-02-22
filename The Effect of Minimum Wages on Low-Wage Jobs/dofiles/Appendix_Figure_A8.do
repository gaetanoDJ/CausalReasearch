
*This do file is created by DC on 07/11/2016.

do "${dofiles}grouplevelanalysis_new_QJE.do"

clear

foreach name in base groups moregroups CKgroups{
do "${dofiles}Figure8_`name'_est.do"
}
*

clear
svmat forfigure
rename forfigure1 chabovesh
rename forfigure2 chbelowsh



gen group = "."
local n : word count $lastcol
di "`n'"
forval i = 1/`n' {
	local a : word `i' of $lastcol
	replace group="`a'" in `i'
}
*
save "${data}FigureA8data_new2.dta",replace

use "${data}FigureA8data_new2.dta", clear
gen chemp = chabovesh - chbelowsh
keep if strpos(group, "hsd") == 1 |  strpos(group, "hsg") == 1 |  strpos(group, "sc") == 1 |  strpos(group, "coll") == 1
drop if group=="coll0020"


foreach vv in chabovesh chbelowsh{
local counter = `counter' + 1
sum `vv' ,meanonly
scalar low`counter' = r(min)
scalar up`counter' = r(max)
}
*

if `=low1'<`=low2'{
local lowrange = low1
}
else{
local lowrange = low2
}
if `=up1'<`=up2'{
local uprange = up2
}
else{
local uprange = up1
}

cap drop wide
cap drop groupnum
gen wide=1
egen groupnum=group(group)
sort groupnum
mkmat chabovesh chbelowsh, mat(forfigure)
rename forfigure3 belowshare
reshape wide chabovesh chbelowsh group belowshare chemp ,i(wide) j(groupnum)




svmat forfigure
reg forfigure1 forfigure2
local slope_fit: di %04.3f _b[forfigure2]
local se_fit: di %04.3f _se[forfigure2]
binscatter forfigure1 forfigure2 , nq(8)  


local mlabsize small
local bmlabsize vlarge


replace group4  = "Coll; [50, 60)"
replace group6  = "LTHS; [16, 20)"
replace group7  = "LTHS; [20, 30)"
replace group8  = "LTHS; [30, 40)"
replace group9  = "LTHS; [40, 50)"
replace group10 = "LTHS; [50, 60)"
replace group11 = "LTHS; 60+"
replace group12 = "HSG; [16, 20)"
replace group18 = "SC; [16, 20)"
replace group19 = "SC; [20, 30)"

local mlabcolorsize mlabcolor(black) mlabsize(medsmall)

twoway (scatter chabovesh6 chbelowsh6,   `mlabcolorsize'  msize(`mlabsize') mcolor(gray*0.5)  mlabposition(9) mlab(group6) mcolor(black) /*text(0.19 0.022 "Slope = 1.164" ,box fcolor(none) margin(small))*/) ///
(scatter chabovesh7 chbelowsh7, `mlabcolorsize' mlabposition(9) mlab(group7) msize(`mlabsize') mcolor(black)) ///
(scatter chabovesh8 chbelowsh8,  mlabposition(9) `mlabcolorsize' mlab(group8)  msize(`mlabsize') mcolor(black)) ///
(scatter chabovesh9 chbelowsh9,  mlabposition(3)  `mlabcolorsize' mlab(group9)  msize(`mlabsize') mcolor(black)) ///
(scatter chabovesh10 chbelowsh10,  mlabposition(9) `mlabcolorsize'  mlab(group10)  msize(`mlabsize') mcolor(black)) ///
(scatter chabovesh11 chbelowsh11, mlabposition(9) `mlabcolorsize' mlab(group11)  msize(`mlabsize') mcolor(black)) ///
(scatter chabovesh12 chbelowsh12,  mlabposition(9) `mlabcolorsize' mlab(group12) msize(`mlabsize') mcolor(black)) ///
(scatter chabovesh13 chbelowsh13,  msize(`mlabsize') mcolor(gray*0.5)) ///
(scatter chabovesh14 chbelowsh14,  msize(`mlabsize') mcolor(gray*0.5)) ///
(scatter chabovesh15 chbelowsh15,  msize(`mlabsize') mcolor(gray*0.5)) ///
(scatter chabovesh16 chbelowsh16,  msize(`mlabsize') mcolor(gray*0.5)) ///
(scatter chabovesh17 chbelowsh17,  msize(`mlabsize') mcolor(gray*0.5)) ///
(scatter chabovesh18 chbelowsh18, mlab(group18) `mlabcolorsize' msize(`mlabsize') mcolor(black)) ///
(scatter chabovesh19 chbelowsh19, mlab(group19) `mlabcolorsize' msize(`mlabsize') mcolor(black)) ///
(scatter chabovesh20 chbelowsh20,  msize(`mlabsize') mcolor(gray*0.5)) ///
(scatter chabovesh21 chbelowsh21,  msize(`mlabsize') mcolor(gray*0.5)) ///
(scatter chabovesh22 chbelowsh22,  msize(`mlabsize') mcolor(gray*0.5)) ///
(scatter chabovesh23 chbelowsh23,  msize(`mlabsize') mcolor(gray*0.5)) ///
(scatter chabovesh1 chbelowsh1,  msize(`mlabsize') mcolor(gray*0.5)) ///
(scatter chabovesh2 chbelowsh2, msize(`mlabsize') mcolor(gray*0.5)) ///
(scatter chabovesh3 chbelowsh3,  msize(`mlabsize') mcolor(gray*0.5)) ///
(scatter chabovesh4 chbelowsh4,  `mlabcolorsize' mlab(group4) msize(`mlabsize') mcolor(black)) ///
(scatter chabovesh5 chbelowsh5,  msize(`mlabsize') mcolor(gray*0.5))  ///
(scatteri -.00076353813832 .0021545001848911 .0080677356260518 .0042668418027461 .0063289176032413 .0067334366030991 .0085774615872651 .0085344494630893 .0285693503295382 .0213541354363163 .0425982661545277 .0365639192362626 .0596072919045885 .0647201538085938 .1051569283008575 .1009243801236153 ///
, ///
mcolor(navy) msymbol(S) msize(`bmlabsize')) ///
(lfit forfigure1 forfigure2 ,lcolor(maroon) range(0 0.12) lwidth(thick) ) ///
(function y=x , range(0 0.12) lcolor(black) lpattern(dash)) ,  ///
graphregion(color(white)) ytitle("Excess jobs relative to the pre-treatment total employment ({&Delta}a)", height(4.5) size(small)) xtitle("Magnitude of missing jobs relative to the pre-treatment total employment ({&Delta}b)", size(medsmall)) yscale(range(0 0.12)) ylabel(0(0.04)0.12) xscale(range(0 0.12)) xlabel(0(0.04)0.12) ///
 legend(order(25 "Linear fit" 26 "45 degree line")) ///
text(0.12 0.0145 "Slope = `slope_fit' (`se_fit')", box  fcolor(white) margin(0.2 0.2 1 1) justification(right) )



graph export "${figures}FigureA8_b.pdf", replace


use "${data}FigureA8data_new2.dta", clear
gen chemp = chabovesh - chbelowsh
rename forfigure3 belowshare


drop if strpos(group, "hsd") == 1 |  strpos(group, "hsg") == 1 |  strpos(group, "sc") == 1 |  strpos(group, "coll") == 1
drop if group=="overall"
sort group


gen label1 = "B/H"
gen label2 = "LTHS"
gen label3 = "HSL"
gen label4 = "Women"
gen label5 = "Low prob."
gen label6 = "High prob."
gen label7 = "Medium prob."
gen label8 = "Teen"

local mlabsize large


twoway (scatter chabovesh chbelowsh if _n==1, msize(vlarge) msymbol(O) mlabel(label1) mlabsize(`mlabsize') /*text(0.24 0.022 "Slope = 1.214" ,box fcolor(none) margin(small))*/ ) ///
(scatter chabovesh chbelowsh if _n==2, msymbol(D)  msize(vlarge) mlabel(label2) mlabposition(9) mlabsize(`mlabsize')) ///
(scatter chabovesh chbelowsh if _n==3, msymbol(D)  msize(vlarge) mlabel(label3) mlabposition(11) mlabsize(`mlabsize')) ///
(scatter chabovesh chbelowsh if _n==4, msymbol(S)  msize(vlarge) mlabel(label4) mlabposition(10) mlabsize(`mlabsize')) ///
(scatter chabovesh chbelowsh if _n==5, msymbol(S)  msize(vlarge) mlabel(label5) mlabposition(3) mlabsize(`mlabsize')) ///
(scatter chabovesh chbelowsh if _n==6, msymbol(T)  msize(vlarge) mlabel(label6) mlabposition(9) mlabsize(`mlabsize')) ///
(scatter chabovesh chbelowsh if _n==7, msymbol(S)  msize(vlarge) mlabel(label7) mlabposition(4) mlabsize(`mlabsize')) ///
(scatter chabovesh chbelowsh if _n==8, msymbol(T)  msize(vlarge) mlabel(label8) mlabposition(10) mlabsize(`mlabsize')) ///
(function y=x , range(0 0.15) lcolor(black) lpattern(dash))  ///
, graphregion(color(white)) ytitle("Excess jobs relative to the pre-treatment total employment ({&Delta}a)", height(4.5) size(small)) xtitle("Magnitude of missing jobs relative to the pre-treatment total employment (-{&Delta}b)", size(medsmall)) yscale(range(0 0.16)) ylabel(0(0.04)0.16) xscale(range(0 0.16)) xlabel(0(0.04)0.16) ///
 legend(order(9 "45 degree line"))

graph export "${figures}FigureA8_a.pdf", replace
