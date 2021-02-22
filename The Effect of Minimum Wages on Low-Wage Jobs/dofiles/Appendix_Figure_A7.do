use "${data}LitReviewLDE.dta", clear
rename est coeff
replace coeff= 0.33 if n==6
replace se = 0.57 if n==6

expand 2, gen(aux)
gen ci=coeff-1.96*se if aux==0
replace ci=coeff+1.96*se if aux==1

bysort aux: gen id=_n
gen ygraphval=.
 
 
replace ygraphval=1 if id==1   // "Addison et al. 2010"
replace ygraphval=2 if id==2   // "Allegretto et al. 2011"
replace ygraphval=3 if id==3   // "Bell 1997 Columbia"
replace ygraphval=4 if id==4   // "Burkhauser et al. 2000"
replace ygraphval=5 if id==5   // "Campolieti et al 2006"
replace ygraphval=6 if id==6   // "Card 1992a"
replace ygraphval=7 if id==7   // "Card 1992b"
replace ygraphval=8 if id==8   // "Card et al 1994"
replace ygraphval=9 if id==9   // "Currie and Fallick 1996"
replace ygraphval=10 if id==10   // "Dube et al. 2010"
replace ygraphval=11 if id==11   // "Dube et al. 2007"
replace ygraphval=12 if id==12   // "Eriksson and Pytlikova 2004, Slovakia"
replace ygraphval=13 if id==13   // "Erickssov and Pytlikova 2004, Czech"
replace ygraphval=14 if id==14   // "Fang and Lin 2015"
replace ygraphval=15 if id==15   // "Giuliano 2013"
replace ygraphval=16 if id==16   // "Hirsch et al 2015"
replace ygraphval=17 if id==17   // "Kim and Taylor 1995"
replace ygraphval=18 if id==18   // Neumark and Nizalove (2007)
replace ygraphval=19 if id==19   // Pereira (2003)
replace ygraphval=20 if id==20   // "Sabia 2008"
replace ygraphval=21 if id==21   //  
replace ygraphval=22 if id==22   // "This paper - baseline"
replace ygraphval=23 if id==23   // "This paper - CK high prob group"




label define ygraphvallabels    ///	
1 "Addison et al. 2010" ///
2 "Allegretto et al. 2011" ///
3 "Bell 1997, Colombia" ///
4 "Burkhauser et al. 2000" ///
5 "Campolieti et al. 2006" ///
6 "Card 1992a" ///
7 "Card 1992b" ///
8 "Card et al. 1994" ///
9 "Currie and Fallick 1996" ///
10 "Dube et al. 2010" ///
11 "Dube et al. 2007" ///
12 "Ericksson and Pytlikova 2004, Slovakia" ///
13 "Erickssov and Pytlikova 2004, Czech Rep." ///
14 "Fang and Lin 2015" ///
15 "Giuliano 2013" ///
16 "Hirsch et al 2015" ///
17 "Kim and Taylor 1995" ///
18 "Neumark and Nizalova 2007" ///
19 "Perira 2003" ///
20 "Sabia 2008" ///
21 " " ///
22 "This paper - baseline" ///
23 "This paper - CK high prob group"



label values ygraphval ygraphvallabels



twoway scatter ygraphval coeff if id<22, mc(navy) m(s)  mcolor(navy) ///
	|| scatter ygraphval coeff if id>=22, mc(navy) m(s)  mcolor(purple) ///
	|| scatter ygraphval ci if ygraphval==1, c(l) lc(navy) m(i) lp(solid)  ///
	|| scatter ygraphval ci if ygraphval==2, c(l) lc(navy) m(i)lp(solid)  ///
	|| scatter ygraphval ci if ygraphval==3, c(l) lc(navy) m(i) lp(solid) ///
	|| scatter ygraphval ci if ygraphval==4, c(l) lc(navy) m(i) lp(solid) ///
	|| scatter ygraphval ci if ygraphval==5, c(l) lc(navy) m(i) lp(solid) ///
	|| scatter ygraphval ci if ygraphval==6, c(l) lc(navy) m(i) lp(solid) ///
	|| scatter ygraphval ci if ygraphval==7, c(l) lc(navy) m(i) lp(solid) ///
	|| scatter ygraphval ci if ygraphval==8, c(l) lc(navy) m(i) lp(solid) ///
	|| scatter ygraphval ci if ygraphval==9, c(l) lc(navy) m(i) lp(solid) ///
	|| scatter ygraphval ci if ygraphval==10, c(l) lc(navy) m(i) lp(solid)  ///
	|| scatter ygraphval ci if ygraphval==11, c(l) lc(navy) m(i) lp(solid)  ///
	|| scatter ygraphval ci if ygraphval==12, c(l) lc(navy) m(i) lp(solid) ///
	|| scatter ygraphval ci if ygraphval==13, c(l) lc(navy) m(i) lp(solid) ///
	|| scatter ygraphval ci if ygraphval==14, c(l) lc(navy) m(i) lp(solid) ///
	|| scatter ygraphval ci if ygraphval==15, c(l) lc(navy) m(i) lp(solid) ///
	|| scatter ygraphval ci if ygraphval==16, c(l) lc(navy) m(i) lp(solid) ///
	|| scatter ygraphval ci if ygraphval==17, c(l) lc(navy) m(i) lp(solid) ///				
	|| scatter ygraphval ci if ygraphval==18, c(l) lc(navy) m(i) lp(solid) ///
	|| scatter ygraphval ci if ygraphval==19, c(l) lc(navy) m(i) lp(solid) ///
	|| scatter ygraphval ci if ygraphval==20, c(l) lc(navy) m(i) lp(solid) ///
	|| scatter ygraphval ci if ygraphval==21, c(l) lc(navy) m(i) lp(solid) ///
	|| scatter ygraphval ci if ygraphval==22, c(l) lc(purple) m(i) lp(solid) ///
	|| scatter ygraphval ci if ygraphval==23, c(l) lc(purple) m(i) lp(solid) ///
	yline(21,lc(gs12) lwidth(thin) lp(dot)) ///
	xline(-.43, lp(dash)lc(purple%50)) ///
	graphregion(color(white)) ///
	ylab(1/23, labsize(small) valuelabel angle(0) nogrid tlength(0)) ytitle("") ///
	xlab(-2(.5)2, labsize(small)) xtitle("Estimated Employment Elasticity With Respect To Own Wage", size(small)) legend(off) ///
	scheme(plotplain)
	
graph export "${figures}FigureA7.pdf", replace
