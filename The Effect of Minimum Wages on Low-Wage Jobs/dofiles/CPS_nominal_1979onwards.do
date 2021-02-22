



use "${data}totransfer.dta", clear


keep month state age marital race sex esr ethnic uhours earnhr uearnwk earnwt ///
uhourse paidhre earnhre earnwke I25a I25b I25c I25d year lfsr89 lfsr94 statenum ///
orgwt monthdate quarterdate quarter division region censusnum stfips gradeat gradecp ihigrdc ///
grade92 unioncov unionmme hourslw class class94 smsa80 smsa93 smsa04 smsastat occ*
*Drop self-employed
drop if (class==5 | class==6) & year<=1993
drop if (class94==6 | class94==7) & year>=1994

drop class class94

compress

*Tipped occupations

gen tipocc = 0
replace tipocc = 1 if (occ70==910 | occ70==914 | occ70==915 | occ70==911 | occ70==916 | occ70==934 | occ70==935 | occ70==944 | (occ70>=281 & occ70<=286 & occ70!=283)) & year >= 1979 & year <= 1982
replace tipocc = 1 if (occ80==434 | occ80==435 | occ80==438 | occ80==439 | occ80==443 | occ80==444 | occ80==466 | occ80==457 | occ80==458 | (occ80>=263 & occ80<=269)) & year >= 1983 & year <= 1991
replace tipocc = 1 if (occ80==434 | occ80==435 | occ80==438 | occ80==439 | occ80==443 | occ80==444 | occ80==464 | occ80==457 | occ80==458 | (occ80>=263 & occ80<=269)) & year >= 1992 & year <= 2002
replace tipocc = 1 if (occ00==4040 | occ00==4050 | occ00==4060 | occ00==4110 | occ00==4120 | occ00==4130 | occ00==4150 | occ00==4160 | occ00==4530 | occ00==4500 | occ00==4510 | occ00==4750 | occ00==4760 | occ00==4940) & year >= 2003 & year <= 2010
replace tipocc = 1 if (occ2011==4040 | occ2011==4050 | occ2011==4060 | occ2011==4110 | occ2011==4120 | occ2011==4130 | occ2011==4150 | occ2011==4160 | occ2011==4530 | occ2011==4500 | occ2011==4510 | occ2011==4750 | occ2011==4760 | occ2011==4940) & monthdate >= tm(2011m1) & monthdate <= tm(2012m4)
replace tipocc = 1 if (occ2012==4040 | occ2012==4050 | occ2012==4060 | occ2012==4110 | occ2012==4120 | occ2012==4130 | occ2012==4150                 | occ2012==4530 | occ2012==4500 | occ2012==4510 | occ2012==4750 | occ2012==4760 | occ2012==4940) & monthdate >= tm(2012m5) 


*** Getting rid of imputed ones and those whose incomes are not positive.

gen wage = earnhre / 100 if paidhre == 1
replace wage = earnwke/uhourse if paidhre == 2

* use allocation variables to determine hours, earnings, and hourly wage imputations
gen hoursimputed = I25a > 0 & I25a ~= .
gen earningsimputed = I25d > 0 & I25d ~= .
gen wageimputed = I25c > 0 & I25c ~= .

foreach var of varlist hoursimputed earningsimputed wageimputed {
	* use different method to identify imputations in 1989-1993 data
	replace `var' = 0 if year >= 1989 & year <= 1993
	* no reliable imputation data from Jan 1994 through Aug 1995; see Hirsch & Schumacher
	replace `var' = 0 if year == 1994 | (year == 1995 & month <= 8)
}

* use method analogous to that employed by Hirsch & Schumacher to identify imputations
* in 1989-1993 data
replace wageimputed = 1 if year >= 1989 & year <= 1993 & (earnhr == . | earnhr == 0) & (earnhre > 0 & earnhre ~= .)
replace hoursimputed = 1 if year >= 1989 & year <= 1993 & (uhours == . | uhours == 0) & (uhourse > 0 & uhourse ~= .)
replace earningsimputed = 1 if year >= 1989 & year <= 1993 & (uearnwk == . | uearnwk == 0) & (earnwke > 0 & earnwke ~= .)

gen imputed = 0
replace imputed = 1 if paidhre == 2 & (hoursimputed == 1 | earningsimputed == 1)
replace imputed = 1 if paidhre == 1 & (wageimputed == 1)

replace wage = . if imputed == 1
replace wage=wage*100	
	
g count = earnwt

keep count wage statenum year quarterdate tipocc age
compress
keep if wage!=.
save "${data}CPS_nominal_1979onwards.dta", replace

