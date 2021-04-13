********************************************************************
* name: crime_yaxuan.do
* author: yaxuan zhang (ut)
* description: creates the aggregate crime counts from raw UCR files
*	by state and month
* last update: April 3, 2020
********************************************************************

clear
capture log close
set more off, perm
clear matrix
clear mata
set scheme s1mono, perm


* Change directory, but find your own directory using Stata pull down menu under File
*	then change directory. Go into your meth /do subdirectory.  Then copy and paste the
* 	the path below.

cd"C:\Users\solar_neptune\Desktop\Casual Paper Topic\Do"
capture log using ./crime.log, replace

* Append on each year

use "C:\Users\solar_neptune\Desktop\Casual Paper Topic\Data\ucr1995.dta", replace
foreach num of numlist 1996/2000 {
append using "../data/ucr`num'.dta"
save ../data/temp_crime.dta, replace
								}

sort ori year								
save ../data/temp_crime.dta, replace

foreach num of numlist 95/99 {
replace year=19`num' if year==`num'
								}
								

replace year=2000 if year==0 | year==00
							

save ../data/temp_crime.dta, replace


** Generate the state variable using substring on ORI
gen abbr="AL" if regexm(ori, "AL")
foreach abbr in AK AZ AR CA CO CT DE DC FL GA HI ID IL IN IA KS KY LA ME MD MA MI MN MS MO MT NB NV NH NJ NM NY NC ND OH OK OR PA RI SC SD TN TX UT VT VA WA WV WI WY {	
replace abbr="`abbr'" if regexm(ori, "^`abbr'")
							}

gen 	statefip=1 if abbr=="AL"
replace statefip=2 if abbr == "AK"
replace statefip=4 if abbr == "AZ"
replace statefip=5 if abbr == "AR"
replace statefip=6 if abbr == "CA"
replace statefip=8 if abbr == "CO"
replace statefip=9 if abbr == "CT"
replace statefip=10 if abbr == "DE"
replace statefip=11 if abbr == "DC"
replace statefip=12 if abbr == "FL"
replace statefip=13 if abbr == "GA"
replace statefip=15 if abbr == "HI"
replace statefip=16 if abbr == "ID"
replace statefip=17 if abbr == "IL"
replace statefip=18 if abbr == "IN"
replace statefip=19 if abbr == "IA"
replace statefip=20 if abbr == "KS"
replace statefip=21 if abbr == "KY"
replace statefip=22 if abbr == "LA"
replace statefip=23 if abbr == "ME"
replace statefip=24 if abbr == "MD"
replace statefip=25 if abbr == "MA"
replace statefip=26 if abbr == "MI"
replace statefip=27 if abbr == "MN"
replace statefip=28 if abbr == "MS"
replace statefip=29 if abbr == "MO"
replace statefip=30 if abbr == "MT"
replace statefip=31 if abbr == "NB"
replace statefip=32 if abbr == "NV"
replace statefip=33 if abbr == "NH"
replace statefip=34 if abbr == "NJ"
replace statefip=35 if abbr == "NM"
replace statefip=36 if abbr == "NY"
replace statefip=37 if abbr == "NC"
replace statefip=38 if abbr == "ND"
replace statefip=39 if abbr == "OH"
replace statefip=40 if abbr == "OK"
replace statefip=41 if abbr == "OR"
replace statefip=42 if abbr == "PA"
replace statefip=44 if abbr == "RI"
replace statefip=45 if abbr == "SC"
replace statefip=46 if abbr == "SD"
replace statefip=47 if abbr == "TN"
replace statefip=48 if abbr == "TX"
replace statefip=49 if abbr == "UT"
replace statefip=50 if abbr == "VT"
replace statefip=51 if abbr == "VA"
replace statefip=53 if abbr == "WA"
replace statefip=54 if abbr == "WV"
replace statefip=55 if abbr == "WI"
replace statefip=56 if abbr == "WY"

gen 	stname="ALA" if statefip==1
replace stname="ALASK" if statefip==2
replace stname="ARIZ" if statefip==4
replace stname="ARK" if statefip==5
replace stname="CALIF" if statefip==6
replace stname="COLO" if statefip==7
replace stname="CONN" if statefip==9
replace stname="CONN." if statefip==9
replace stname="D C" if statefip==11
replace stname="DEL" if statefip==10
replace stname="FLA" if statefip==12
replace stname="GA" if statefip==13
replace stname="HAWAI" if statefip==15
replace stname="IDAHO" if statefip==16
replace stname="ILL" if statefip==17
replace stname="IND" if statefip==18
replace stname="IOWA" if statefip==19
replace stname="KANS" if statefip==20
replace stname="KY" if statefip==21
replace stname="LA" if statefip==22
replace stname="MAINE" if statefip==23
replace stname="MASS" if statefip==25
replace stname="MD" if statefip==24
replace stname="MICH" if statefip==26
replace stname="MINN" if statefip==27
replace stname="MISS" if statefip==27
replace stname="MO" if statefip==29
replace stname="MONT" if statefip==30
replace stname="N C" if statefip==37
replace stname="N DAK" if statefip==38
replace stname="N H" if statefip==33
replace stname="N J" if statefip==34
replace stname="N MEX" if statefip==35
replace stname="N Y" if statefip==36
replace stname="NEBR" if statefip==36
replace stname="NEV" if statefip==32
replace stname="OHIO" if statefip==39
replace stname="OKLA" if statefip==40
replace stname="OREG" if statefip==41
replace stname="P A" if statefip==42
replace stname="PA" if statefip==42
replace stname="R I" if statefip==44
replace stname="S C" if statefip==45
replace stname="S DAK" if statefip==46
replace stname="TENN" if statefip==47
replace stname="TEXAS" if statefip==48
replace stname="UTAH" if statefip==49
replace stname="VA" if statefip==51
replace stname="VT" if statefip==50
replace stname="W VA" if statefip==54
replace stname="WASH" if statefip==53
replace stname="WIS" if statefip==55
replace stname="WYO" if statefip==56


replace year=1995 if year==95
replace year=1996 if year==96
replace year=1997 if year==97
replace year=1998 if year==98
replace year=1999 if year==99
replace year=2000 if year==0


sort statefip year			
save ../data/temp_ucr.dta, replace


use ../data/temp_ucr.dta, replace

bysort year ori: gen i=_n
ta i
drop if i==2


gen rape1=c1f4_1
gen rape2=c1f4_2
gen rape3=c1f4_3
gen rape4=c1f4_4
gen rape5=c1f4_5
gen rape6=c1f4_6
gen rape7=c1f4_7
gen rape8=c1f4_8
gen rape9=c1f4_9
gen rape10=c1f4_10
gen rape11=c1f4_11
gen rape12=c1f4_12

gen larceny1=c1f21_1
gen larceny2=c1f21_2
gen larceny3=c1f21_3
gen larceny4=c1f21_4
gen larceny5=c1f21_5
gen larceny6=c1f21_6
gen larceny7=c1f21_7
gen larceny8=c1f21_8
gen larceny9=c1f21_9
gen larceny10=c1f21_10
gen larceny11=c1f21_11
gen larceny12=c1f21_12

gen vehicle1=c1f26_1
gen vehicle2=c1f26_2
gen vehicle3=c1f26_3
gen vehicle4=c1f26_4
gen vehicle5=c1f26_5
gen vehicle6=c1f26_6
gen vehicle7=c1f26_7
gen vehicle8=c1f26_8
gen vehicle9=c1f26_9
gen vehicle10=c1f26_10
gen vehicle11=c1f26_11
gen vehicle12=c1f26_12

gen burglary1=c1f17_1
gen burglary2=c1f17_2
gen burglary3=c1f17_3
gen burglary4=c1f17_4
gen burglary5=c1f17_5
gen burglary6=c1f17_6
gen burglary7=c1f17_7
gen burglary8=c1f17_8
gen burglary9=c1f17_9
gen burglary10=c1f17_10
gen burglary11=c1f17_11
gen burglary12=c1f17_12

gen manslaughter1=c1f2_1
gen manslaughter2=c1f2_2
gen manslaughter3=c1f2_3
gen manslaughter4=c1f2_4
gen manslaughter5=c1f2_5
gen manslaughter6=c1f2_6
gen manslaughter7=c1f2_7
gen manslaughter8=c1f2_8
gen manslaughter9=c1f2_9
gen manslaughter10=c1f2_10
gen manslaughter11=c1f2_11
gen manslaughter12=c1f2_12

gen robbery1=c1f6_1
gen robbery2=c1f6_2
gen robbery3=c1f6_3
gen robbery4=c1f6_4
gen robbery5=c1f6_5
gen robbery6=c1f6_6
gen robbery7=c1f6_7
gen robbery8=c1f6_8
gen robbery9=c1f6_9
gen robbery10=c1f6_10
gen robbery11=c1f6_11
gen robbery12=c1f6_12

* Murder
gen murder1=c1f1_1
gen murder2=c1f1_2
gen murder3=c1f1_3
gen murder4=c1f1_4
gen murder5=c1f1_5
gen murder6=c1f1_6
gen murder7=c1f1_7
gen murder8=c1f1_8
gen murder9=c1f1_9
gen murder10=c1f1_10
gen murder11=c1f1_11
gen murder12=c1f1_12

keep abbr statefip stname year rape* larceny* vehicle* burglary* manslaughter* robbery* agency_name pop* murder*

collapse (sum) rape* larceny* vehicle* burglary* manslaughter* robbery* murder* (max) pop1, by( year statefip stname)

reshape long rape larceny vehicle burglary manslaughter robbery murder, i(statefip year stname) j(month)

gen date=ym(year,month)
format date  %tm

egen id=group(statefip)


save ../data/ucr_meth.dta, replace

collapse (sum) rape* larceny* vehicle* burglary* manslaughter*  robbery* murder* (max) pop1, by( year statefip stname month date)

* Per capita measures

foreach x of varlist rape larceny vehicle burglary manslaughter robbery murder {
	
	gen `x'_pc = `x'/pop1 * 100000
	
	}

replace rape_pc = 0 if rape_pc == .

replace larceny_pc = 0 if larceny_pc == .

replace vehicle_pc = 0 if vehicle_pc == .

replace burglary_pc = 0 if burglary_pc == .

replace manslaughter_pc = 0 if manslaughter_pc == .

replace robbery_pc = 0 if robbery_pc == .

replace murder_pc = 0 if murder_pc == .


tsset statefip date

save ../data/ucr_meth.dta, replace

capture log close
exit

MODELS
reg CRIME_TYPE COVARIATES meth_selfln if teds_meth_quality, cluster(state)
ivregress 2sls CRIME_TYPE COVARIATES (meth_selfln = price_iv) if  teds_meth_quality, cluster(state) 

	