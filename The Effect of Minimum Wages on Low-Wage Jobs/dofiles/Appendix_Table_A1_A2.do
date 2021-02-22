sysuse auto,clear
reg mpg weight
tempfile dummyestimates
estimates save `dummyestimates'

* list of groups to use
local grouplist overall HSD HSL teen female BH CK1 CK2 CK3
local numgroups = 0
foreach group in `grouplist' {
	local numgroups = `numgroups' + 1
}

* identify estimates files
foreach group in `grouplist' {
	* bunching estimates
	if "`group'" == "overall" {
		local ster_bunchemp`group' "${estimates}Table1aafterqcew_stateonly_1_overall_1979_aggcont_A_1612_estadd.ster"
		local ster_bunchwage`group' "${estimates}Table1aafterqcew_stateonly_1_overall_1979_aggcont_A_1612_estadd.ster"
	}
	else if "`group'" == "CK1" {
		local ster_bunchemp`group' "${estimates}CKforplacebofig_true_stateonly_1_first__1979_w_aggcont_A_1612_estadd.ster"
		local ster_bunchwage`group' "${estimates}CKforplacebofig_true_stateonly_1_first__1979_w_aggcont_A_1612_estadd.ster"
	}
	else if "`group'" == "CK2" {
		local ster_bunchemp`group' "${estimates}CKforplacebofig_true_stateonly_1_fourth__1979_w_aggcont_A_1612_estadd.ster"
		local ster_bunchwage`group' "${estimates}CKforplacebofig_true_stateonly_1_fourth__1979_w_aggcont_A_1612_estadd.ster"
	}
	else if "`group'" == "CK3" {
		local ster_bunchemp`group' "${estimates}CKforplacebofig_true_stateonly_1_fifth__1979_w_aggcont_A_1612_estadd.ster"
		local ster_bunchwage`group' "${estimates}CKforplacebofig_true_stateonly_1_fifth__1979_w_aggcont_A_1612_estadd.ster"
	}
	else {
		local ster_bunchemp`group' "${estimates}Table4aqcewafter_stateonly_1_`group'_1979_aggcont_A_1612_estadd.ster"
		local ster_bunchwage`group' "${estimates}Table4aqcewafter_stateonly_1_`group'_1979_aggcont_A_1612_estadd.ster"
	}

	* aggregate estimates
	if "`group'" == "CK1" {
		local ster_aggemp`group' "${estimates}eventbased_aggregate_CKgroup1_empelas_TWFE.ster"
		local ster_aggwage`group' `dummyestimates'
	}
	else if "`group'" == "CK2" {
		local ster_aggemp`group' "${estimates}eventbased_aggregate_CKgroup2_empelas_TWFE.ster"
		local ster_aggwage`group' `dummyestimates'
	}
	else if "`group'" == "CK3" {
		local ster_aggemp`group' "${estimates}eventbased_aggregate_CKgroup3_empelas_TWFE.ster"
		local ster_aggwage`group' `dummyestimates'
	}
	else {
		local ster_aggemp`group' "${estimates}eventbased_aggregate_1979_1_1_`group'epop_estadd.ster"
		local ster_aggwage`group' `dummyestimates'
	}
}

* names of groups, in case we want this in the table
local name_overall "All workers"
local name_HSD "Less than high school"
local name_HSL "High school or less"
local name_teen "Teens"
local name_female "Women"
local name_BH "Black or Hispanic"
local name_CK1 "Predicted low wage"
local name_CK2 "Predicted middle wage"
local name_CK3 "Predicted high wage"

foreach group in `grouplist' {
	* elasticities of emp wrt mw
	* bunching
	estimates use "`ster_bunchemp`group''"
	local bunchempmwb = e(bunchelas_Eb)
	local bunchempmwse = e(bunchelas_Ese)
	local bunchempmwsenum = e(bunchelas_Ese_num)
	local bunchaffwb = e(WB_Eb)
	local bunchaffwse = e(WB_Ese)
	local bunchaffwt = e(WB_Et)

	* aggregate
	estimates use "`ster_aggemp`group''"
	if substr("`group'",1,2) == "CK" {
		local aggempmwb = e(allempgroupb)
		local aggempmwse = e(allempgroupse)
		local aggempmwsenum = e(allempgroupse_num)
		local aggwagemwb = e(allwageb)
		local aggwagemwse = e(allwagese)
		local aggwagemwt = e(allwaget)
	}
	else {
		local aggempmwb = e(allempb)
		local aggempmwse = e(allempse)
		local aggempmwsenum = e(allempse_num)
		local aggwagemwb = e(allwageb)
		local aggwagemwse = e(allwagese)
		local aggwagemwt = e(allwaget)
	}

	* ratio of se's
	local empmwseratio: di %04.3f `bunchempmwsenum' / `aggempmwsenum'

	* creating bunching estimates
	estimates use `dummyestimates'
	estadd local empmwb `bunchempmwb'
	estadd local empmwse `bunchempmwse'
	estadd local wageb `bunchaffwb'
	estadd local wagese `bunchaffwse'
	estadd local waget `bunchaffwt'
	eststo bunch`group', title("Bunching")

	* create aggregate estimates
	estimates use `dummyestimates'
	estadd local empmwb `aggempmwb'
	estadd local empmwse `aggempmwse'
	estadd local wageb `aggwagemwb'
	estadd local wagese `aggwagemwse'
	estadd local waget `aggwagemwt'
	eststo agg`group', title("Aggregated")

	* create seratio estimates
	estimates use `dummyestimates'
	estadd local empmwb `empmwseratio'
	eststo seratio`group', title("\bigcell{c}{Ratio of bunching to DOUBLEBACKSLASH aggregated standard errors}")

}

* Table 1: compare bunching vs aggregate affected wage change t-statistics
local counter = 0
foreach group in `grouplist' {
	local counter = `counter' + 1
	if `counter' == 1 {
		local replaceorappend replace
		local options nodep nogaps nonumbers mtitles fragment booktabs
	}
	else {
		local replaceorappend append
		local options nodep nogaps nonumbers nomtitles fragment booktabs nolines
	}
	if `counter' == `numgroups' {
		local stats stats(empmwb empmwse, label("`name_`group''" " "))
	}
	else {
		local stats stats(empmwb empmwse blankspace, label("`name_`group''" " " " "))
	}
	#delimit ;
	esttab
		bunch`group' agg`group' seratio`group'
		using "${tables}table_prec_bunch_agg_empmw.tex", `replaceorappend'
		cells(none)
		`stats'
		`options'
		substitute(DOUBLEBACKSLASH \\)
	;
	#delimit cr;
}

* Table 2: compare bunching vs aggregate affected wage change t-statistics
local counter = 0
foreach group in `grouplist' {
	local counter = `counter' + 1
	if `counter' == 1 {
		local replaceorappend replace
		local options nodep nogaps nonumbers mtitles fragment booktabs
	}
	else {
		local replaceorappend append
		local options nodep nogaps nonumbers nomtitles fragment booktabs nolines
	}
	if `counter' == `numgroups' {
		local stats stats(waget, label("`name_`group''"))
	}
	else {
		local stats stats(waget blankspace, label("`name_`group''" " "))
	}
	#delimit ;
	esttab
		bunch`group' agg`group'
		using "${tables}table_prec_bunch_agg_waget.tex", `replaceorappend'
		cells(none)
		`stats'
		`options'
	;
	#delimit cr;
}

* Table 3: compare bunching vs aggregate affected wage change estimates
local counter = 0
foreach group in `grouplist' {
	local counter = `counter' + 1
	if `counter' == 1 {
		local replaceorappend replace
		local options nodep nogaps nonumbers mtitles fragment booktabs
	}
	else {
		local replaceorappend append
		local options nodep nogaps nonumbers nomtitles fragment booktabs nolines
	}
	if `counter' == `numgroups' {
		local stats stats(wageb wagese, label("`name_`group''" " "))
	}
	else {
		local stats stats(wageb wagese blankspace, label("`name_`group''" " " " "))
	}
	#delimit ;
	esttab
		bunch`group' agg`group'
		using "${tables}table_prec_bunch_agg_wageb.tex", `replaceorappend'
		cells(none)
		`stats'
		`options'
	;
	#delimit cr;
}
