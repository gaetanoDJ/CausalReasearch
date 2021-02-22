
*This do file is created by DC on 11/27/2017.
********************************************************************************
****************     Measurement Error Estimation Table                *********
********************************************************************************


foreach vv in p rho2 eta{
sysuse auto,clear
reg price
		foreach data in CPS Admin{
			use "${data}ML_results_pooled_`data'_MNWAOR__fullypool.dta", clear
			replace p = 1-p
			replace rho2 = (1-rho2)/rho2
			sum `vv' ,meanonly
			local `vv': di %04.3f r(mean)
			estadd local `data' "``vv''"
		}
	
		foreach data in CPS Admin{
			use "${data}ML_results_pooled_`data'_MNWAOR__fullypool_noshift.dta", clear
			replace p = 1-p
			replace rho2 = (1-rho2)/rho2
			sum `vv' ,meanonly
			local `vv': di %04.3f r(mean)
			estadd local `data'_nos "``vv''"
		}
	
	eststo est_`vv'
	
	
	}
	
*

local mgroups mgroups("\specialcell{Misreporting rate \\ \\ 1-$\gamma$}" "\specialcell{Conditional error variance \\  \\ $\frac{1 \ \text{-} \rho^2}{\rho^2}$}" "\specialcell{Ratio of std. deviations of true \\ to observed latent distribution \\ $\frac{\sigma_{w}}{\sigma}$}" , pattern(1 1 1) lhs(\hfil \bigcell{c}{\\ \\ Dataset})  ) 
local stats stats(CPS Admin blankspace CPS_nos Admin_nos, label("CPS" "Administrative data" " " "CPS" "Administrative data")) 


esttab est_p est_rho2 est_eta ///
using "${tables}TableF2.tex" ///
, replace `stats' ///
nodep nogaps cells(none) nomtitles fragment booktabs nonumbers `mgroups'

