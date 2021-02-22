
*This do file is created by DC on 07/08/2016.
********************************************************************************
**************        Table 4             **************************************
********************************************************************************


*Alternative wage windows
* The first argument in do is for upper bound. User can add a second argument for lower bound.
*If lower bound is not specified, it is 4.

do "${dofiles}alternativewagewindows.do" 2
do "${dofiles}alternativewagewindows.do" 3
do "${dofiles}alternativewagewindows.do" 4
do "${dofiles}alternativewagewindows.do" 5
do "${dofiles}alternativewagewindows.do" 6


Table_create w2_w4 w3_w4 w4_w4 w5_w4 w6_w4 , /// 
group(overall) tablename(Table_A5) mgroup("Alternative_wage_window" ) mpattern(1 0 0 0 /*1 0*/ 0)

