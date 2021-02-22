
*This do file is created by DC on 07/11/2016.
********************************************************************************
**************        Alternative Event Definitions           ******************
********************************************************************************

*The first argument adds federal events. The second one adds small events.

do "${dofiles}alternativeeventdef_QJE.do" Y

do "${dofiles}Table5_tipnontip_withprog.do" notip


do "${dofiles}alternativeworkforce.do" hourly
do "${dofiles}alternativeworkforce.do" nontip
do "${dofiles}fulltimeequivalent.do"
do "${dofiles}Table1_Baseline_withprog_noqcew.do"
do "${dofiles}Table1_Baseline_withprog_qcew_w0.do"
Table_create ednt edf  FTE wfh wfn noq unw , group(overall) tablename(Table5_new)



