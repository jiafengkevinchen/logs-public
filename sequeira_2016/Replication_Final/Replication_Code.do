**Replication code for "Corruption, Trade Costs and Gains from Tariff Liberalization: Evidence from Southern Africa"
**Sandra Sequeira - London School of Economics


*Table 1: "Comparability of Trade Patterns and Product Characteristics across Treatment and Control Products, Prior to the 2008 Tariff"

clear
use Trade_Flow.dta

*Generating variables (note: Values for Rest of the World do not include South Africa)

gen share_value=tradevalue_adj/ tradevalue_adjrow_wsa
gen share_weight=netweightkg_rsa/netweightkg_row_wsa
gen unit_value_rsa=netweightkg_rsa/tradevalue_adjrow_wsa
gen unit_value_row=netweightkg_row_wsa/tradevalue_adjrow_wsa
gen share_unit_value=unit_value_rsa/unit_value_rowsa

*PANEL A: T-test for Patterns of Trade

ttest share_weight if period==2006 | period==2007, by(treated) unequal
ttest share_value if period==2006 | period==2007, by(treated) unequal
ttest share_unit_value if period==2006 | period==2007, by(treated) unequal

clear
use Bribes_Descriptive.dta

*PANEL B: T-test for product level characteristics

ttest value_ton if year==1, by(treated) unequal
tab bulk treated if year==1,chi2 
ttest nc if year==1, by(treated) unequal
tab rauch treated if year==1,chi2 


*** Table 2: "Tariff Liberalization and Import Volumes (2006-2014): Aggregate Import Flows"
clear 
use Trade_Flow.dta

*Generating variables

gen ltariff=log(tariff+1)
gen lshare_weight=log(share_weight+1)
gen lbaseline=log(baseline_tariff+1)
gen lshare_unit_value=log(share_unit_value+1)

global outreg_settings replace bdec(3) sdec(3) excel nocons drop(_Iperiod_*)

* Column (1): Fixed Effects

xtset commoditycode period
xi: xtreg lshare_weight ltariff i.period , fe i(ccd) vce (cluster ccd)
sum lshare_weight if e(sample)==1
estimates store m1

* Column (2): First Difference

xtset commoditycode period
xi: reg d.lshare_weight d.ltariff, vce(cluster ccd)
estimates store m2
sum lshare_weight if e(sample)==1

* Column (3): Long Difference

*gen t=period if period==2006 | period==2014
tsset commoditycode t
reg d.lshare_weight d.ltariff ,  vce (cluster ccd)
estimates store m3
sum lshare_weight if e(sample)==1

* Column (4): Instrumental Variable

xtset commoditycode period
xi: ivreg2 lshare_weight (ltariff= L1.ltariff L2.ltariff lbaseline) i.period , cluster (ccd) first
estimates store m4
sum lshare_weight if e(sample)==1

outreg2 [m1 m2 m3 m4] using Table2.xls, $outreg_settings

*** Table 3: "Tariff Liberalization and Import Volumes (2006-2014): Aggregate Import FLows"

global outreg_settings replace bdec(3) sdec(3) excel nocons drop(_Iperiod_*)

* Column (1): Fixed Effects

xtset commoditycode period
xi: xtreg lshare_unit_value ltariff i.period , fe i(ccd) vce (cluster ccd)
sum lshare_unit_value if e(sample)==1
estimates store m1

* Column (2): First Difference

xtset commoditycode period
xi: reg d.lshare_unit_value d.ltariff, vce(cluster ccd)
estimates store m2
sum lshare_unit_value if e(sample)==1

* Column (3): Long Difference

*gen t=period if period==2006 | period==2014
tsset commoditycode t
reg d.lshare_unit_value d.ltariff ,  vce (cluster ccd)
estimates store m3
sum lshare_unit_value if e(sample)==1


* Column (4): Instrumental Variable

xtset commoditycode period
xi: ivreg2 lshare_unit_value (ltariff= L1.ltariff L2.ltariff lbaseline) i.period , cluster (ccd) first
estimates store m4
sum lshare_unit_value if e(sample)==1

outreg2 [m1 m2 m3 m4] using Table3.xls, $outreg_settings

*** Table 4: "Tariff Liberalization and Import Volumes (2006-2010): Firm-Level Data"

clear
use Firm.dta

gen ltariff_20072=log(tariff_2007+1) 
gen ltariff_20082=log(tariff_2008+1)
gen ltariff_change2=ltariff_2007-ltariff_2008
gen change_import_status2=import2010-import2006
gen change_pctg_imports2=imported_inputs_2010- imported_inputs_2006

global outreg_settings "replace bdec(3) sdec(3) excel nocons drop (_Iindustry_*)"

* Column 1
xi: oprobit change_import_status ltariff_change , vce(cluster hc_group)
estimate store m1

*Column 2
xi: oprobit change_import_status ltariff_change ethnic  fsize foreign ltariff_2007 age , vce(cluster hc_group)
estimate store m2

*Column 3

xi: oprobit change_import_status ltariff_change ethnic  fsize foreign age ltariff_2007 i.industry , vce(cluster hc_group)
estimate store m3

*Column 4

xi: reg change_pctg_imports ltariff_change , vce(cluster hc_group)
estimate store m4

*Column 5

xi: reg change_pctg_imports ltariff_change ethnic  fsize foreign age ltariff_2007 , vce(cluster hc_group)
estimate store m5

*Column 6
xi: reg change_pctg_imports ltariff_change ethnic  fsize foreign age ltariff_2007 i.industry , vce(cluster hc_group)
estimate store m6

outreg2 [m1 m2 m3 m4 m5 m6] using Table4.xls,  `outreg_settings' 

**F test on Industry Fixed Effects

testparm _I*

*Table 5: "Trade Gaps and Tariff Levels (2006-2014)"

*Generating Trade Gaps

gen ltradevalue_moz=log(tradevalue_moz+1)
gen ltradevalue_rsa=log(tradevalue_rsa+1)
gen value_gap=ltradevalue_rsa-ltradevalue_moz


gen lnetweightkg_rsa=log(netweightkg_rsa+1)
gen lnetweightkg_moz=log(netweightkg_moz+1)
gen weight_gap=lnetweightkg_rsa-lnetweightkg_moz


gen unitvalue_rsa=tradevalue_rsa/netweightkg_rsa
gen lunitvalue_rsa=log(unitvalue_rsa+1)
gen unitvalue_moz=tradevalue_moz/netweightkg_moz
gen lunitvalue_moz=log(unitvalue_moz+1)
gen unit_gap=lunitvalue_rsa-lunitvalue_moz

gen treated_post=treated*post2008
gen lbaseline=log(baseline+1)

*sample_unitgap==1 restricts the analysis to the available sample for the unit gap measure to facilitate comparison across indicators

global outreg_settings replace bdec(3) sdec(3) excel nocons drop (_Iperiod*)

areg weight_gap ltariff  i.period if sample_unitgap==1 , absorb(ccd) vce(cluster ccd)
estimates store m1
sum weight_gap if e(sample)==1

areg value_gap ltariff  i.period if sample_unitgap==1 , absorb(ccd) vce(cluster ccd) 
estimates store m2
sum value_gap if e(sample)==1

areg unit_gap ltariff  i.period if sample_unitgap==1, absorb(ccd) vce(cluster ccd)
estimates store m3
sum unit_gap if e(sample)==1

areg weight_gap treated treated_post lbaseline post2008  if sample_unitgap==1, absorb(ccd) vce(cluster ccd)
estimates store m4
sum weight_gap if e(sample)==1

areg value_gap treated  lbaseline treated_post post2008 if sample_unitgap==1 , absorb(ccd) vce(cluster ccd) 
estimates store m5
sum value_gap if e(sample)==1

areg unit_gap treated treated_post lbaseline post2008 ,absorb(ccd) vce(cluster ccd)
estimates store m6
sum unit_gap if e(sample)==1

outreg2 [m1 m2 m3 m4 m5 m6] using Table5.xls, $outreg_settings


*Table 6: "Summary Statistics: Bribe Payments"

clear
use Bribes_Descriptive.dta

*Probability of Paying a Bribe

tab bp if year==1
tab bp if year==2
tab bp if year==4 | year==5

*Avg Bribe Amount per Ton 

gen ba_tonnage=ba/tonnage

sum ba_tonnage if year==1
sum ba_tonnage if year==2
sum ba_tonnage if  year==4 | year==5


*Primary Bribe Recipient

tab b_recipient if year==1
tab b_recipient if year==2
tab b_recipient if  year==4|year==5

*Primary Reason for Bribe

tab reason_bribe if year==1
tab reason_bribe if year==2
tab reason_bribe if year==4|year==5

*Ratio of Bribe Amount to Tariff Duties Saved

gen tariff_due=actual_tariff_year*value_shipment_metical
gen tariff_saved=0.5*tariff_due \* assumes that only 50% of the tariff due is being paid
gen bribe_ratio_tariff_saved=ba/tariff_saved
replace bribe_ratio_tariff_saved=. if bribe_ratio_tariff_saved>=1 

sum bribe_ratio_tariff_saved if year==1 & reason_bribe==14
sum bribe_ratio_tariff_saved if year==2 & reason_bribe==14
sum bribe_ratio_tariff_saved if (year==4|year==5 )& reason_bribe==14

*Average Clearing Times

sum clear_time if year==1
sum clear_time if year==2
sum clear_time if ( year==4|year==5)

sum clear_time if year==1 & bp==1
sum clear_time if year==2 & bp==1
sum clear_time if ( year==4|year==5) & bp==1

sum clear_time if year==1 & bp==0
sum clear_time if year==2 & bp==0
sum clear_time if ( year==4|year==5) & bp==0

sum clear_time if year==1  & reason_bribe==14
sum clear_time if year==2  & reason_bribe==14
sum clear_time if ( year==4|year==5) & reason_bribe==14


*Table 7: "Bribes Before and After the Tariff Change: by Shipper and Product Characteristics"

*PANEL A

tab bp post_2008 if dfs==1, chi2
tab bp post_2008 if dfs==0, chi2
tab bp post_2008 if agri==1, chi2
tab bp post_2008 if differentiated==1, chi2
tab bp post_2008 if psi==1, chi2

*PANEL B

ttest ba_tonnage if dfs==1, by(post_2008)
ttest ba_tonnage if dfs==0, by(post_2008)
ttest ba_tonnage if agri==1, by(post_2008)
ttest ba_tonnage if differentiated==1, by(post_2008)
ttest ba_tonnage if psi==1, by(post_2008)

*Table 8: "Difference-in-Differences: Determinants of the Probability of Paying a Bribe"


use Bribes_Regressions.dta

gen value_tonnage=value_shipment_metical/tonnage
gen lvalue_tonnage=log(value_shipment_metical+1)

global outreg_settings "replace bdec(3) sdec(3) excel nocons drop(_Ihc_group_* _Iclear_agt_*)"

*Column (1): 

xi: reg bp tariff_change_post2008 tariff_change_2008  tariff2007 differentiated agri perishable dfs i.clear_agent  lvalue_tonnage day_w_arrival  psi monitor post_2008  i.hc_group hc_4digits rsa term , vce(cluster hc_4digits)

estimates store m1

*Column (2):

xi: reg bp  tariff_change_post2008 tariff_change_2008  tariff2007 differentiated agri perishable dfs i.clear_agent  lvalue_tonnage day_w_arrival  psi monitor post_2008  i.hc_group hc_4digits rsa term  diff_post_2008 lvalue_ton_post_2008 agri_post_2008 perishable_post_2008 day_w_arrival_post2008 dfs_post_2008 psi_post_2008 tariff2007_post_2008, vce(cluster hc_4digits)

estimates store m2

*Column (3)

xi: reg bp tariff_reduction_post2008 tariff_reduction tariff2007 differentiated agri perishable dfs i.clear_agent  lvalue_tonnage day_w_arrival  psi monitor post_2008  i.hc_group hc_4digits rsa term , vce(cluster hc_4digits)

estimates store m3

*Column (4) 

xi: reg bp  tariff_reduction_post2008 tariff_reduction  tariff2007 differentiated agri perishable dfs i.clear_agent  lvalue_tonnage day_w_arrival  psi monitor post_2008  i.hc_group hc_4digits rsa term  diff_post_2008 lvalue_ton_post_2008  perishable_post_2008 day_w_arrival_post2008 dfs_post_2008 psi_post_2008 tariff2007_post_2008, vce(cluster hc_4digits)

estimates store m4

outreg2 [m1 m2 m3 m4] using Table8.xls,  `outreg_settings'


* Table 9: "Difference-in-Differences: Determinants of the Amount of Bribe Paid"

global outreg_settings "replace bdec(3) sdec(3) excel nocons drop(_Ihc_group_* _Iclear_agt_*)"

*Column (1):

xi: reg lba tariff_change_post2008 tariff_change_2008  tariff2007 differentiated agri perishable dfs i.clear_agent  lvalue_tonnage day_w_arrival  psi monitor post_2008  i.hc_group hc_4digits rsa term , vce(cluster hc_4digits)

estimates store m1

*Column (2): 

xi: reg lba  tariff_change_post2008 tariff_change_2008  tariff2007 differentiated agri perishable dfs i.clear_agent  lvalue_tonnage day_w_arrival  psi monitor post_2008  i.hc_group hc_4digits rsa term  diff_post_2008 agri_post_2008 lvalue_ton_post_2008  perishable_post_2008 day_w_arrival_post2008 dfs_post_2008 psi_post_2008 tariff2007_post_2008, vce(cluster hc_4digits)

estimates store m2

*Column (3): 
xi: reg lba tariff_reduction_post2008 tariff_reduction tariff2007 differentiated agri perishable dfs i.clear_agent  lvalue_tonnage day_w_arrival  psi monitor post_2008  i.hc_group hc_4digits rsa term , vce(cluster hc_4digits)
estimates store m3

*Column (4) 

xi: reg lba  tariff_reduction_post2008 tariff_reduction  tariff2007 differentiated agri perishable dfs i.clear_agent  lvalue_tonnage day_w_arrival  psi monitor post_2008  i.hc_group hc_4digits rsa term  diff_post_2008 lvalue_ton_post_2008  perishable_post_2008  day_w_arrival_post2008 dfs_post_2008 psi_post_2008 tariff2007_post_2008, vce(cluster hc_4digits)
estimates store m4

*Columns (5): Hurdle Model

xi: hnblogit ba tariff_change_post2008 tariff_change_2008  tariff2007 differentiated agri perishable dfs i.clear_agent  lvalue_tonnage day_w_arrival  psi monitor post_2008  i.hc_group hc_4digits rsa term , vce(cluster hc_4digits)
estimates store m5

xi: hnblogit ba tariff_reduction_post2008 tariff_reduction   tariff2007 differentiated agri perishable dfs i.clear_agent  lvalue_tonnage day_w_arrival  psi monitor post_2008  i.hc_group hc_4digits rsa term , vce(cluster hc_4digits)
estimates store m6

outreg2 [m1 m2 m3 m4 m5 m6] using Table9_final.xls,  `outreg_settings'


**Table 10: "Difference-in-Differences: Bribes as a Share of Shipment Value"

gen lba_value=log(ba_value+1)

global outreg_settings "replace bdec(3) sdec(3) excel nocons drop (_Ihc_group_* _Iclear_agt_*)"

*Column (1):

xi: reg lba_value tariff_change_post2008 tariff_change_2008  tariff2007 differentiated agri ltonnage perishable dfs i.clear_agent  day_w_arrival  psi monitor post_2008  i.hc_group hc_4digits rsa term , vce(cluster hc_4digits)
estimates store m1

*Column (2): 

xi: reg lba_value  tariff_change_post2008 tariff_change_2008  tariff2007 differentiated agri perishable dfs ltonnage i.clear_agent  ltonnage_post2008 day_w_arrival  psi monitor post_2008  i.hc_group hc_4digits rsa term  diff_post_2008   agri_post_2008 perishable_post_2008 day_w_arrival_post2008 dfs_post_2008 psi_post_2008 tariff2007_post_2008, vce(cluster hc_4digits)
estimates store m2

*Column (3): 

xi: reg lba_value tariff_reduction_post2008 tariff_reduction tariff2007 differentiated agri perishable dfs i.clear_agent ltonnage day_w_arrival  psi monitor post_2008  i.hc_group hc_4digits rsa term , vce(cluster hc_4digits)
estimates store m3

*Column (4): 

xi: reg lba_value  tariff_reduction_post2008 tariff_reduction  tariff2007 differentiated agri perishable dfs i.clear_agent  ltonnage ltonnage_post2008 day_w_arrival  psi monitor post_2008  i.hc_group hc_4digits rsa term  diff_post_2008  perishable_post_2008  day_w_arrival_post2008 dfs_post_2008 psi_post_2008 tariff2007_post_2008, vce(cluster hc_4digits)
estimates store m4

outreg2 [m1 m2 m3 m4] using Table10.xls,  `outreg_settings'


*** Table 11: "Difference-in-Differences: Bribe Amounts Paid as a Share of Shipment Tonnage"

gen lba_tonnage=log(ba_tonnage+1)

global outreg_settings "replace bdec(3) sdec(3) excel nocons drop (_Ihc_group_* _Iclear_agt_*)"

*Column (1): basic model

xi: reg lba_tonnage tariff_change_post2008 tariff_change_2008  tariff2007 differentiated agri lvalue_shipment_metical perishable dfs i.clear_agent  day_w_arrival  psi monitor post_2008  i.hc_group hc_4digits rsa term , vce(cluster hc_4digits)

estimates store m1

*Column (2): covariates*post2

xi: reg lba_tonnage  tariff_change_post2008 tariff_change_2008  tariff2007 differentiated agri perishable dfs lvalue_shipment_metical i.clear_agent  lvalue_shipment_metical_post2 day_w_arrival  psi monitor post_2008  i.hc_group hc_4digits rsa term  diff_post_2008   agri_post_2008 perishable_post_2008 day_w_arrival_post2008 dfs_post_2008 psi_post_2008 tariff2007_post_2008, vce(cluster hc_4digits)

estimates store m2


xi: reg lba_tonnage tariff_reduction_post2008 tariff_reduction tariff2007 differentiated agri perishable dfs lvalue_shipment_metical i.clear_agent  day_w_arrival  psi monitor post_2008  i.hc_group hc_4digits rsa term , vce(cluster hc_4digits)
estimates store m3

*Column (5) covariates*post2

xi: reg lba_tonnage  tariff_reduction_post2008 tariff_reduction  tariff2007 differentiated agri perishable dfs i.clear_agent  lvalue_shipment_metical lvalue_shipment_metical_post2008  day_w_arrival  psi monitor post_2008  i.hc_group hc_4digits rsa term  diff_post_2008  perishable_post_2008  day_w_arrival_post2008 dfs_post_2008 psi_post_2008 tariff2007_post_2008, vce(cluster hc_4digits)
estimates store m4

outreg2 [m1 m2 m3 m4] using Table_11.xls,  `outreg_settings'

**Table 12: "Tariff Liberalization and Import Volumes (2006-2014): Excluding Agricultural Products"

use Trade_Flow.dta
gen agricultural=1 if commoditycode>100000& commoditycode<=530599
replace agricultural=0 if commoditycode>530599 & commoditycode!=.

global outreg_settings "replace bdec(3) sdec(3) excel nocons "

*1) Column (1): Fixed Effects

xtset commoditycode period
xi: xtreg lshare_weight ltariff i.period if agricultural==0, fe i(ccd) vce (cluster ccd)
estimates store m1
sum lshare_weight if e(sample)==1

*2) Column (2): First Difference
xtset commoditycode period
reg d.lshare_weight d.ltariff if agricultural==0, vce(cluster ccd)
estimates store m2
sum lshare_weight if e(sample)==1

*3) Column (3): Long Difference

*gen t=period if period==2006 | period==2014
tsset commoditycode t
reg d.lshare_weight d.ltariff if agricultural==0,  vce (cluster ccd)
estimates store m3
sum lshare_weight if e(sample)==1

*4) Column (4): Instrumental Variable

xtset commoditycode period
xi: ivreg2 lshare_weight (ltariff= L1.ltariff L2.ltariff lbaseline) i.period if agricultural==0, cluster (ccd) first
estimates store m4
sum lshare_weight if e(sample)==1


outreg2 [m1 m2 m3 m4] using Table12.xls, $outreg_settings

**Table 13: "The Parallel Trend Hypothesis: Aggregate Trade Flow Data"

use Trade_Gaps.dta

global outreg_settings replace bdec(3) sdec(3) excel nocons 

*PANEL A

*Column (1)
reg weight_gap treated treated_2007 lbaseline post2007 if period==2006 | period==2007 , vce(cluster ccd)
estimates store m1

*Column (2)
reg value_gap treated treated_2007 lbaseline post2007 if period==2006 | period==2007 , vce(cluster ccd)
estimates store m2

*Column (3)
reg unit_gap treated treated_2007 lbaseline post2007 if period==2006 | period==2007 , vce(cluster ccd)
estimates store m3

*PANEL B

*Column (4)
areg weight_gap treated treated_post lbaseline post2008  if sample_unitgap==1, absorb(ccd) vce(cluster ccd)
estimates store m4
sum weight_gap if e(sample)==1

*Column (5)
areg value_gap treated  lbaseline treated_post post2008 if sample_unitgap==1 , absorb(ccd) vce(cluster ccd) 
estimates store m5
sum value_gap if e(sample)==1

*Column (6)
areg unit_gap treated treated_post lbaseline post2008 ,absorb(ccd) vce(cluster ccd)
estimates store m6
sum unit_gap if e(sample)==1

outreg2 [m1 m2 m3 m4 m5 m6] using Table13.xls, $outreg_settings


*Table 14: "Firm Characteristics: Selection"

use Firm.dta

ttest age, by(attrition_panel) unequal
ttest age, by(attrition_covariates) unequal
ksmirnov age, by(attrition_panel) exact
ksmirnov age, by(attrition_covariates) exact
ttest sales_2006_CPI, by(attrition_panel) unequal
ttest sales_2006_CPI, by(attrition_covariates) unequal
ksmirnov sales_2006_CPI, by(attrition_panel) exact
ksmirnov sales_2006_CPI, by(attrition_covariates) exact
ttest reported_2006_workers, by(attrition_panel) unequal
ttest reported_2006_workers, by(attrition_covariates) unequal
ksmirnov reported_2006_workers, by(attrition_panel) exact
ksmirnov reported_2006_workers, by(attrition_covariates) exact
ttest imported_inputs_2006, by(attrition_panel) unequal
ttest imported_inputs_2006, by(attrition_covariates) unequal
ksmirnov imported_inputs_2006, by(attrition_panel) exact
ksmirnov imported_inputs_2006, by(attrition_covariates) exact
tab export_status attrition_panel, chi2
tab export_status attrition_covariates, chi2
ttest female_own , by(attrition_panel) unequal
ttest female_own , by(attrition_covariates) unequal
ksmirnov female_own , by(attrition_panel) exact
ksmirnov female_own , by(attrition_covariates) exact
tab ethnic attrition_panel, chi2
tab ethnic attrition_covariates, chi2


*Table 15: "Tariff Changes and Misreporting of Trade Flows"
 
 use Trade_Gaps.dta

*sample_misreporting constrains the sample to be the same across all specifications
 
*Column (1)

reg lnetweightkg_moz treated treated_post lbaseline post2008 if sample_misreporting==1, vce(cluster ccd)
estimates store m1
sum lnetweightkg_moz if e(sample)==1

*Column (2)
reg lnetweightkg_rsa treated treated_post lbaseline post2008 if sample_misreporting==1, vce(cluster ccd)
estimates store m2
sum lnetweightkg_rsa if e(sample)==1

*Column (3)
reg ltradevalue_moz treated treated_post lbaseline post2008 if sample_misreporting==1, vce(cluster ccd)
estimates store m3
sum ltradevalue_moz if e(sample)==1

*Column (4)
reg ltradevalue_rsa treated treated_post lbaseline post2008 if sample_misreporting==1, vce(cluster ccd)
estimates store m4
sum ltradevalue_rsa if e(sample)==1

outreg2 [m1 m2 m3 m4] using Table15.xls,  $outreg_settings

*Table 16: "Monitoring Experiment"

use Bribes_Regressions.dta
gen monitor_tariff=monitor*lactual_tariff_year

global outreg_settings "replace bdec(3) sdec(3) excel nocons drop (_Ihc_group_* _Iclear_agent_*)"

*column (1)
reg bp  monitor differentiated agri  lactual_tariff_year dfs  lvalue_tonnage day_w_arrival  perishable psi bulk  month_arrival rsa term  hc_4digits  i.year if year==4| year==5, vce(cluster hc_4digits)
estimates store m1
*column (2)
xi: reg bp  monitor  differentiated agri lvalue_tonnage  lactual_tariff_year dfs i.clear_agent lvalue_tonnage day_w_arrival  perishable psi bulk rsa month_arrival term i.hc_group hc_4digits  i.year if year==4| year==5, vce(cluster hc_4digits)
estimates store m2
*column (3)
xi: reg bp  monitor monitor_tariff differentiated agri  lactual_tariff_year dfs  lvalue_tonnage day_w_arrival  perishable psi year month_arrival rsa hc_4digits bulk term i.year if year==4| year==5, vce(cluster hc_4digits)
estimates store m3
*column(4)
xi: reg bp  monitor monitor_tariff differentiated agri  lactual_tariff_year dfs i.clear_agent lvalue_tonnage day_w_arrival  perishable psi year rsa month_arrival i.hc_group hc_4digits bulk term i.year if year==4| year==5, vce(cluster hc_4digits)
estimates store m4

reg lba monitor differentiated agri lactual_tariff_year dfs  lvalue_tonnage day_w_arrival  perishable psi bulk  month_arrival term  hc_4digits rsa  i.year if year==4| year==5, vce(cluster hc_4digits)
estimates store m5
*column (5)
xi: reg lba  monitor differentiated agri  lactual_tariff_year dfs i.clear_agent lvalue_tonnage day_w_arrival  perishable psi year month_arrival rsa hc_4digits bulk term i.hc_group i.year if year==4| year==5, vce(cluster hc_4digits)
estimates store m6
*column (6)
xi: reg lba monitor monitor_tariff  differentiated agri  lactual_tariff_year dfs  lvalue_tonnage day_w_arrival   perishable psi year month_arrival rsa hc_4digits bulk term i.year if year==4 | year==5, vce(cluster hc_4digits)
estimates store m7

xi: reg lba monitor monitor_tariff  differentiated agri  lactual_tariff_year dfs i.clear_agent lvalue_tonnage day_w_arrival perishable psi year rsa month_arrival i.hc_group hc_4digits bulk term i.year if year==4 | year==5, vce(cluster hc_4digits)
estimates store m8

outreg2  [m1 m2 m3 m4 m5 m6 m7 m8] using Table16.xls, `outreg_settings'



*Table 17: "Summary Statistics: Shipment Characteristics"

use Bribes_Regressions.dta

*PANEL A

*All products
ttest value_ton, by(post_2008) unequal
tab bulk post_2008, chi2
tab perishable post_2008, chi2
tab differentiated post_2008, chi2


*Treatment Products

ttest value_ton if tariff_change_2008==1, by(post_2008) unequal
tab bulk post_2008 if tariff_change_2008==1, chi2
tab differentiated post_2008 if tariff_change_2008==1, chi2
tab perishable post_2008 if tariff_change_2008==1, chi2

*Control Products

ttest value_ton if tariff_change_2008==0, by(post_2008) unequal
tab bulk post_2008 if tariff_change_2008==0, chi2
tab differentiated post_2008 if tariff_change_2008==0, chi2
tab perishable post_2008 if tariff_change_2008==0, chi2

*PANEL B

*High-High Tariff products

ttest value_ton  if H_H==1, by(post_2008) unequal
tab bulk post_2008 if H_H==1, chi2
tab perishable post_2008 if H_H==1, chi2
tab differentiated post_2008 if H_H==1, chi2

*High-Low Tariff products

ttest value_ton  if H_L==1, by(post_2008) unequal
tab bulk post_2008 if H_L==1, chi2
tab perishable post_2008 if H_L==1, chi2
tab differentiated post_2008 if H_L==1, chi2

* Low-Low Tariff Products
ttest value_ton  if L_L==1, by(post_2008) unequal
tab bulk post_2008 if L_L==1, chi2
tab perishable post_2008 if L_L==1, chi2
tab differentiated post_2008 if L_L==1, chi2

*Table 18: "Shift from Collusive to Coercive Corruption"

use Bribes_Regressions.dta

*shift_coercive: Equals 0 if Collusive Bribe, 1 if No Bribe and 2 if Coercive Bribe

global outreg_settings replace bdec(3) sdec(3) excel nocons drop (_Ihc_group*)

*Column 1

xi: reg coercive tariff_change_post2008 tariff_change_2008 tariff2007 differentiated agri perishable dfs clear_agent  lvalue_tonnage day_w_arrival   monitor post_2008  i.hc_group hc_4digits rsa  , vce(cluster hc_4digits)
estimates store m1

*Column 2
xi: reg coercive tariff_change_post2008 tariff_change_2008  tariff2007 differentiated agri perishable dfs clear_agent  lvalue_tonnage day_w_arrival  psi monitor post_2008  hc_group hc_4digits rsa   diff_post_2008 lvalue_ton_post_2008 agri_post_2008 perishable_post_2008 day_w_arrival_post2008 dfs_post_2008 psi_post_2008 tariff2007_post_2008, vce(cluster hc_4digits)
estimates store m2


*Column 3 
xi: reg coercive tariff_reduction_post2008 tariff_reduction tariff2007 differentiated agri perishable dfs clear_agent  lvalue_tonnage day_w_arrival   monitor post_2008  i.hc_group hc_4digits rsa  , vce(cluster hc_4digits)
estimates store m3

*Column 4
xi: reg coercive tariff_reduction_post2008 tariff_reduction  tariff2007 differentiated agri perishable dfs clear_agent  lvalue_tonnage day_w_arrival  monitor post_2008  hc_group hc_4digits rsa  diff_post_2008 lvalue_ton_post_2008 agri_post_2008 perishable_post_2008 day_w_arrival_post2008 dfs_post_2008 psi_post_2008 tariff2007_post_2008, vce(cluster hc_4digits)
estimates store m4

*Column 5
xi: oprobit shift_coercive  tariff_change_post2008 tariff_change_2008  tariff2007 differentiated agri perishable dfs clear_agent  lvalue_tonnage day_w_arrival  psi monitor post_2008  i.hc_group hc_4digits rsa   diff_post_2008 lvalue_ton_post_2008 agri_post_2008 perishable_post_2008 day_w_arrival_post2008 dfs_post_2008 psi_post_2008 tariff2007_post_2008, vce(cluster hc_4digits)

estimates store m5

*Column 6
xi: oprobit shift_coercive  tariff_reduction_post2008 tariff_reduction   tariff2007 differentiated agri perishable dfs clear_agent  lvalue_tonnage day_w_arrival  psi monitor post_2008  i.hc_group hc_4digits rsa   diff_post_2008 lvalue_ton_post_2008 agri_post_2008 perishable_post_2008 day_w_arrival_post2008 dfs_post_2008 psi_post_2008 tariff2007_post_2008, vce(cluster hc_4digits)
estimates store m6

outreg2 [m1 m2 m3 m4 m5 m6] using Table18.xls, replace


*Table 19: "Tariff Liberalization and Bribe Dispersion"

*PANEL A:

bysort hc_4digits: egen std_bt=sd(ba_tont)

gen tariff_evasion=1 if reason_bribe==14
replace  tariff_evasion=0 if reason_bribe!=14 
ttest std_bt , by(post_2008) unequal
ttest std_bt if tariff_change_2008==1, by(post_2008) unequal
ttest std_bt if b_recipient=="customs" & tariff_evasion==1, by(post_2008) unequal
ttest std_bt if b_recipient=="customs" & tariff_change_2008==1, by(post_2008) unequal

*PANEL B:

*Column (1): 

xi: reg std_bt tariff_change_post2008 tariff_change_2008  tariff2007 differentiated agri perishable dfs i.clear_agent  lvalue_tonnage day_w_arrival  psi monitor post_2008  i.hc_group  rsa term , robust
estimates store m1

*Column (2): 

xi: reg std_bt  tariff_change_post2008 tariff_change_2008  tariff2007 differentiated agri perishable dfs i.clear_agent  lvalue_tonnage day_w_arrival  psi monitor post_2008  i.hc_group  rsa term   diff_post_2008 lvalue_ton_post_2008 agri_post_2008 perishable_post_2008 day_w_arrival_post2008 dfs_post_2008 psi_post_2008 tariff2007_post_2008, robust
estimates store m2

outreg2 [m1 m2] using Table19.xls, replace


