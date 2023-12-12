
/*

 For each paper, we create the following globals

 paperX_command_start = the start of the command (e.g. "reg" or "areg" or "ivregress 2sls")
 paperX_arcsinh_var = the variable that is in arcsinhs
 paperX_command_end = the remainder of the command after the explanatory variables
 paperX_main_xvar = the main variable of interest (e.g. a treatment indicator)
 paperX_location = the location of this coefficient in the paper


 If this is done correctly, then we should be able to replicate the original results
 and extract the main coefficient by running the following commands

use _output/paperX.dta, clear

lobcal paperX_original_command "`paperX_command_start' `paperX_arcsinh_var' `paperX_command_end'"

`paperX_original_command'

display _b[$paperX_main_xvar]
*/

/*
use ${secdata}paper4.dta, clear

qui rdrobust total_viol_tc focal_penalty_geo_minus_thresh, c(0) covs(construction programmed) vce(nncluster peer_group_`peer_name') fuzzy(focal_press_release) bwselect(msetwo)
	local bw_l `e(h_l)'
	local bw_r `e(h_r)'
*/
local bw_l = 3819.6621
local bw_r = 8578.2745
local control1 construction programmed

global paper7_command_start = "reghdfe "
global paper7_arcsinh_var = "asinh_pub_yr"
global paper7_command_end = "linter_cites1 lexter_cites_mahalsic1    lgrd1  lpat_stock1     dum_nopat_year1     dum_nopub1  dum_nocorp_exter_cites_yr1 dum_nocite_year1,  ab(year permno_adj_long_grp) cluster(permno_adj_long_grp)"

global paper7_main_xvar = "linter_cites1"
global paper7_location "Table 6, Column (6)"

use _output/paper7.dta, clear
local paper7_original_command "$paper7_command_start $paper7_arcsinh_var $paper7_command_end"
`paper7_original_command'
display _b["$paper7_main_xvar"]

/*
reghdfe asinh_pub_yr linter_cites1 lexter_cites_mahalsic1    lgrd1  lpat_stock1     dum_nopat_year1     dum_nopub1  dum_nocorp_exter_cites_yr1 dum_nocite_year1,  ab(year permno_adj_long_grp) cluster(permno_adj_long_grp)
/* sum pub_yr if e(sample) == 1
local pu = `r(mean)'
unique permno_adj_long_grp if e(sample) == 1
outreg2 using Table_6.xls, append   drop(i.year dum_nopat_year1   dum_nopub1  dum_nocite_year1 dum_nocorp_exter_cites_yr1 ) nocons ctitle(6:ASINH) bdec(3) tdec(2) rdec(2) alpha(.01, .05)     title (Table 6)  addtext(avg, `pu', firms,`r(unique)') addnote(Notes: Add notes here) */ */ */ */
