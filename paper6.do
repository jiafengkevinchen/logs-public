


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

global paper6_command_start = "reghdfe "
global paper6_arcsinh_var = "ashonset_cntypop1600"
global paper6_command_end = "interaction1, absorb(i.OBJECTID i.year) cluster(OBJECTID) "

global paper6_main_xvar = "interaction1"
global paper6_location "Table 3, Column (1)"

use _output/paper6.dta, clear
local paper6_original_command "$paper6_command_start $paper6_arcsinh_var $paper6_command_end"
`paper6_original_command'
display _b["$paper6_main_xvar"]

/* reghdfe $Y $X, absorb(i.OBJECTID i.year) cluster(OBJECTID) */
