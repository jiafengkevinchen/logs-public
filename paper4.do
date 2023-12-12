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

global paper4_command_start = "rdrobust "
global paper4_arcsinh_var = "arcsinh_var"
global paper4_command_end = "focal_penalty_geo_minus_thresh if peer_group_5km!=., c(0) covs(`control1') vce(nncluster peer_group_5km) p(2) bwselect(msetwo) h(`bw_l' `bw_r')"

global paper4_main_xvar = "RD_Estimate"
global paper4_location "Table A.6, Column (5)"

use _output/paper4.dta, clear
local paper4_original_command "$paper4_command_start $paper4_arcsinh_var $paper4_command_end"
`paper4_original_command'
display _b["$paper4_main_xvar"]

gen overThreshold = focal_penalty_geo_minus_thresh > 0
global paper4_dvar = "overThreshold"

