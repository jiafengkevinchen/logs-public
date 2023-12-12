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




**** Paper set-up **** 
global treatvars "public private placebo"
global covs_miss "Edn_Matric bl_score_num bl_score_lit bl_score_cft bl_score_grit noncog_score_nm noncog_score_md bl_belsco_num_nm bl_belsco_num_md bl_belsco_lit_ter_nm bl_belsco_lit_ter_md bl_belsco_cft_ter_nm bl_belsco_cft_ter_md bl_belest_index bl_age bl_male bl_emp_7d bl_time_med_nm bl_time_med_md bl_time_presentbias_nm bl_time_presentbias_md bl_risk_high_nm bl_risk_high_md"
global fes "bl_block"
global myvce "cluster( bl_date )"



*Save the start of the command run (here ivregress 2sls)
global paper3_command_start "reghdfe "

*Save the arcsinh variable name (if possible, use the full variable name, not an abbreviation)
global paper3_arcsinh_var "el_emp_hour_all_uncd_i"

*Save the remainder of the command after the outcome variable 
global paper3_command_end "$treatvars $covs_miss , absorb( $fes ) $myvce"

*Save the main x variable (treatment) as a local
global paper3_main_xvar "public"

*Save the location of the coefficient in the paper
global paper3_location "Table 1, column 2"

*** If the locals above are done right, the following should replicate the 
***    regression run in the paper, and extract the main coefficient ***
use _output/paper3.dta, clear

local paper3_original_command "$paper3_command_start $paper3_arcsinh_var $paper3_command_end" 

`paper3_original_command'
display _b["$paper3_main_xvar"]




// use _output/paper3.dta, clear
//
// reghdfe  el_emp_hour_all_uncd_i $treatvars $covs_miss , absorb( $fes ) $myvce
