
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




**** Paper 10 set-up **** 

local CONTROLS = "d_charcoalbuy_KSH spend50 savings_KSH b_incomeself_KSH RiskAverse CreditConstrained b_residents b_children d_jikokoalast_years v1_beliefs_annual_mean v1_beliefs_annual_sd" 


*Save the start of the command run (here ivregress 2sls)
global paper10_command_start "xi: ivreg2 "

*Save the arcsinh variable name (if possible, use the full variable name, not an abbreviation)
global paper10_arcsinh_var "ihs_amount_weekly"

*Save the remainder of the command after the outcome variable 
global paper10_command_end " finwtp_USD TsinceV2_* i.SMS_date i.treata i.treatc sms_amount_weekly_pre `CONTROLS' 		 (jikokoa=price_USD), cluster(respondent_id)	"

*Save the main x variable (treatment) as a local
global paper10_main_xvar "jikokoa"

*Save the location of the coefficient in the paper
global paper10_location "Table 2, Column (4)"

*** If the locals above are done right, the following should replicate the 
***    regression run in the paper, and extract the main coefficient ***
use _output/paper10.dta, clear

local paper10_original_command "$paper10_command_start $paper10_arcsinh_var $paper10_command_end" 

`paper10_original_command'
display _b["$paper10_main_xvar"]



// xi: ivreg2 ihs_amount_weekly finwtp_USD TsinceV2_* i.SMS_date i.treata i.treatc sms_amount_weekly_pre `CONTROLS' 		 (jikokoa=price_USD), cluster(respondent_id)	
