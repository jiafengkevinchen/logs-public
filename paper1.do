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

local paperX_original_command "$paperX_command_start $paperX_arcsinh_var $paperX_command_end'" 

`paperX_original_command'

display _b[$paperX_main_xvar]

*/



**** Paper 1 set-up **** 


*Save the start of the command run (here areg)
global paper1_command_start "areg"

*Save the arcsinh variable name (if possible, use the full variable name, not an abbreviation)
global paper1_arcsinh_var "loginternet"

*Save the remainder of the command after the outcome variable 
global paper1_command_end "treatment i.country_year if allcountries == 1 & ip_count > 10 & distance < 0.1, a(city) cluster(city)"

*Save the main x variable (treatment) as a local
global paper1_main_xvar "treatment"

*Save the location of the coefficient in the paper
global paper1_location "Table 2, Column 1"





*** If the globals above are done right, the following should replicate the 
***    regression run in the paper, and extract the main coefficient ***
use _output/paper1.dta, clear

local paper1_original_command "$paper1_command_start $paper1_arcsinh_var $paper1_command_end" 

`paper1_original_command'
display _b["$paper1_main_xvar"]
