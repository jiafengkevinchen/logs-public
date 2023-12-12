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




**** Paper 5 set-up ****

**Specify any local/global variables as needed needed

local csofdeath "allcod"
local pctl "0"
local collab "ncoauth"


*Save the start of the command run (here ivregress 2sls)
global paper5_command_start "areg "

*Save the arcsinh variable name (if possible, use the full variable name, not an abbreviation)
global paper5_arcsinh_var "ihs_prox_`pctl'_`collab'"

*Save the remainder of the command after the outcome variable
global paper5_command_end "after_death after_death_cmmn i.year i.field_age if `csofdeath'==1, absorb(id) cluster(star_id)"

*Save the main x variable (treatment) as a local
global paper5_main_xvar "after_death"

*Save the location of the coefficient in the paper
global paper5_location "Figure 3b, 1st coef."

*** If the locals above are done right, the following should replicate the
***    regression run in the paper, and extract the main coefficient ***
use _output/paper5.dta, clear

local paper5_original_command "$paper5_command_start $paper5_arcsinh_var $paper5_command_end"

`paper5_original_command'
display _b["$paper5_main_xvar"]

