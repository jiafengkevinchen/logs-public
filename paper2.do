/*

 For each paper, we create the following locals

 paperX_command_start = the start of the command (e.g. "reg" or "areg" or "ivregress 2sls")
 paperX_arcsinh_var = the variable that is in arcsinhs
 paperX_command_end = the remainder of the command after the explanatory variables
 paperX_main_xvar = the main variable of interest (e.g. a treatment indicator)
 paperX_location = the location of this coefficient in the paper


 If this is done correctly, then we should be able to replicate the original results
 and extract the main coefficient by running the following commands 

use _output/paperX.dta, clear

local paperX_original_command "`paperX_command_start' `paperX_arcsinh_var' `paperX_command_end'" 

`paperX_original_command'

display _b[`paperX_main_xvar']

*/




**** Paper 2 set-up **** 

**Specify any local/global variables as needed needed

global xLISTMAIN "  LT_trans lrainLT_buffer  lroads_dis lrainGENO_buff  lrainGENO_sector lrainLT_sector " 
global xLISTGROW "lrainGROW_sector cropLTGROW_transINV cropGROW_transINV lrainLTGROW_sector   "
global xLIST "   lkigali_dis2  lborder_dis2 ltutsi_dis2 rpf lmaincity_dis2 lpopdens larea  "
// drop if rpf==. #added this to the if statement of the command
global clust "commune"


*Save the start of the command run (here ivregress 2sls)
global paper2_command_start "ivregress 2sls "

*Save the arcsinh variable name (if possible, use the full variable name, not an abbreviation)
global paper2_arcsinh_var "lcat2_pcT"

*Save the remainder of the command after the outcome variable 
global paper2_command_end "$xLIST $xLISTMAIN $xLISTGROW     _Ipr* (lcat1_pcT=mil_trans) if rpf != ., clust($clust)"

*Save the main x variable (treatment) as a local
global paper2_main_xvar "lcat1_pcT"

*Save the location of the coefficient in the paper
global paper2_location "Table A.26, column 1"

*** If the locals above are done right, the following should replicate the 
***    regression run in the paper, and extract the main coefficient ***
use _output/paper2.dta, clear

local paper2_original_command "$paper2_command_start $paper2_arcsinh_var $paper2_command_end" 

`paper2_original_command'
display _b[$paper2_main_xvar]


