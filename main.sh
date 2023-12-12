# Ensure _output/ exists and contains paper1 through paper10.dta

# Make sure R and Stata are in PATH
# For instance, `export PATH=$PATH:/Applications/Stata/StataMP.app/Contents/MacOS/`
# adds Stata to the path if Stata is in
#`/Applications/Stata/StataMP.app/Contents/MacOS/'
#If running StataSE rather than ME, replace StataMP with StataSE below
StataMP -b do stata-package-setup.do

StataMP -b do recalculate-with-rescaled-outcomes # output logged in recalculate-with-rescaled-outcomes.log

#Install R package dependencies
Rscript r-package-setup.R

# Generate summary tables
Rscript summary-tables.R

# Further applications
Rscript carranza-application.R
Rscript sequeira-2016-application.R
Rscript berkouwer-dean-application.R
