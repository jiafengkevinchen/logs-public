
local K 10

matrix M = J(`K',5,.)
matrix SEs = J(`K',5,.)


forvalues k = 1/`K'{
	do paper`k'.do

	*Store the original result in the first column
	matrix M[`k',1] = _b["${paper`k'_main_xvar}"]

	*Store the SE
	matrix SEs[`k',1] = _se["${paper`k'_main_xvar}"]

	*Save the number of observations
	local N1 = `e(N)'

	*** Recalculate the effect using asinh(100*Y) ***
	gen yOrig = sinh(${paper`k'_arcsinh_var})
	gen arcsinhY100 = asinh(yOrig*100)

	* Indicator for whether Y > 0
	gen Ygt0 = yOrig > 0 if yOrig < .

	* Define command with arsinh(100Y) on the LHS
	local arcsinhY100_command "${paper`k'_command_start} arcsinhY100 ${paper`k'_command_end}"
	`arcsinhY100_command'

	*Store the modified result in the second column
	matrix M[`k',2] = _b["${paper`k'_main_xvar}"]

	*Store its SE
	matrix SEs[`k',2] = _se["${paper`k'_main_xvar}"]

	*Save the number of observations
	local N2 = `e(N)'

	** Calculate the extensive margin effect ***
	local Ygt0_command "${paper`k'_command_start} Ygt0 ${paper`k'_command_end}"

    *Run it
    `Ygt0_command'

	*Store the result in the third column
	matrix M[`k',3] = _b["${paper`k'_main_xvar}"]

	*Store its SE
	matrix SEs[`k',3] = _se["${paper`k'_main_xvar}"]

	*Save the number of observations
	local N3 = `e(N)'

	if(`N1' != `N2' | `N1' != `N3'){
		_error(1,"Number of observations doesn't match across regs'")
	}

	** Calculate the analogous quantities for log(1+Y) **
	gen log1pY = log(1+yOrig)
	gen log1p100Y = log(1+100*yOrig)

	local log1pY_command "${paper`k'_command_start} log1pY ${paper`k'_command_end}"

    `log1pY_command'
	matrix M[`k',4] = _b["${paper`k'_main_xvar}"]
	matrix SEs[`k',4] = _se["${paper`k'_main_xvar}"]

	local log1p100Y_command "${paper`k'_command_start} log1p100Y ${paper`k'_command_end}"

    `log1p100Y_command'
	matrix M[`k',5] = _b["${paper`k'_main_xvar}"]
	matrix SEs[`k',5] = _se["${paper`k'_main_xvar}"]


}


matrix list M

cap net install dm79.pkg

svmat2 M, names("OriginalY" "YTimes100" "ExtensiveMargin" "Log1pY" "Log1p100Y")
keep OriginalY YTimes100 ExtensiveMargin Log1pY Log1p100Y
keep in 1/`K'

outsheet * using "results/arcsinh-transformation-results.csv", comma replace

svmat2 SEs, names("SE_OriginalY" "SE_YTimes100" "SE_ExtensiveMargin" "SE_Log1pY" "SE_Log1p100Y")
keep SE_OriginalY SE_YTimes100 SE_ExtensiveMargin SE_Log1pY SE_Log1p100Y
keep in 1/`K'

outsheet * using "results/arcsinh-transformation-ses.csv", comma replace
