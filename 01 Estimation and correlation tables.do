/******************************************************************************
The European Internal Energy Market’s Worth to the UK
Authors: Farid Gasmi and Philip Hanspach

Author of the program: Philip Hanspach
*******************************************************************************/
quietly {	
	clear all
	program drop _all
	scalar drop _all
	macro drop _all 
	
	* Set paths: Enter your file directory as path. The following is a suggested
	* folder structure: Path -> (Data analysis, Raw data, Output (->Graphs)).
	* Of course, other folder structures are possible, but the macros for the output
	* and input files have to be changed accordingly
	global path = "YOURPATH"
	global code = "${path}Data analysis\"
	global raw = "${path}Raw data\"
	global output = "${path}Output\"
	global graphs = "${output}Graphs\"
	
	cd "${code}"
	
	version 15.0
	set more off
	set type double
}	
*******************************************************************************/
use "${output}UK data 2016.dta", clear
/* Generate fitted demand data */
reg demand i.d_month i.d_hour
outreg2 using "${output}01 UK demand.xls", lab replace
preserve
clear
set obs 366
gen edate = _n+20453
format edate %td
expand 24
bys edate: gen d_hour = _n
gen d_month = month(edate)
predict D_A
export delimited D_A using "${output}D_A.csv", replace
gen id = _n
tempfile D_A
save `D_A'
restore

reg renewable i.d_month i.d_hour
outreg2 using "${output}01 UK renewables.xls" ,lab replace
preserve
clear
set obs 366
gen edate = _n+20453
format edate %td
expand 24
bys edate: gen d_hour = _n
gen d_month = month(edate)
predict Q_R_A
export delimited Q_R_A using "${output}Q_R_A.csv", replace
gen id = _n
tempfile Q_R_A
save `Q_R_A'
restore

correlate wind solar hydro pumped


use "${output}France data 2016.dta", clear
/* Generate fitted demand data */
reg demand i.d_month i.d_hour
outreg2 using "${output}01 France demand.xls" ,lab replace


/*We adjust French data for difference in time zones: hour h in UK is equal to hour h+1
in France. We norm to UK time. Therefore, the French value at any given time is computed using the 
dummy of the subsequent hour. This is implemented by predicting data first on local French time, then moving
all predicted values one observation back. The final, missing observation (last hour of the last day of the year)
is replaced by the first observation (value for the first hour of the year). We assume that there are no macro trends.*/
preserve
clear
set obs 366
gen edate = _n+20453
format edate %td
expand 24
bys edate: gen d_hour = _n
gen d_month = month(edate)
predict D_B2
gen D_B = D_B2[_n+1]
replace D_B = D_B2[1] if missing(D_B)
export delimited D_B using "${output}D_B.csv", replace
gen id = _n
tempfile D_B
save `D_B'
restore

reg renewable i.d_month i.d_hour
outreg2 using "${output}01 France renewables.xls",lab replace


/*We adjust French data for difference in time zones: hour h in UK is equal to hour h+1
in France. We norm to UK time. Therefore, the French value at any given time is computed using the 
dummy of the subsequent hour. This is implemented by predicting data first on local French time, then moving
all predicted values one observation back. */
preserve
clear
set obs 366
gen edate = _n+20453
format edate %td
expand 24
bys edate: gen d_hour = _n
gen d_month = month(edate)
predict Q_R_B2
gen Q_R_B = Q_R_B2[_n+1]
replace Q_R_B = Q_R_B2[1] if missing(Q_R_B)
export delimited Q_R_B using "${output}Q_R_B.csv", replace
gen id = _n
tempfile Q_R_B
save `Q_R_B'
restore

correlate wind solar hydro biomass pumped

/* Output fitted values for R */
merge 1:1 id using `D_A', nogen
merge 1:1 id using `D_B', nogen
merge 1:1 id using `Q_R_A', nogen
merge 1:1 id using `Q_R_B', nogen

preserve
keep D_A D_B Q_R_A Q_R_B
export delimited "${output}Fitted values.csv", replace
restore
keep edate id D_A D_B Q_R_A Q_R_B
save "${output}Fitted values.dta", replace
