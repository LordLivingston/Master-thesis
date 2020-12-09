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
import delimited "${raw}UK data 2016.csv", clear
/* Create date variable */
replace timestamp = strtrim(timestamp)
gen date= substr(timestamp,1,10)
gen year = substr(date,1,4)
gen month = substr(date,6,2)
gen day = substr(date,9,2)
destring year month day, replace
gen edate = mdy(month, day, year)
gen time_hour = substr(timestamp,-8,.)
gen hour = substr(timestamp,-8,2)
format edate %td
drop year month day date timestamp
gen id2 = _n
/* Generate regression variables (dummies for month and hour)*/
gen d_month=month(edate)
encode(hour), gen(d_hour)
gen renewable = hydro + solar + wind + pumped

/* Define labels */
lab def months 1 "January" 2 "February" 3"March"4"April" 5 "May" 6 "June" 7 "July" ///
8 "August" 9 "September" 10 "October" 11 "November" 12 "December"

lab val d_month months
save "${output}UK data 2016.dta", replace

import delimited "${raw}french-grid raw.csv", clear
/* Create date variable */
gen date= substr(time,1,10)
gen year = substr(date,1,4)
gen month = substr(date,6,2)
gen day = substr(date,9,2)
destring year month day, replace
gen edate = mdy(month, day, year)
gen time_hour = substr(time,-8,.)
gen hour = substr(time,-8,2)
format edate %td
drop year month day date time
/* Restrict dates to 2016 */
keep if year(edate) == 2016
gen id2 = _n
/* Generate regression variables (dummies for month and hour)*/
gen d_month=month(edate)
encode(hour), gen(d_hour)
gen renewable = hydro + solar + wind + pumped

lab def months 1 "January" 2 "February" 3 "March" 4 "April" 5 "May" 6 "June" 7 "July" ///
8 "August" 9 "September" 10 "October" 11 "November" 12 "December"

lab val d_month months

save "${output}France data 2016.dta", replace
