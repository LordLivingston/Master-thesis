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
	global iem_graph ylabel(, nogrid) graphregion(color(white) lcolor(white)) ///
	                 legend(region(lcolor(white))) bgcolor(white) m(smx) ///
					 msize(small) mcolor(grey) ytitle("") ylab(,format(%12.0fc) ///
					 angle(horizontal)) xtit("") xlabel(20454(70)20789, angle(45) ///
					 format(%tdNN+/+CCYY))
	
	global iem_graph_demand_fr ylabel(, nogrid) graphregion(color(white) lcolor(white)) ///
	                 legend(region(lcolor(white))) bgcolor(white) m(smx) ///
					 msize(small) mcolor(grey) ytitle("") ylab(,format(%12.0fc) ///
					 angle(horizontal)) xtit("") xlab(49 "01 Jan, 12:00am" ///
					 145 "02 Jan, 12:00am" 241 "03 Jan, 12:00am" 337 "04 Jan, 12:00am" ///
					 433 "05 Jan, 12:00am" 529 "06 Jan, 12:00am" 625 "07 Jan, 12:00am", angle(45))
	
	global iem_graph_demand_uk ylabel(, nogrid) graphregion(color(white) lcolor(white)) ///
	                 legend(region(lcolor(white))) bgcolor(white) m(smx) ///
					 msize(small) mcolor(grey) ytitle("") ylab(,format(%12.0fc) ///
					 angle(horizontal)) xtit("") xlab(49 "01 Jan, 12:00am" ///
					 433 "02 Jan, 12:00am" 721 "03 Jan, 12:00am" 1009 "04 Jan, 12:00am" ///
					 1297 "05 Jan, 12:00am" 1585 "06 Jan, 12:00am" 1872 "07 Jan, 12:00am", angle(45))
	
	global iem_line  ylabel(, nogrid) graphregion(color(white) lcolor(white)) ///
	                 legend(region(lcolor(white))) bgcolor(white) ///
					 ytitle("") ylab(,format(%12.0fc) ///
					 angle(horizontal)) xtit("") xlab(12 "01 Jan, 12:00am" ///
					 36 "02 Jan, 12:00am" 60 "03 Jan, 12:00am" 84 "04 Jan, 12:00am" ///
					 108 "05 Jan, 12:00am" 132 "06 Jan, 12:00am" 156 "07 Jan, 12:00am", angle(45))
	
	global iem_welfare ylabel(, nogrid) graphregion(color(white) lcolor(white)) ///
	                   legend(region(lcolor(white)) row(2)) bgcolor(white) ///
					   ytitle("") ylab(,angle(horizontal)) ytit("") xtit("") ///		 
	
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
use "${output}France data 2016.dta", clear

twoway scatter nuclear edate, $iem_graph
graph export "${graphs}B5 French nuclear electricity generation in MWh 2016.emf", replace

twoway scatter coal edate, $iem_graph
graph export "${graphs}B6 French coal electricity generation in MWh 2016.emf", replace

twoway scatter gas edate, $iem_graph
graph export "${graphs}B7 French gas electricity generation in MWh 2016.emf", replace

twoway scatter oil edate, $iem_graph
graph export "${graphs}B8 French oil electricity generation in MWh 2016.emf", replace

twoway scatter demand id2 if id2 <= 672, $iem_graph_demand_fr
graph export "${graphs}B10 French electricity demand in MWh, first week of 2016.emf", replace

use "${output}UK data 2016.dta", clear

twoway scatter nuclear edate, $iem_graph
graph export "${graphs}B1 UK nuclear electricity generation in MWh 2016.emf", replace

twoway scatter coal edate, $iem_graph
graph export "${graphs}B2 UK coal electricity generation in MWh 2016.emf", replace

twoway scatter ccgt edate, $iem_graph
graph export "${graphs}B3 UK CCGT electricity generation in MWh 2016.emf", replace

twoway scatter ocgt edate, $iem_graph
graph export "${graphs}B4 UK OCGT electricity generation in MWh 2016.emf", replace

twoway scatter demand id2 if id2 <= 2015, $iem_graph_demand_uk
graph export "${graphs}B9 UK electricity demand in MWh, first week of 2016.emf", replace

use "${output}Fitted values.dta", clear

twoway (line D_A id if id <= 168), $iem_line
graph export "${graphs}B11 Predicted UK electricity demand in MWh, first week of 2016.emf", replace

twoway (line D_B id if id <= 168), $iem_line
graph export "${graphs}B12 Predicted French electricity demand in MWh, first week of 2016.emf", replace

import delimited "${output}percentage figures.csv", clear
sort tsfrance
gen szenarios = _n - 1

label var csbothuk "Percentage change in consumer surplus from trade, UK" 
label var psbothuk "Percentage change in producer surplus from trade, UK" 
label var tsbothuk "Percentage change in total surplus from trade, UK" 
label var csbothfrance "Percentage change in consumer surplus from trade, France" 
label var psbothfrance "Percentage change in producer surplus from trade, France" 
label var tsbothfrance "Percentage change in total surplus from trade, France"


twoway (line tsuk szenarios, sort lcolor(cranberry)), $iem_welfare
graph export "${graphs}B13 Gains from trade in UK total welfare with increased number of peak periods in the UK.emf", replace

twoway (line csuk szenarios, sort lcolor(cranberry)), $iem_welfare
graph export "${graphs}B14 Gains from trade in UK consumer welfare with increased number of peak periods in the UK.emf", replace

twoway (line psuk szenarios, sort lcolor(cranberry)), $iem_welfare
graph export "${graphs}B15 Gains from trade in UK producer welfare with increased number of peak periods in the UK.emf", replace

twoway (line tsfrance szenarios, sort lcolor(navy)), $iem_welfare
graph export "${graphs}B16 Gains from trade in French total welfare with increased number of peak periods in France.emf", replace

twoway (line csfrance szenarios, sort lcolor(navy)), $iem_welfare
graph export "${graphs}B17 Gains from trade in French consumer welfare with increased number of peak periods in France.emf", replace

twoway (line psfrance szenarios, sort lcolor(navy)), $iem_welfare
graph export "${graphs}B18 Gains from trade in French producer welfare with increased number of peak periods in France.emf", replace

twoway  (line tsbothuk szenarios, sort lcolor(cranberry)) (line tsbothfrance szenarios, sort lcolor(navy)), $iem_welfare
graph export "${graphs}B19 Gains from trade in UK total welfare with increased number of peak periods in France and the UK.emf", replace

twoway (line csbothuk szenarios, sort lcolor(cranberry)) (line csbothfrance szenarios, sort lcolor(navy)), $iem_welfare
graph export "${graphs}B20 Gains from trade in UK consumer welfare with increased number of peak periods in France and the UK.emf", replace

twoway (line psbothuk szenarios, sort lcolor(cranberry)) (line psbothfrance szenarios, sort lcolor(navy)), $iem_welfare
graph export "${graphs}B21 Gains from trade in UK producer welfare with increased number of peak periods in France and the UK.emf", replace
