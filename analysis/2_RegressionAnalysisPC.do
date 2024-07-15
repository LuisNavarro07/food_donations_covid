*******************************************************************************
**** In-kind and Money Donations 
**** Authors: Denvil Duncan, Shelley Suttles and Luis Navarro
**** Code by: Luis Navarro
**** Last Update: August 2022
**** Script: Regression Analysis - Per Capita Outcomes 
********************************************************************************

use "${bt}/gleaners_food_regression.dta", clear 
merge m:1 fips using "${bt}/quartiles_fipcrosswalk.dta", keep(match master) nogen
/// Assumption: Drop observations that do not appear in the quartile cross walk data (ie: they only have a couple observations by year.)
drop if  PalletTotalWeightq == .
rename (PalletTotalWeightq donation_sumq) (Quartile Quartile_Financial)
*******************************************************************************
/// Rename Outcomes to Use Per Capite Measures and keep the code intact
drop food foodng money 
rename (foodpc foodngpc moneypc) (food foodng money)
global outcomes food foodng money
label variable food "In-Kind Donation Per Capita"
label variable foodng "NG In-Kind Donation Per Capita"
label variable money "Fianncial Donation Per Capita"
*****************************************************************************

*******************************************************************************
/// Descriptive Statistics 

/// Table 1. Descriptive Statistics  
global variables $outcomes $controls $economic 
format $variables %12.4fc
estpost summarize $variables, listwise 
esttab ., cells("mean(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3))") nomtitles nonumber label 
esttab . using "${pg}/descriptive_stats_pc.tex" , cells("mean(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3))") nomtitles nonumber label replace 


/// Table 2. Mean Comparison between Pre and Post Pandemic 
sum $outcomes $controls $economic
*iebaltab $outcomes $controls $economic, grpvar(post) savetex(${pg}\MeanComparisonPrePostCovidpc.tex) ${bal_opts}  
/// Model 1 - Determinants of Donation Behavior 
/// Indiana Analysis - Different Specifications 
/// Regression Analysis 
local varlist $outcomes
foreach var of local varlist {
/// 0. No Covariates
qui eststo `var'0a: reghdfe `var' ry if year < 2020, $feopts
quietly estadd local econ "No", replace
quietly estadd local demo "No", replace
quietly estadd ysumm, replace
	
/// 1. Controls
qui eststo `var'1a: reghdfe `var' $economic ry if year < 2020, $feopts
quietly estadd local econ "Yes", replace
quietly estadd local demo "No", replace
quietly estadd ysumm, replace

/// 2. Controls + Economic 
qui eststo `var'2a: reghdfe `var' $economic $controls ry if year < 2020 , $feopts
quietly estadd local econ "Yes", replace
quietly estadd local demo "Yes", replace
quietly estadd ysumm, replace
}

/// Table for Paper - Models with and without controls 
esttab foodng1a foodng2a food1a food2a money1a money2a, replace keep($economic $controls) order($economic $controls) $table_opts
qui esttab foodng1a foodng2a food1a food2a money1a money2a using "${pg}/model1_indiana_pc.tex", replace keep($economic $controls) $table_opts

*******************************************************************************
********************************************************************************

/// Model 2 - Effects of COVID - 19 

local varlist food money foodng
foreach var of local varlist {
/// 0. No Covariates
qui eststo `var'0b: reghdfe `var' post ry, $feopts
quietly estadd local econ "No", replace
quietly estadd local demo "No", replace
quietly estadd ysumm, replace
	
/// 1. Controls
qui eststo `var'1b: reghdfe `var' post $economic ry, $feopts
quietly estadd local econ "Yes", replace
quietly estadd local demo "No", replace
quietly estadd ysumm, replace

/// 2. Controls + Economic 
qui eststo `var'2b: reghdfe `var' post $economic $controls ry , $feopts
quietly estadd local econ "Yes", replace
quietly estadd local demo "Yes", replace
quietly estadd ysumm, replace

}

/// Table for Paper - Models with and without controls 
esttab foodng1b foodng2b food1b food2b money1b money2b, replace keep(post $economic $controls)  $table_opts
esttab foodng1b foodng2b food1b food2b money1b money2b using "${pg}/model2_covid_pc.tex", replace keep(post $economic $controls) $table_opts

*******************************************************************************
*******************************************************************************

/// Model 2 (Quartile Analysis) - Effects of COVID 
/// Regression Analysis 
cap drop quart* postquart*
tab Quartile, gen(quart)
forvalues j=1(1)4{
	gen postquart`j' = post*quart`j'
	label variable postquart`j' "Post March 2020 (Quartile `j')"
	label variable quart`j' "Quartile `j'"
}

global quart_vars quart2 quart3 quart4 postquart2 postquart3 postquart4
local varlist food foodng
foreach var of local varlist {
/// 0. No Covariates
qui eststo `var'0bq: reghdfe `var' post $quart_vars ry, $feopts
quietly estadd local econ "No", replace
quietly estadd local demo "No", replace
quietly estadd ysumm, replace
	
/// 1. Controls
qui eststo `var'1bq: reghdfe `var' post $quart_vars $economic ry, $feopts
quietly estadd local econ "Yes", replace
quietly estadd local demo "No", replace
quietly estadd ysumm, replace

/// 2. Controls + Economic 
qui eststo `var'2bq: reghdfe `var' post $quart_vars $economic $controls ry , $feopts
quietly estadd local econ "Yes", replace
quietly estadd local demo "Yes", replace
quietly estadd ysumm, replace

}

******************
/// Financial Donations 
cap drop quart* postquart*
tab Quartile_Financial, gen(quart)
forvalues j=1(1)4{
	gen postquart`j' = post*quart`j'
	label variable postquart`j' "Post March 2020 (Quartile `j')"
	label variable quart`j' "Quartile `j'"
}

local varlist money
foreach var of local varlist {
/// 0. No Covariates
qui eststo `var'0bq: reghdfe `var' post $quart_vars ry, $feopts
quietly estadd local econ "No", replace
quietly estadd local demo "No", replace
quietly estadd ysumm, replace
	
/// 1. Controls
qui eststo `var'1bq: reghdfe `var' post $quart_vars $economic ry, $feopts
quietly estadd local econ "Yes", replace
quietly estadd local demo "No", replace
quietly estadd ysumm, replace

/// 2. Controls + Economic 
qui eststo `var'2bq: reghdfe `var' post $quart_vars $economic $controls ry , $feopts
quietly estadd local econ "Yes", replace
quietly estadd local demo "Yes", replace
quietly estadd ysumm, replace
}

/// Table for Paper - Models with and without controls 
esttab foodng1bq foodng2bq food1bq food2bq money1bq money2bq, replace keep(post postquart2 postquart3 postquart4)  $table_opts
esttab foodng1bq foodng2bq food1bq food2bq money1bq money2bq using "${pg}/model2_covid_quartiles_pc.tex", replace keep(post postquart2 postquart3 postquart4) $table_opts order(post $economic $controls) 

********************************************************************************


*********************************************************************************

/// Model 3 - Donations during Covid 
local varlist food money foodng
foreach var of local varlist {
/// 0. No Covariates
qui eststo `var'0c: reghdfe `var' casespc if post == 1, $feopts
quietly estadd local econ "No", replace
quietly estadd local demo "No", replace
quietly estadd ysumm, replace

/// 1. Controls
qui eststo `var'1c: reghdfe `var' casespc $economic if post == 1, $feopts
quietly estadd local econ "Yes", replace
quietly estadd local demo "No", replace
quietly estadd ysumm, replace
/// 2. Controls + Economic 
qui eststo `var'2c: reghdfe `var' casespc $economic $controls if post == 1, $feopts
quietly estadd local econ "Yes", replace
quietly estadd local demo "Yes", replace
quietly estadd ysumm, replace
}

/// Table for Paper - Models with and without controls 
esttab foodng1c foodng2c food1c food2c money1c money2c, replace keep(casespc $economic $controls) $table_opts order(casespc $economic $controls)
qui esttab foodng1c foodng2c food1c food2c money1c money2c using "${pg}/model3_covidpost_pc.tex", replace $table_opts order(casespc $economic $controls)

********************************************************************************


/// Model 3. Quartile Analysis 
drop quart* postquart*
tab Quartile, gen(quart)
forvalues j=1(1)4{
	gen postquart`j' = casespc*quart`j'
	label variable postquart`j' "Post March 2020 (Quartile `j')"
	label variable quart`j' "Quartile `j'"
}

local varlist food foodng
foreach var of local varlist {
/// 0. No Covariates
qui eststo `var'0cq: reghdfe `var' casespc $quart_vars if post == 1, $feopts
quietly estadd local econ "Yes", replace
quietly estadd local demo "No", replace
quietly estadd ysumm, replace

/// 1. Controls
qui eststo `var'1cq: reghdfe `var' casespc $quart_vars $economic if post == 1, $feopts
quietly estadd local econ "Yes", replace
quietly estadd local demo "No", replace
quietly estadd ysumm, replace
/// 2. Controls + Economic 
qui eststo `var'2cq: reghdfe `var' casespc $quart_vars $economic $controls if post == 1, $feopts
quietly estadd local econ "Yes", replace
quietly estadd local demo "Yes", replace
quietly estadd ysumm, replace
}


******************
/// Financial Donations 
drop quart* postquart*
tab Quartile_Financial, gen(quart)
forvalues j=1(1)4{
	gen postquart`j' = casespc*quart`j'
	label variable postquart`j' "Post March 2020 (Quartile `j')"
	label variable quart`j' "Quartile `j'"
}

local varlist money
foreach var of local varlist {
/// 0. No Covariates
qui eststo `var'0cq: reghdfe `var' casespc $quart_vars if post == 1, $feopts
quietly estadd local econ "Yes", replace
quietly estadd local demo "No", replace
quietly estadd ysumm, replace

/// 1. Controls
qui eststo `var'1cq: reghdfe `var' casespc $quart_vars $economic if post == 1, $feopts
quietly estadd local econ "Yes", replace
quietly estadd local demo "No", replace
quietly estadd ysumm, replace
/// 2. Controls + Economic 
qui eststo `var'2cq: reghdfe `var' casespc $quart_vars $economic $controls if post == 1, $feopts
quietly estadd local econ "Yes", replace
quietly estadd local demo "Yes", replace
quietly estadd ysumm, replace
}



/// Table for Paper - Models with and without controls 
esttab foodng1cq foodng2cq food1cq food2cq money1cq money2cq, replace keep(casespc postquart2 postquart3 postquart4) $table_opts order(casespc postquart2 postquart3 postquart4)
esttab foodng1cq foodng2cq food1cq food2cq money1cq money2cq using "${pg}/model3_covid_quartiles_pc.tex", replace keep(casespc postquart2 postquart3 postquart4) $table_opts order(casespc postquart2 postquart3 postquart4) 

********************************************************************************

exit 
