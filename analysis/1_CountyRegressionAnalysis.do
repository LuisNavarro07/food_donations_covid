*******************************************************************************
**** In-kind and Money Donations 
**** Authors: Denvil Duncan, Shelley Suttles and Luis Navarro
**** Code by: Luis Navarro
**** Last Update: August 2022
**** Script: Regression Analysis  
********************************************************************************
/*
Links for coefficient interpretation 
- https://cscu.cornell.edu/wp-content/uploads/83_logv.pdf
- https://davegiles.blogspot.com/2011/03/dummies-for-dummies.html 
*/

cap log using "${ao}/log_regression_analysis.log", replace 
use "${bt}/gleaners_food_regression.dta", clear 
merge m:1 fips using "${bt}/quartiles_fipcrosswalk.dta", keep(match master) nogen
/// Assumption: Drop observations that do not appear in the quartile cross walk data (ie: they only have a couple observations by year.)
drop if  PalletTotalWeightq == .
rename (PalletTotalWeightq donation_sumq) (Quartile Quartile_Financial)
*******************************************************************************
/// Linear Regression Options 
global feopts absorb(month) vce(cluster fips)
global outcomes food foodng money
global controls female white hispanic age17 age44 age64
global economic unemployment_rate logsnap_person food_price_index
*global controls female white hispanic age17 age44 
*global economic unemployment_rate logsnap_person 
global common_opts label mtitles("NG In-Kind" "NG In-Kind" "In-Kind" "In-Kind" "Financial" "Financial") se(%12.4fc) b(%12.4fc)
global table_opts $common_opts s(demo econ ymean N F,label("Demographic Controls" "Economic Controls" "Mean of Dep. Variable" "Observations" "F-Stat")) sfmt(%12.0fc) 
*****************************************************************************
*******************************************************************************
drop post 
gen post = mofd > tm(2020m3)
cap label variable post "Post April 2020"
 
/// Model 1 - Determinants of Donation Behavior 
/// Indiana Analysis - Different Specifications 
/// Regression Analysis 
global modopts1 ry if year < 2020, $feopts

local varlist food money foodng 
foreach var of local varlist {
/// tempfiles
tempfile `var'0a
tempfile `var'1a
tempfile `var'2a

/// 0. No Covariates
qui sum `var'
qui eststo `var'0a: reghdfe `var' $modopts1
quietly estadd local econ = "No", replace
quietly estadd local demo = "No", replace
quietly estadd local timefe = "Yes", replace
quietly estadd ysumm, replace
quietly regsave using ``var'0a',  $regsaveopts table(`var'0a, parentheses(stderr) brackets(pval) format(%8.4fc) asterisk(10 5 1)) addlabel(economic,`econ', demographic, `demo', timefe, `timefe')

/// 1. Controls
qui sum `var'
qui eststo `var'1a: reghdfe `var' $economic $modopts1
quietly estadd local econ = "Yes", replace
quietly estadd local demo = "No", replace
quietly estadd local timefe = "Yes", replace
quietly estadd ysumm, replace
quietly regsave using ``var'1a',  $regsaveopts table(`var'1a, parentheses(stderr) brackets(pval) format(%8.4fc) asterisk(10 5 1)) addlabel(economic,`econ', demographic, `demo', timefe, `timefe')


/// 2. Controls + Economic 
qui sum `var'
qui eststo `var'2a: reghdfe `var' $economic $controls $modopts1
quietly estadd local econ = "Yes", replace
quietly estadd local demo = "Yes", replace
quietly estadd local timefe = "Yes", replace
quietly estadd ysumm, replace
quietly regsave using ``var'2a',  $regsaveopts table(`var'2a, parentheses(stderr) brackets(pval) format(%8.4fc) asterisk(10 5 1)) addlabel(economic,`econ', demographic, `demo', timefe, `timefe')
}


/// Table for Paper - Models with and without controls 
esttab foodng1a foodng2a food1a food2a money1a money2a, replace keep($economic $controls) order($economic $controls) $table_opts
qui esttab foodng1a foodng2a food1a food2a money1a money2a using "${pg}/model1_indiana.tex", replace keep($economic $controls) order($economic $controls) $table_opts

/// Coefficient Interpretation 
********************************************************************************
/// 1. Mean of Dep Variable 
sum PalletTotalWeight if year < 2020
scalar meanmt_ptw = r(mean)/2204.6 
sum PalletTotalWeight_NonGov if year < 2020
scalar meanmt_ptwng = r(mean)/2204.6 

/// Interpretation: a one unit increse in x leads to an increase of 100*(exp(beta) -1) 
/// Excluding TEFAP
qui esttab foodng2a, drop(_cons) label wide se b
scalar betat_unmp = 100*(exp(r(coefs)[1,1]) -1)
*scalar betat_food = 100*(exp(r(coefs)[2,1]) -1)
scalar betat_snap = 100*(1.01^(r(coefs)[2,1]) -1)
dis "A 1 pp increase in unemployment is associated with a " betat_unmp " % change in donations" 
dis "A 1 pp increase in snap recipients is associated with a " betat_snap " % change in donations" 

/// All Donations 
qui esttab food2a, drop(_cons) label wide se b
scalar betat_unmp = 100*(exp(r(coefs)[1,1]) -1)
*scalar betat_food = 100*(exp(r(coefs)[2,1]) -1)
scalar betat_snap = 100*(1.01^(r(coefs)[2,1]) -1)
dis "A 1 pp increase in unemployment is associated with a " betat_unmp " % change in donations" 
dis "A 1 pp increase in snap recipients is associated with a " betat_snap " % change in donations" 

dis meanmt_ptwng*betat_unmp/100
dis meanmt_ptwng

/// Financial Donations 
qui esttab money2a, drop(_cons) label wide se b
scalar betat_unmp = 100*(exp(r(coefs)[1,1]) -1)
*scalar betat_food = 100*(exp(r(coefs)[2,1]) -1)
scalar betat_snap = 100*(1.01^(r(coefs)[2,1]) -1)
dis "A 1 pp increase in unemployment is associated with a " betat_unmp " % change in financinal donations" 
dis "A 1 pp increase in snap recipients is associated with a " betat_snap " % change in financinal donations" 

*dis "A 1 pp increase in food prices is associated with a " betat_food " % change in financinal donations" 
*******************************************************************************

********************************************************************************

/// Model 2 - Effects of COVID - 19: Post Variable 

local varlist food money foodng
foreach var of local varlist {
tempfile `var'0b
tempfile `var'1b
tempfile `var'2b

/// 0. No Covariates
qui sum `var'
qui eststo `var'0b: reghdfe `var' post ry, $feopts
quietly estadd local econ = "No", replace
quietly estadd local demo = "No", replace
quietly estadd local timefe = "Yes", replace
quietly estadd ysumm, replace
quietly regsave using ``var'0b',  $regsaveopts table(`var'0b, parentheses(stderr) brackets(pval) format(%8.4fc) asterisk(10 5 1)) addlabel(economic,`econ', demographic, `demo', timefe, `timefe')
	
/// 1. Controls
qui sum `var'
qui eststo `var'1b: reghdfe `var' post $economic ry, $feopts
quietly estadd local econ = "Yes", replace
quietly estadd local demo = "No", replace
quietly estadd local timefe = "Yes", replace
quietly estadd ysumm, replace
quietly regsave using ``var'1b',  $regsaveopts table(`var'1b, parentheses(stderr) brackets(pval) format(%8.4fc) asterisk(10 5 1)) addlabel(economic,`econ', demographic, `demo', timefe, `timefe')

/// 2. Controls + Economic 
qui sum `var'
qui eststo `var'2b: reghdfe `var' post $economic $controls ry , $feopts
quietly estadd local econ = "Yes", replace
quietly estadd local demo = "Yes", replace
quietly estadd local timefe = "Yes", replace
quietly estadd ysumm, replace
quietly regsave using ``var'2b',  $regsaveopts table(`var'2b, parentheses(stderr) brackets(pval) format(%8.4fc) asterisk(10 5 1)) addlabel(economic,`econ', demographic, `demo', timefe, `timefe')

}

/// Table for Paper - Models with and without controls 
	
esttab foodng1b foodng2b food1b food2b money1b money2b, replace keep(post $economic $controls) $table_opts order(post $economic $controls)
esttab foodng1b foodng2b food1b food2b money1b money2b using "${pg}/model2_covid.tex", replace keep(post $economic $controls) $table_opts order(post $economic $controls)
/// Coefficient Interpretation 
********************************************************************************

/// 1. Mean of Dep Variable 
sum PalletTotalWeight if post == 0 
scalar meanmt_ptw = r(mean)/2204.6 
sum PalletTotalWeight_NonGov if post == 0 
scalar meanmt_ptwng = r(mean)/2204.6 

/// Interpretation: a one unit increse in x leads to an increase of 100*(exp(beta) -1) 
/// Excluding TEFAP
qui esttab foodng2b, drop(_cons) label wide ci b
scalar betat_post = 100*(exp(r(coefs)[1,1]) -1)
*scalar betat_unmp = 100*(exp(r(coefs)[2,1]) -1)
*scalar betat_snap = 100*(1.01^(r(coefs)[3,1]) -1)
dis "After the pandemic, donations increased by " betat_post " %" 
*dis "A 1 pp increase in unemployment is associated with a " betat_unmp " % change in donations"  
*dis "A 1 pp increase in snap recipients is associated with a " betat_snap " % change in donations" 

/// All Donations 
qui esttab food2b, drop(_cons) label wide ci b
scalar betat_post = 100*(exp(r(coefs)[1,1]) -1)
*scalar betat_unmp = 100*(exp(r(coefs)[2,1]) -1)
*scalar betat_snap = 100*(1.01^(r(coefs)[3,1]) -1)
dis "After the pandemic, donations increased by " betat_post " %" 
*dis "A 1 pp increase in unemployment is associated with a " betat_unmp " % change in donations" 
*dis "A 1 pp increase in snap recipients is associated with a " betat_snap " % change in donations" 

dis meanmt_ptwng*betat_post/100
dis meanmt_ptwng

/// Financial Donations 
esttab money2b, drop(_cons) label wide se b
scalar betat_post = 100*(exp(r(coefs)[1,1]) -1)
dis "After the pandemic, financial donations increased by " betat_post " %" 

sum donation_sum if post == 0 
scalar meanmt_money = r(mean)/1000
dis meanmt_money
dis meanmt_money*betat_post/100

/// Excluding TEFAP - No Controls 
esttab foodng1b, drop(_cons) label wide ci b
scalar betat_post = 100*(exp(r(coefs)[1,1]) -1)
dis "After the pandemic, donations increased by " betat_post " %" 

********************************************************************************
********************************************************************************

/// Model 2 - Cumulative Effects: Length of the Post Variable 
gen timepost = 0 
replace timepost = 1 if mofd >= tm(2020m4) 
replace timepost = 2 if mofd >= tm(2021m1) 
replace timepost = 3 if mofd > tm(2021m12)
tab mofd timepost

tab timepost, gen(postd)
label variable postd2 "Apr 2020 -Dec 2020"
label variable postd3 "Jan 2021- Dec 2021"
label variable postd4 "Jan 2022- May 2022"
replace postd2 = 1 if mofd >= tm(2020m4) 
replace postd3 = 1 if mofd >= tm(2021m1) 
replace postd4 = 1 if mofd > tm(2021m12)
*table mofd if post == 1, content(mean postd2 mean postd3 mean postd4)

****
global postvars postd2 postd3 postd4 
/// Post 2021 Comparison 
local varlist food money foodng
foreach var of local varlist {
*tempfile `var'0bp
*tempfile `var'1bp
*tempfile `var'2bp

/// 0. No Covariates
qui sum `var'
qui eststo `var'0bp: reghdfe `var' $postvars ry , $feopts
qui test postd2 = postd3 
quietly estadd local pd1 = round(r(p),0.004), replace 
qui test postd3 = postd4
quietly estadd local pd2 = round(r(p),0.004), replace 
quietly estadd local econ = "No", replace
quietly estadd local demo = "No", replace  
quietly estadd local timefe = "Yes", replace
quietly estadd ysumm, replace
*quietly regsave using ``var'0bp',  $regsaveopts table(`var'0bp, parentheses(stderr) brackets(pval) format(%8.4fc) asterisk(10 5 1)) addlabel(economic,`econ', demographic, `demo', timefe, `timefe', pd1, `pd1', pd2, `pd2')

/// 1. Economic
qui sum `var'
qui eststo `var'1bp: reghdfe `var' $postvars unemployment_rate food_price_index ry , $feopts
qui test postd2 = postd3 
quietly estadd local pd1 = round(r(p),0.004), replace 
qui test postd3 = postd4
quietly estadd local pd2 = round(r(p),0.004), replace 
quietly estadd local econ = "Yes", replace
quietly estadd local demo = "No", replace  
quietly estadd local timefe = "Yes", replace
quietly estadd ysumm, replace
*quietly regsave using ``var'1bp',  $regsaveopts table(`var'1bp, parentheses(stderr) brackets(pval) format(%8.4fc) asterisk(10 5 1)) addlabel(economic,`econ', demographic, `demo', timefe, `timefe', pd1, `pd1', pd2, `pd2')

/// 2. Controls + Economic 
qui sum `var'
qui eststo `var'2bp: reghdfe `var' $postvars unemployment_rate food_price_index ry $controls , $feopts
qui test postd2 = postd3 
quietly estadd local pd1 = round(r(p),0.004), replace 
qui test postd3 = postd4
quietly estadd local pd2 = round(r(p),0.004), replace 
quietly estadd local econ = "Yes", replace
quietly estadd local demo = "Yes", replace  
quietly estadd local timefe = "Yes", replace
quietly estadd ysumm, replace
*quietly regsave using ``var'2bp',  $regsaveopts table(`var'2bp, parentheses(stderr) brackets(pval) format(%8.4fc) asterisk(10 5 1)) addlabel(economic,`econ', demographic, `demo', timefe, `timefe', pd1, `pd1', pd2, `pd2')

}

/// Table for Paper - Models with and without controls 
esttab foodng1bp foodng2bp food1bp food2bp money1bp money2bp, replace keep($postvars) $common_opts s(demo econ ymean pd1 pd2 N F,label("Demographic Controls" "Economic Controls" "Mean of Dep. Variable" "Pval Test 2020" "Pval Test 2021" "Observations" "F-Stat")) sfmt(%12.0fc)
esttab foodng1bp foodng2bp food1bp food2bp money1bp money2bp using "${pg}/model2_covid_post.tex", replace keep($postvars) $common_opts s(demo econ ymean pd1 pd2 N F,label("Demographic Controls" "Economic Controls" "Mean of Dep. Variable" "Pval Test 2020" "Pval Test 2021" "Observations" "F-Stat")) sfmt(%12.0fc)

*******************************************************************************
/// Coefficient Interpretation 
/// 1. Mean of Dep Variable 
qui sum PalletTotalWeight if post == 0
scalar meanmt_ptw = r(mean)/2204.6 
qui sum PalletTotalWeight_NonGov if post == 0
scalar meanmt_ptwng = r(mean)/2204.6 
qui sum donation_sum if post == 0
scalar meanmt_money = r(mean)/1000

/// Interpretation: a one unit increse in x leads to an increase of 100*(exp(beta) -1) 
/// Excluding TEFAP
qui esttab foodng2bp, drop(_cons) label wide ci b
scalar betat_20 = 100*(exp(r(coefs)[1,1]) -1)
scalar betat_21 = 100*(exp(r(coefs)[2,1]) -1)
scalar betat_22 = 100*(exp(r(coefs)[3,1]) -1)
dis "After the pandemic, private donations increased by " betat_20 " %" 
dis "During 2021, private donations decreased by " betat_21 " %" 
dis "During 2022, private donations decreased by " betat_22 " %"
/// All Donations 
qui esttab food2bp, drop(_cons) label wide ci b
scalar betat_20 = 100*(exp(r(coefs)[1,1]) -1)
scalar betat_21 = 100*(exp(r(coefs)[2,1]) -1)
scalar betat_22 = 100*(exp(r(coefs)[3,1]) -1)
dis "After the pandemic, all in-kind donations increased by " betat_20 " %" 
dis "During 2021, all in-kind donations decreased by " betat_21 " %" 
dis "During 2022, all in-kind donations decreased by " betat_22 " %"


/// Financial Donations 
qui esttab money2bp, drop(_cons) label wide se b
scalar betat_20 = 100*(exp(r(coefs)[1,1]) -1)
scalar betat_21 = 100*(exp(r(coefs)[2,1]) -1)
scalar betat_22 = 100*(exp(r(coefs)[3,1]) -1)
dis "After the pandemic, financial donations increased by " betat_20 " %" 
dis "During 2021, financial donations decreased by " betat_21 " %" 
dis "During 2022, financial donations decreased by " betat_22 " %"


dis meanmt_money
dis meanmt_money*betat_post/100



********************************************************************************
*********************************************************************************
*global economic food_price_index logsnap_person
/// Model 3 - Donations during Covid 

local varlist foodng food money
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

label variable post "Post April 2020"
/// Table for Paper - Models with and without controls 
esttab foodng1c foodng2c food1c food2c money1c money2c, replace keep(casespc $economic $controls) $table_opts order(casespc $economic $controls)
esttab foodng1c foodng2c food1c food2c money1c money2c using "${pg}/model3_covidpost.tex", replace $table_opts order(casespc $economic $controls)

********************************************************************************
/// 1. Mean of Dep Variable 
sum casespc if post == 1
qui sum PalletTotalWeight if post == 1
scalar meanmt_ptw = r(mean)/2204.6 
qui sum PalletTotalWeight_NonGov if post == 1
scalar meanmt_ptwng = r(mean)/2204.6 
qui sum donation_sum if post == 1
scalar meanmt_money = r(mean)/1000

/// Interpretation: a one unit increse in x leads to an increase of 100*(exp(beta) -1) 
/// A one standard deviation. 
/// Excluding TEFAP
qui esttab foodng2c, drop(_cons) label wide ci b
scalar betat_cases = 100*(exp(r(coefs)[1,1]) -1)
scalar betat_snap = 100*(1.01^(r(coefs)[3,1]) -1)
dis "An increase of 1 unit in cases per thousand people leads to a change of " betat_cases " % in private in-kind donations" 
dis "A 1 pp increase in snap recipients is associated with a " betat_snap " % change in private in-kind donations" 

/// All Donations 
qui esttab food2c, drop(_cons) label wide ci b
scalar betat_cases = 100*(exp(r(coefs)[1,1]) -1)
scalar betat_snap = 100*(1.01^(r(coefs)[3,1]) -1)
dis "An increase of 1 unit in cases per thousand people leads to a change of " betat_cases " % in all in-kind donations" 
dis "A 1 pp increase in snap recipients is associated with a " betat_snap " % change in private in-kind donations" 

dis meanmt_ptwng*betat_cases/100
dis meanmt_ptwng

/// Financial Donations 
qui esttab money2c, drop(_cons) label wide se b
scalar betat_cases = 100*(exp(r(coefs)[1,1]) -1)
scalar betat_snap = 100*(1.01^(r(coefs)[3,1]) -1)
dis "An increase of 1 unit in cases per thousand people leads to a change of " betat_cases "%  in financial donations" 
dis "A 1 pp increase in snap recipients is associated with a " betat_snap " % change in financial donations" 

dis meanmt_money
dis meanmt_money*betat_post/100

/// Formula for Casespc 

********************************************************************************

/*
tab mofd if post == 1, matrow(F)
global periods = r(r)

forvalues i = 1(1)$periods {
	tab mofd post if post == 1 & mofd <= F[`i',1]
	local varlist food money
	foreach var of local varlist {
	/// 2. Controls + Economic 
	qui eststo `var'cum`i': reghdfe `var' post $economic $controls ry if mofd <= F[`i',1] , $feopts
	*esttab `var'cum`i', keep(post) p b 
}	
}

label variable post " "
local marker msize(vsmall) msymbol(circle) mcolor(black)
local lines lcolor(black) lwidth(thin) 
local foodcumplot "(foodcum1, `marker' ciopts(recast(rcap) `lines'))"
local moneycumplot "(moneycum1, `marker' ciopts(recast(rcap) `lines'))"
forvalues i = 2(1)$periods {
	local foodcumplot " `foodcumplot' (foodcum`i', `marker' ciopts(recast(rcap) `lines'))" 
	local moneycumplot " `moneycumplot' (moneycum`i', `marker' ciopts(recast(rcap) `lines'))"
	
}
coefplot `foodcumplot' , vertical keep(post) legend(off) title("In-Kind Donations:Cumulative Effects of Covid-19", pos(11) size(small)) name(foodcumul, replace) xtitle("Months After March 2020") ylabel(,angle(0)) yline(0)
coefplot `moneycumplot' , vertical keep(post) legend(off) title("Financial Donations:Cumulative Effects of Covid-19", pos(11) size(small)) name(moneycumul, replace) xtitle("Months After March 2020") ylabel(,angle(0)) yline(0)
graph combine foodcumul moneycumul, xcommon ycommon rows(2) name(cumulcomb, replace)
graph export "${pg}\model2_cumulative.png", $export
*/

********************************************************************************
/// Post pandemic variable - Excluding March
label variable post "Post April 2020"
*******************************************************************************
global economic unemployment_rate food_price_index logsnap_person
/// Model 2 (Quartile Analysis) - Effects of COVID 
/// Regression Analysis 
cap drop quart* postquart*
tab Quartile, gen(quart)
forvalues j=1(1)4{
	gen postquart`j' = post*quart`j'
	label variable postquart`j' "Interaction (Quartile `j')"
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
label variable post "Post April 2020"
******************
/// Financial Donations 
cap drop quart* postquart*
tab Quartile_Financial, gen(quart)
forvalues j=1(1)4{
	gen postquart`j' = post*quart`j'
	label variable postquart`j' "Interaction (Quartile `j')"
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
esttab foodng1bq foodng2bq food1bq food2bq money1bq money2bq using "${pg}/model2_covid_quartiles.tex", replace keep(post postquart2 postquart3 postquart4) $table_opts order(post $economic $controls) 
label variable post "Post April 2020"

*******************************************************************************
/// Coefficients Interpretation 
/// 1. Mean of Dep Variable 
sum PalletTotalWeight if post == 1
scalar meanmt_ptw = r(mean)/2204.6 
sum PalletTotalWeight_NonGov if post == 1
scalar meanmt_ptwng = r(mean)/2204.6 
sum donation_sum if post == 1
scalar meanmt_money = r(mean)/1000

/// Interpretation: a one unit increse in x leads to an increase of 100*(exp(beta) -1) 
/// Excluding TEFAP
esttab foodng2c, drop(_cons) label wide ci b
scalar betat_cases = 100*(exp(r(coefs)[1,1]) -1)*0.01
dis "An increase of 0.01 in cases per capita leads to a change of " betat_cases " % in in-kind donations" 

/// All Donations 
esttab food2c, drop(_cons) label wide ci b
scalar betat_cases = 100*(exp(r(coefs)[1,1]) -1)*0.01
dis "An increase of 0.01 in cases per capita leads to a change of " betat_cases " % in in-kind donations" 


dis meanmt_ptwng*betat_cases/100
dis meanmt_ptwng


/// Excluding TEFAP - No Controls 
esttab foodng1b, drop(_cons) label wide ci b
scalar betat_post = 100*(exp(r(coefs)[1,1]) -1)
dis "After the pandemic, donations increased by " betat_post " %" 

********************************************************************************
********************************************************************************


/// Model 3. Quartile Analysis 
drop quart* postquart*
tab Quartile, gen(quart)
forvalues j=1(1)4{
	gen postquart`j' = casespc*quart`j'
	label variable postquart`j' "Interaction (Quartile `j')"
	label variable quart`j' "Quartile `j'"
}


global quart_vars quart2 quart3 quart4 postquart2 postquart3 postquart4
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
	label variable postquart`j' "Interaction (Quartile `j')"
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
esttab foodng1cq foodng2cq food1cq food2cq money1cq money2cq using "${pg}/model3_covid_quartiles.tex", replace keep(casespc postquart2 postquart3 postquart4) $table_opts order(casespc postquart2 postquart3 postquart4) 

********************************************************************************
cap log close 
exit 

********************************************************************************
/// Format Tables for Paper 
/// Model 1. Determinants of Donation Behavior 
/*
use `foodng1a', clear 
gen id = _n 
merge 1:1 var using `foodng2a', keep(match master) nogen
merge 1:1 var using `food1a', keep(match master) nogen
merge 1:1 var using `food2a', keep(match master) nogen
merge 1:1 var using `money1a', keep(match master) nogen
merge 1:1 var using `money2a', keep(match master) nogen
sort 



/// Model 2. COVID 19 
foodng1b foodng2b food1b food2b money1b money2b
use `foodng1b', clear 
gen id = _n 
merge 1:1 var using `foodng2b', keep(match master) nogen
merge 1:1 var using `food1b', keep(match master) nogen
merge 1:1 var using `food2b', keep(match master) nogen
merge 1:1 var using `money1b', keep(match master) nogen
merge 1:1 var using `money2b', keep(match master) nogen
sort 

exit 
*/
