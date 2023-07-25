*******************************************************************************
**** In-kind and Money Donations 
**** Authors: Denvil Duncan, Shelley Suttles and Luis Navarro
**** Code by: Luis Navarro
**** Last Update: August 2022
**** Script: Graphs
********************************************************************************

/*Note: unlike the dataset used for the linear regression, this one is not collapsed at the county by month. Instead, we collapse it at the donation level. The reason for this to not add more complexity to the statistic being displayed.  
*/

graph drop _all 
local varlist qofd 
*foreach x of  local varlist {
global time qofd 
global dimensions $time fips 

///Full Data Food 
use "${bt}/GleanersFoodClean2022.dta", clear  
gen mofd = mofd(date)
gen year = year(date)
gen qofd = qofd(date)
drop if fips == . 
///  Take the sum of all donations  
gcollapse (sum) PalletTotalWeight (mean) date, by($dimensions)
tempfile food 
save `food', replace 

*******************************************************************************
/// Non Gov Dataset Food 
use "${bt}/GleanersFoodClean2022NonGov.dta", clear  
gen mofd = mofd(date)
gen year = year(date)
gen qofd = qofd(date)
drop if fips == . 
///  Take the sum of all donations  
gcollapse (sum) PalletTotalWeight, by($dimensions)
rename PalletTotalWeight PalletTotalWeight_NonGov
tempfile foodng 
save `foodng', replace

********************************************************************************
********************************************************************************
/// Load Data for Money Donations
use "${bt}\GleanersFinancialDonations.dta", clear 
drop if donation_value > 1000000
drop if fips == . 
clonevar donation_sum = donation_value
label variable donation_sum "Sum Donations"
gen mofd = mofd(date)
gen year = year(date)
gen qofd = qofd(date)
///  Take the sum of all donations financial 
gcollapse (sum) donation_sum, by($dimensions)
tempfile money 
save `money', replace


/// Master Data Set 
********************************************************************************
use `food', clear 
merge 1:1 $dimensions using `foodng', keep(match master) nogen
merge 1:1 $dimensions using `money', keep(match master) nogen
gen tefap = PalletTotalWeight - PalletTotalWeight_NonGov
********************************************************************************
/// Setting for Graphs and Tables 
qui sum qofd if qofd == tq(2013q1)
local min = r(mean)
qui sum qofd if qofd == tq(2022q1)
local max = r(mean)
global time_cond inrange(qofd,`min',`max') 
global two_opts xlabel(#37, labsize(vsmall) angle(90) nogrid) ylabel(#15, labsize(vsmall) angle(0) nogrid) xtitle("", size(small)) title(, size(small) pos(11)) legend(on size(vsmall) rows(1)) ytitle(,size(small))


***** observations to drop 
/// Generate Quartiles by Outcome 
cap gen year = year(date)
drop if year < 2013

/// Drop the ones that will not be used. Only lost 5% of the sample 
tab fips, matrow(F) sort 
local fips = r(r)
forvalues i = 1(1)50 {
	local id = F[`i',1]
	dis `id'
	qui tab fips if fips == `id'
	local count = r(N)
	drop if fips == `id' & `count' <= 5
	
}
tab fips, matrow(F) sort 
********************************************************************************

/// Generate Variables 
gen food = ln(PalletTotalWeight)
gen foodng = ln(PalletTotalWeight_NonGov)
gen money = ln(donation_sum)
label variable PalletTotalWeight "In-Kind Donations "
label variable PalletTotalWeight_NonGov "NG In-Kind Donations"
label variable donation_sum "Financial Donations"
label variable food "In-Kind Donations"
label variable foodng "NG In-Kind Donations"
label variable money "Financial Donations"
****************************************************
/// Quartiles for Cohort 2013
preserve 
gcollapse (mean) PalletTotalWeight PalletTotalWeight_NonGov donation_sum if year == 2013, by(fips)
local varlist PalletTotalWeight PalletTotalWeight_NonGov donation_sum
foreach var of local varlist {
	gen `var'q = . 
	cumul `var', gen(`var'_cdf)
	replace `var'q = 1 if `var'_cdf <= 0.25
	replace `var'q = 2 if `var'_cdf > 0.25 & `var'_cdf <= 0.50
	replace `var'q = 3 if `var'_cdf > 0.50 & `var'_cdf <= 0.75
	replace `var'q = 4 if `var'_cdf > 0.75 
	drop `var'_cdf `var'
}

tempfile quartiles
save `quartiles', replace 
save "${bt}\quartiles_fipcrosswalk.dta", replace 
*******
/// Quartile Maps
gen state_fips = 18 
rename fips county 
global options_map title("Indiana Counties - 2013 In-Kind Donations", size(medsmall) pos(12)) legend(lab(2 "1st Quartile") lab(3 "2nd Quartile") lab(4 "3rd Quartile") lab(5 "4th Quartile")) legend(size(small) pos(5)) name(counties0, replace) 
set scheme s1color
/// sample map
*maptile PalletTotalWeightq if state_fips == 18, geo(county2014) mapif(state_fips==18) twopt(${options_map}) fcolor(Heat)
*graph export "${pg}\Indiana_Quartiles_2013_InKind.pdf", replace
restore
*********************************************************************************
/// Merge with Quartiles 
merge m:1 fips using `quartiles', keep(match master) nogen
drop if PalletTotalWeightq == . 
/// Define Quartile 
label define PalletTotalWeightq 1 "1st Quartile" 2 "2nd Quartile" 3 "3rd Quartile" 4 "4th Quartile"
label values PalletTotalWeightq PalletTotalWeightq
rename PalletTotalWeightq Quartile
rename donation_sumq Quartile_Financial
save "${bt}\data_descriptive.dta", replace 

********************************************************************************
********************************************************************************

/// Quartile Attrition Analysis 
use "${bt}\data_descriptive.dta", clear
label define quartiles 1 "1st Quartile" 2 "2nd Quartile" 3 "3rd Quartile" 4 "4th Quartile"
local t = 2013
forvalues t=2013(1)2022{
preserve 
qui gcollapse (mean) PalletTotalWeight PalletTotalWeight_NonGov donation_sum if year == `t', by(fips)
local varlist PalletTotalWeight PalletTotalWeight_NonGov donation_sum
foreach var of local varlist {
	qui gen `var'`t' = . 
	qui cumul `var', gen(`var'_cdf)
	qui replace `var'`t' = 1 if `var'_cdf <= 0.25
	qui replace `var'`t' = 2 if `var'_cdf > 0.25 & `var'_cdf <= 0.50
	qui replace `var'`t' = 3 if `var'_cdf > 0.50 & `var'_cdf <= 0.75
	qui replace `var'`t' = 4 if `var'_cdf > 0.75 
	qui drop `var'_cdf `var'
}

qui gen yr = `t'
qui tempfile quart`t'
qui save `quart`t'', replace 
restore
}

qui use "${bt}\gleaners_food_regression.dta", clear 
forvalues t=2013(1)2022{
qui merge m:1 fips using `quart`t'', keep(match master) nogen
}

*tabstat PalletTotalWeight2013 PalletTotalWeight2014 PalletTotalWeight2015 PalletTotalWeight2016 PalletTotalWeight2017 PalletTotalWeight2018 PalletTotalWeight2019 PalletTotalWeight2020 PalletTotalWeight2021 PalletTotalWeight2022 , by(fips) stat(mean)
gen non_miss2013p = PalletTotalWeight2013
gen non_miss2013c = 1/_N if PalletTotalWeight2013 != . 


rename PalletTotalWeight* y*
gcollapse (percent) non_miss2013p (sum) non_miss2013c (mean) y*, by(fips)
drop y_NonGov*

tabstat y2013 y2014 y2015 y2016 y2017 y2018 y2019 y2020 y2021 y2022, stat(count)

local varlist y2013 y2014 y2015 y2016 y2017 y2018 y2019 y2020 y2021 y2022
local i = 2013
foreach var of local varlist {
	label variable `var' "`i'"
	local i = `i' + 1
}
replace non_miss2013c = non_miss2013c*100
gen miss2013c = 100 - non_miss2013c
countyfips, fips(fips) nogen

tabstat y2013 y2014 y2015 y2016 y2017 y2018 y2019 y2020, by(county_name) stat(count)

drop if y2013 == . 

graph dot (asis) y2013, over(county_name, sort(y2013) label(labsize(small))) legend(off rows(2) cols(5) size(small)) name(quartile13,replace) title("Quartiles 2013", pos(11) size(small))
graph export "${pg}\Quartiles2013dot.pdf", replace

graph dot (asis) y2013 y2014 y2015 y2016 y2017 y2018 y2019 y2020 y2021 y2022, over(county_name, sort(y2013) label(labsize(small))) legend(on rows(2) cols(5) size(small)) name(quartile_all,replace) title("Quartiles by Year", pos(11) size(small))
graph export "${pg}\Quartiles2013changesdot.pdf", replace

sort non_miss2013c county_name 
gen cum_sum_nm = 0 
local sumfac = non_miss2013c[1] 
dis `sumfac'
replace cum_sum_nm =  `sumfac' if _n== 1
local obs = _N
forvalues i=2(1)`obs' {
	qui sum non_miss2013c if _n == `i'
	local sumfac = `sumfac' + r(mean)
	dis `sumfac'
	replace cum_sum_nm = `sumfac' if _n == `i'
}


graph dot (asis) miss2013c, over(county_name, sort(non_miss2013c) label(labsize(small))) legend(on rows(2) cols(5) size(small)) name(quartile_sample_size,replace) title("NonMissing Obs in Regression Sample", pos(11) size(small))


graph dot (asis) cum_sum_nm, over(county_name, sort(non_miss2013c) label(labsize(small))) legend(on rows(2) cols(5) size(small)) name(quartile_sample_size_cumul,replace) title("Cumulative Weight in Regression Sample", pos(11) size(small))

graph combine quartile_sample_size quartile_sample_size_cumul, cols(2) name(combined_sample_rep, replace)
*graph export "${pg}\Quartiles2013SampleRep.pdf", replace

gen quartile = y2013 
keep fips quartile y2013 y2014 y2015 y2016 y2017 y2018 y2019 y2020 y2021 y2022 non_miss2013c county_name

reshape long y, i(county_name) j(yr)
gcollapse (sd) y (mean) non_miss2013c, by(county_name quartile)

********************************************************************************
********************************************************************************
/// Table of Counties by Quartile 
use "${bt}\quartiles_fipcrosswalk.dta", clear
countyfips, fips(fips) nogen
sort PalletTotalWeightq fips
list PalletTotalWeightq fips county_name




*********************************************************************************
/// Balance Tables doing the mean comparison across quartiles and in general 
/// Load Regression data 

global outcomes food foodng money
global bal_opts vce(robust) pboth fnoobs format(%12.3fc) rowvarlabels replace grplabels("0 2013-2019 @ 1 2020-2021") normdiff 

preserve 
use "${bt}\gleaners_food_regression.dta", clear 
merge m:1 fips using `quartiles', keep(match master) nogen
/// Drop Outcomes that do not have quartiles  
drop if PalletTotalWeightq == . 
/// First Balance Table Full 
iebaltab $outcomes, grpvar(post) savetex("${pg}\MeanCompOutcomesFull.tex") ${bal_opts}  

forvalues i=1(1)4{
	iebaltab $outcomes if PalletTotalWeightq == `i', grpvar(post) savetex("${pg}\MeanCompOutcomesQuartile`i'.tex") ${bal_opts}  
}
restore 


********************************************************************************
/// Collapse the Dataset 

use  "${bt}\data_descriptive.dta", clear 
/// Collapse the dataset such that it takes the sum of total donations, as well as the average. Collapsing by Quarter and Quartile. 
gen avg_donation = PalletTotalWeight
gcollapse (sum) PalletTotalWeight PalletTotalWeight_NonGov (mean) avg_donation date (nunique) fips, by($time Quartile)
gen log_sum_in_kind = ln(PalletTotalWeight)
gen log_avg_in_kind = ln(avg_donation)
label variable log_sum_in_kind "In-Kind Donations"
label variable log_avg_in_kind "In-Kind Donations"
capture format qofd %tq
cap gen year = year(date)
********************************************************************************
/// Graph 1. In- Kind Donations 


local title1 = "Total In-Kind Donations (Log Pounds, Index = 1 (2013q1))"
local j = 1

foreach var of varlist log_sum_in_kind {
/// Loop for both outcomes 
preserve 
gen food_index = . 
forvalues i = 1(1)4{
sum `var' if qofd == tq(2013q1) & Quartile == `i'
local initial = r(mean)
replace food_index = `var'/`initial' if Quartile == `i'
}

sum qofd if qofd == tq(2019q4)
global xline = r(mean)
global lineall lwidth(thin) msize(vsmall)
global line1 lpattern(solid) msymbol(circle) lcolor(black) mcolor(black) $lineall
global line2 lpattern(shortdash) msymbol(diamond) lcolor(cranberry) mcolor(cranberry) $lineall
global line3 lpattern(longdash) msymbol(plus) lcolor(navy) mcolor(navy) $lineall
global line4 lpattern(shortdash) msymbol(X) lcolor(dkgreen) mcolor(dkgreen) $lineall
global line5 lpattern(solid) msymbol(plus) lcolor(blue) mcolor(blue) $lineall

/*
/// Option 1 - Outcome 
twoway  (connected `var' $time if $time_cond & Quartile == 1 ,  $line1) /// 
		(connected `var' $time if $time_cond & Quartile == 2 , $line2) ///
		(connected `var' $time if $time_cond & Quartile == 3 , $line3) ///
		(connected `var' $time if $time_cond & Quartile == 4 , $line4), ///
		$two_opts ytitle("") xline($xline, lcolor(black) lpattern(dash) lwidth(thin)) name(`var'_`j'_a, replace) title("`title`j'': Analysis by Quartile") legend(on order(1 "1st Quartile" 2 "2nd Quartile" 3 "3rd Quartile" 4 "4th Quartile") rows(1) cols(4) size(small))
*graph export "${pg}\food_qofd_quart1_`var'.pdf", replace  

/// Option 2 - Outcome 
twoway 	(line `var' $time if $time_cond , $line1) , ///
		$two_opts name(`var'_`j'_b, replace) title("`title`j'': Analysis by Quartile") legend(off order(1 "1st Quartile" 2 "2nd Quartile" 3 "3rd Quartile" 4 "4th Quartile") rows(1) cols(4) size(small)) by(Quartile) yline(1, lcolor(black) lpattern(dash)) subtitle(,fcolor(none))
*graph export "${pg}\food_qofd_quart2_`var'.pdf", replace  
*/

/// Option 3 - Index 
twoway  (connected food_index $time if $time_cond & Quartile == 1 ,  $line1) ///
		(connected food_index $time if $time_cond & Quartile == 2 , $line2) ///
		(connected food_index $time if $time_cond & Quartile == 3 , $line3) ///
		(connected food_index $time if $time_cond & Quartile == 4 , $line4), ///
		$two_opts ytitle("") xline($xline, lcolor(black) lpattern(dash) lwidth(thin)) name(index_`var'_`j'_a, replace) title(Panel D: `title`j'': Analysis by Quartile) legend(on order(1 "1st Quartile" 2 "2nd Quartile" 3 "3rd Quartile" 4 "4th Quartile") rows(1) cols(4) size(vsmall)) yline(1, lcolor(black) lpattern(dash) lwidth(thin))
*graph export "${pg}\food_qofd_quart3_`var'.pdf", replace   		

/*
/// Option 4 - Index 
twoway 	(line food_index $time if $time_cond , $line1) , ///
		$two_opts ytitle("Index = 1 (2013q1)") xline($xline, lcolor(black) lpattern(dash) lwidth(thin)) name(index_`var'_`j'_b, replace) title("`title`j'': Analysis by Quartile") legend(off order(1 "1st Quartile" 2 "2nd Quartile" 3 "3rd Quartile" 4 "4th Quartile") rows(1) cols(4) size(small)) by(Quartile) yline(1, lcolor(black) lpattern(dash)) subtitle(,fcolor(none))
*graph export "${pg}\food_qofd_quart4_`var'.pdf", replace 
*/  
local j = `j' + 1
restore 
}


******************** ***************************************************888
/// Graph 2 - Money Donations 
use  "${bt}\data_descriptive.dta", clear 
gcollapse (sum) donation_sum (mean) date, by($time)
gen money = ln(donation_sum)
label variable money "Financial Donations"
capture format qofd %tq
cap gen year = year(date)

qui sum qofd if qofd == tq(2019q4)
local xline = r(mean)

local varlist money
foreach x of local varlist {
sum `x' if inrange(year,2013,2021) & qofd <= tq(2019q4)
local `x'_pre = r(mean)
sum `x' if inrange(year,2013,2021) & qofd > tq(2019q4)
local `x'_post = r(mean)
}

cap gen money_pre = `money_pre' if $time <= tq(2019q4)
label variable money_pre "(Mean) 2013-2019"

twoway (connected money $time if $time_cond , lpattern(solid) msymbol(circle) lcolor(black) mcolor(black) $line_opts) ///
		(line money_pre $time if $time_cond & qofd <= tq(2020q1), lcolor(red) lpattern(dash) lwidth(thin)), ///
		$two_opts ytitle("") xline(`xline', lcolor(black) lpattern(dash) lwidth(thin)) legend(on order(1 "Financial Donations" 2 "Mean 2013-2019") size(small) rows(1)) name(money_graph1, replace) title("Total Financial Donations (Log US $)") 
		
*graph export "${pg}\money_qofd_quart.pdf", replace


/// Graphs For Paper 
*********************************************************************************
use  "${bt}\data_descriptive.dta", clear 
gcollapse (sum) PalletTotalWeight PalletTotalWeight_NonGov donation_sum tefap (mean) date, by($time)

tab qofd if $time_cond
local xlab = r(r)
 
cap gen mofd = mofd(date)
cap gen year = year(date)
cap gen qofd = qofd(date)

capture format mofd %tmMon_CCYY
capture format qofd %tq

sort $time
/// gen index variables 
gen ng_index = . 
sum PalletTotalWeight_NonGov if qofd == tq(2013q1)
local initial = r(mean)
replace ng_index = PalletTotalWeight_NonGov/`initial'

gen tefap_index = . 
sum tefap if qofd == tq(2013q1)
local initial = r(mean)
replace tefap_index = tefap/`initial'


gen food = ln(PalletTotalWeight)
gen foodng = ln(PalletTotalWeight_NonGov)
gen money = ln(donation_sum)

label variable PalletTotalWeight "In-Kind Donations "
label variable PalletTotalWeight_NonGov "NG In-Kind Donations"
label variable donation_sum "Financial Donations"

label variable food "In-Kind Donations"
label variable foodng "NG In-Kind Donations"
label variable money "Financial Donations"

qui tab $time if inrange(year,2013,2021)
local periods = r(r)

qui sum qofd if qofd == tq(2019q4)
local xline = r(mean)

local varlist food foodng money
foreach x of local varlist {
sum `x' if inrange(year,2013,2021) & qofd <= tq(2019q4)
local `x'_pre = r(mean)
sum `x' if inrange(year,2013,2021) & qofd > tq(2019q4)
local `x'_post = r(mean)
}

cap gen food_pre = `food_pre' if $time <= tq(2019q4)
label variable food_pre "NG-In-Kind (Mean) 2013-2019"

/// Varianbles used to interpret the coefficients 
tabstat PalletTotalWeight PalletTotalWeight_NonGov if $time <= tq(2019q4), stat(mean sd sum) format(%12.0fc)
gen qperiod = 0 
replace qperiod = 1 if $time <= tq(2018q4)
replace qperiod = 2 if $time >= tq(2019q1) & $time <= tq(2019q4)
replace qperiod = 3 if $time >= tq(2020q1) & $time <= tq(2020q4)
replace qperiod = 4 if $time >= tq(2021q1) & $time <= tq(2021q4)

label define qperiod 1 "2013-2018" 2 "2019" 3 "2020" 4 "2021"
label values qperiod qperiod

table qperiod, content(mean PalletTotalWeight sum PalletTotalWeight mean food) format(%12.0fc)
table year, content(mean PalletTotalWeight sum PalletTotalWeight mean food) format(%12.0fc)

table qperiod, content(mean donation_sum sum donation_sum mean money) format(%12.0fc)
table year, content(mean donation_sum sum donation_sum mean money) format(%12.0fc)

table year if year <= 2019, content(mean donation_sum sum donation_sum) format(%12.0fc) row

table year, content(mean food mean money) format(%12.4fc) row
table year if year <= 2019, content(mean food mean money) format(%12.4fc) row

matrix define M = J(2,3,.)
matrix colnames M = "2013-2019" "2020-2021" "% Change"
matrix rownames M = "Food" "Money"
local i = 1
local varlist PalletTotalWeight donation_sum
foreach var of local varlist {
sum `var' if year <= 2019 
local t0 = r(mean)
sum `var' if year <= 2020 & year >= 2021
local t1 = r(mean)
local tc = `t1'/`t0' - 1 
matrix M[`i',1] = `t0' 
matrix M[`i',2] = `t1'
matrix M[`i',3] = `tc'
local i = `i' + 1
}
matlist M 
 
********************************************************************************
/// Graph 1. In- Kind Donations 

twoway (connected food $time if $time_cond , lpattern(solid) msymbol(circle) lcolor(black) mcolor(black) $lineall) ///
		(connected foodng $time if $time_cond, lpattern(dash) msymbol(square) lcolor(blue) mcolor(blue) $lineall) ///
		(line food_pre $time if $time_cond & qofd <= tq(2020q1), lcolor(maroon) lpattern(dash) lwidth(thin)), ///
		$two_opts ytitle("") xline(`xline', lcolor(black) lpattern(dash) lwidth(thin)) name(food_graph1, replace) title("Panel A: Total In-Kind Donations (Log Pounds)")
		
*graph export "${pg}\food_qofd.pdf", replace

******************** ***************************************************888
/// Graph 2 - Money Donations 
cap gen money_pre = `money_pre' if $time <= tq(2019q4)
label variable money_pre "(Mean) 2013-2019"

twoway (connected money $time if $time_cond , $line1) ///
		(line money_pre $time if $time_cond & qofd <= tq(2020q1), $line2), ///
		$two_opts ytitle("") xline(`xline', lcolor(black) lpattern(dash) lwidth(thin)) legend(on order(1 "Financial Donations" 2 "Mean 2013-2019") size(vsmall) rows(1)) name(money_graph1, replace) title("Panel B: Total Financial Donations (Log US $)")
		
*graph export "${pg}\money_qofd.pdf", replace

*graph combine food_graph1 money_graph1, xcommon rows(2) name(graph_combined, replace) 
*graph export "${pg}\graph_combined_qofd.pdf", replace
********************************************************************************
/// Graph 3. Tefap and NG index 

qui sum qofd if qofd == tq(2013q1)
local min = r(mean)
qui sum qofd if qofd == tq(2021q1)
local max = r(mean)
 
twoway (connected tefap_index $time if $time_cond , $line1) ///
		(connected ng_index $time if $time_cond, $line2), ///
		$two_opts ytitle("") xline(`xline', lcolor(black) lpattern(dash) lwidth(thin)) yline(1, lcolor(black) lpattern(dash) lwidth(thin)) legend(on order(1 "TEFAP" 2 "Non-Government Donations") size(vsmall) rows(1)) name(index_graph, replace) title("Panel C: Total In-Kind Donations (Log Pounds, Index = 1 (2013q1))")
		
*graph export "${pg}\index_qofd.pdf", replace 

********************************************************************************
graph combine food_graph1 money_graph1 index_graph index_log_sum_in_kind_1_a, rows(2) cols(2) xsize(16) ysize(8) name(all_comb, replace) xcommon 
graph export "${pg}\all_combined.pdf", replace 
exit 