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
use "${bt}/GleanersFinancialDonations.dta", clear 
/// Filtering Assumption 
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
global line_opts lwidth(thin) msize(vsmall)

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
save "${bt}/quartiles_fipcrosswalk.dta", replace 
restore

merge m:1 fips using `quartiles', keep(match master) nogen
drop if PalletTotalWeightq == . 
/// Define Quartile 
label define PalletTotalWeightq 1 "1st Quartile" 2 "2nd Quartile" 3 "3rd Quartile" 4 "4th Quartile"
label values PalletTotalWeightq PalletTotalWeightq
rename PalletTotalWeightq Quartile
rename donation_sumq Quartile_Financial
save "${bt}/data_descriptive.dta", replace 

********************************************************************************
********************************************************************************

/// Balance Tables doing the mean comparison across quartiles and in general 
/// Load Regression data 

global outcomes food foodng money
global bal_opts vce(robust) pboth fnoobs format(%12.3fc) rowvarlabels replace grplabels("0 2013-2019 @ 1 2020-2021") normdiff 

*preserve 
*use "${bt}/gleaners_food_regression.dta", clear 
*merge m:1 fips using `quartiles', keep(match master) nogen
*/// Drop Outcomes that do not have quartiles  
*drop if PalletTotalWeightq == . 
*/// First Balance Table Full 
*iebaltab $outcomes, grpvar(post) savetex("${pg}/MeanCompOutcomesFull.tex") ${bal_opts}  
*
*forvalues i=1(1)4{
*	iebaltab $outcomes if PalletTotalWeightq == `i', grpvar(post) savetex("${pg}/MeanCompOutcomesQuartile`i'.tex") ${bal_opts}  
*}
*restore 


********************************************************************************
/// Collapse the Dataset 

use  "${bt}/data_descriptive.dta", clear 
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

local name1 = "log_sum"
local name2 = "log_mean"
local title1 = "Log Total Donation"
local title2 = "Log Average Donation"
local varlist log_sum_in_kind log_avg_in_kind
local i = 1
foreach var of local varlist {
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

/// Option 1 - Outcome 
twoway (line `var' $time if $time_cond & Quartile == 1 , lpattern(solid) msymbol(circle) lcolor(black) mcolor(black) $line_opts) /// 
		(line `var' $time if $time_cond & Quartile == 2 , lpattern(solid) msymbol(square) lcolor(cranberry) mcolor(cranberry) $line_opts) ///
		(line `var' $time if $time_cond & Quartile == 3 , lpattern(solid) msymbol(diamond) lcolor(blue) mcolor(black) $line_opts) ///
		(line `var' $time if $time_cond & Quartile == 4 , lpattern(solid) msymbol(triangle) lcolor(green) mcolor(cranberry) $line_opts) , ///
		$two_opts ytitle(`title`i'') xline($xline, lcolor(black) lpattern(dash) lwidth(thin)) name(`var'_`i'_a, replace) title("In-Kind Donations by Quartile") legend(on order(1 "1st Quartile" 2 "2nd Quartile" 3 "3rd Quartile" 4 "4th Quartile") rows(1) cols(4) size(small))
graph export "${pg}/food_qofd_quart1_`var'.png", $export  

/// Option 2 - Outcome 
twoway 	(line `var' $time if $time_cond , lpattern(solid) msymbol(triangle) lcolor(black) mcolor(cranberry) $line_opts) , ///
		$two_opts ytitle(`title`i'') name(`var'_`i'_b, replace) title("In-Kind Donations") legend(off order(1 "1st Quartile" 2 "2nd Quartile" 3 "3rd Quartile" 4 "4th Quartile") rows(1) cols(4) size(small)) by(Quartile) yline(1, lcolor(black) lpattern(dash)) subtitle(,fcolor(none))
graph export "${pg}/food_qofd_quart2_`var'.png", $export  

/// Option 3 - Index 
twoway (line food_index $time if $time_cond & Quartile == 1 , lpattern(solid) msymbol(circle) lcolor(black) mcolor(black) $line_opts) /// 
		(line food_index $time if $time_cond & Quartile == 2 , lpattern(solid) msymbol(square) lcolor(cranberry) mcolor(cranberry) $line_opts) ///
		(line food_index $time if $time_cond & Quartile == 3 , lpattern(solid) msymbol(diamond) lcolor(blue) mcolor(black) $line_opts) ///
		(line food_index $time if $time_cond & Quartile == 4 , lpattern(solid) msymbol(triangle) lcolor(green) mcolor(cranberry) $line_opts) , ///
		$two_opts ytitle("Index = 1 (2013q1)") xline($xline, lcolor(black) lpattern(dash) lwidth(thin)) name(index_`var'_`i'_a, replace) title("In-Kind Donations by Quartile") legend(on order(1 "1st Quartile" 2 "2nd Quartile" 3 "3rd Quartile" 4 "4th Quartile") rows(1) cols(4) size(small)) yline(1, lcolor(black) lpattern(dash) lwidth(thin))
graph export "${pg}/food_qofd_quart3_`var'.png", $export   		

/// Option 4 - Index 
twoway 	(line food_index $time if $time_cond , lpattern(solid) msymbol(triangle) lcolor(black) mcolor(cranberry) $line_opts) , ///
		$two_opts ytitle("Index = 1 (2013q1)") xline($xline, lcolor(black) lpattern(dash) lwidth(thin)) name(index_`var'_`i'_b, replace) title("In-Kind Donations") legend(off order(1 "1st Quartile" 2 "2nd Quartile" 3 "3rd Quartile" 4 "4th Quartile") rows(1) cols(4) size(small)) by(Quartile) yline(1, lcolor(black) lpattern(dash)) subtitle(,fcolor(none))
graph export "${pg}/food_qofd_quart4_`var'.png", $export   
local i = `i' + 1
restore 
}


******************** ***************************************************888
/// Graph 2 - Money Donations 
use  "${bt}/data_descriptive.dta", clear 
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
		$two_opts ytitle("Log Outcome") xline(`xline', lcolor(black) lpattern(dash) lwidth(thin)) legend(on order(1 "Financial Donations" 2 "Mean 2013-2019") size(small) rows(1)) name(money_graph1, replace) title("Charitable Giving in the Midwest: Financial Donations") 
		
graph export "${pg}/money_qofd_quart.png", $export 



*********************************************************************************
use  "${bt}/data_descriptive.dta", clear 
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

********************************************************************************
/// Graph 1. In- Kind Donations 

twoway (connected food $time if $time_cond , lpattern(solid) msymbol(circle) lcolor(black) mcolor(black) $line_opts) ///
		(connected foodng $time if $time_cond, lpattern(dash) msymbol(square) lcolor(blue) mcolor(blue) $line_opts) ///
		(line food_pre $time if $time_cond & qofd <= tq(2020q1), lcolor(maroon) lpattern(dash) lwidth(thin)), ///
		$two_opts ytitle("Log Outcome") xline(`xline', lcolor(black) lpattern(dash) lwidth(thin)) name(food_graph1, replace) title("Charitable Giving in the Midwest: In-Kind Donations")
		
graph export "${pg}/food_qofd.png", $export 

******************** ***************************************************888
/// Graph 2 - Money Donations 
cap gen money_pre = `money_pre' if $time <= tq(2019q4)
label variable money_pre "(Mean) 2013-2019"

twoway (connected money $time if $time_cond , lpattern(solid) msymbol(circle) lcolor(black) mcolor(black) $line_opts) ///
		(line money_pre $time if $time_cond & qofd <= tq(2020q1), lcolor(red) lpattern(dash) lwidth(thin)), ///
		$two_opts ytitle("Log Outcome") xline(`xline', lcolor(black) lpattern(dash) lwidth(thin)) legend(on order(1 "Financial Donations" 2 "Mean 2013-2019") size(small) rows(1)) name(money_graph1, replace) title("Charitable Giving in the Midwest: Financial Donations")
		
graph export "${pg}/money_qofd.png", $export 

graph combine food_graph1 money_graph1, xcommon rows(2) name(graph_combined, replace) 
graph export "${pg}/graph_combined_qofd.png", $export 
********************************************************************************
/// Graph 3. Tefap and NG index 

qui sum qofd if qofd == tq(2013q1)
local min = r(mean)
qui sum qofd if qofd == tq(2021q1)
local max = r(mean)
 
twoway (connected tefap_index $time if $time_cond , lpattern(solid) msymbol(circle) lcolor(black) mcolor(black) $line_opts) ///
		(connected ng_index $time if $time_cond, lcolor(red) lpattern(dash)msymbol(square) mcolor(red) $line_opts), ///
		$two_opts ytitle("Index = 1 (2013q1)") xline(`xline', lcolor(black) lpattern(dash) lwidth(thin)) yline(1, lcolor(black) lpattern(dash) lwidth(thin)) legend(on order(1 "TEFAP" 2 "Non-Government Donations") size(small) rows(1)) name(index_graph, replace) title("Charitable Giving in the Midwest: In-Kind Donations")
		
graph export "${pg}/index_qofd.png", $export 


preserve 
gcollapse (sum) donation_value, by(Donor year)
drop if Donor == . 
reshape wide donation_value, i(year) j(Donor)
/// Relabel and rename 
label variable donation_value1 "Co-Packer "
label variable donation_value2 "Distributor"
label variable donation_value3 "Drug Store"
label variable donation_value4 "Feeding America Affiliate"
label variable donation_value5 "Food Service"
label variable donation_value6 "Grower"
label variable donation_value7 "Hiome Center"
label variable donation_value8 "Manufacturer"
label variable donation_value11 "Other Class of Trade"
label variable donation_value12 "Processor"
label variable donation_value13 "Reclamation"
label variable donation_value14 "Restaurant"
label variable donation_value15 "Retailer"
label variable donation_value16 "Super Market"
label variable donation_value18 "Unknown"
label variable donation_value19 "Warehouse Club"
label variable donation_value20 "Wholesaler"

rename donation_value1 copacker
rename donation_value2 distributor
rename donation_value3 drugstore
rename donation_value4 faa
rename donation_value5 foodservice
rename donation_value6 grower
rename donation_value7 homecenter
rename donation_value8 manufacturer
rename donation_value11 othertrade
rename donation_value12 processor
rename donation_value13 reclamation
rename donation_value14 restaurant
rename donation_value15 retailer
rename donation_value16 supermarket
rename donation_value18 unknown
rename donation_value19 warehouseclub
rename donation_value20 wholesaler

local varlist copacker distributor drugstore faa foodservice grower homecenter manufacturer othertrade processor reclamation restaurant retailer supermarket unknown warehouseclub wholesaler
foreach x of local varlist {
	replace `x' = 0 if `x' == . 
}
gen others = copacker + drugstore + foodservice + homecenter + processor + restaurant + supermarket + unknown + wholesaler + reclamation
label variable others "Others"
drop copacker drugstore foodservice homecenter processor restaurant supermarket unknown wholesaler reclamation
local donors retailer manufacturer distributor faa othertrade warehouseclub grower others 
graph bar (mean) `donors', over(year, label(labsize(small))) $bars legend(on order(1 "Retailer" 2 "Manufacturer" 3 "Distributor" 4 "Feeding America Affiliate" 5 "Other Class of Trade" 6 "WH Club" 7 "Grower" 8 "Others") rows(2) $legend) name(donors, replace) title("Donations by Donor Type", size(small) pos(11)) ylabel(#12, labsize(small) angle(0) nogrid) ytitle("Percentage of Total Food Donated", size(small)) 
graph export "${ao}/FoodbyDonor.png", ${export}
restore 

**********************************
/// Graph 5. Food by Storage 
preserve 
use  "${bt}/data_descriptive.dta", clear 

gcollapse (sum) donation_value, by(Storage year)
drop if Storage == . 
reshape wide donation_value, i(year) j(Storage)

rename donation_value1 chemical
rename donation_value2 dry
rename donation_value3 drytote
rename donation_value4 frozen
rename donation_value5 frozenote
rename donation_value6 ref

local varlist chemical dry drytote frozen frozenote ref 
foreach var of local varlist {
	replace `var' = 0 if `var' == . 
}

gen dry_total = dry + drytote
gen frozen_total = frozen + frozenote 
gen ref_total = ref + chemical 

/// Check Code 
graph bar (mean) ref_total frozen_total dry_total, over(year, label(labsize(small))) $bars legend(on order(1 "Refrigerated" 2 "Frozen" 3 "Dry") rows(1) $legend) name(storage, replace) title("Donations by Storage", size(small) pos(11)) ytitle("Percentage of Total Food Donated", size(small)) 
graph export "${ao}/FoodbyStorage.png", ${export}
restore 
exit 
