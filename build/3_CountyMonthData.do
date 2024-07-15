*******************************************************************************
**** In-kind and Money Donations 
**** Authors: Denvil Duncan, Shelley Suttles and Luis Navarro
**** Code by: Luis Navarro
**** Last Update: August 2022
**** Script: Create County Level Dataset 
********************************************************************************
///Full Data Food 
use "${bt}/GleanersFoodClean2022.dta", clear
  
/// Collapse county by month 
gen mofd = mofd(date)
format mofd %tm
// Collapse to create dataset by county and month 
gcollapse (sum) PalletTotalWeight (mean) date, by(county_fips fips mofd)
drop if fips == . 
gen month = month(date)
gen year = year(date)
save "${bt}/GleanersFoodCountyMonth.dta", replace 


*******************************************************************************
/// Non Gov Dataset Food 
use "${bt}/GleanersFoodClean2022NonGov.dta", clear  
/// Collapse county by month 
gen mofd = mofd(date)
format mofd %tm
// Collapse to create dataset by county and month 
gcollapse (sum) PalletTotalWeight, by(county_fips fips mofd)
drop if fips == . 
// rename outcome 
rename PalletTotalWeight PalletTotalWeight_NonGov
save "${bt}/GleanersFoodCountyMonthNonGov.dta", replace 


********************************************************************************
********************************************************************************
/// Load Data for Money Donations
use "${bt}/GleanersFinancialDonations.dta", clear 
/// Filtering Assumption 
drop if donation_value > 1000000
clonevar donation_sum = donation_value
clonevar donation_mean = donation_value 
label variable donation_sum "Sum Donations"
label variable donation_mean "Mean Donations"

gen mofd = mofd(date)
// Collapse to create dataset by county and month 
gcollapse (sum) donation_sum (mean) donation_mean date, by(state_fips county_fips fips mofd)
gen month = month(date)
gen year = year(date)
save "${bt}/GleanersMoneyCountyMonth.dta", replace 

exit 
