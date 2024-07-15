*********************************************************************
**** Food and Cash Donations 
**** Denvil Duncan, Shelley Suttles, Luis Navarro
*********************************************************************

***** Step 4. Clean Gleaners Financial Data

***** Gleaners Shape DataSet
***** Financial DataSet
clear all 
/// 2010-2020
import excel "${bi}/Gleaners Food Bank of Indiana_IU Data_Redacted.xlsx", sheet("Etapestry_MostlyCash") firstrow clear 
tempfile financial
save `financial', replace 
/// New Data 
import excel "${bi}/GFBI_IU_Data_updated.xlsx", sheet("eTapestry_MostlyCash") firstrow clear 
append using `financial', force 

rename StateProvince State
replace State = "IN" if State == "In"
********************************************************************************
/// Keep Only observations from Indiana 
keep if State == "IN"
********************************************************************************
/// Homogeneous State Names and Fip Codes 
statastates, abbreviation(State) nogen
merge m:1 City State using "${bt}/CityCode.dta", keep(match master) nogen
rename county_fips fips 

countyfips, fips(fips) nogen 
drop state_code county_code state_abb State
rename state_name State
*********************************************************************************
/// Keep Relevant Variables 
egen id = group(AccountNumber)
keep Date id Received State County City state_fips county_fips fips 
rename (Date Received) (date donation_value)
destring _all, replace
sort donation_value 
sum donation_value if donation_value < 0 
/// Remove Donations with Zero Value 
collapse (sum) donation_value (first) fips City County State state_fips county_fips, by(id date)
sum donation_value if donation_value < 0 
drop if donation_value < 0 
gen donation_type = "Money"
*save "${bt}/GleanersFinancialDonationsFull.dta", replace 
/// Assumptions: Drop Outliers == Any Donations Above 10 Million Dollars --> 1 observations lost 
drop if donation_value > 10000000
save "${bt}/GleanersFinancialDonations.dta", replace 
exit
