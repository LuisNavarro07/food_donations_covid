*******************************************************************************
**** In-kind and Money Donations 
**** Authors: Denvil Duncan, Shelley Suttles and Luis Navarro
**** Code by: Luis Navarro
**** Last Update: August 2022
**** Script: Clean Gleaners Food Data 
********************************************************************************

//// Load Full DataSet -- This comes from the Excel File 
//// Load Full DataSet -- This comes from the Excel File - Mew data 
*import excel "${bi}/GFBI_IU_Data_updated.xlsx", sheet("Warehouse_In-Kind") firstrow clear 
*save "${bi}/GFBI_IU_Data_updated.dta", replace 

use "${bi}/GFBI_IU_Data_updated.dta", clear 
append using "${bi}/GleanersFoodRaw.dta" 

/// County
replace State = "In" if County == "IN"

*** Merge DataSets
merge m:1 ProductType using "${bt}/ProductTypeCode.dta", keep(match master using) nogen
*merge m:1 County using "${bt}/CountyCode.dta", keep(match master using) nogen
merge m:1 City State using "${bt}/CityCode.dta", keep(match master using) nogen
merge m:1 DonorClassofTrade using "${bt}/DonorClass.dta" , keep(match master using) nogen
merge m:1 ProductCategoryCode using "${bt}/ProductCategory.dta" , keep(match master using) nogen
merge m:1 ProductSourceCode using "${bt}/ProductSource.dta", keep(match master using) nogen
merge m:1 FBCProductCategory using "${bt}/FBCProductCategory.dta", keep(match master using) nogen
merge m:1 UNCProductCategory using "${bt}/UNCProductCategory.dta", keep(match master using) nogen

/// Drop Unnecessary Variables 
drop ProductType County DonorClassofTrade ProductCategoryCode ProductSourceCode UNCProductCategory UNCProductCategoryCode UNCDonorClassofTradeCode

/// Encode Variables 
encode StorageZone, gen(Storage)
encode ItemCategoryCode, gen(ItemCategory)
drop StorageZone ItemCategoryCode
rename (county_name DonorDes) (County Donor)

/// Create Date Variables 
// Date Variables 
gen Month = month(DateReceived)
gen Year = year(DateReceived)
gen Quarter = quarter(DateReceived)
gen Qdate = yq(Year,Quarter)
gen Week = week(DateReceived)
gen MonthlyDate = mofd(DateReceived)
format Qdate %tqCCYY!qq
format MonthlyDate %tm

/// Labels 
label variable Month "Month"
label variable Year "Year"
label variable Quarter "Quarter No"
label variable Qdate "Quarter Date"
label variable Week "Week No"
label variable MonthlyDate "Monthly Date"
label variable County "County"
label variable ProductDes "Product Description"
label variable Donor "Donor"
label variable ProductCategory "Product Category"
label variable ProductSource "ProductSource"
label variable FBCProdCat "FBC Product Category"
label variable UNCProdCat "UNC Product Category"


**** Typos Management

// State
replace State = "AK" if State == "Ak"
replace State = "AK" if State == "Ak."
replace State = "AR" if State == "Ar."
replace State = "AR" if State == "Arkansas"
replace State = "AZ" if State == "Az"
replace State = "AZ" if State == "Az."
replace State = "CA" if State == "Ca"
replace State = "CA" if State == "Ca."
replace State = "CA" if State == "California"
replace State = "CO" if State == "Co"
replace State = "DE" if State == "De."
replace State = "FL" if State == "Fl"
replace State = "FL" if State == "Fl."
replace State = "FL" if State == "Florida"
replace State = "GA" if State == "Ga"
replace State = "GA" if State == "Ga."
replace State = "IA" if State == "Ia"
replace State = "IA" if State == "Iowa"
replace State = "ID" if State == "Id"
replace State = "IL" if State == "Il"
replace State = "IL" if State == "Il."
replace State = "IL" if State == "Illiinois"
replace State = "IL" if State == "Illinois"
replace State = "IN" if State == "IN "
replace State = "IN" if State == "In"
replace State = "IN" if State == "In."
replace State = "IN" if State == "In "
replace State = "IN" if State == "Indiana"
replace State = "IN" if State == "Indiana "
replace State = "KY" if State == "KY."
replace State = "KY" if State == "Kentucky"
replace State = "KS" if State == "Kansas"
replace State = "LA" if State == "La"
replace State = "MD" if State == "MD."
replace State = "MA" if State == "Massachusetts"
replace State = "MI" if State == "Mi"
replace State = "MI" if State == "Mi."
replace State = "NC" if State == "NC."
replace State = "NE" if State == "NE."
replace State = "NE" if State == "Ne."
replace State = "NJ" if State == "NJ."
replace State = "OH" if State == "Oh."
replace State = "PA" if State == "PA."
replace State = "TX" if State == "Texas"
replace State = "TX" if State == "Tx"
replace State = "TN" if State == "Tn"
replace State = "VA" if State == "Va."
replace State = "WA" if State == "Wa"
replace State = "WI" if State == "Wi"
replace State = "AL" if State == "al"
replace State = "IA" if State == "ia"
replace State = "IL" if State == "il"
replace State = "KY" if State == "ky"
replace State = "MI" if State == "mi"
replace State = "WI" if State == "wi"
replace County = "Marion" if City == "Indianapolis"

/// Homogeneous State Names and Fip Codes 
statastates, abbreviation(State)
drop _merge 
rename county_fips fips 
countyfips, fips(fips)
drop state_code county_code _merge state_abb State
rename state_name State
//// Cities typos and Names 
replace City = "Battle Creek" if City == "Battle Creek "
replace City = "Bellefontaine" if City == "Bellefountaine"
replace City = "Broomfield" if City == "Broomfield "
replace City = "Cincinnati" if City == "Cincinatti"
replace City = "Cincinnati" if City == "Cinciinnati"
replace City = "Dade City" if City == "Dade City "
replace City = "Dallas" if City == "Dallas "
replace City = "Downers Grove" if City == "Dowers Grove "
replace City = "Edinburgh" if City == "Edinburg" & state_fips == 18
replace City = "Fontana" if City == "Fontana."
replace City = "Jefferson City" if City == "Jefferson City "
replace City = "Liberal" if City == "Liberal "
replace City = "Little Chute" if City == "Little Chute "
replace City = "McCook" if City == "McCook "
replace City = "Minneapolis" if City == "Minnneapolis"
replace City = "Monte Vista" if City == "Monte Vista "
replace City = "North Hollywood" if City == "North Hollywood."
replace City = "Santa Barbara" if City == "Santa Barabara"
replace City = "Seattle" if City == "Seattle "
replace City = "Shepherdsville" if City == "Shepperdsville"
replace City = "Springfield" if City == "Springfield "
replace City = "St. Louis" if City == "St.louis"
replace City = "Zionsville" if City == "Zionsville "
replace City = "Indianapolis" if City == "indianapolis"
replace City = "Depere" if City == "depere"
replace City = "LeMars" if City == "le mars"
replace City = "Marshfield" if City == "marshfield"
replace City = "Wooldridge" if City == "woodridge"


********************************************************************************
/// Keep Only observations from Indiana 
keep if state_fips == 18
********************************************************************************
/// rename identifiers
rename (DonorRandomID DateReceived) (id date)
//// Export all data 
/// Assumptions: Drop Outliers == Any Donations Above 1 Million Pounds --> 3 observations lost 
drop if PalletTotalWeight > 1000000
/// Sort the DataSet
order id Donor date County City ProductDonated ProductDes QuantityPerPallet WeightPerCase PalletTotalWeight ValuePerUnitofMeasure TotalPalletValue ProductCategory ProductSource ItemCategory PalletNo UnitofMeasure Storage Month Year Quarter Qdate Week
sort State County City id date
********************************************************************************
********************************************************************************
save "${bt}/GleanersCleanDonations.dta", replace 

/// data for bar graphs describing composition of donations
preserve 
gcollapse (sum) PalletTotalWeight, by(id fips date UNCProdCat ProductSource Donor Storage)
save "${bt}/GleanersFoodFull.dta", replace 
restore


********************************************************************************

save "${bt}/GleanersCleanDonations.dta", replace 

/// Regular Dataset - Full data (TEFAP included)
preserve 
/// Keep Relevant Variables and Remove Donations with Zero Value 
collapse (sum) PalletTotalWeight (first) fips City County State county_fips, by(id date)
drop if PalletTotalWeight < 0 
gen donation_type = "Food"
/// Save Dataset 
save "${bt}/GleanersFoodClean2022.dta", replace 
restore 


/// NonGov Dataset - Excluding TEFAP donations 
preserve 
drop if FBCProductCategory == "TEFAP"
/// Keep Relevant Variables and Remove Donations with Zero Value 
collapse (sum) PalletTotalWeight (first) fips City County State county_fips, by(id date)
drop if PalletTotalWeight < 0 
gen donation_type = "Food"
/// Save Dataset 
save "${bt}/GleanersFoodClean2022NonGov.dta", replace 
restore 

exit 
/*
********************************************************************************
********************************************************************************
**** Script: Gleaners Food Data: Negative Donations Correction and Salvage Value 
********************************************************************************
/// Gleaner Food Data 
/// Keep Relevant Variables 
keep DateReceived TotalPalletValue Donor ProductSource PalletTotalWeight ValuePerUnitofMeasure DonorRandomID State County City state_fips county_fips UnitofMeasure
rename (DonorRandomID DateReceived ProductSource) (id date donor_type)
gen donor = 1 
/// Here Individual donations are the ones categorized as donated local 
replace donor = 0 if Donor == 2 | Donor == 4 | Donor == 8 | Donor == 11 | Donor == 19 
label define donor 0 "Firm" 1 "Individual"
label values donor donor 

/// Remove Donations with Zero Value
collapse (sum) PalletTotalWeight (mean) donor (first) City County State state_fips county_fips, by(id date)
sum PalletTotalWeight if PalletTotalWeight < 0 
drop if PalletTotalWeight < 0 
gen donation_type = "Food"
gen bank = "Gleaners"
gen year = year(date)
/// Salvage Value to Convert Pallet Weight into Dollars 
gen salvage = 0 
replace salvage = 1.66 if year <= 2012
replace salvage = 1.69 if year == 2013
replace salvage = 1.72 if year == 2014
replace salvage = 1.70 if year == 2015
replace salvage = 1.67 if year == 2016
replace salvage = 1.73 if year == 2017
replace salvage = 1.68 if year == 2018
replace salvage = 1.62 if year == 2019
replace salvage = 1.74 if year == 2020
gen donation_value = salvage*PalletTotalWeight

/// Save Dataset 
/// Assumptions: Drop Outliers == Any Donations Above 1 Million Pounds --> 3 observations lost 
drop if PalletTotalWeight > 1000000
save "${bt}/GleanersFoodClean.dta", replace 
*/
