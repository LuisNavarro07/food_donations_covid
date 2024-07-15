*******************************************************************************
**** In-kind and Money Donations 
**** Authors: Denvil Duncan, Shelley Suttles and Luis Navarro
**** Code by: Luis Navarro
**** Last Update: August 2022
**** Script: Clean Control Variables and Merge them with Gleaners Dataset  
********************************************************************************
/// County Controls 
/// Population

import delimited "${bi}/co-est2020-alldata.csv", varnames(1) clear 
keep if sumlev == 50 
statastates, name(stname) nogen 
split ctyname, parse(" County") 
countyfips, statefips(state_fips) name(ctyname1) nogen
drop if fips == . 
keep fips popestimate*
reshape long popestimate, i(fips) j(year)
tsset fips year
tempfile population 
save `population', replace
keep if year == 2020 
rename popestimate pop20
tempfile pop20
save `pop20', replace

/// Cases 
import delimited "${bi}/nyt_covid_counties_may2022.csv", clear varnames(1)
gen date1 = date(date, "YMD" 2050)
gen mofd = mofd(date1)
drop date 
rename date1 date 
format mofd %tmMon_CCYY
gcollapse (max) cases deaths date, by(fips mofd)
sort fips mofd
bysort fips: gen cases_flow = cases[_n] - cases[_n-1]
bysort fips: replace cases_flow = cases if cases_flow == .
/// we have some missings
merge m:1 fips using `pop20', keep(match master) nogen
bysort fips: gen casespc = (cases_flow/pop20)*1000
bysort fips: gen deathspc = (deaths/pop20)*1000
label variable casespc "Cases per Thousand People"
label variable deathspc "Death per Thousand People"
drop pop20
tempfile cases 
save `cases'

/// Ethnicities 
import delimited "${bi}/cc-est2019-alldata.csv", varnames(1) clear 
drop if year == 1 | year == 2 
keep if agegrp == 0 
keep if sumlev == 50 
statastates, name(stname) nogen 
split ctyname, parse(" County") 
countyfips, statefips(state_fips) name(ctyname1) nogen
drop if fips == . 
tsset fips year
qui destring _all, replace 

local yr = 2010 
local j = 0
forvalues i=3(1)12{
    replace year = `yr' + `j' if year == `i'
	local j = `j' + 1
}
/// one variable is share hispanic, the other variable is share white. 

gen white  = (wa_male + wa_female) / tot_pop
gen hispanic = (h_male + h_female) / tot_pop
gen black  = (ba_male + ba_female) / tot_pop
gen asian  = (aa_male + aa_female) / tot_pop
gen native = (ia_male + ia_female) / tot_pop


/*
gen tot_man = white + black + asian + native + hispanic 
local varlist white black asian native hispanic 
foreach var of local varlist {
    replace `var' = `var'/tot_man 
}
*/

keep fips year white black asian native hispanic tot_pop
tempfile races
save `races'

/// Demographics 
import delimited "${bi}/CC-EST2020-AGESEX-ALL.csv", varnames(1) clear 
drop if year == 1 | year == 2 | year == 13
replace year = 13 if year == 14
local yr = 2010 
local j = 0
forvalues i=3(1)13{
    replace year = `yr' + `j' if year == `i'
	local j = `j' + 1
}
statastates, name(stname) nogen 
split ctyname, parse(" County") 
countyfips, statefips(state_fips) name(ctyname1) nogen
drop if fips == . 
tsset fips year
qui destring _all, replace 
gen female = popest_fem/(popest_fem+popest_male)
gen tot = age513_tot + age1417_tot + age1824_tot + age2544_tot + age4564_tot + age6569_tot + age7074_tot + age7579_tot + age8084_tot + age85plus_tot

gen age17 = (age513_tot + age1417_tot) /tot
gen age44 = (age1824_tot + age2544_tot) /tot
gen age64 = (age4564_tot) / tot
gen age65 = (age6569_tot + age7074_tot + age7579_tot + age8084_tot + age85plus_tot)/tot
keep fips year female tot age17 age44 age64 age65
merge 1:1 fips year using `population', keep(match master) nogen
merge 1:1 fips year using `races', keep(match master) nogen
tempfile controls
save `controls'


/// Supplemental Data 
import excel "${bi}/Supplemental Data/supdata.xlsx", firstrow clear
local varlist unemploy_rate unemploy_count employ_count labor_force quarter_yearx estab_count quarterx food_price_index quarter_yeary weekly_wage quartery SNAP_All_Persons_PA SNAP_All_Persons_NPA SNAP_Total_People SNAP_All_Households_PA SNAP_All_Households_NPA SNAP_Total_Households SNAP_All_Total_Actual_Issuance half_year shelter_in_place no_nonessential_biz
foreach var of local varlist {
	capture replace `var' = "" if `var' == "NA"
}
 
gen mofd = mofd(mdy(month,1,year))
format mofd %tm
destring _all, replace
drop year month month_year quarter_yearx quarterx quarter_yeary quartery
rename fips_code fips 
order fips mofd 
tempfile supdata 
save `supdata', replace 

/// SOI and BLS data 
import delimited "${bt}/county_month_data_bls_soi.csv", clear 
rename (female asian black white)(female1 asian1 black1 white1)
gen date = date(monthyear, "YMD", 2050)
gen mofd = mofd(date)
gen month = month(date)
format mofd %tm
order geoid mofd 
sort fips mofd
*drop fips 
*rename geoid fips 
tempfile bls_soi 
save `bls_soi', replace 

********************************************************************************
//// Gleaners Data - Money  
use "${bt}/GleanersMoneyCountyMonth.dta", clear 
drop if fips == . 
tempfile money 
save `money'
/// Food Data - Non Gov 
use "${bt}/GleanersFoodCountyMonthNonGov.dta", clear 
tempfile nongov 
save `nongov'

use "${bt}/GleanersFoodCountyMonth.dta", clear 
drop if fips == .  
sort fips mofd
merge m:1 fips mofd using `nongov', keep(match master) nogen
merge m:1 fips mofd using `money', keep(match master) nogen 
merge m:1 fips mofd using `cases', keep(match master) nogen 
merge 1:1 fips mofd using "${bt}/county_labordata.dta", keep(match master) nogen  
merge 1:1 fips mofd using `supdata', keep(match master) nogen
merge 1:1 fips mofd using `bls_soi', keep(match master) nogen

/// merge controls year
merge m:1 fips year using `controls', keep(match master) nogen

********************************************************************************
/// Prepare Dataset For Regression Analysis 
statastates, fips(state_fips) nogen
/// Generate Outcomes 
gen food = ln(PalletTotalWeight)
gen foodng = ln(PalletTotalWeight_NonGov)
gen money = ln(donation_sum)
/// Per Capita Outcomes 
sort fips mofd year
/// Assumption: Population is kept constant at its last measurement, for the sake of computing the log percapita outcomes. 
bysort fips: carryforward popestimate, replace
gen foodpc = PalletTotalWeight/popestimate
gen foodngpc = PalletTotalWeight_NonGov/popestimate
gen moneypc = donation_sum/popestimate

/// Scale some covariates 
gen pop = ln(popestimate)
gen logsnap_person = ln(SNAP_All_Persons_PA)
gen logsnap_hh = ln(SNAP_All_Households_PA)
gen ry = year - 2009
/// Carryforward year characteristics from the census 
sort fips mofd 
local varlist female age17 age44 age64 white black asian native hispanic tot_pop 
foreach var of local varlist{
	bysort fips: carryforward `var', replace 
}
/// Post pandemic variable - Excluding March
gen post = mofd > tm(2020m3)

// Label Variables 
cap label variable unemployment_rate "Unemployment Rate"
cap label variable female "Percent Female"
cap label variable age17 "Age $<$ 17"
cap label variable age44 "Age 18-44"
cap label variable age64 "Age 44-64"
cap label variable age65 "Age 65 +"
cap label variable hispanic "Percent Hispanic"
cap label variable white "Percent White"
cap label variable casespc "Covid-19 Cases Per Capita"
cap label variable food_price_index "Food Price Index"
cap label variable weekly_wage "Wage"
cap label variable pop " Log Population"
cap label variable logsnap_person "Log SNAP Persons"
cap label variable logsnap_hh "Log SNAP Households"
cap label variable ry "Time Trend (Year)"
cap label variable post "Post April 2020"
cap label variable popestimate "Population"
cap label variable black "Percent Black"
cap label variable asian "Percent Asian"
cap label variable native "Percent Native"
cap label variable PalletTotalWeight "In-Kind Donations "
cap label variable PalletTotalWeight_NonGov "NG In-Kind Donations"
cap label variable donation_sum "Financial Donations"
cap label variable food "In-Kind Donations (Log)"
cap label variable foodng "NG In-Kind Donations (Log)"
cap label variable money "Financial Donations (Log)"
cap label variable percent_returns_charitable "\% Tax Returns with Donations"
cap label variable donation_percapita "Donation (Tax Return Report) Per Capita"
cap label variable percent_charitable_month "Charitable Donors (\% Population)"
cap label variable rank_returns_charitable "Charitable Donations - Annual Rank"
cap label variable quartile_returns_charitable "Quartile Donations (Annual)"
cap label variable quartile_returns_charitable_2013 "Quartile Donations (Annual)"

************************************************
/// Main Assumption: Only Indiana after 2012 
keep if state_fips == 18
keep if year > 2012 
save "${bt}/gleaners_food_regression.dta", replace 
