*******************************************************************************
**** In-kind and Money Donations 
**** Authors: Denvil Duncan, Shelley Suttles and Luis Navarro
**** Code by: Luis Navarro
**** Last Update: August 2022
**** Script: Create County Level Dataset 
********************************************************************************
///Full Data Food 
global red "105 0 0"
global yellow "255 192 0"
global gray none
global two_opts lcolor(dknavy) lpattern(solid) msize(vsmall) mcolor(dknavy) msymbol(circle)
global twoway_opts xlabel(#10, labsize(small) angle(90) nogrid) ylabel(#8, labsize(small) angle(0) nogrid) xtitle("") ytitle("", size(small)) plotregion(color(none)) graphregion(color(none) margin(4 4 4 4)) plotregion(lcolor(none)) 
global legend size(small) pos(6)
global bars percentages stack bar(1, fcolor(maroon*1.2) lcolor(none)) bar(2, fcolor(maroon*0.6) lcolor(none)) bar(3, fcolor("255 86 29") lcolor(none)) bar(4, fcolor("251 162 127") lcolor(none)) bar(5, fcolor(none) lcolor(none)) bar(6, fcolor("146 195 51") lcolor(none)) bar(7, fcolor(ltblue*1.2) lcolor(none)) bar(8, fcolor(ltblue*0.6)) ytitle("", size(small)) plotregion(color(${gray})) graphregion(color(${gray}) margin(4 4 4 4)) plotregion(lcolor(black)) ylabel(#8, labsize(small) angle(0) nogrid)

use "${bt}/GleanersFoodFull.dta", clear 
/// Generate Date variables 
gen mofd = mofd(date)
format mofd %tm
gen year = year(date)
gen qofd = qofd(date)
format qofd %tq
/// Outcome Definition 
gen donation_value = PalletTotalWeight
format donation_value %12.0fc
sort donation_value 
*sum donation_value if donation_value < 0 




preserve 
gcollapse (sum) donation_value, by(Donor year)
drop if Donor == . 
reshape wide donation_value, i(year) j(Donor)
/// Relabel and rename 
*label variable donation_value1 "Co-Packer "
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

*rename donation_value1 copacker
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

local varlist distributor drugstore faa foodservice grower homecenter manufacturer othertrade processor reclamation restaurant retailer supermarket unknown warehouseclub wholesaler
foreach x of local varlist {
	replace `x' = 0 if `x' == . 
}
gen others =  drugstore + foodservice + homecenter + processor + restaurant + supermarket + unknown + wholesaler + reclamation
label variable others "Others"
drop drugstore foodservice homecenter processor restaurant supermarket unknown wholesaler reclamation



/// Graph 1. Bar Distribution of Donations Across Years 
/// Graph 1. Bar Distribution of Donations Across Years 
global bars percentages stack ///
    bar(1, fcolor(ebblue*1.25) lcolor(none)) ///
    bar(2, fcolor(ebblue*0.75) lcolor(none)) ///
    bar(3, fcolor(ltblue*1.25) lcolor(none)) ///
    bar(4, fcolor(ltblue*0.75) lcolor(none)) ///
    bar(5, fcolor(maroon*1.25) lcolor(none)) ///
    bar(6, fcolor(maroon*0.75) lcolor(none)) ///
    bar(7, fcolor(cranberry*1.25) lcolor(none)) ///
    bar(8, fcolor(cranberry*0.75) lcolor(none)) ///
    ytitle("", size(small)) ///
    ylabel(#8, labsize(small) angle(0) nogrid)

local donors retailer manufacturer distributor faa othertrade warehouseclub grower others 
graph bar (mean) `donors', over(year, label(labsize(small))) $bars legend(on order(1 "Retailer" 2 "Manufacturer" 3 "Distributor" 4 "FAA" 5 "Other Class of Trade" 6 "WH Club" 7 "Grower" 8 "Others") rows(2) size(vsmall) pos(6)) name(donors, replace) title("Donations by Donor Type (% of Total In-Kind Donations)", size(small) pos(11)) ylabel(#10, labsize(small) angle(0) nogrid) ytitle("", size(small)) 
*graph export "${ao}\FoodbyDonor.png", ${export}

gen tot = retailer + manufacturer + distributor + faa + othertrade + warehouseclub + grower + others
gen top3 = retailer + manufacturer + distributor
local varlist retailer manufacturer distributor faa othertrade warehouseclub grower others top3
foreach var of local varlist  {
	qui replace `var' = `var'/tot
}

local donors retailer manufacturer distributor faa othertrade warehouseclub grower others top3
tabstat `donors', by(year) stat(mean) format(%12.4fc)

restore 

**********************************
/// Graph 5. Food by Storage 
preserve 
gcollapse (sum) donation_value, by(Storage year)
drop if Storage == . 
reshape wide donation_value, i(year) j(Storage)

rename donation_value1 chemical
rename donation_value2 dry
rename donation_value3 drytote
rename donation_value4 frozen
rename donation_value5 frozenote
rename donation_value6 nf
rename donation_value7 ref

local varlist chemical dry drytote frozen frozenote nf ref 
foreach var of local varlist {
	replace `var' = 0 if `var' == . 
}

gen dry_total = dry + drytote
gen frozen_total = frozen + frozenote 
gen ref_total = ref + chemical 

/// Check Code 
global bars1 percentages stack bar(1, fcolor(navy) lcolor(none)) bar(2, fcolor(ebblue) lcolor(none)) bar(3, fcolor(ltblue) lcolor(none)) 
graph bar (mean) ref_total frozen_total dry_total, over(year, label(labsize(small))) $bars1 legend(on order(1 "Refrigerated" 2 "Frozen" 3 "Dry") rows(1) size(vsmall) pos(6)) name(storage, replace) title("Donations by Storage  (% of Total In-Kind Donations)", size(small) pos(11)) ylabel(#10, labsize(small) angle(0) nogrid) ytitle("", size(small)) 
*graph export "${ao}\FoodbyStorage.png", ${export}

gen tot = ref_total + frozen_total + dry_total
local varlist ref_total frozen_total dry_total 
foreach var of local varlist  {
	qui replace `var' = `var'/tot
}

local freezer ref_total frozen_total dry_total
tabstat `freezer', by(year) stat(mean) format(%12.4fc)


restore 



graph combine storage donors, rows(1) cols(2) xsize(16) ysize(8) name(char_comb, replace) xcommon ycommon
graph export "${pg}\Donations_Characteristics.pdf", replace



********************************************************************************
/// Load Data for Money Donations
use "${bt}\GleanersFinancialDonations.dta", clear 
drop if donation_value > 1000000
clonevar donation_sum = donation_value
clonevar donation_mean = donation_value 
label variable donation_sum "Sum Donations"
label variable donation_mean "Mean Donations"
