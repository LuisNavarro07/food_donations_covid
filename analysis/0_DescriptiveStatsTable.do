*******************************************************************************
**** In-kind and Money Donations 
**** Authors: Denvil Duncan, Shelley Suttles and Luis Navarro
**** Code by: Luis Navarro
**** Last Update: June 2023
**** Script: Descriptive Statistics 
********************************************************************************

use "${bt}\gleaners_food_regression.dta", clear 
merge m:1 fips using "${bt}\quartiles_fipcrosswalk.dta", keep(match master) nogen
/// Assumption: Drop observations that do not appear in the quartile cross walk data (ie: they only have a couple observations by year.)
drop if  PalletTotalWeightq == .
rename (PalletTotalWeightq donation_sumq) (Quartile Quartile_Financial)
*******************************************************************************
/// Linear Regression Options 
global feopts absorb(month) vce(cluster fips)
global outcomes food foodng money
global controls female white hispanic age17 age44 age64
global economic unemployment_rate food_price_index logsnap_person
global table_opts mtitles("NG In-Kind" "NG In-Kind" "In-Kind" "In-Kind" "Financial" "Financial") se(%12.3fc) b(%12.3fc) label s(demo econ ymean N F,label("Demographic Controls" "Economic Controls" "Mean of Dep. Variable" "Observations" "F-Stat")) sfmt(%12.0fc)
*****************************************************************************

*******************************************************************************
/// Table 1. Descriptive Statistics  
global variables $outcomes $controls $economic 
format $variables %12.4fc
describe $variables
***** Table 1: Descriptive Statistics
***** Rows: Variables in the Regression: Outcomes, indep, controls. 
***** Columns: Statistics: Mean, Std Dev, 25th Pctile, Median, 75th Pctile, N" 
sum $variables 
matrix define S = J(12,7,.)
matrix colnames S = "Mean" "SD" "P25" "Median" "P75" "Max" "N"
local i = 1
local varlist $variables
foreach var of local varlist {
/// Summarize the Variable using the same conditional as in the model. 
qui sum `var' , detail 
matrix S[`i',1] = r(mean)
matrix S[`i',2] = r(sd)
matrix S[`i',3] = r(p25)
matrix S[`i',4] = r(p50)
matrix S[`i',5] = r(p75)
matrix S[`i',6] = r(max)
matrix S[`i',7] = r(N)
local i = `i' + 1
}

esttab mat(S, fmt(4))

/// Mean of Dep Variables in Units for Paper (Metric Tons)
sum PalletTotalWeight 
scalar meanmt_ptw = r(mean)/2204.6 
sum PalletTotalWeight_NonGov 
scalar meanmt_ptwng = r(mean)/2204.6 
sum donation_sum
scalar mean_money = r(mean)/1000
sum SNAP_All_Persons_PA
scalar mean_snap = r(mean)
dis "Average of In-Kind Donations (MT) =" meanmt_ptw
dis "Average of In-Kind Donations NG (MT) =" meanmt_ptwng
dis "Average of Financial Donations (Thousands) =" mean_money
dis "Average of SNAP Recipients =" mean_snap
********************************************************************************
/// Table 2. Mean Comparison by Quartile 
tab Quartile

local s =  1
local varlist $outcomes
foreach var of local varlist {
matrix define M =J(10,3,.)
matrix colnames M = "Pre" "Post" "Pval"
matrix rownames M = "FullSample" "SE" "Q1" "seq1" "Q2" "seq2" "Q3" "seq3" "Q4" "seqQ4"
matlist M

/// Full Sample
ttest `var', by(post)
local mu_pre = r(mu_1)
local mu_post = r(mu_2)
local se_pre = r(sd_1)/sqrt(r(N_1))
local se_post = r(sd_2)/sqrt(r(N_2))
local p_val = r(p)
local j = 1
/// Store 
matrix M[`j',1] = `mu_pre'
matrix M[`j'+1,1] = `se_pre'
matrix M[`j',2] =`mu_post'
matrix M[`j'+1,2] = `se_post'
matrix M[`j',3] = `p_val'

/// By Quartile 
forvalues i=1(1)4{
ttest `var' if Quartile == `i', by(post)
local mu_pre = r(mu_1)
local mu_post = r(mu_2)
local se_pre = r(sd_1)/sqrt(r(N_1))
local se_post = r(sd_2)/sqrt(r(N_2))
local p_val = r(p)


/// Store 
matrix M[`j'+`i'+ 1,1] = `mu_pre'
matrix M[`j'+`i'+ 2,1] = `se_pre'
matrix M[`j'+`i'+ 1,2] =`mu_post'
matrix M[`j'+`i'+ 2,2] = `se_post'
matrix M[`j'+`i'+ 1,3] = `p_val'
local j = `j' + 1
}


matrix define M`s' = M
matrix drop M 
local s = `s' + 1
}

********************************************************************************
/// Redo Test but with Post Variable only including to 2020 
local s =  1
local varlist $outcomes
foreach var of local varlist {
matrix define G =J(10,3,.)
matrix colnames G = "Pre" "Post" "Pval"
matrix rownames G = "FullSample" "SE" "Q1" "seq1" "Q2" "seq2" "Q3" "seq3" "Q4" "seqQ4"
matlist G

/// Full Sample
ttest `var' if year < 2021, by(post)
local mu_pre = r(mu_1)
local mu_post = r(mu_2)
local se_pre = r(sd_1)/sqrt(r(N_1))
local se_post = r(sd_2)/sqrt(r(N_2))
local p_val = r(p)
local j = 1
/// Store 
matrix G[`j',1] = `mu_pre'
matrix G[`j'+1,1] = `se_pre'
matrix G[`j',2] =`mu_post'
matrix G[`j'+1,2] = `se_post'
matrix G[`j',3] = `p_val'

/// By Quartile 
forvalues i=1(1)4{
ttest `var' if Quartile == `i' & year < 2021, by(post)
local mu_pre = r(mu_1)
local mu_post = r(mu_2)
local se_pre = r(sd_1)/sqrt(r(N_1))
local se_post = r(sd_2)/sqrt(r(N_2))
local p_val = r(p)


/// Store 
matrix G[`j'+`i'+ 1,1] = `mu_pre'
matrix G[`j'+`i'+ 2,1] = `se_pre'
matrix G[`j'+`i'+ 1,2] =`mu_post'
matrix G[`j'+`i'+ 2,2] = `se_post'
matrix G[`j'+`i'+ 1,3] = `p_val'
local j = `j' + 1
}


matrix define G`s' = G
matrix drop G 
local s = `s' + 1
}
********************************************************************************

**** Export the Results in Tables 
local title1 = "\textbf{Panel A:} In-Kind (Log Pounds)"
local title2 = "\textbf{Panel B:} In-Kind NG (Log Pounds)"
local title3 = "\textbf{Panel C:} Financial (Log US \$)"
forvalues s = 1(1)3{
clear 
svmat M`s'
rename (M`s'1 M`s'2 M`s'3) (pre post pval)
format pre post pval %12.4fc

gen var = ""
order var 
replace var = "Full Sample" if _n == 1
replace var = "" if _n == 2
replace var = "1st Quartile" if _n == 3
replace var = "" if _n == 4
replace var = "2nd Quartile" if _n == 5
replace var = "" if _n == 6
replace var = "3rd Quartile" if _n == 7
replace var = "" if _n == 8
replace var = "4th Quartile" if _n == 9
replace var = "" if _n == 10

replace pval = 0 if pval <= 0.0001
tostring pre post pval, force replace
local varlist pre post pval
foreach var of local varlist {
qui gen point_pos = strpos(`var',".")
qui replace `var' = substr(`var',1,point_pos + 4) 
qui replace `var' = "0" + `var' if strpos(`var',".") == 1
qui replace `var' = `var' + ".0000" if strpos(`var',".") == 0
qui replace `var' = `var' + "000" if length(`var') == 3
qui drop point_pos
}

replace pval = "" if pval == "0."

tempfile ttest_res
save `ttest_res', replace 

clear 
set obs 1
gen var = ""
gen pre = ""
gen post = ""
gen pval = ""
replace var = "`title`s''" if _n == 1
append using `ttest_res'
tempfile ttest_res`s'
save `ttest_res`s'', replace 
}

use `ttest_res1', clear 
append using `ttest_res2'
append using `ttest_res3'

label variable pre "2013:Q1-2020:Q1"
label variable post "2020:Q2-2022:Q1"
label variable pval "p-value "

replace pre = "("+ pre +")" if var == ""
replace post = "("+ post +")" if var == ""

*** Export the Table 
global texopts autonumber varlabels nofix replace frag 
local title "In-Kind and Financial Donations: Mean Comparison Before and After the COVID-19 Pandemic by Donation Quartile"
local fn "Notes: This table shows the mean estimation for each outcome variable at the full sample and by each quartile, in the period before the pandemic (2013:Q1 - 2020:Q1) and the period after (2020:Q2 - 2022:Q1) in our sample. Outcome variables expressed in logarithms. Standard errors are reported in parentheses. Reported p-value correspond to the t-test of mean difference between each period."
texsave using "${pg}\table2_ttestquartiles.tex", marker(tab:table2_ttestquartiles) title("`title'") footnote("`fn'") $texopts hlines(1 11 12 22 23)
list 
********************************************************************************

/// Redo Table with Post only 2020 
local title1 = "\textbf{Panel A:} In-Kind (Log Pounds)"
local title2 = "\textbf{Panel B:} In-Kind NG (Log Pounds)"
local title3 = "\textbf{Panel C:} Financial (Log US \$)"
forvalues s = 1(1)3{
clear 
svmat G`s'
rename (G`s'1 G`s'2 G`s'3) (pre post pval)
format pre post pval %12.4fc

gen var = ""
order var 
replace var = "Full Sample" if _n == 1
replace var = "" if _n == 2
replace var = "1st Quartile" if _n == 3
replace var = "" if _n == 4
replace var = "2nd Quartile" if _n == 5
replace var = "" if _n == 6
replace var = "3rd Quartile" if _n == 7
replace var = "" if _n == 8
replace var = "4th Quartile" if _n == 9
replace var = "" if _n == 10

replace pval = 0 if pval <= 0.0001
tostring pre post pval, force replace
local varlist pre post pval
foreach var of local varlist {
qui gen point_pos = strpos(`var',".")
qui replace `var' = substr(`var',1,point_pos + 4) 
qui replace `var' = "0" + `var' if strpos(`var',".") == 1
qui replace `var' = `var' + ".0000" if strpos(`var',".") == 0
qui replace `var' = `var' + "000" if length(`var') == 3
qui drop point_pos
}

replace pval = "" if pval == "0."

tempfile ttest_res
save `ttest_res', replace 

clear 
set obs 1
gen var = ""
gen pre = ""
gen post = ""
gen pval = ""
replace var = "`title`s''" if _n == 1
append using `ttest_res'
tempfile ttest_res`s'
save `ttest_res`s'', replace 
}

use `ttest_res1', clear 
append using `ttest_res2'
append using `ttest_res3'

label variable pre "2013:Q1-2020:Q1"
label variable post "2020:Q2-2022:Q1"
label variable pval "p-value "

replace pre = "("+ pre +")" if var == ""
replace post = "("+ post +")" if var == ""
list 
*** Export the Table 
global texopts autonumber varlabels nofix replace frag 
local title "In-Kind and Financial Donations: Mean Comparison Before and After the COVID-19 Pandemic by Donation Quartile"
local fn "Notes: This table shows the mean estimation for each outcome variable at the full sample and by each quartile, in the period before the pandemic (2013:Q1 - 2020:Q1) and the period after (2020:Q2 - 2022:Q1) in our sample. Outcome variables expressed in logarithms. Standard errors are reported in parentheses. Reported p-value correspond to the t-test of mean difference between each period."
texsave using "${pg}\table2_ttestquartiles_post20.tex", marker(tab:table2_ttestquartiles_post20) title("`title'") footnote("`fn'") $texopts hlines(1 11 12 22 23)

********************************************************************************


********************************************************************************
**** Table 1. Descriptive Statistics 

clear 
svmat S
gen var = ""
order var 
        
replace var = "Total In-Kind Donations (Log Pounds)" if _n == 1
replace var = "Total In-Kind Donations NG (Log Pounds)" if _n == 2
replace var = "Financial Donations (Log US \$)" if _n == 3
replace var = "Percent Female" if _n == 4
replace var = "Percent White" if _n == 5
replace var = "Percent Hispanic" if _n == 6
replace var = "Age $<$ 17" if _n == 7
replace var = "Age 18-44" if _n == 8
replace var = "Age 44-64" if _n == 9
replace var = "Unemployment Rate" if _n == 10
replace var = "Food Price Index" if _n == 11
replace var = "SNAP Beneficiaries (Log)" if _n == 12
**** 


/// Label Variables 
label variable S1 "Mean"
label variable S2 "SD"
label variable S3 "P25"
label variable S4 "P50"
label variable S5 "P75"
label variable S6 "Max"
label variable S7 "N"
format S1 S2 S3 S4 S5 S6 %12.3fc
format S7 %12.0fc


qui tostring S1 S2 S3 S4 S5 S6, replace force
local varlist S1 S2 S3 S4 S5 S6
foreach var of local varlist {
qui gen point_pos = strpos(`var',".")
qui replace `var' = substr(`var',1,point_pos + 4) 
qui replace `var' = "0" + `var' if strpos(`var',".") == 1
qui replace `var' = `var' + ".0000" if strpos(`var',".") == 0
qui replace `var' = `var' + "000" if length(`var') == 3
qui drop point_pos
}

drop S6
list


*** Export the Table qui label define rating_group 1 "AAA,AA" 2 "A" 3 "BBB,BB,NR"
local title "Descriptive Statistics"
local fn "Notes: This panel show the descriptive statistics of the main variables used for the analysis. The first two columns show the sample mean and standard deviation. P25, P50 and P75 show the 25, 50 and 75 percentiles, respectively. Variables representing total donations are expressed in logarithms. Demographic covariates are expressed as percentage of the population."
texsave using "${pg}\table1_descriptivestats.tex", marker(tab:table1_descriptivestats) title("`title'") footnote("`fn'") $texopts hlines(0)
