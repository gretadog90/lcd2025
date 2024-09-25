/*

Purpose:

compare full GHS SMOD city list to what's used in LCD countdown. 
	1. make any city name changes to match what's being used in LCD.
	2. export a list of just the cities used in the analysis so that we can
		subset the GHS data and upload just these cities to GEE
		
*/

cd "/Users/gretam/Documents/"


*import name list from full 2019A GHS UCDB gpkg file
import delimited using "data/shapefiles/all_cities.csv", ///
	varn(1) clear

*keep just city, country, id, and population vars
drop v1 geometry uc_nm_lst

*rename pop var to find match in LCD data
rename  p15 p15_g

save "LCD/analysis/ucdb_full.dta", replace

*import the final results file from LCD project for the edited names of cities/countries
import delimited using "LCD/Previous LCD/final_data_2024LCD_greenness.csv", ///
	varn(1) clear
keep city country 

save "LCD/analysis/LCD_ua_list", replace

*import the .csv export from GEE that is merged to the 2019A GHS data for matching
import delimited using "LCD/matching/2023/Jen/2020_sum2_popsize_AllCities.csv", ///
	varn(1) clear
	
keep ctr_mn_nm uc_nm_mn p15 urban_area country

*match on the city and country name from GHS UCDB data
merge 1:m ctr_mn_nm uc_nm_mn using "LCD/analysis/ucdb_full"

*many repeat names of city, country so flag these
bys uc_nm_mn: gen count=_N
count if inrange(count, 2, .) & _merge==3

*find which they matched it to by matching the population variable as well
destring p15_g, replace
gen match=(p15_g==p15 & _merge==3)

*keep just these cities (should be 1042)
count if match==1
assert r(N)==1042
keep if match==1

*drop excess vars and merge with cleaned names file
keep id_hdc_g0 uc_nm_mn ctr_mn_nm urban_area country

rename urban_area city

merge 1:1 city country using "LCD/analysis/LCD_ua_list"

*save off the ones that matched exactly
gen match=(_merge==3)

savesome id_hdc_g0 city country if match==1 using ///
	"LCD/analysis/exact_matches", replace
	
keep if _merge==1
drop match _merge

*match on just city
merge 1:1 city using "LCD/analysis/LCD_ua_list", keep(1 3)

append using "LCD/analysis/exact_matches"
replace country="Côte d'Ivoire" if country=="CÃ´te d'Ivoire"
drop ctr_mn_nm uc_nm_mn _merge

export delimited using "/Users/gretam/Documents/LCD/analysis/edited_names.csv"


