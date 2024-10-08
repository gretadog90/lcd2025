/*

Try to isolate population data source change for 2023

*/

cd "/Users/gretam/Documents/LCD/matching/2023/"


foreach file in 2023_Fall_AllCities.csv 2023_Winter_AllCities.csv ///
	2023_Summer_AllCities.csv 2023_Spring_AllCities.csv {
		import delimited "Jen/`file'", varn(1) clear
		keep urban_area country xc_iso_lst mean 
		rename mean mean_j
		local o=subinstr("`file'", "_AllCities.csv", "", 1)
		save "Jen/`o'.dta", replace
		import delimited "Greta/`file'", varn(1) clear
		keep urban_area country xc_iso_lst mean
		rename mean mean_g
		local o=subinstr("`file'", "_AllCities.csv", "", 1)
		save "Greta/`o'.dta", replace
}

foreach file in 2023_Fall 2023_Winter 2023_Summer 2023_Spring {
	use "Jen/`file'", clear
	merge 1:1 urban_area using "Greta/`file'", assert(3) nogen
	gen diff=mean_j-mean_g
	summ diff
}

foreach file in 2020_sum2_popsize_AllCities.csv 2023_sum1_fall_AllCities.csv ///
	2023_sum1_winter_AllCities.csv 2023_sum1_summer_AllCities.csv ///
 	2023_sum1_spring_AllCities.csv {
		import delimited "Jen/`file'", varn(1) clear
		keep urban_area country xc_iso_lst sum
		rename sum sum_j
		local o=subinstr("`file'", "_AllCities.csv", "", 1)
		save "Jen/`o'.dta", replace
		import delimited "Greta/`file'", varn(1) clear
		keep urban_area country xc_iso_lst sum
		rename sum sum_g
		local o=subinstr("`file'", "_AllCities.csv", "", 1)
		save "Greta/`o'.dta", replace
}

foreach file in 2020_sum2_popsize 2023_sum1_fall 2023_sum1_winter ///
	2023_sum1_summer 2023_sum1_spring {
		use "Jen/`file'", clear
		merge 1:1 urban_area using "Greta/`file'", assert(3) nogen
		gen diff=sum_j-sum_g
		summ diff
}

*pull in 100m population data and compare denominators across cities
import delimited "Pop_update/2020_sum2_popsize_AllCities.csv", varn(1) clear
keep urban_area country xc_iso_lst sum
rename sum pop_100m
save "Pop_update/2020_100m", replace

import delimited "Pop_update/2020_denom_1km_count.csv", varn(1) clear
keep urban_area country xc_iso_lst sum
rename sum pop_1km_count
save "Pop_update/2020_1km_count", replace

merge 1:1 urban_area using "Pop_update/2020_100m", assert(3) nogen
merge 1:1 urban_area using "Jen/2020_sum2_popsize", assert(3) nogen
rename sum_j pop_1km
corr pop_1km pop_1km_count pop_100m

gen pop_diff=pop_1km-pop_100m
summ pop_diff

gen pct_diff=(pop_1km-pop_100m)/pop_1km
summ pct_diff

