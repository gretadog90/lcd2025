/*

Compare 2015 results with old/new shapefiles

*/

cd "/Users/gretam/Documents/LCD/matching/Lancet2025/new_shape/"

local files : dir . files "*.csv"

foreach f in `files' { 
	if regexm("`f'", "sum"){
		import delimited "`f'", varn(1) clear
		keep city sum
		local g=subinstr("`f'", "_AllCities.csv", "", 1)
		save "`g'.dta", replace
	}
	else {
		import delimited "`f'", varn(1) clear
		keep city mean
		local g=subinstr("`f'", "_AllCities.csv", "", 1)
		save "`g'.dta", replace
	}
}

cd "/Users/gretam/Documents/LCD/matching/Lancet2025/"

foreach file in 2015_Fall.dta 2015_Winter.dta 2015_Summer.dta 2015_Spring.dta {
	di "`file'"
	use "Greta/`file'", clear
	drop city
	rename (mean city1) (mean_old city)
	merge 1:1 city using "new_shape/`file'"
	gen diff=mean_old-mean
	summ diff
	summ diff if city!="Male"
}


use "Greta/2015_Spring.dta", clear
drop city
rename (mean city1) (mean_old city)
merge 1:1 city using "new_shape/2015_Spring.dta"
gen diff=mean_old-mean
summ diff
sort diff 
br

use "Greta/2015_sum2_popsize.dta", clear
drop city 
rename (sum city1) (sum_old city)
merge 1:1 city using "new_shape/2015_sum2_popsize"
gen diff=sum_old-sum
summ diff
sort diff 
br
