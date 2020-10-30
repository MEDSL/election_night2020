# Code book for county_acs_demos

This csv/rds file provides the relevant information by county statistics related to sex, age, education, economic inequality, and race, both as counts and percents. The following are the fields. 

## ID variables 

Geo_FIPS - The five digit fips code for the county. First two digits reflect the state fip code, and the last three the county identifier. Note: Might have to string pad if reading the csv. 

Geo_GEOID - The census long form id for a county. Better just to use the Geo_FIPS, but is present just in case. 

Geo_NAME - The name of the county 

Geo_QName - The name of the county, followed by the state name. 

Geo_STUSAB - The state postal code for the state, abbreviated. 


## Count variables 

The following variables all derive from the American Community Survey 5 year estimates, 2018. 

total_pop - The total population of the county. 

pop_ppsm - Population per square mile for the county. 

area_miles - Total geographic area of the county as measured in square miles. 

male_pop - The total male population of a county. 

male5under:male85over_pop - The male population by age counts. Broken up in the following intervals: 
	<5
	5 - 9
	10 - 14
	15 - 17
	18 - 24
	25 - 34
	35 - 44
	45 - 54
	55 - 64
	64 - 75
	75 - 84
	> 85 
female_pop - The total female population of a county. 

female5under:female85over_pop - The female population by age counts. Broken up in the following intervals: 
	<5
	5 - 9
	10 - 14
	15 - 17
	18 - 24
	25 - 34
	35 - 44
	45 - 54
	55 - 64
	64 - 75
	75 - 84
	> 85 
pop25over - The county's population over the age of 25. Used as the denominator for the education fields. 

less_than_hs - The county's population with less than a high school degree 

high_school - THe county's population with a high school degree or equivalent. 

some_college - The county's population with some college education, but not a degree. 

college_degree - The county's population with either an Associate's or Bachelor's degree. (NOTE: coded this way to better integrate with previous years of Census data). 

masters_degree - The county's population with an Master's degree. 

prof_degree - The county's population with some type of professional degree, such as law, which are above a BA but not an MA or PhD. 

phd_degree - The county's population with a PhD. 


## Other:

gini_index - The degree of inequality/wealth concentration within a county. Scores near 0 represent perfect equality, and scores closer to 1 perfect inequality. 

## Percents 

male_pct - PErcentage of the county's population that is male. 

female_pct - Percentage of the county's population that is female. 

age18underpct - Percentage of the county's population below the age of 18. 

age18to24pct - Percentage of the county's population between the ages 18 and 24. 

age25to34pct - Percentage of the county's population between the ages 25 and 34. 

age35to44pct - PErcentage of the county's population between the ages 35 and 44. 

age45to54pct - Percentage of the county's population between the ages 45 and 54. 

age55to64pct - percentage of the county's population between the ages 55 and 64. 

age65to74pct - percentage of the county's population between the ages 65 and 74. 

age75to84pct - percentage of the county's population between the ages 75 and 84. 

age85over_pct - percentage of the county's population 85 or over. 

white_pct - Percentage of the population that identifies as white, non-hispanic. 

black_pct - Percentage of the population that identifies as Black, non-hispanic.

asianpi_pct - Percentage of the population that identifies as Asian or Pacific Islander, non-hispanic. 

hispanic_pct - Percentage of the population that identifies as Hispanic/Latino as a race. 

other_pct - Percentage of the population that identifies as some other race not included above. 

less_than_hs_pct - Percentage of the population with less than a high school degree 

high_school_pct - Percentage of the population with a high school degree of equivalent. 

some_college_pct - Percentage of the population with some college experience. 

college_degree_pct - Percentage of the population with an Associate's or Bachelor's degree 

masters_degree_pct - Percentage of the population with a Master's degree. 

prof_degree_pct - Percentage of the population with a professional degree, such as law, that is above a Bachelor's, but not a Master's or PhD. 

phd_degree_pct - Percentage of the population with a PhD. 





























 


 

 
 

 




 

 
 

