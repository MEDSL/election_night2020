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

school_enrolled_pct - The percentage of children enrolled in school 

income_under25kpct - The percentage of households earning under $25,000

income25k_49kpct - The percentage of households earning between $25,000 and $49,999

pop50k_74kpct - The percentage of households earning between $50,000 and $74,999

pop75k_99kpct - The percentage of households earning between $75,000 and $99,999

pop100koverpct - The percentage of households earning over $100,000

pov_below_line - The percentage of households below the poverty line 

pov_1_2overline - The percentage of households earning between 1 and 2 times the poverty line 

pov_doubleline - The percentage of households earning over 2 times the poverty line

# other 

metro_type - The coding from the NCHS on how to rank counties relative to metro status. They can be found on this link: https://www.cdc.gov/nchs/data/series/sr_02/sr02_166.pdf
They are as follows: 

	Large metro - counties with 1 million or more people, considered part of a major metro area. 44 are present within the US. 

	Medium metro - counties with under 1 million people, but more than 250,000 within a major metro area. 244 are present within the U.S. 

	Small metro - counties with under 250,000 people and part of the metro region. 899 present within the U.S. 

	Micro - counties part of micropolitan statistical areas. 658 present within the U.S. 

	Noncore - Counties that meet none of the above conditions, categorized as rural; 1,395 present within the U.S. 

	










 










 


































 


 

 
 

 




 

 
 

