# covid_county_ts Readme 

This is a data set drawn from the New York Times COVID-19 tracker. The data are transformed into a time series data set by county to track new deaths and cases by week. The following are the fields. 

fips - The five digit FIPs code for counties, with the first two digits the state fips, and the last three the coutny identifier 

week - The week number marking the time series. 

state - The state name for the county, upper case. 

county - The county name, upper case. 

cases - The number of new cases of COVID-19. 

deaths - The number of new deaths related to COVID-19

st_fips - The two digit state fips code. 

total_pop - The county's total population, as estimated from the 2018 5 year ACS data . 

pop_ppsm - The population per square mile for a county. 

gini_index - The gini index for economic inequality, with scores of 0 reflecting perfect equality, and score of 1 perfect inequality. 

white_pct - The percentage of the population that identifies as white, non-hispanic 

state_po - The state postal abbreviation 

deaths_per_cap - The new deaths per capita from COVID-19

cases_per_cap - The new cases per capita from COVID-19

cases_per100k - The new cases per 100,000 people 

deaths_per100k - The new deaths per 100,000 people 