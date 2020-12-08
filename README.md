# election_night2020

The election_night2020 repository will act as a place to store election night results as they arise, in addition to election context resources. Therefore, the two folders to share results are resources and context. The following are the descriptions: 

## Scripts: 

county_mapR.R - A script for creating county level maps of election results. Uses readlines commands for all but the initial reading of the user's data set in order to guide the user through the process. Strucutred to read files from the master parent directory, set to automatically update according to the unique setup of the user's directory.  

hexmapR.R - A script for creating a national level hexagonal map of election results. Uses readlines commands for all but the initial reading of the user's data set in order to guide the user through the process. Strucutred to read files from the master parent directory, set to automatically update according to the unique setup of the user's directory.

figure_creatR.R - A script that takes county level data and produces either a bar plot of the Democratic vote share by metro, or scatter plots with 0 - 100 percent range x and y data. Prompts guide the user, and strucutred to read files from the master parent directory, set to automatically update according to the unique setup of the user's directory.  

## Results

The results folder has results for each state, with state folders by state postal code. These can and should be updated with results in some type of csv/xls, etc. format as appropriate.

As of 12//08/2020, scraped state and county level data from the New York Times is present. The data frames are as follows: 

	complete_nyt_by_time3.csv - The scraped New York Times presidential election results at the state level. The fields are as follows:

	time - the time stamp of the scraped results, formatted %m/%d/%Y %H/%M%S 

	state - the state name. 

	rep - votes cast for Republican presidential candidate

	dem - votes cast for the Democratic candidate. 

	total - the total votes cast for the state. 

	percent reported - the percent of votes reported from what's expected 

	rep.diff - change in the number of Republican votes from the previously reported results. 

	dem.diff - change in the number of Democratic votes from the previously reported results.

	total.diff - change in the number of total votes from the previously reported results.

	state_cumulative - The proportion of votes counted.

	polls.close - the hours from the polls closing in the state. 

	hours.from.close_rounded - Above, but rounded 

		

	nyt15minute_county_data.csv - The New York Times data reported in 15 minute intervals from the state's first reported results. 

	nyt_counties_final.csv - The final New York Times scraped county level election data. The last reported results from the nyt15minute_county_data.csv data frame. 

The fields are as follows:

	fips - county fips code

	interval15min_num - the number indicator of quarter hours from the start of when the first results in a given state were reported. 

	name - the name of the county 
	
	state - the state name 

	interval15min - the time of the last reported results 

	tot_exp_vote - The New York Times number of expected votes from a given county. Note: It is not entirely clear how the New York Times estimated the total expected vote, and it can be immensely off from the actual reported results. Checked against other data sources of county votes (i.e. NBC) reveals that when the total.votes over the total expected vote exceeds 100, it is not due to reported results being incorrect. 

	total.votes - the total number of votes present within the county. 

	trumpd - the total number of votes cast for Donald Trump.

	bidenj - the total number of votes cast for Joe Biden. 

	abs_trumpd - the total number of absentee votes cast for Trump. 

	abs_bidenj - the total number of absentee votes cast for Biden. 

	lag_total_votes - the total number of votes cast in the previous 15 minute interval .

	lag_bidenj - the total number of votes cast for Biden in the last 15 minute interval.

	lag_trumpd - the total number of votes cast for Trump in the last 15 minute interval.

	total_vote_chg - the change in the number of votes from the last 15 minute interval .

	biden_chg - the change in the number of votes for Biden from the last 15 minute interval.

	trump_chg - the change in the number of votes for Trump from the last 15 minute interval.

	prop_expect_returned - The proportion of votes counted relative to the New York Times expected turnout. Note: See note above for the tot_exp_vote field. 

	dem_pct - The Democratic percent of the total vote. 

	dem2party_pct - The Democratic percent of the two party vote. 

	final_vote - the final total vote present within a county. 

	returned percent - the cumulative proportion of vote tabulated at a given time period.



	 


### plots - a folder of general plots from national data and/or state by some general category. 

As of 12/08/2020, most of these plots are related to the blue shift/red shift. The folders are as follows: 

county tabulate - time series plots on the time to tabulate ballots by state, grouped by county. Created using New York Times county level scraped data 

enr_plots - Election night plots of states over the course of election night/week regarding proportion of votes counted by party over time, and the Democratic lead over the night. 

	first24hours - same as above, though with the x-axis constrained to reflect the first 24 hours of the night from a state's polls closing. 

 

## Context

The folders related to county and state level information that can be used on the fly for analyses of interest. They are grouped into the following folders: 

### acs_demos

Information related to American Community Surveys of demographics of interest. These include information for age, sex, race, education, income, poverty level, and school enrollment in both counts and percentages. 

### covid 

Information by county on new cases of COVID-19 over time. 

### maps

Useful spatial R data to create maps by state and county, along with template scripts. 

### historical_elections

Historical election results by county and state

