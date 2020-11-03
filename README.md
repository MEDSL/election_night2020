# election_night2020

The election_night2020 repository will act as a place to store election night results as they arise, in addition to election context resources. Therefore, the two folders to share results are resources and context. The following are the descriptions: 

## Scripts: 

county_mapR.R - A script for creating county level maps of election results. Uses readlines commands for all but the initial reading of the user's data set in order to guide the user through the process. Strucutred to read files from the master parent directory, set to automatically update according to the unique setup of the user's directory.  

hexmapR.R - A script for creating a national level hexagonal map of election results. Uses readlines commands for all but the initial reading of the user's data set in order to guide the user through the process. Strucutred to read files from the master parent directory, set to automatically update according to the unique setup of the user's directory.

figure_creatR.R - A script that takes county level data and produces either a bar plot of the Democratic vote share by metro, or scatter plots with 0 - 100 percent range x and y data. Prompts guide the user, and strucutred to read files from the master parent directory, set to automatically update according to the unique setup of the user's directory.  

## Results

The results folder has results for each state, with state folders by state postal code. These can and should be updated with results in some type of csv/xls, etc. format as appropriate. 

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

