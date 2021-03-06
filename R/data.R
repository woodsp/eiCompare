#' Corona 2006 Election Results
#'
#' This dataset contains precinct vote data from a 2006 election in Corona, CA.
#'
#' @name cor_06
#' @format A data frame with 47 observations on the following 8 variables:
#' \describe{
#'  \item{precinct}{Precinct ID number.}
#'  \item{totvote}{The total vote, per precinct.}
#'  \item{pct_latino}{Percent of voters identifying as Latino.}
#'  \item{pct_other}{Percent of voters identifying as non-Latino.}
#'  \item{pct_breitenbucher}{Percent of vote for Breitenbucher.}
#'  \item{pct_montanez}{Percent of voters for Montanez.}
#'  \item{pct_spiegel}{Percent of voters for Spiegel.}
#'  \item{pct_skipworth}{Percent of voters for Skipworth.}
#' }
#' @usage data(cor_06)
#' @source Riverside County, CA Board of Elections.
"cor_06"


#' Corona 2014 Election Results
#'
#' This dataset contains precinct vote data and racial demographics from a 2014
#' election in Corona, CA.
#'
#' @name corona
#' @format A data frame with 46 observations on the following 12 variables:
#' \describe{
#'  \item{precinct}{Precinct ID number.}
#'  \item{totvote}{The total vote, per precinct.}
#'  \item{pct_husted}{Percent of vote for Husted.}
#'  \item{pct_spiegel}{Percent of vote for Spiegel.}
#'  \item{pct_ruth}{Percent of vote for Ruth.}
#'  \item{pct_button}{Percent of vote for Button.}
#'  \item{pct_montanez}{Percent of vote for Montanez.}
#'  \item{pct_fox}{Percent of vote for Fox.}
#'  \item{pct_hisp}{Percent of voters identifying as Hispanic.}
#'  \item{pct_asian}{Percent of voters identifying as Asian.}
#'  \item{pct_white}{Percent of voters identifying as white.}
#'  \item{pct_non_lat}{Percent of voters identifying as non-Latino.}
#' }
#' @usage data(corona)
#' @source Riverside County, CA Board of Elections
"corona"

#' East Ramapo School District Proposed Maps
#'
#' This dataset contains proposed maps and Citizen Voting Age Population (CVAP)
#' totals for East Ramapo School District.
#'
#' @name ersd_maps
#' @format A data frame with 8 observations on the following 8 variables:
#' \describe{
#'  \item{WARD}{The ward ID number.}
#'  \item{TOT_CVAP}{The total vote, according to CVAP, per precinct.}
#'  \item{WHI_CVAP}{The number of white CVAP voters, per precinct.}
#'  \item{BLA_CVAP}{The number of Black CVAP voters, per precinct.}
#'  \item{HIS_CVAP}{The number of Hispanic CVAP voters, per precinct.}
#'  \item{ASI_CVAP}{The number of Asian CVAP voters, per precinct.}
#'  \item{MIN_AGG_FRAC}{The number of Black/Hispanic CVAP voters, per precinct.}
#'  \item{geometry}{The geomtry for each ward.}
#' }
#' @usage data(ersd_maps)
#' @source East Ramapo School District
"ersd_maps"


#' Los Angeles County 2010 Election Results
#'
#' This dataset contains precinct vote data and racial demographics from a 2010
#' election in Los Angeles County.
#'
#' @name lac_10
#' @format A data frame with 4980 observations on the following 10 variables:
#' \describe{
#'  \item{precinct}{Precinct ID number.}
#'  \item{tot_reg}{The total number of registered voters.}
#'  \item{i_jones}{Number of votes for Jones.}
#'  \item{i_delatore}{Number of votes for Delatorre.}
#'  \item{votescast}{The total number of votes cast.}
#'  \item{lat_voters}{Number of Latino voters.}
#'  \item{pct_latino}{Percent of voters identifying as Latino.}
#'  \item{pct_delatorre}{Percent of vote for Delatorre.}
#'  \item{pct_jones}{Percent of vote for Jones.}
#'  \item{pct_other}{Percent of vote for other candidates.}
#' }
#' @usage data(lac_10)
#' @source Los Angeles County
"lac_10"


#' New York State FIPS codes
#'
#' New York State FIPS codes for 500 voters.
#'
#' @name ny_fips
#' @format A data frame with 500 observations on the following 2 variables:
#' \describe{
#'  \item{row_id}{Unique identifier.}
#'  \item{FIP}{The 15-digit FIPS code.}
#' }
#' @usage data(ny_fips)
"ny_fips"


#' New York Voter File Sample
#'
#' This dataset contains a sample of 500 voters in East Ramapo School District,
#' New York.
#'
#' @name ny_voter
#' @format A data frame with 500 observations on the following 10 variables:
#' \describe{
#'  \item{Voter.ID}{Anonymized voter ID.}
#'  \item{SD..Poll}{Precinct ID.}
#'  \item{fips}{The 15-digit FIPS code}
#'  \item{st}{State FIPS code}
#'  \item{county}{County FIPS code}
#'  \item{tract}{Tract FIPS code}
#'  \item{block}{Block FIPS code}
#'  \item{st_cty}{State-county FIPS code}
#'  \item{st_cty_tract}{State-county-tract FIPS code}
#'  \item{Last.Name}{Voter surname.}
#' }
#' @usage data(ny_voter)
#' @source East Ramapo School District Board of Elections.
"ny_voter"


#' East Ramapo School District 2018 Voter File
#'
#' This dataset contains a subset of the voter file for voters in East Ramapo
#' School District, in 2018. This file has been modified to protect the privacy
#' of the voters. The voter IDs have been replaced, surnames have been replaced
#' with "similar" surnames, and the file comes already geocoded into Census
#' block and ward.
#'
#' @name ramapo2018
#' @format A data frame with 9401 observations on the following 7 variables:
#' \describe{
#'  \item{voter_id}{The voter ID, recoded to protect voter privacy.}
#'  \item{last_name}{The surname of the voter.}
#'  \item{ward}{The ward, as a character, that the voter is located in.}
#'  \item{state}{FIPS code of state for voter.}
#'  \item{county}{FIPS code of Census county for voter.}
#'  \item{tract}{FIPS code of Census tract for voter.}
#'  \item{block}{FIPS code of Census block for voter.}
#' }
#' @usage data(ramapo2018)
#' @source East Ramapo School District
"ramapo2018"


#' Rockland County, NY, Census demographic dataset.
#'
#' This dataset contains the demographic information for Rockland County in New
#' York, which is where East Ramapo School District is located.
#'
#' @format A nested list which can be sent to the `predict_race` function in
#'  WRU. Within "NY", the "block", "tract", and "county" keys contain the
#'  following columns.
#' \describe{
#'  \item{state}{State FIPS code}
#'  \item{county}{County FIPS code}
#'  \item{tract}{Tract FIPS code}
#'  \item{block}{Block FIPS code}
#'  \item{P005003}{White alone population}
#'  \item{P005004}{Black or African American alone population}
#'  \item{P005005}{American Indian and Alaska Native alone population}
#'  \item{P005006}{Asian alone population}
#'  \item{P005007}{Native Hawaiian and Other Pacific Islander alone population}
#'  \item{P005008}{Some other race alone population}
#'  \item{P005009}{Two or more races population}
#'  \item{P005010}{Hispanic or Latino population}
#'  \item{r_whi}{White voters; from Census Bureau.}
#'  \item{r_bla}{Black voters; from Census Bureau.}
#'  \item{r_his}{Hispanic voters; from Census Bureau.}
#'  \item{r_asi}{Asian voters; from Census Bureau.}
#'  \item{r_oth}{Other voters; from Census Bureau.}
#' }
#' @usage data(rockland_census)
#' @source Census Bureau via the WRU package.
"rockland_census"

#' Election results and racial turnout data for Gwinnett County, Georgia, US
#'
#' This dataset contains results of the 2018 Georgia gubernatorial election for
#' precincts in Gwinnett County. Data includes counts of votes cast for each
#' candidate and turnout by racial group.
#'
#' Data contain the following intentional errors mean for illustration in
#' vignettes: Rows 35 and 36 split up election results for the same precinct.
#' These should be collapsed.
#'
#' @format A data frame with 157 rows and 9 columns
#' \describe{
#' \item{precinct}{Unique precinct identifier}
#' \item{turnout}{Count of voter turnout}
#' \item{kemp}{Count of votes cast for Republican candidate Brian Kemp}
#' \item{abrams}{Count of votes cast for Democratic candidate Stacey Abrams}
#' \item{metz}{Count of votes cast for Libertarian candidate Ted Metz}
#' \item{white}{Count of voters self-reporting as white}
#' \item{black}{Count of voters self-reporting as black}
#' \item{hispanic}{Count of voters self-reporting as hispanic}
#' \item{other}{Count of voters self-reporting any other racial/ethnic group}
#' }
#' @usage data(gwinnett)
"gwinnett"
