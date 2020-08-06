#' Pre-processes voter file by checking zipcode, and any special characters or typos within the address.
#'
#'
#' @param voter_file A voter file containing the address of the voter.
#' @param geocoder The type of geocoder used. For US Census API, use "census". For Opencage, use "opencage".
#' @param parallel A logical variable indicating whether the run will use parallel processing.
#' @param street_number The location in which the person lives. Ex. 1442 Market Street
#' @param city The name of the city that the voter lives in.
#' @param state The state (based on the United States 50 states) that the voter lives in.
#' @param zipcode The United States Postal Service (USPS) postal code.
#' @param census_benchmark The US Census Geocoder API benchmark that references a US Census locator like "Public_AR_Current".
#' @param census_vintage The US Census Geocoder API vintage that uses the benchmark to determine vintage geography. This can be a name or number.
#' @param census_output The geocoder output desired can be either: "simple", including only coordinates, or "full including vintage geographies.
#' @param census_class The object class of the census output. "df" for dataframe, "sf" for shapefile, or "json" for a json file.
#' @param opencage_key For using the Opencage geocoder API. Make sure to have a authorization key when signing up for Opencage via opencagedata.org
#'
#' @return The geocoded voter file with latitude and longitude coordinates.
#'
#' @export voter_file
#' @importFrom data.table dataframe manipulation
#' @importFrom censusxy The US Census Geocoder API https://cran.r-project.org/web/packages/censusxy/index.html
#' @importFrom opencage The commercial geocoder, Opencage that returns latitude and longitude vaues. https://opencagedata.com/
#'

run_geocoder <- function(voter_file,
                         geocoder = "census",
                         parallel = FALSE,
                         voter_id = "voter_id",
                         street = "street",
                         city = "city",
                         state = "GA",
                         zipcode = "residence_zipcode",
                         census_benchmark = "Public_AR_Current",
                         census_vintage = 4,
                         census_output = "single",
                         census_class = "sf",
                         opencage_key = NULL) {

  # Determine number of observations in the voter_file.
  num_obs <- nrow(voter_file)

  # Runs ONLY if Census Geocoder API is selected and there are less than 10,000
  # or less observations in the voter file.
  if (geocoder == "census" & parallel == FALSE & num_obs <= 10000) {
    census_voter_file <- cxy_geocode(
      .data = voter_file,
      id = voter_file,
      street = street,
      city = "city",
      state = "state",
      zip = "zipcode",
      return = "geographies",
      benchmark = census_benchmark,
      vintage = census_vintage,
      output = census_output,
      class = census_class
    )

    voter_file <- cenusus_voter_file
    return(voter_file)
  }

  # Runs ONLY if Opencage geocoder is selected and there is an opencage_key.
  if (geocoder == "opencage" & !is.null(opencage_key)) {
    voter_file$opencage_address <- paste(voter_file[[street]],
      voter_file[[city]],
      voter_file[[state]],
      voter_file[[zipcode]],
      sep = ","
    )
    for (m in 1:num_obs) {
      tryCatch(
        {
          opencage_latlon <- opencage_forward(
            placename = voter_file$opencage_address[m],
            country = "US",
            key = opencage_key
          )
          voter_file$lon[m] <- opencage_latlon$results$geometry.lng[1]
          voter_file$lat[m] <- opencage_latlon$results$geometry.lat[1]
          voter_file$confidence[m] <- opencage_latlon$results$confidence[1]
          voter_file$address[m] <- opencage_latlon$results$formatted[1]
          voter_file$county[m] <- opencage_latlon$results$components.county[1]
          voter_file$county_fips[m] <-
            opencage_latlon$results$annotations.FIPS.county[1]
        },
        error = function(e) {
          print(e)
        }
      )
    }

    return(voter_file)
  }
}
