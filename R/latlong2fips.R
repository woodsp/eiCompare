#' Latitude-Longitude Coordinates to FIPS Geocode
#'
#' Converts latitude/longitude coordinates to 15-digit FIPS code. Communicates
#' with FCC API.
#'
#'
#' @param latitude Numeric. Latitude coordinate.
#' @param longitude Numeric. Longitude coordinate.
#' @param number Numeric. Usually part of a loop index counter
#' @return Character string 15-digit FIPS code corresponding to Lat/Long entry
#' @author Loren Collingwood <loren.collingwood@@ucr.edu>
#' @references https://geo.fcc.gov/api/census/block/
#' @examples
#'
#' \dontrun{
#' # EXAMPLE: NOT RUN #
#' # census_block <- list()
#' # num_catch <- rep(NA, nrow(nom_geo))
#'
#' # for (i in 1:nrow(nom_geo)) {
#'
#' #  census_block[[i]] <- latlong2fips(nom_geo$lat[i], nom_geo$lon[i], i)
#' # }
#'
#' # Row Bind the list into a data.frame object #
#' # fips_df <- rbindlist(census_block)
#' }
#'
#' @export latlong2fips
latlong2fips <- function(latitude, longitude, number) {
  cat("Communicating with geo.fcc.gov...\n")
  url <- paste("https://geo.fcc.gov/api/census/block/find?latitude=", latitude, "&longitude=", longitude, "&showall=true&format=json", sep = "")
  url <- sprintf(url, latitude, longitude)
  json <- RJSONIO::fromJSON(url)
  if (length("json$Block$FIPS") == 0 | is.null(json$Block$FIPS)) { # error here
    cat(paste("Probably Bad LAT/LONG Coordinate. Couldn't calculate.\nRow:", number, sep = " "))
    return(data.frame(row_id = number, FIP = NA, stringsAsFactors = F))
  } else {
    if (json$status == "OK") {
      number <- number
    } else {
      number <- "CONVERGE-FAIL"
    }
    return(data.frame(
      row_id = number, FIP = as.character(json$Block["FIPS"]),
      stringsAsFactors = F
    ))
  }
}
