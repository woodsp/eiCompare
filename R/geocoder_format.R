#' Pre-processes voter file by checking zipcode, and any special characters or typos within the address.
#'
#'
#' @param voter_file A voter file containing the address of the voter.
#' @param street_number The number attached to the street name. Ex. 1442
#' @param street_name The name of the place in which a voter lives. Ex. Market Street
#' @param city The name of the city that the voter lives in.
#' @param state The state (based on the United States 50 states) that the voter lives in.
#' @param zipcode The United States Postal Service (USPS) postal code.
#'
#' @return The voter file with pre-processed format for each address variable.
#'
#' @export voter_file
#' @importFrom data.table dataframe manipulation
#' @importFrom stringr for character manipulation and pattern matching functions
#'

geocode_format <- function(voter_file = ram,
                           voter_id = "voter_id",
                           street_number = "street_number",
                           street_name = "street_name",
                           city = "city",
                           state = "GA",
                           zipcode = "residence_zipcode") {

  # Convert each variable as a character.
  voter_file[[zipcode]] <- as.character(voter_file[[zipcode]])

  # Create a zipcode_5_digit column in the voter file
  voter_file$zipcode_5_digit <- NA
  voter_file$zipcode_5_digit <- voter_file[[zipcode]]

  # Extract the zipcodes with pattern 9 digit zipcode with a hyphen and make a 5 digit code only.
  hyphen_zip_ids <- grep("-", voter_file[[zipcode]])
  hyphen_zip_df <- data.frame(cbind(voter_id = voter_file[[voter_id]][hyphen_zip_ids], hyphen_zip = voter_file[[zipcode]][hyphen_zip_ids]))
  hyphen_zip_df$new_zipcode <- NA
  hyphen_zip_df$new_zipcode <- sub("-.*", "", hyphen_zip_df[, 2])

  voter_file$zipcode_5_digit[match(hyphen_zip_df$voter_id, voter_file[[voter_id]])] <- hyphen_zip_df$new_zipcode

  # Extract the zipcodes with 9 characters, no hyphen
  nchar9_ids <- which(nchar(voter_file[[zipcode]]) == 9)
  nchar9_zip_df <- data.frame(cbind(voter_id = voter_file[[voter_id]][nchar9_ids], nchar9_zip = voter_file[[zipcode]][nchar9_ids]))
  nchar9_zip_df$new_zipcode <- NA
  nchar9_zip_df$new_zipcode <- gsub(".{4}$", "", nchar9_zip_df$nchar9_zip)

  voter_file$zipcode_5_digit[match(nchar9_zip_df$voter_id, voter_file[[voter_id]])] <- nchar9_zip_df$new_zipcode

  # internal check
  new_voter_file <- voter_file

  return(voter_file)
}
