#' Auxiliary functions



# date_repair -----------------------------------------------------------------------------------------------------

#' Fix dates to include missing zeroes
#' @param x A `character` vector.
#' @importFrom stringr str_sub str_pad str_replace
#' @details
#' The function will result in a `character` vector in the standard Brazilian "DDMMAAAA" format. It can be easily
#'  converted into a functional date variable, e.g. with `lubridate::dmy(x)`.

date_repair <- function(x) {
  x |>
    map_chr(
      \(date)
      if(nchar(date) == 8) {
        date
      } else {
        if(str_sub(date, -6, -6) == 0 & nchar(date == 7)) {
          str_pad(date, 8, "left", "0")
        } else {
          if(nchar(date) == 6) {
            paste0("0", str_sub(date, 1, 1), "0", str_sub(date, 2, 2), str_sub(date, -4, -1))
          } else {
            paste0(str_sub(date, 1, 2), "0", str_sub(date, -5, -5), str_sub(date, -4, -1))
          }
        }
      }
    )
}



# clean_postcodes -------------------------------------------------------------------------------------------------

#' Clean postcodes
#' @description Standardize Brazilian postal codes (CEP) to 8-digit, all-numeric format.
#' @param x A `vector` or data column of `character` class.
#' @importFrom stringr str_remove str_squish str_pad

clean_postcodes <- function(x) {
  str_remove({{x}}, "-") |> str_squish() |> str_pad(8, "left", 0)
}



# clean_rais_names ------------------------------------------------------------------------------------------------

#' Clean character vectors
#' @description Removes any diacritics present on strings, extra spaces, leading zeroes, and converts them to lowercase.
#' @inheritParams clean_postcodes
#' @importFrom stringi stri_trans_general
#' @importFrom stringr str_conv str_remove_all str_replace_all str_to_lower
#' @details
#' Auxiliary function to `street_sweeper`. This function standardizes street names, which increases the chances of
#'  matching firms addresses. Can also be used with any string function, e.g. worker name.
#'  @export

clean_rais_names <- function(x) {
  str_conv({{x}}, "UTF8") |>
    str_to_lower() |>
    stri_trans_general("latin-ascii") |>
    str_remove_all("^[',\\-\\s]+") |>
    str_remove_all("[(){}\\[\\]\\+\\|\\?\\*\\\\]+") |>
    str_replace_all("\\b0+(\\d+)", "\\1")  # Remove leading zeros
}



# decimal_repair --------------------------------------------------------------------------------------------------

#' Replace decimal mark for numbers read as characters
#' @name decimal_repair
#' @param x A `vector` of soon-to-be numbers.

arrow::register_scalar_function(
  "decimal_repair",
  function(context, x) {str_replace(x, ",", ".") |> as.numeric()},
  in_type = arrow::string(),
  out_type = arrow::float32(),
  auto_convert = TRUE
)
