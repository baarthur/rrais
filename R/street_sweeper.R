#' Tidy addresses from RAIS dataset
#'
#' @param df A `data.frame` or any object coercible to it.
#' @param addr `<data-masking>` Column in `df` with addresses.
#' @param split If `TRUE` (default), separates the street type (e.g., "Rua", "Avenida"), name, number, and "other"
#'  (floor, unit etc) into specific columns.
#' @param keep_original If `TRUE` (default), original `addr` data is preserved.
#' @importFrom dplyr mutate case_when
#' @importFrom stringr str_extract str_remove str_replace str_replace_all str_trim
#' @importFrom tidyr replace_na
#'
#' @description
#' `street_sweeper()` sweeps the dirt out of street names, removing misplaced characters and breaking data into
#' useful chunks.
#'
#' @details
#' `street_sweeper` uses a predetermined set of street types, including common typos (such as Ra instead of Rua), to
#'  separate `street_type` from the full address.
#'  This is useful for matching
#'  addresses across firms or along time and for geocoding.
#'
#' @example inst/examples/street_sweeper.R
#' @export

# street_sweeper --------------------------------------------------------------------------------------------------

street_sweeper <- function(df, addr, split = TRUE, keep_original = TRUE) {

  addr_full <- addr_name <- addr_number <- addr_other <- addr_type <- alias <- name_number <- NULL
  nocaps <- true_name <- NULL

  typos <- read_delim(system.file("extdata", "addr_typos.csv", package = "rrais"))

  df <- df %>%
    mutate(nocaps = clean_rais_names({{addr}})) %>%
    mutate(
      alias = str_extract(nocaps, paste0(paste0("\\b", typos$alias, "\\b"), collapse = "|")) %>%
        str_trim()
    ) %>%
    left_join(typos, by = "alias") %>%
    select(-alias) %>%
    mutate(addr_type = true_name, .keep = "unused")

  df <- df %>%
    mutate(
      nocaps = clean_rais_names({{addr}}) %>% str_replace_all("\\.(\\S)", "\\. \\1"),
      addr_type = str_extract(nocaps, paste0(paste0("\\b", typos$alias, "\\b"),
                                             collapse = "|")) %>% str_trim(),
      name_number = str_remove(nocaps, paste0(addr_type, "?\\.?\\s"))
    ) %>%
    left_join(typos, by = c("addr_type" = "alias")) %>%
    mutate(
      addr_type = true_name, .keep = "unused"
    )

  df <- df %>%
    mutate(
      addr_name = case_when(
        str_detect(name_number, "^[\\d?\\-\\w]+.*?(\\d|(s[?/?\\.n]))") ~
          str_extract(name_number, "^[\\d?\\-\\w]+.*?(\\d|(s[?/?\\.n]))") %>%
          str_remove("(\\d|(s[?/?\\.n]))$"),
        .default = name_number
      ),
      addr_number = str_remove(name_number, addr_name),
      addr_other = str_replace(addr_number, "^(\\d+|(s[?/?\\.]?n))", "") %>% str_trim()
    )


  df <- df %>%
    mutate(
      addr_number = ifelse(addr_number == "", NA, addr_number),
      addr_other = ifelse(addr_other == "", NA, addr_other),
      addr_name = str_trim(addr_name) %>% str_remove(",|\\."),
      addr_number = ifelse(is.na(addr_other), addr_number, str_remove(addr_number, addr_other)) %>%
        str_trim()
    )

  df <- df %>%
    mutate(
      addr_tidy = paste0(replace_na(addr_type, ""), " ", replace_na(addr_name, ""),
                         ", ", replace_na(addr_number, "")) %>%
        str_trim()
    )

  if(split == FALSE) {
    df <- df %>% select(-c(addr_type, addr_name, addr_number, addr_other))
  }

  if(keep_original == FALSE) {
    df <- df %>% select(-{{addr}})
  }

  df <- df %>% select(-c(nocaps, name_number))

  return(df)
}
