#' Auxiliary functions


# clean_postcodes -------------------------------------------------------------------------------------------------

#' Clean postcodes
#' Standardize Brazilian postal codes (CEP) to 8-digit, all-numeric format.
#' @param x A `vector` or data column of `character` class.
#' @importFrom stringr str_remove str_squish str_pad

clean_postcodes <- function(x) {
  str_remove({{x}}, "-") %>% str_squish() %>% str_pad(8, "left", 0)
}



# clean_rais_names ------------------------------------------------------------------------------------------------

#' Clean character vectors
#' Removes any diacritics present on strings, extra spaces, leading zeroes, and converts them to lowercase.
#' @inheritParams clean_postcodes
#' @importFrom stringi stri_trans_general
#' @importFrom stringr str_to_lower str_remove_all str_replace_all
#' @details
#' Auxiliary function to `street_sweeper`. This function standardizes street names, which increases the chances of
#'  matching firms addresses. Can also be used with any string function, e.g. worker name.
#'  @export

clean_rais_names <- function(x) {
  stringr::str_conv({{x}}, "UTF8") %>%
    stringr::str_to_lower() %>%
    stringi::stri_trans_general("latin-ascii") %>%
    stringr::str_remove_all("^[,\\-\\s]+") %>%
    stringr::str_remove_all("[(){}\\[\\]\\+\\|\\\\]+") %>%
    stringr::str_replace_all("\\b0+(\\d+)", "\\1")  # Remove leading zeros
}



# street_sweeper --------------------------------------------------------------------------------------------------

#' Tidy addresses from RAIS dataset
#' @param df A `data.frame` or any object coercible to it.
#' @param addr `<data-masking>` Column in `df` with addresses.
#' @param sep_type If `TRUE` (default), separates the street type (e.g., "Rua", "Avenida") into a specific column.
#' @param sep_number If `TRUE` (default), separates the address number into a specific column.
#' @param keep_original If `TRUE` (default), original `addr` data is preserved.
#' @details
#' `street_sweeper` sweeps the dirt out of street names, by fixing common typos such as "Ra" instead of "Rua"
#'  and standardizes street types (e.g, "Avenida", "Ave." "Av" all become the same). This is useful for matching
#'  addresses across firms or along time and for geocoding.
#' @export

street_sweeper <- function(df, addr, sep_type = TRUE, sep_number = TRUE, keep_original = T) {

  . <- addr_tidy <- street_name <- street_type <- NULL

  types <- c(
    ## avenida alameda, avenida a(.)v(.)(e), avenida al(.)(a)
    "\\bavenida\\salameda\\b", "\\bavenida\\sa\\.?v\\.?e?\\b", "\\bavenida\\sal\\.?a?\\b",
    ## avenida r(.)(u)(a), alameda al(.)(a), "ala alameda"
    "\\bavenida\\sr\\.?u?a?\\b", "\\balameda al\\.?a?\\b", "\\bala alameda",
    ## ruaa, rua al(.)(a)
    "\\bruaa", "\\brua\\sal\\.?a?\\b",
    ## r al(.)(a), r(.) avenida, rua a(.)v(.)(e)
    "\\br\\.?\\sal\\.?a?\\b", "\\br\\.?\\s\\avenida", "\\br\\.?\\sa\\.?v\\.?e?\\b",
    ## rua, ra(.), r(.)
    "\\brua", "\\bra\\.?\\b", "\\br\\.?\\b",
    ## avenida
    "\\bavenida",
    ## abenida
    "\\babenida",
    ## alameda, alaemda, "alamaeda", "alam(.)"
    "\\balameda", "\\balaemda", "\\balamaeda", "\\balam\\.?\\b",
    ## ala al(.), ala(.), al(.) al(.), al(.)
    "\\bala al\\.?\\b", "\\bala\\.?\\b", "\\bal\\.?\\s?al\\.?\\b", "\\bal\\.?\\b",
    ## a(.)v(.) al(.)
    "\\ba\\.?v\\.?\\sal\\.?\\b",
    ## a(.)v(.)(e)(a)
    "\\ba\\.?v\\.?e?a?\\b",
    ## acesso, beco, calcadao, calcada, caminho
    "\\bacesso\\b", "\\bbeco\\b", "\\bcalcadao", "\\bcalcada\\b", "\\bacesso",
    ## largo, lrg(.), lg(.)
    "\\blargo", "\\blrg\\.?\\b", "\\blg\\.?\\b",
    ## rodovia, rod(.), rd(.), rdv(.)
    "\\brodovia", "\\brod\\.?\\b", "\\brd\\.?\\b", "\\brdv\\.?\\b",
    ## rodovia br(-), rodovia sp(-)
    "\\brodovia\\sbr\\-?\\d+\\b", "\\brodovia\\ssp\\-?\\d+\\b",
    ## rod br(-), rod sp(-),
    "\\brod\\sbr\\-?\\d+\\b", "\\brod\\ssp\\-?\\d+\\b",
    ## rdv br(-), rdv sp(-), rd br(-), rd sp(-), sp(-), br(-),
    "\\brdv\\sbr\\-?\\d+\\b", "\\brdv\\ssp\\-?\\d+\\b", "\\brd\\sbr\\-?\\d+\\b", "\\brd\\ssp\\-?\\d+\\b",
    ## estrada, estr(.), marginal, marg, mrg(.), mg(.)
    "\\bestrada", "\\bestr\\.?", "\\bmarginal", "\\bmarg\\.?\\b", "\\bmrg\\.?\\b", "\\bmg\\.?\\b",
    ## estacao, est(.)
    "\\bestacao", "\\best\\.?\\b",
    ## praca, pca(.), pc(.), prca(.), prc(.)
    "\\bpraca", "\\bpca\\.?\\b", "\\bpc\\.?\\b", "\\bprca\\.?\\b", "\\bprc\\.?\\b",
    ## pat(i)(e)o, ponte, pt(.), passagem
    "\\bpati?e?o", "\\bponte", "\\bpt\\.?\\b", "\\bpassagem",
    ## travessa, trav(.), trv(.), tv(.)
    "\\btravessa", "\\btrav\\.?\\b", "\\btrv\\.?\\b", "\\btv\\.?\\b",
    ## tunel, trevo, tn(.), to(.)
    "\\btunel\\b", "\\btrevo", "\\btn\\.?\\b", "\\bto\\.?\\b",
    ## vereda, vr(.), vrd(.)
    "\\bvereda", "\\bvr\\.?\\b", "\\bvrd\\.?\\b",
    ## viaduto, vdto(.), viad(.), vd(.)
    "\\bviaduto", "\\bvdto\\.?\\b", "\\bviad\\.?\\b", "\\bvd\\.?\\b",
    ## via, va(.), via elevada, ladeira, acesso
    "\\bvia", "\\bva\\.?\\b", "\\bvia\\selevada", "ladeira", "acesso"
    )

  df <- df %>%
    dplyr::mutate(
      addr_tidy = clean_rais_names({{addr}}) %>% stringr::str_replace("ru ada", "rua da")
    )

  df <- if(sep_type) {
    df %>%
      dplyr::mutate(
        street_type = stringr::str_extract(addr_tidy, paste0(types, collapse = "|")),
        street_name = stringr::str_remove(addr_tidy, street_type)
      ) %>%
      dplyr::mutate(
        street_name = dplyr::case_when(
          stringr::str_detect(street_name, "\\b\\d+\\sde\\b") ~
            stringr::str_extract(street_name, "[ ]\\b\\d*\\sde\\s\\w+\\b\\D*"),
          TRUE ~ stringr::str_extract(street_name, "^\\D*")
        )
      )
  } else df

  df <- if(sep_number) {
    if(sep_type) {
      df %>%
        dplyr::mutate(
          street_number = stringr::str_remove(addr_tidy, paste0(street_type, street_name)) %>%
            stringr::str_extract("\\d+")
        )
    } else {
      df %>%
        dplyr::mutate(
          street_number = stringr::str_extract(addr_tidy, "\\d+")
        )
    }
  } else df

  df <- df %>%
    dplyr::mutate(dplyr::across(
      tidyselect::starts_with("street_"),
      \(x) stringr::str_remove(x, ",") %>% stringr::str_squish() %>% clean_rais_names() %>%
        stringr::str_remove_all("\\.") %>% stringr::str_replace_all("\\b0+(\\d+)", "\\1") %>%
        stringr::str_squish()
    )) %>%
    {if(keep_original) {.} else {select(-{{addr}})}}

  return(df)
}



# decimal_repair --------------------------------------------------------------------------------------------------

#' Replace decimal mark for numbers read as characters
#' @param x A `vector` of soon-to-be numbers.

decimal_repair <- function(x) {str_replace(x, ",", ".") %>% as.numeric()}



# date_repair -----------------------------------------------------------------------------------------------------

#' Fix dates to include missing zeroes
#' @param x A `character` vector.
#' @details
#' The function will result in a `character` vector in the standard Brazilian "DDMMAAAA" format. It can be easily
#'  converted into a functional date variable, e.g. with `lubridate::dmy(x)`.

date_repair <- function(x) {
  x %>%
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



# reprex --------------------------------------------------------------------------------------

# library(magrittr)
#
# df <- tibble::tibble(
#   addr_full = c("'RUA SANTO ANTÔNIO, 687 APTO 1904 NESTA", "AV. PIRAJU-COMBO, 4159  TERREO",
#                 "RUA 13 DE MAIO, 7  NESTA", "-RUA VERBO DIVINO, 1830  TERREO",
#                 ",ALAMEDA VOVOZINHA, 1577  NESTA", "AV ALA ALAMOS, 173", "RUA Z  QUADRA X LOTE +++9")
# )
#
# df %>% dplyr::mutate(addr_full = clean_rais_names(addr_full))
#
# df <- street_sweeper(df, addr_full)
# street_sweeper(df, addr_full, sep_type = F)
# street_sweeper(df, addr_full, sep_number = F)
