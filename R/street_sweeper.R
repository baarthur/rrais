#' Tidy addresses from RAIS dataset
#' @param df A `data.frame` or any object coercible to it.
#' @param addr `<data-masking>` Column in `df` with addresses.
#' @param seplit If `TRUE` (default), separates the street type (e.g., "Rua", "Avenida"), name, number, and "other"
#'  (floor, unit etc) into specific columns.
#' @param keep_original If `TRUE` (default), original `addr` data is preserved.
#' @importFrom dplyr mutate
#' @importFrom stringr str_extract str_remove str_replace str_replace_all str_trim
#' @importFrom tidyr replace_na
#' @details
#' `street_sweeper` sweeps the dirt out of street names, by fixing common typos such as "Ra" instead of "Rua"
#'  and standardizes street types (e.g, "Avenida", "Ave." "Av" all become the same). This is useful for matching
#'  addresses across firms or along time and for geocoding.
#'  @example inst/examples/street_sweeper.R
#' @export

# street_sweeper --------------------------------------------------------------------------------------------------

street_sweeper <- function(df, addr, split = TRUE, keep_original = TRUE) {

  addr_name <- addr_number <- addr_other <- addr_type <- name_number <- nocaps <- NULL

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


  ## common preposition typos
  # prepo <- c(`ru ada` = "rua da", `ru ado` = "rua do")


  ## numeric street date names to words
  dates <- c(primeiro = "1", dois = 2, tres = 3, quatro = 4, cinco = 5, seis = 6, sete = 7, oito = 8, nove = 9,
             dez = 10, onze = 11, doze = 12, treze = 13, catorze = 14, quinze = 15, dezesseis = 16, dezessete = 17,
             dezoito = 18, dezenove = 19, vinte = 20, `vinte e um` = 21, `vinte e dois` = 22, `vinte e tres` = 23,
             `vinte e quatro` = 24, `vinte e cinco` = 25, `vinte e seis` = 26, `vinte e sete` = 27,
             `vinte e oito` = 28, `vinte e nove` = 29, `trinta` = 30, `trinta e um` = 31)

  df <- df %>%
    mutate(
      nocaps = clean_rais_names({{addr}}) %>%
        # str_replace(prepo) %>%
        str_replace_all("\\.(\\S)", "\\. \\1"),
      addr_type = str_extract(nocaps, paste0(types, collapse = "|")),
      name_number = str_remove(nocaps, paste0(addr_type, "?\\.?\\s"))
    )

  df <- df %>%
    mutate(
      addr_name = str_extract(name_number, "^[\\d?\\-\\w]+.*?(\\d|(s[?/?\\.n]))") %>%
        str_remove("(\\d|(s[?/?\\.n]))$"),
      addr_number = str_remove(name_number, addr_name),
      addr_other = str_replace(addr_number, "^(\\d+|(s[?/?\\.]?n))", "") %>% str_trim()
    )

  df <- df %>%
    mutate(
      addr_other = ifelse(addr_other == "", NA, addr_other),
      addr_name = str_trim(addr_name) %>% str_remove(",|\\."),
      addr_number = ifelse(is.na(addr_other), addr_number, str_remove(addr_number, addr_other)) %>% str_trim()
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
