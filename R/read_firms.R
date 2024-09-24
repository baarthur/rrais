#' Read Establishments from RAIS dataset.
#' @param file RAIS dataset in `.csv` format.
#' @param firm_filter A `vector` of firm CNPJs to use as filter.
#' @param muni_filter A `vector` of municipalities in 6-digit IBGE code to use as filter.
#' @param cep_filter A `vector` of postal codes to use as filter.
#' @param address_filter A `vector` of addresses to use as filter for identified data up to 2010.
#' @param street_filter A `vector` of street names to use as filter for identified data from 2011 onwards.
#' @param delim Delimiter type. If `NULL` (the default), semicolon (;) will be used.
#' @param year Reserved for future use.
#' @param rais_negativa Should only establishments with active employees in that year  ("Rais Negativa") be selected?
#'  Default is `TRUE`.
#' @param columns A `vector` of variables to be selected.
#' @param \dots Other variables passed on to readr::read_delim
#' @returns A `tibble`
#' @importFrom data.table as.data.table
#' @importFrom dplyr filter pull select rename rename_with case_when mutate across
#' @importFrom purrr map_chr
#' @importFrom readr read_delim locale
#' @importFrom rlang :=
#' @importFrom stringr str_replace str_pad str_extract str_sub
#' @importFrom tidyselect starts_with any_of everything
#' @export



# read_firms ----------------------------------------------------------------------------------

read_firms <- function(file, firm_filter = NULL, muni_filter = NULL, cep_filter = NULL, address_filter = NULL,
                       street_filter = NULL, delim = NULL, year = NULL, rais_negativa = TRUE, columns = NULL, ...) {

  ## parameters
  delim <- if(is.null(delim)) {
    if(is.null(year)) {
      ";"
    } else {
      ifelse(year<2010, "|", ";")
    }
  } else delim

  .data <- ind_rais_negativa <- municipio <- endereco <- nome_logradouro <- cep <- cnpj_cei <- ibge_subsetor <- NULL
  cnae_95_classe <- addr_tidy <- street_type <- street_name <- col_types <- new_name <- alias <- NULL


  ## load dictionary and column types
  types <- if(is.null(year)) {
    NULL
  } else {
    readRDS("inst/extdata/col_types_firms.RDS") %>%
      filter(.data$year == !!year) %>%
      pull(col_types)
  }

  dic <- readRDS("data/dic_firms.rda") %>%
    select(new_name, alias) %>%
    tibble::deframe()

  rename_ibge <- readRDS("inst/extdata/rename_ibge.RDS") %>%
    select(alias, new_name) %>%
    mutate(new_name = as.character(new_name)) %>%
    tibble::deframe()


  ## tidying functions
  # try_pad <- possibly(str_pad, otherwise = NA)
  # try_chr <- possibly(as.character, otherwise = NA)
  # try_num <- possibly(as.numeric, otherwise = NA)
  # try_ext <- possibly(str_extract, otherwise = NA)
  # try_dec_repair <- possibly(decimal_repair)


  ## read raw file
  df <- read_delim(
    file = file,
    delim = delim,
    locale = locale(encoding = "latin1", decimal_mark = ","),
    col_select = !!columns,
    col_types = types,
    ...
  )


  ## fix names
  df <- df %>% rename(any_of(dic))


  ## filters
  df <- as.data.table(df)
  df <- if(rais_negativa) df else df[ind_rais_negativa == 0, ]
  df <- if(is.null(firm_filter)) df else df[cnpj_cei %in% firm_filter, ]
  df <- if(is.null(cep_filter)) df else df[cep %in% cep_filter, ]
  df <- if(is.null(street_filter)) df else df[nome_logradouro %in% street_filter, ]
  df <- if(is.null(address_filter)) df else df[endereco %in% address_filter, ]
  df <- if(is.null(muni_filter)) df else df[municipio %in% muni_filter, ]

#
#   ## filters
#   df <- df[ , cep := clean_postcodes(cep)]
#   df <- if(is.null(cep_filter)) df else df[cep %in% cep_filter, ] ### does not work well!
#
#   df <- df[ , cnpj_cei := try_pad(cnpj_cei, 14, "left", "0")]
#
  ## tidying
  df <- df %>%
    mutate(ano = year, .before = everything()) %>%
    mutate(
      across(
        starts_with(c("cei_", "cep", "cnae_", "cnpj_", "telefone")),
        \(x) stringr::str_remove_all(x, "\\D") %>% as.character()
      ),
      across(
        starts_with(c("qtd_", "tamanho", "municipio")),
        \(x) stringr::str_remove_all(x, "\\D") %>% as.numeric()
      )
    )

  df <- df %>%
      mutate(across(starts_with("data_"), date_repair))

  df <- if(year < 2011) {
    df %>%
      mutate(
        ibge_subsetor = as.integer(str_replace_all(ibge_subsetor, rename_ibge))
      )
  } else {
    df
  }



  ## DEPRECATED: REMOVE ME!
#
# num_cols <- names(df)[grep("^(qtd_|tamanho_)", names(df))]
# chr_cols <- names(df)[grep("^(tipo|cnae|cei_v|ibge_)", names(df))]
# data_cols <- names(df)[grep("^data_", names(df))]
#
# df[
#   , cnpj_cei := try_pad(cnpj_cei, 14, "left", "0")
# ][
#   , (num_cols) := lapply(.SD, try_num), .SDcols = num_cols
# ][
#   , (chr_cols) := lapply(.SD, try_chr), .SDcols = chr_cols
# ][
#   , (data_cols) := lapply(.SD, function(x) try_pad(x, 8, "left", "0")), .SDcols = data_cols
# ][
#   , cnae_95_classe := try_ext(cnae_95_classe, "\\d+")
# ]
}
