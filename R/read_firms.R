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
#' @returns A `data.table`
#' @importFrom data.table as.data.table
#' @importFrom dplyr across case_match filter left_join mutate pull rename rename_with select tibble
#' @importFrom purrr map_chr
#' @importFrom readr read_delim locale
#' @importFrom rlang :=
#' @importFrom stats na.omit
#' @importFrom stringr str_detect str_replace
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

  .data <- alias <- addr_tidy <- cep <- cnae_95_classe <- cnpj_cei <- col_types <- endereco <- from <- NULL
  ibge_subsetor <- ind_rais_negativa <- municipio <- new_name <- nome_logradouro <- rename_ibge <- NULL
  skips <- street_type <- street_name <- to <- NULL


  ## load dictionary and column types
  types <- if(is.null(year)) {
    NULL
  } else {
    readRDS("inst/extdata/col_types_firms.RDS") %>%
      filter(.data$year == !!year) %>%
      pull(col_types)
  }

  load("data/dic_firms.rda")

  dic_firms <- dic_firms %>%
    filter(from <= year & to >= year & !str_detect(skips, as.character(year)))

  renamer <- dic_firms %>%
    select(new_name, alias) %>%
    tibble::deframe()

  columns <- if(is.null(columns)) {
    NULL
  } else {
    columns <- tibble(ano = year, new_name = columns) %>%
      left_join(dic_firms) %>%
      na.omit() %>%
      pull(alias)
  }


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
  df <- df %>% rename(any_of(renamer))


  ## filters
  df <- as.data.table(df)
  df <- if(rais_negativa) df else df[ind_rais_negativa == 0, ]
  df <- if(is.null(firm_filter)) df else df[cnpj_cei %in% firm_filter, ]
  df <- if(is.null(cep_filter)) df else df[cep %in% cep_filter, ]
  df <- if(is.null(street_filter)) df else df[nome_logradouro %in% street_filter, ]
  df <- if(is.null(address_filter)) df else df[endereco %in% address_filter, ]
  df <- if(is.null(muni_filter)) df else df[municipio %in% muni_filter, ]


  ## tidying
  df <- df %>%
    mutate(ano = year, .before = everything()) %>%
    mutate(
      # across(
        # starts_with(c("cep", "cnae_", "telefone")),
        # \(x) stringr::str_remove_all(x, "\\D") %>% as.character()
      # ),
      across(
        starts_with(c("qtd_", "tamanho", "municipio", "escolaridade", "genero", "dia_", "mes_")),
        \(x) stringr::str_remove_all(x, "\\D") %>% as.numeric()
      )
    )

  df <- df %>%
      mutate(across(starts_with("data_"), date_repair))

  df <- df %>%
    mutate(across(starts_with(c("rem_", "ultima_", "salario_", "tempo_e")), decimal_repair))

  df <- if(year < 2011 & "ibge_subsetor" %in% colnames(df)) {
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
