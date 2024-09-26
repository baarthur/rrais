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
#' @importFrom stats na.omit
#' @importFrom stringr str_detect str_remove_all str_replace_all
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

  alias <- addr_tidy <- cep <- cnae_95_classe <- cnpj_cei <- col_types <- endereco <- from <- ibge_subsetor <- NULL
  ind_rais_negativa <- municipio <- new_name <- nome_logradouro <- skips <- street_type <- street_name <- to <- NULL


  ## load dictionary and column types

  ### column types
  types <- readRDS("inst/extdata/col_types_workers.RDS") %>%
    filter(.data$year == !!year) %>%
    pull(col_types)

  ### dictionary
  load("data/dic_firms.rda")
  dic_firms <- dic_firms %>%
    filter(from <= year & to >= year & !str_detect(skips, as.character(year)))

  ### new names
  renamer <- dic_firms %>%
    select(new_name, alias) %>%
    tibble::deframe()

  ### optional column selector
  columns <- if(is.null(columns)) {
    NULL
  } else {
    columns <- tibble(ano = year, new_name = columns) %>%
      left_join(dic_firms) %>%
      na.omit() %>%
      pull(alias)
  }



  ## read raw file
  df <- read_delim(
    file = file,
    delim = delim,
    locale = locale(encoding = "ISO-8859-1", decimal_mark = ","),
    col_select = !!columns,
    col_types = types,
    ...
  )



  ## pre-filtering

  ### standardize variable names
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

  ### standardize IBGE industry variable
  if(year < 2011 & "ibge_subsetor" %in% colnames(df)) {

    rename_ibge <- readRDS("inst/extdata/rename_ibge.RDS") %>%
      select(alias, new_name) %>%
      mutate(new_name = as.character(new_name)) %>%
      tibble::deframe()

    df <- df %>%
      mutate(
        ibge_subsetor = as.integer(str_replace_all(ibge_subsetor, rename_ibge))
      )
  } else {
    df
  }

  ### characters to integers, date repair, and replace comma by dot
  df <- df %>%
    mutate(ano = year, .before = everything()) %>%
    mutate(
      across(starts_with(c("qtd_", "tamanho", "municipio", "escolaridade", "genero", "dia_", "mes_")),
             \(x) str_remove_all(x, "\\D") %>% as.numeric()
      ),
      across(starts_with("data_"), date_repair),
      across(starts_with(c("rem_", "ultima_", "salario_", "tempo_e")), decimal_repair)
    )

  }
