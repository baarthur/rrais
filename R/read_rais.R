#' Read Establishments and Work Relationships from RAIS dataset.
#'
#' @param file RAIS dataset in `.csv` format.
#' @param worker_dataset When reading establishment data, set `worker_dataset = FALSE`. Default option is to read
#'  work relationship data (`worker_dataset = TRUE`).
#' @param firm_filter A `vector` of firm CNPJs to use as filter, if using the identified dataset.
#' @param muni_filter A `vector` of municipalities in 6-digit IBGE code to use as filter.
#' @param cep_filter A `vector` of postal codes to use as filter.
#' @param address_filter A `vector` of addresses to use as filter for identified data up to 2010.
#' @param street_filter A `vector` of street names to use as filter for identified data from 2011 onwards.
#' @param delim Delimiter type. If `NULL` (the default), semicolon (;) will be used.
#' @param year Reserved for future use.
#' @param remove_rais_negativa Should only establishments with active employees in that year  ("Rais Negativa") be selected?
#'  Default is `TRUE`.
#' @param vinculo_ativo Should only workers that were active at the end of the year be selected?
#'  Default is `TRUE`.
#' @param columns A `vector` of variables to be selected.
#' @param \dots Other variables passed on to `readr::read_delim()`
#' @importFrom data.table as.data.table
#' @importFrom dplyr across case_match filter left_join mutate pull rename rename_with select tibble
#' @importFrom purrr map_chr
#' @importFrom readr read_delim locale
#' @importFrom stats na.omit
#' @importFrom stringr str_detect str_remove str_remove_all str_replace_all
#' @importFrom tidyselect starts_with any_of everything
#' @returns A `data.table`
#'
#' @description
#' `read_rais()` reads raw RAIS data and generates consistent variables for different years.
#'
#' @details
#'  `read_rais()` first uses `readr::read_delim()` to read the .txt RAIS files. Then, it standardizes variables
#'    across years with unified names and column types and repairs continuous values and dates, making data robust to
#'    the various layout changes between 1985 and 2020.
#'
#'    This standardization enhances data analysis, e.g. by enabling the user to stack different years in panel data
#'    format. The multiple available filters reduces memory use, by getting rid of unnecessary data before tidying.
#'
#'    Since 2011, RAIS split the address variable into three columns (street name, street number, and neighborhood).
#'    We opted not to unify these columns, since it would imply a moderate level of arbitrariness into how we split
#'    pre-2011 data or unify post-2011 data. If needed, use the companion `street_sweeper()` function to clean street
#'    names following a consistent pattern, useful for geolocating or tracking firm address changes.
#'
#' @seealso [readr::read_delim()] for the arguments passed on to it and the
#'  \href{https://www.gov.br/trabalho-e-emprego/pt-br}{Ministry of Labor and Employment (MTE)} website for
#'  information on RAIS.
#'
#' @example inst/examples/read_rais.R
#'
#' @export



# read_firms ----------------------------------------------------------------------------------

read_rais <- function(file, year, worker_dataset = TRUE, columns = NULL, remove_rais_negativa = TRUE,
                       vinculo_ativo = TRUE, muni_filter = NULL,  cep_filter = NULL, address_filter = NULL,
                       street_filter = NULL, firm_filter = NULL, delim = NULL, ...) {

  ## dealbreakers
  if(missing(file)) {
    stop("File was not provided.")
  }

  if(missing(year)) {
    stop("Year was not determined.")
  }



  ## parameters
  delim <- if(is.null(delim)) {
    if(worker_dataset) {
      ";"
    } else {
      ifelse(year<2010, "|", ";")
    }
  } else delim

  addr_tidy <- alias <- cbo_2002 <- cei_vinculado <- cep <- cnae_95_classe <- cnpj_cei <- col_types <- NULL
  cpf <- data_nascimento <- dic_firms <- dic_workers <- endereco <- escolaridade <- from <- genero <- NULL
  horas_contr <- ibge_subsetor <- ind_rais_negativa <- mes_desligamento <- municipio <- new_name <- NULL
  nome_logradouro <- raca_cor <- skips <- tempo_emprego <- to <- vinculo_ativo_31_12 <- NULL




  ## load dictionary and column types

  ### column types
  coltypes <- if(worker_dataset) {
    readRDS(system.file("extdata", "col_types_workers.RDS", package = "rrais"))
  } else {
    readRDS(system.file("extdata", "col_types_firms.RDS", package = "rrais"))
  }

  coltypes <- coltypes %>%
    filter(.data$year == !!year) %>%
    pull(col_types)


  ### dictionary
  if(worker_dataset) {
    dic <- get0("dic_workers", envir = asNamespace("rrais"))
  } else {
    # data("dic_firms", package = "rrais")
    dic <- get0("dic_firms", envir = asNamespace("rrais"))
  }

  dic <- dic %>%
    filter(from <= year & to >= year & !str_detect(skips, as.character(year)))


  ### new names
  renamer <- dic %>%
    select(new_name, alias) %>%
    tibble::deframe()


  ### optional column selector
  columns <- if(is.null(columns)) {
    NULL
  } else {
    columns <- tibble(ano = year, new_name = columns) %>%
      left_join(dic) %>%
      na.omit() %>%
      pull(alias)
  }



  ## read raw file
  df <- read_delim(
    file = file,
    delim = delim,
    locale = locale(encoding = "ISO-8859-1", decimal_mark = ","),
    col_select = !!columns,
    col_types = coltypes,
    ...
  )



  ## pre-filtering

  ### standardize variable names
  df <- df %>% rename(any_of(renamer))



  ## filters
  df <- as.data.table(df)

  df <- if(!worker_dataset & remove_rais_negativa) {df[ind_rais_negativa == 0, ]} else df

  if(worker_dataset) {
    df <- df[ , vinculo_ativo_31_12 := as.numeric(vinculo_ativo_31_12)]
    df <- if(vinculo_ativo) df[vinculo_ativo_31_12 == 1, ] else df
  }


  df <- if(is.null(firm_filter)) df else df[cnpj_cei %in% firm_filter, ]
  df <- if(is.null(cep_filter)) df else df[cep %in% cep_filter, ]
  df <- if(is.null(street_filter)) df else df[nome_logradouro %in% street_filter, ]
  df <- if(is.null(address_filter)) df else df[endereco %in% address_filter, ]
  df <- if(is.null(muni_filter)) df else df[municipio %in% muni_filter, ]



  ## tidying

  ### standardize IBGE industry variable
  if(year < 2011 & "ibge_subsetor" %in% colnames(df)) {

    rename_ibge <- readRDS(system.file("extdata", "rename_ibge.RDS", package = "rrais")) %>%
      select(alias, new_name) %>%
      mutate(new_name = as.character(new_name)) %>%
      tibble::deframe()

    df <- df %>%
      mutate(
        ibge_subsetor = ibge_subsetor %>% str_remove("\\w[;\\|]") %>% str_replace_all(rename_ibge) %>% as.integer()
      )
  } else {
    df
  }



  ### standardize gender
  df <- if("genero" %in% colnames(df)) {
    if(year %in% 2005:2010) {
      df %>%
        mutate(genero = as.integer(case_match(genero, "MASCULINO" ~ 1, "FEMININO" ~ 2, .default = NA)))
    } else {
      df %>%
        mutate(genero = as.integer(genero))
    }
  } else {
    df
  }


  ### characters to integers, date repair, and replace comma by dot
  df <- df %>%
    mutate(ano = year, .before = everything()) %>%
    mutate(
      across(starts_with(c("causa_", "cbo_", "dia_", "escolaridade", "genero", "ind_", "idade", "mes_", "municipio",
                           "qtd_", "raca_cor", "tamanho", "tipo_")), \(x) str_remove_all(x, "\\D") %>% as.integer()),
      across(starts_with("data_"), date_repair),
      across(starts_with(c("rem_", "ultima_", "salario_", "tempo_e")), decimal_repair)
    )


  ### create age variable for years in which it was not included
  df <- if(worker_dataset & !("idade" %in% colnames(df)) & "data_de_nascimento" %in% colnames(df)) {
    df %>% mutate(idade = year - lubridate::year(data_nascimento))
  } else {
    df
  }

}
