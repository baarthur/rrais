#' Read Workers from RAIS dataset.
#' @inherit read_firms
#' @param vinculo_ativo Should only workers that were active at the end of the year be selected?
#'  Default is `TRUE`.
#' @param \dots Other variables passed on to readr::read_delim
#' @returns A `data.table`
#' @importFrom data.table as.data.table
#' @importFrom dplyr across case_match filter left_join mutate pull rename rename_with select tibble
#' @importFrom readr read_delim locale
#' @importFrom stringr str_detect str_remove_all str_replace_all
#' @importFrom stats na.omit
#' @importFrom tidyselect starts_with any_of everything
#' @export

# read workers --------------------------------------------------------------------------------

read_workers <- function(file, firm_filter = NULL, muni_filter = NULL, delim = NULL, year = NULL,
                         vinculo_ativo = F, columns = NULL, ...) {

  ## parameters
  delim <- ifelse(is.null(delim), ";", delim)

  addr_tiddy <- alias <- cbo_2002 <- cei_vinculado <- cnpj_cei <- col_types <- cpf <- data_nascimento <- NULL
  escolaridade <- from <- genero <- horas_contr <- ibge_subsetor <- mes_desligamento <- municipio <- NULL
  new_name <- raca_cor <- skips <- street_name <- street_type <- tempo_emprego <- to <- vinculo_ativo_31_12 <- NULL



  ## load dictionary and column types

  ### column types
  types <- readRDS("inst/extdata/col_types_workers.RDS") %>%
    filter(.data$year == !!year) %>%
    pull(col_types)

  ### dictionary
  load("data/dic_firms.rda")
  dic_workers <- dic_workers %>%
    filter(from <= year & to >= year & !str_detect(skips, as.character(year)))

  ### new names
  renamer <- dic_workers %>%
    select(new_name, alias) %>%
    tibble::deframe()

  ### optional column selector
  columns <- if(is.null(columns)) {
    NULL
  } else {
    columns <- tibble(ano = year, new_name = columns) %>%
      left_join(dic_workers) %>%
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

  ### filters
  df <- as.data.table(df)
  df <- df[ , vinculo_ativo_31_12 := as.numeric(vinculo_ativo_31_12)]
  df <- if(vinculo_ativo) df[vinculo_ativo_31_12 == 1, ] else df
  df <- if(is.null(firm_filter)) df else df[cnpj_cei %in% firm_filter, ]
  df <- if(is.null(muni_filter)) df else df[municipio %in% muni_filter, ]



  ## tidying

  ### standardize gender
  df <- if(year %in% 2005:2010) {
    df %>%
      mutate(genero = as.integer(case_match(genero, "MASCULINO" ~ 1, "FEMININO" ~ 2, .default = NA)))
  } else {
    df %>%
      mutate(genero = as.integer(genero))
  }

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
  }

  ### characters to integers, date repair, and replace comma by dot
  df <- df %>%
    mutate(ano = year, .before = everything()) %>%
    mutate(
      across(starts_with(c("causa_", "cbo_", "dia_", "escolaridade", "ind_", "idade", "mes_", "municipio",
                           "raca_cor", "tipo_")), \(x) str_remove_all(x, "\\D") %>% as.integer()),
      across(starts_with("data_"), date_repair),
      across(starts_with(c("rem_", "ultima_", "salario_", "tempo_")), decimal_repair)
    )

  ### create age variable for years in which it was not included
  df <- if(!("idade" %in% colnames(df)) & "data_de_nascimento" %in% colnames(df)) {
    df %>% mutate(idade = year - lubridate::year(data_nascimento))
  } else {
    df
  }

}
