#' Read Workers from RAIS dataset.
#' @inherit read_firms
#' @param vinculo_ativo Should only workers that were active at the end of the year be selected?
#'  Default is `TRUE`.
#' @param \dots Other variables passed on to readr::read_delim
#' @returns A `tibble`
#' @importFrom data.table as.data.table
#' @importFrom dplyr select rename_with case_when mutate across
#' @importFrom janitor clean_names
#' @importFrom purrr map_chr possibly
#' @importFrom readr read_delim locale
#' @importFrom rlang :=
#' @importFrom stringr str_replace str_pad str_extract
#' @importFrom tidyselect starts_with any_of everything
#' @export

# read workers --------------------------------------------------------------------------------

read_workers <- function(file, firm_filter = NULL, muni_filter = NULL, delim = NULL, year = NULL,
                         vinculo_ativo = F, discard = T, discard_other = NULL, col_select = NULL, ...) {

  ## parameters
  delim <- ifelse(is.null(delim), ";", delim)

  .data <- addr_tiddy <- alias <- cbo_2002 <- cei_vinculado <- cnpj_cei <- col_types <- columns <- cpf <- NULL
  data_nascimento <- escolaridade <- genero <- horas_contr <- ibge_subsetor <- mes_desligamento <- NULL
  municipio <- new_name <- raca_cor <- street_name <- street_type <- tempo_emprego <- vinculo_ativo_31_12 <- NULL



  ## load dictionary and column types
  types <- if(is.null(year)) {
    NULL
  } else {
    readRDS("inst/extdata/col_types_workers.RDS") %>%
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
  try_dec_repair <- possibly(decimal_repair)


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

  df <- df[
    , `:=`(vinculo_ativo_31_12 = as.numeric(vinculo_ativo_31_12)) #cnpj_cei = try_pad(cnpj_cei, 14, "left", "0"),
  ]

  df <- if(is.null(firm_filter)) df else df[cnpj_cei %in% firm_filter, ]

  df <- if(vinculo_ativo) df else df[vinculo_ativo_31_12 == 1, ]

  df <- if(is.null(muni_filter)) df else df[municipio %in% muni_filter, ]


  ## tidying
  df <- df %>%
    select(-starts_with("ano_chegada")) %>%
    mutate(
      ano = year,
      across(c(starts_with("tipo"), escolaridade, raca_cor, genero,
               cei_vinculado, cbo_2002, mes_desligamento), as.double),
      across(starts_with("data_"), date_repair),
      cpf = str_pad(cpf, 11, "left", 0),
      tempo_emprego = try_dec_repair(tempo_emprego),
      horas_contr = try_dec_repair(horas_contr),
      across(starts_with("rem_"), try_dec_repair),
      .keep = "unused"
    ) %>%
    suppressWarnings()

  df <- if("idade" %in% colnames(df)) {df} else {
    df %>% mutate(idade = year - lubridate::year(data_nascimento))
  }

  return(df)
}
