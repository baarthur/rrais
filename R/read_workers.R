#' Read Workers from RAIS dataset.
#' @inherit read_firms
#' @param vinculo_ativo Should only workers that were active at the end of the year be selected?
#'  Default is `TRUE`.
#' @param \dots Other variables passed on to readr::read_delim
#' @returns A `data.table`
#' @importFrom data.table as.data.table
#' @importFrom dplyr across case_match filter left_join mutate pull rename rename_with select tibble
#' @importFrom readr read_delim locale
#' @importFrom rlang :=
#' @importFrom stringr str_detect
#' @importFrom stats na.omit
#' @importFrom tidyselect starts_with any_of everything
#' @export

# read workers --------------------------------------------------------------------------------

read_workers <- function(file, firm_filter = NULL, muni_filter = NULL, delim = NULL, year = NULL,
                         vinculo_ativo = F, columns = NULL, ...) {

  ## parameters
  delim <- ifelse(is.null(delim), ";", delim)

  .data <- addr_tiddy <- alias <- cbo_2002 <- cei_vinculado <- cnpj_cei <- col_types <- cpf <- NULL
  data_nascimento <- escolaridade <- from <- genero <- horas_contr <- ibge_subsetor <- mes_desligamento <- NULL
  municipio <- new_name <- raca_cor <- skips <- street_name <- street_type <- tempo_emprego <- to <- vinculo_ativo_31_12 <- NULL


  ## load dictionary and column types
  types <- readRDS("inst/extdata/col_types_workers.RDS") %>%
    filter(.data$year == !!year) %>%
    pull(col_types)

  load("data/dic_firms.rda")

  dic_workers <- dic_workers %>%
    filter(from <= year & to >= year & !str_detect(skips, as.character(year)))

  renamer <- dic_workers %>%
    select(new_name, alias) %>%
    tibble::deframe()

  columns <- if(is.null(columns)) {
    NULL
  } else {
    columns <- tibble(ano = year, new_name = columns) %>%
      left_join(dic_workers) %>%
      na.omit() %>%
      pull(alias)
  }

  rename_ibge <- readRDS("inst/extdata/rename_ibge.RDS") %>%
    select(alias, new_name) %>%
    mutate(new_name = as.character(new_name)) %>%
    tibble::deframe()


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

df <- df[
  , vinculo_ativo_31_12 := as.numeric(vinculo_ativo_31_12)
]

  df <- if(is.null(firm_filter)) df else df[cnpj_cei %in% firm_filter, ]

  df <- if(vinculo_ativo) df[vinculo_ativo_31_12 == 1, ] else df

  df <- if(is.null(muni_filter)) df else df[municipio %in% muni_filter, ]


  ## tidying
  df <- if(year %in% 2005:2010) {
    df %>%
      mutate(genero = as.integer(case_match(genero, "MASCULINO" ~ 1, "FEMININO" ~ 2, .default = NA)))
  } else {
    df %>%
      mutate(genero = as.integer(genero))
  }

  df <- df %>%
    mutate(ano = year, .before = everything()) %>%
    mutate(
      # across(
        # starts_with(c("cep", "cnae_")),
        # \(x) stringr::str_remove_all(x, "\\D") %>% as.character()
      # ),
      across(
        starts_with(c("municipio", "tipo_", "escolaridade", "raca_cor",
                      "ind", "mes", "dia", "cbo_", "causa_")),
        \(x) stringr::str_remove_all(x, "\\D") %>% as.integer()
      ),
      across(starts_with("data_"), date_repair),
      across(starts_with(c("rem_", "ultima_", "salario_", "tempo_e")), decimal_repair)
    )

  df <- if(!("idade" %in% colnames(df)) & "data_de_nascimento" %in% colnames(df)) {
    df %>% mutate(idade = year - lubridate::year(data_nascimento))
  } else {
    df
  }
#   df <- df %>%
#     select(-starts_with("ano_chegada")) %>%
#     mutate(
#       ano = year,
#       across(c(starts_with("tipo"), escolaridade, raca_cor, genero,
#                cei_vinculado, cbo_2002, mes_desligamento), as.double),
#       across(starts_with("data_"), date_repair),
#       cpf = str_pad(cpf, 11, "left", 0),
#       tempo_emprego = try_dec_repair(tempo_emprego),
#       horas_contr = try_dec_repair(horas_contr),
#       across(starts_with("rem_"), try_dec_repair),
#       .keep = "unused"
#     ) %>%
#     suppressWarnings()
#
#   df <- if("idade" %in% colnames(df)) {df} else {
#     df %>% mutate(idade = year - lubridate::year(data_nascimento))
#   }
#
#   return(df)
}
