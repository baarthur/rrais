#' Read Establishments and Work Relationships from RAIS dataset.
#'
#' @inheritParams rais_to_parquet
#' @param file RAIS dataset, either in `.txt` or `.parquet` format.
#' @param vinculo_ativo Should only workers that were active at the end of the year be selected?
#'  Default is `TRUE`.
#' @param remove_rais_negativa Should only establishments with active employees in that year  ("Rais Negativa") be selected?
#'  Default is `TRUE`.
#' @param fix_dates If `TRUE` (the default), corrupt dates are repaired and converted to POSIXct
#'  format. Also creates `idade` (worker's age) variable in worker dataset for the years in which
#'  it is not included.
#' @param collect If `TRUE`, data is pulled into memory (defaults to `FALSE`). Read the details for
#' more information.
#' @param firm_filter A `vector` of firm CNPJs to use as filter, if using the identified dataset.
#' @param state_filter A numeric or character `vector` of federal units in 2-digit IBGE code to use
#' as filter.
#' @param muni_filter A numeric or character `vector` of IBGE municipality codes to use as filter.
#' If the 7-digit code is supplied, it will be converted to the 6-digit version.
#' @param cep_filter A `vector` of postal codes to use as filter.
#' @param address_filter A `vector` of addresses to use as filter for identified data up to 2010.
#' @param street_filter A `vector` of street names to use as filter for identified data from 2011 onwards.
#' @param \dots Other variables passed on to `readr::read_delim()`
#' @importFrom arrow read_parquet write_parquet
#' @importFrom data.table as.data.table
#' @importFrom dplyr across case_match filter mutate rename select tibble collect
#' @importFrom purrr map_chr
#' @importFrom readr read_delim locale
#' @importFrom stats na.omit
#' @importFrom stringr str_detect str_extract str_remove str_remove_all str_replace_all
#' @importFrom tidyselect any_of everything starts_with where
#' @returns A `data.table`
#'
#' @description
#' `read_rais()` reads raw RAIS data and generates consistent variables for different years.
#'
#' @details
#'  `read_rais()` standardizes variables across years with unified names and column types and
#'    repairs continuous values and dates, making data robust to the various layout changes between
#'    1985 and 2020.
#'
#'    To make the data cleaning and filtering process faster, `read_rais()` uses
#'    `{arrow}`'s `parquet` format. You can either supply raw RAIS .txt files, which will be passed
#'    on to `rais_to_parquet()`, or a `.parquet` file produced by it.
#'
#'    To make the best use of `arrow`'s structure, `read_rais()` does not pull data into memory
#'    unless made explicit by the argument `collect = TRUE`. The final user can then decide
#'    when to load data. More information on data querying with `arrow` can be found at
#'    [this vignette](https://arrow.apache.org/docs/2.0/r/articles/dataset.html).
#'
#'    This standardization enhances data analysis, e.g. by enabling the user to stack different
#'    years in panel data format. The multiple available filters reduces memory use, by getting rid
#'    of unnecessary data before tidying.
#'
#'    Since 2011, RAIS split the address variable into three columns (street name, street number,
#'    and neighborhood). We opted not to unify these columns, since it would imply a moderate level
#'    of arbitrariness into how we split pre-2011 data or unify post-2011 data. If needed, use the
#'    companion `street_sweeper()` function to clean street names following a consistent pattern,
#'    useful for geolocating or tracking firm address changes.
#'
#' @seealso
#'  * [readr::read_delim()] for the arguments passed onto it
#'
#'  * The [Ministry of Labor and Employment (MTE)](https://www.gov.br/trabalho-e-emprego/pt-br)
#'    website for information on RAIS.
#'
#'  * [geobr::lookup_muni()] or the
#'    [Official IBGE list](https://www.ibge.gov.br/explica/codigos-dos-municipios.php) of state
#'    and municipality codes.
#'
#' @example inst/examples/read_rais.R
#'
#' @export



# read_firms ----------------------------------------------------------------------------------

read_rais <- function(file, year, worker_dataset = TRUE, columns = NULL, vinculo_ativo = TRUE,
                      remove_rais_negativa = TRUE, fix_dates = TRUE, collect = FALSE,
                      state_filter = NULL, muni_filter = NULL, cep_filter = NULL,
                      address_filter = NULL, street_filter = NULL, firm_filter = NULL,
                      delim = NULL, ...) {


  addr_tidy <- alias <- cbo_2002 <- cei_vinculado <- cep <- cnae_95_classe <- cnpj_cei <- NULL
  col_types <- cpf <- data_nascimento <- dic_firms <- dic_workers <- endereco <- NULL
  escolaridade <- from <- genero <- horas_contr <- ibge_subsetor <- ind_rais_negativa <- NULL
  mes_desligamento <- municipio <- new_name <- nome_logradouro <- raca_cor <- skips <- NULL
  tempo_emprego <- to <- vinculo_ativo_31_12 <- NULL



  ## dealbreakers and notes
  if(year < 2011 & !is.null(street_filter)) {
    message("Ignoring street_filter since year < 2011. Before that year, use address_filter.")
  }

  if(!is.null(muni_filter)) {
    if(any(nchar(muni_filter) == 7)) {
      muni_filter <- str_sub(muni_filter, 1, 6)
      message("Converting 7-digit IBGE municipality code to 6-digit code version")
    }
    if(any(nchar(muni_filter) != 6)) {
      stop("Municipality filter must either be in 6. or 7-digit format.")
    }
  }

  if(!is.null(state_filter)) {
    if(str_detect(state_filter, "[a-zA-Z]")) {
      stop("State filter must be composed of two numbers, e.g. 31")
    }
    if(any(nchar(state_filter) != 2)) {
      stop("State filter must be in 2-digit code")
    }
  }



  ## standardize variable names

  if(worker_dataset) {
    renamer <- get0("dic_workers", envir = asNamespace("rrais"))
  } else {
    # data("dic_firms", package = "rrais")
    renamer <- get0("dic_firms", envir = asNamespace("rrais"))
  }

  renamer <- renamer |>
    filter(from <= year & to >= year & !str_detect(skips, as.character(year))) |>
    select(new_name, alias)

  ### optional column selector
  columns_raw <- if(is.null(columns)) {
    pull(renamer, alias)
  } else {
    tibble(new_name = columns) |>
      left_join(renamer, by = c("new_name")) |>
      na.omit() |>
      pull(alias)
  }

  renamer <- tibble::deframe(renamer)


  ## load file
  if(stringr::str_detect(file,  "parquet$")) {
    df <- read_parquet(file, as_data_frame = FALSE, col_select = any_of(columns_raw))
  } else {
    tempfile <- tempfile()

    rais_to_parquet(file = file, year = year, columns = columns_raw,
                    worker_dataset = worker_dataset, filename = tempfile, delim = delim, ...)

    df <- read_parquet(tempfile, as_data_frame = FALSE)
  }

  df <- df |>
    dplyr::rename(tidyselect::any_of(renamer))



  ## filters

  if(!worker_dataset & remove_rais_negativa) {
    if("ind_rais_negativa" %in% names(df)) {
      df <- df |>
        filter(ind_rais_negativa == 0)
    } else {
      stop(paste("Can't remove inactive firms since the indicator column is absent from data.",
                 "Tip: if you used the `columns` specification, make sure that you",
                 "included `ind_rais_negativa`."))
    }
  }

  if(worker_dataset & "vinculo_ativo_31_12" %in% names(df)) {
    df <- df |>
      mutate(
        vinculo_ativo_31_12 = ifelse(
          is.integer(vinculo_ativo_31_12)),
        vinculo_ativo_31_12,
        str_remove_all(vinculo_ativo_31_12, "\\s"
        ) |>
          as.integer()
      )
    if(vinculo_ativo) {
      df <- df |>
        filter(vinculo_ativo_31_12 == 1)
    } else {
      stop(paste("Can't select active workers since the indicator column is absent from data.",
                 "Tip: if you used the `columns` specification, make sure that you",
                 "included `vinculo_ativo_31_12`."))
    }
  }

  if(!is.null(firm_filter)) {
    df <- df |>
      inner_join(tibble(cnpj_cei = firm_filter))
  }

  if(!is.null(cep_filter)) {
    df <- df |>
      inner_join(tibble(cep = cep_filter))
  }

  if(!is.null(street_filter)) {
    df <- df |>
      inner_join(tibble(nome_logradouro = street_filter))
  }

  if(!is.null(address_filter)) {
    df <- df |>
      inner_join(tibble(endereco = address_filter))
  }

  if(!is.null(muni_filter)) {
    df <- df |>
      inner_join(tibble(municipio = muni_filter))
  }

    if(!is.null(state_filter)) {
      df <- df |>
        filter(stringr::str_sub(as.character(municipio), 1, 2) %in% state_filter)
  }



  ## tidying

  ### standardize gender
  if("genero" %in% names(df)) {
    if(year %in% 2005:2010) {
      df <- df |>
        mutate(genero = as.integer(case_match(genero, "MASCULINO" ~ 1, "FEMININO" ~ 2,
                                              .default = NA)))
    } else {
      df <- df |>
        mutate(genero = as.integer(str_trim(genero)))
    }
  }


  ### characters to integers, and replace comma by dot.
  df <- df |>
    mutate(year = !!year, .before = everything()) |>
    # collect() |>
    mutate(
      across(starts_with(c("causa_", "cbo_", "dia_", "escolaridade", "genero", "ind_",
                           "idade", "mes_", "municipio", "qtd_", "raca_cor", "tamanho", "tipo_")) &
               where(is.character), ~ str_remove_all(.x, "\\D") |> as.integer()),
      across(starts_with(c("rem_", "ultima_", "salario_", "tempo_e")), decimal_repair)
    )


  ### standardize IBGE industry variable
  if(year < 2011 & "ibge_subsetor" %in% names(df)) {

    rename_ibge <- readRDS(system.file("extdata", "rename_ibge.RDS", package = "rrais")) |>
      select(alias, new_name) |>
      mutate(new_name = as.character(new_name)) |>
      tibble::deframe()

    df <- df |>
      mutate(
        ibge_subsetor = as.integer(ibge_subsetor |> str_remove("\\w[;\\|]") |>
                                     str_replace_all(rename_ibge))
      )
  }


  ### date repair
  if(fix_dates) {
    df <- df |>
      mutate(across(starts_with("data_"), date_repair))

    #### create age variable for years in which it was not included
    if(worker_dataset & !("idade" %in% names(df)) & "data_de_nascimento" %in% names(df)) {
      df <- df |> mutate(idade = year - lubridate::year(data_nascimento))
    }
  }

  if(collect) {
    collect(df)
  } else
    return(df)
}

