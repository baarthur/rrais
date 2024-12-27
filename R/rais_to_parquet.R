#' Save raw RAIS data on parquet format
#'
#' @param file RAIS dataset in `.txt` format.
#' @param year Fiscal year of the file, necessary to determine column types, which
#'  columns to read, and other time-specific tidying.
#' @param columns A `vector` of variables to read.
#' @param worker_dataset When reading establishment data, set `worker_dataset = FALSE`. Default
#'  option is to read work relationship data (`worker_dataset = TRUE`).
#' @param filename destination file. must end with ".parquet".
#' @param delim Delimiter type. If `NULL` (the default), semicolon (;) will be used.
#' @param \dots Other variables passed on to `readr::read_delim()`
#' @importFrom arrow write_parquet
#' @importFrom dplyr filter left_join pull
#' @importFrom readr read_delim locale
#' @importFrom tibble tibble
#' @export

rais_to_parquet <- function(file, year, columns = NULL, worker_dataset = TRUE, filename,
                            delim = NULL, ...) {

  ## dealbreakers
  if(missing(file)) {
    stop("File was not provided.")
  }

  if(missing(year)) {
    stop("Year was not determined.")
  }


  ## parameters

  alias <- col_types <- from <- skips <- to <- NULL

  delim <- if(is.null(delim)) {
    if(worker_dataset) {
      ";"
    } else {
      ifelse(year < 2010, "|", ";")
    }
  } else delim


  ## load dictionary and column types

  ### column types
  coltypes <- if(worker_dataset) {
    readRDS(system.file("extdata", "col_types_workers.RDS", package = "rrais"))
  } else {
    readRDS(system.file("extdata", "col_types_firms.RDS", package = "rrais"))
  }

  coltypes <- coltypes |>
    filter(.data$year == !!year) |>
    pull(col_types)

  ### dictionary
  if(worker_dataset) {
    dic <- get0("dic_workers", envir = asNamespace("rrais"))
  } else {
    # data("dic_firms", package = "rrais")
    dic <- get0("dic_firms", envir = asNamespace("rrais"))
  }

  dic <- dic |>
    filter(from <= year & to >= year & !str_detect(skips, as.character(year)))

  ### optional column selector
  columns <- if(is.null(columns)) {
    NULL
  } else {
    columns <- tibble(ano = year, new_name = columns) |>
      left_join(dic, by = "new_name") |>
      na.omit() |>
      pull(alias)
  }


  ## read raw file
  read_delim(
    file = file,
    delim = delim,
    locale = locale(encoding = "ISO-8859-1", decimal_mark = ","),
    col_select = !!columns,
    col_types = coltypes,
    ...
  ) |>
    write_parquet(filename)
}


