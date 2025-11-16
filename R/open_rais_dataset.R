#' Read raw RAIS data as an arrow dataset
#'
#' @details
#'
#' Replace a time- and memory-consuming pipeline with a leaner approach. A wrapper around
#'  [arrow::open_delim_dataset()] to automate the first steps of processing RAIS data, such as
#'  choosing the right delimiter and making sure that leading zeroes in identifier columns are not
#'  removed.
#'
#' @param file RAIS dataset in `.txt` format.
#' @param year Fiscal year of the file, necessary to determine column types, which
#'  columns to read, and other time-specific tidying.
#' @param worker_dataset When reading establishment data, set `worker_dataset = FALSE`. Default
#'  option is to read work relationship data (`worker_dataset = TRUE`).
#' @param filename if `NULL`, returns file; otherwise, provide a string for the destination file.
#'  Must end with with ".parquet".
#' @param delim Delimiter type. If `NULL` (the default), semicolon (`;`) will be used.
#' @param \dots Other variables passed on to [arrow::open_delim_dataset()]
#' @import arrow
#' @importFrom dplyr filter left_join pull ungroup
#' @export

open_rais_dataset <- function(file, year, worker_dataset = TRUE, filename, delim = NULL, ...) {

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
  if(worker_dataset) {
    dic <- get0("dic_workers", envir = asNamespace("rrais"))
  } else {
    # data("dic_firms", package = "rrais")
    dic <- get0("dic_firms", envir = asNamespace("rrais"))
  }

  dic <- dic |>
    ungroup() |>
    filter(from <= year & to >= year & !str_detect(skips, as.character(year)))

  coltypes <- dic |>
    select(alias, type) |>
    tibble::deframe() |>
    purrr::imap(
      \(x, idx)
      Field$create(
        name = idx,
        type = if(x == "integer") {
          arrow::int32()
        } else if(idx == "numeric") {
          arrow::float()
        } else {
          arrow::string()
        }
      )
    )

  ## read raw file
  print(paste0("Reading raw file for year ", year))
  arrow::open_delim_dataset(
    sources = file,
    delim = delim,
    read_options = arrow::csv_read_options(encoding = "ISO-8859-1"),
    # convert_options = arrow::csv_convert_options(decimal_point = ","),
    ...
  )
}


