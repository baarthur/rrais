\dontrun{
  library(rrais)

  rais_to_parquet(
    "data/rais-est-2010.txt", 2010, worker_dataset = FALSE, filename = "data/rais-est-2010.parquet"
  )
}
