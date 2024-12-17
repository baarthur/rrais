# read_rais example
\dontrun{
  library(rrais)

  # simplest use: provide raw file and year
  path <- "path/to/rais_firms.txt"

  df <- read_rais(file = path, year = 2003, worker_dataset = FALSE)

  # using a ".parquet" file
  df <- read_rais("path/to/rais_workers.parquet", year = 2019)

  # filter a vector of cities
  cities <- c(355030, 310620, 320530)
  df <- read_rais(path, 2013, muni_filter = cities, worker_dataset = FALSE)

  # select a set of columns
  # data("dic_firms") # optional: lookup variable names
  cols <- c("cbo_94", "cbo_2002", "genero", "rem_med", "vinculo_ativo_31_12")

  df <- read_rais("path/to/rais_worker.txt", 2015, columns = cols, n_max = 10)

  # to load data directly into column, simply determine `collect = TRUE`
  df <- read_rais(path, year = 2014, collect = TRUE)

}
