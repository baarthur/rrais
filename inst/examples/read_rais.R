# read_rais example
\dontrun{
  library(rrais)

  # simplest use: provide file and year

  path <- "path/to/rais_firms.txt"

  df <- read_rais(file = path, year = 2003, worker_dataset = FALSE)


  # filter for a vector of cities in IBGE-6 format
  cities <- c(355030, 310620, 320530)
  df <- read_rais(path, 2013, muni_filter = cities, worker_dataset = FALSE)


  # select a set of columns
  ## data("dic_firms") # optional: lookup variable names
  cols <- c("cbo_94", "cbo_2002", "genero", "rem_med", "vinculo_ativo_31_12")

  df <- read_rais("path/to/rais_worker.txt", 2015, columns = cols, n_max = 10)

}
