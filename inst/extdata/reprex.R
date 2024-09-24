# a minimal reprex

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(purrr)
library(data.table)
devtools::load_all()

path <- Sys.getenv("rais_path")

source("R/read_firms-v2.R")

# read firms ------------------------------------------------------------------------------------------------------



files <- tibble(
  year = c(2003, 2006, 2010, 2016),
  file = c("Estb2003ID.TXT", "Estb2006ID.TXT", "Estb2010ID.TXT", "Estb2016ID.txt")
)

firms <- files %>%
  split(1:nrow(files)) %>%
  map(
    \(x) read_firms(paste0(path, x$file), year = x$year, n_max = 1e4)
  )

df <-  bind_rows(firms)
