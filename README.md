
# rrais

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/baarthur/rrais/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/baarthur/rrais/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`{rrais}` offers a simple framework to clean and standardize data from RAIS, the Brazilian employer-employee dataset. Now you don't need to worry about layout changes across years, numbers incorrectly registered as characters, commas instead of dots, and so on!

Keep in mind that this is an experimental package and please report any bugs!

Note: `{rrais}` does NOT come with any sensible information. Its used is intended for researchers who the Brazilian Ministry of Labor authorized to use identified RAIS data, or using the public available dataset. 



## Installation

You can install the development version of rrais from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("baarthur/rrais")
```

## Example

A minimal example showing how to read and join firm data from different years with different layouts.

``` r
library(rrais)

## set your RAIS path and files
ext_data <- Sys.getenv("rais_path")
firm_files <- c("ESTB2003ID.txt", "ESTB2013ID.txt")
worker_files <- c("sp2003.TXT", "SP2013ID.txt")


# read firms

## lookup variable names and create a vector with the ones we want.
data("dic_firms")
cols_firms <- c("cnae_95", "cnae_20", "cnpj_cei", "municipio", "qtd_vinculos_clt")

## we'll filter only firms for SP city. don't forget to use the old, 6-digit IBGE code!
firms <- read_firms(file = paste0(ext_data, firm_files[2]), year = 2013, 
                    muni_filter = 355030, columns = cols_firms)

firms <- read_firms(file = paste0(ext_data, firm_files[1]), year = 2003, 
                    muni_filter = 355030, columns = cols_firms) %>% 
  dplyr::bind_rows(firms)

## create a list of firms
firm_list <- firms %>% 
  dplyr::distinct(cnpj_cei) %>% 
  dplyr::pull(cnpj_cei)


# read workers

## here we'll also use the `firm_filter` feature to select only workers from the selected firms.
##  since this would take a long time, we'll restrict the search to the first million workers.

## lookup variable names and create a vector with the ones we want.
data("dic_workers")
cols_workers <- c("cnpj_cei", "cbo_94", "cbo_2002", "genero", "rem_med", "vinculo_ativo_31_12")

workers <- read_workers(file = paste0(ext_data, worker_files[2]), year = 2013, columns = cols_workers,
                        n_max = 1e6,
                        firm_filter = firm_list)

workers <- read_workers(file = paste0(ext_data, worker_files[1]), year = 2003, columns = cols_workers,
                        firm_filter = firm_list, 
                        n_max = 1e6) %>% 
  dplyr::bind_rows(workers)
```

