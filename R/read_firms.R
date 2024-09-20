#' Read Establishments from RAIS dataset.
#' @param file RAIS dataset in `.csv` format.
#' @param firm_filter A `vector` of firm CNPJs to use as filter.
#' @param muni_filter A `vector` of municipalities in 6-digit IBGE code to use as filter.
#' @param cep_filter A `vector` of postal codes to use as filter.
#' @param address_filter A `vector` of addresses to use as filter for identified data up to 2010.
#' @param street_filter A `vector` of street names to use as filter for identified data from 2011 onwards.
#' @param delim Delimiter type. If `NULL` (the default), semicolon (;) will be used.
#' @param year Reserved for future use.
#' @param rais_negativa Should only establishments with active employees in that year  ("Rais Negativa") be selected?
#'  Default is `TRUE`.
#' @param discard Discard a pre-selected set of variables? Default is `TRUE`.
#' @param discard_other A `vector` of variables to discard instead of (or in addition to) the default set.
#' @param col_select A `vector` of variables to be selected.
#' @param \dots Other variables passed on to readr::read_delim
#' @returns A `tibble`
#' @importFrom data.table as.data.table
#' @importFrom dplyr select rename_with case_when mutate across
#' @importFrom janitor clean_names
#' @importFrom purrr possibly
#' @importFrom readr read_delim locale
#' @importFrom rlang :=
#' @importFrom stringr str_replace str_pad str_extract
#' @importFrom tidyselect starts_with any_of everything
#' @export

# read_firms ----------------------------------------------------------------------------------

read_firms <- function(file, firm_filter = NULL, muni_filter = NULL, cep_filter = NULL, address_filter = NULL,
                       street_filter = NULL, delim = NULL, year = NULL, rais_negativa = TRUE, discard = T,
                       discard_other = NULL, col_select = NULL, ...) {

  ## parameters
  delim <- ifelse(is.null(delim), ";", delim)

  . <- ind_rais_negativa <- municipio <- endereco <- nome_logradouro <- cep <- cnpj_cei <- NULL
  cnae_95_classe <- addr_tidy <- street_type <- street_name <- NULL

nome_logradouro
  undesired_cols <- c(
    discard_other,
    # 2003-2011
    "EMAIL", "IND PAT", "TELEF CONT", "RAZAO SOCIAL", "RADIC CNPJ", "IND ATIV ANO",
    # 2011 onwards
    "Email Estabelecimento", "Ind Estab Participa PAT", "N\\u00c3\\u00bamero Telefone Empresa", "Raz\\u00c3\\u00a3o Social",
    "CNPJ Raiz", "UF", "Ind Atividade Ano"
  )

  new_names <- c(
    cei_vinc = "cei_vinculado", clas_cnae_95 = "cnae_95_classe", identificad = "cnpj_cei",
    data_abertura = "dt_abert_com", dt_baixa_com = "data_baixa", dt_encer_or = "data_encerramento",
    ind_cei_vinc = "ind_cei_vinculado", ind_rais_neg = "ind_rais_negativa",
    natur_jur = "natureza_juridica",
    tipo_estbl = "tipo_estab", estoque = "qtd_vinculos_ativos", est_clt_out = "qtd_vinculos_clt",
    estoque_esta = "qtd_vinculos_estatutarios", tamestab = "tamanho_estabelecimento",
    tipo_estbl = "tipo_estab", subs_ibge = "ibge_subsetor",
    clas_cnae_20 = "cnae_20_classe", sb_clas_20 = "cnae_20_subclasse",
    nat_juridica = "natureza_juridica",
    cep_estab = "cep", cnae_2_0_classe = "cnae_20_classe", cnae_2_0_subclasse = "cnae_20_subclasse"
  )


  ## tidying functions
  try_pad <- possibly(str_pad, otherwise = NA)
  try_chr <- possibly(as.character, otherwise = NA)
  try_num <- possibly(as.numeric, otherwise = NA)
  try_ext <- possibly(str_extract, otherwise = NA)
  decimal_repair <- function(x) {str_replace(x, ",", ".") %>% as.numeric()}
  try_dec_repair <- possibly(decimal_repair)
  date_repair <- function(x) {str_pad(x, 8, "left", "0") %>% lubridate::dmy()}


  ## read raw file
  df <- if(discard) {
    read_delim(
      file = file,
      delim = delim,
      locale = locale(encoding = "latin1", decimal_mark = ",", date_format = "%d/%m/%y"),
      col_select = -any_of(undesired_cols),
      ...
    )
  } else {
    read_delim(
      file = file,
      delim = delim,
      locale = locale(encoding = "latin1", decimal_mark = ",", date_format = "%d/%m/%y"),
      col_select = any_of(col_select),
      ...
    )
  }

  ## fix names
  df <- df %>%
    clean_names()

  df <- df %>%
    rename_with(
      \(old_name) case_when(old_name %in% names(new_names) ~ new_names[old_name], .default = old_name),
      .cols = everything()
    )

  ## filters
  df <- as.data.table(df)
  df <- if(rais_negativa) df else df[ind_rais_negativa == 0, ]
  df <- if(is.null(firm_filter)) df else df[cnpj_cei %in% firm_filter, ]
  df <- if(is.null(muni_filter)) df else df[municipio %in% muni_filter, ]
  df <- if(is.null(address_filter)) df else df[endereco %in% address_filter, ]
  df <- if(is.null(street_filter)) df else df[nome_logradouro %in% street_filter, ]

  # df <- if(year < 2008 & year != 2006) {
  #   df %>% rename(
  #     cei_vinculado = cei_vinc, cnae_95_classe = clas_cnae_95, cnpj_cei = identificad,
  #     data_abertura = dt_abert_com, data_baixa = dt_baixa_com, data_encerramento = dt_encer_or,
  #     ind_cei_vinculado = ind_cei_vinc, ind_rais_negativa = ind_rais_neg,
  #     natureza_juridica = natur_jur,
  #     tipo_estab = tipo_estbl, qtd_vinculos_ativos = estoque, qtd_vinculos_clt = est_clt_out,
  #     qtd_vinculos_estatutarios = estoque_esta, tamanho_estabelecimento = tamestab,
  #     tipo_estab = tipo_estbl, ibge_subsetor = subs_ibge
  #   )
  # } else {
  #   if(year == 2006) {
  #     df %>% rename(
  #       cei_vinculado = cei_vinc, cnae_95_classe = clas_cnae_95, cnpj_cei = identificad,
  #       data_abertura = dt_abert_com, data_baixa = dt_baixa_com, data_encerramento = dt_encer_or,
  #       ind_cei_vinculado = ind_cei_vinc, ind_rais_negativa = ind_rais_neg,
  #       natureza_juridica = natur_jur,
  #       tipo_estab = tipo_estbl, qtd_vinculos_ativos = estoque, qtd_vinculos_clt = est_clt_out,
  #       qtd_vinculos_estatutarios = estoque_esta, tamanho_estabelecimento = tamestab,
  #       tipo_estab = tipo_estbl, ibge_subsetor = subs_ibge,
  #       cnae_20_classe = clas_cnae_20, cnae_20_subclasse = sb_clas_20
  #     )
  #   } else {
  #     if(year < 2011) {
  #       df %>% rename(
  #         cei_vinculado = cei_vinc, cnae_95_classe = clas_cnae_95, cnpj_cei = identificad,
  #         data_abertura = dt_abert_com, data_baixa = dt_baixa_com, data_encerramento = dt_encer_or,
  #         ind_cei_vinculado = ind_cei_vinc, ind_rais_negativa = ind_rais_neg,
  #         natureza_juridica = nat_juridica,
  #         tipo_estab = tipo_estbl, qtd_vinculos_ativos = estoque, qtd_vinculos_clt = est_clt_out,
  #         qtd_vinculos_estatutarios = estoque_esta, tamanho_estabelecimento = tamestab,
  #         tipo_estab = tipo_estbl, ibge_subsetor = subs_ibge,
  #         cnae_20_classe = clas_cnae_20, cnae_20_subclasse = sb_clas_20
  #       )
  #     } else {
  #       df %>% rename(cep = cep_estab, cnae_20_classe = cnae_2_0_classe, cnae_20_subclasse = cnae_2_0_subclasse)
  #     }
  #   }
  # }

  ## filters
  df <- df[ , cep := clean_postcodes(cep)]
  df <- if(is.null(cep_filter)) df else df[cep %in% cep_filter, ] ### does not work well!

  df <- df[ , cnpj_cei := try_pad(cnpj_cei, 14, "left", "0")]

  ## tidying
  df <- df %>%
    mutate(
      across(starts_with(c("qtd", "tamanho", "cei_v")), try_num),
      across(starts_with(c("tipo","cnae", "ibge_")), try_chr),
      across(starts_with("data"), \(x) try_pad(x, 8, "left", "0"), .names = "{.col}"),
      .keep = "unused"
    ) %>%
    mutate(cnae_95_classe = try_ext(cnae_95_classe, "\\d+"))



  ## DEPRECATED: REMOVE ME!
#
# num_cols <- names(df)[grep("^(qtd_|tamanho_)", names(df))]
# chr_cols <- names(df)[grep("^(tipo|cnae|cei_v|ibge_)", names(df))]
# data_cols <- names(df)[grep("^data_", names(df))]
#
# df[
#   , cnpj_cei := try_pad(cnpj_cei, 14, "left", "0")
# ][
#   , (num_cols) := lapply(.SD, try_num), .SDcols = num_cols
# ][
#   , (chr_cols) := lapply(.SD, try_chr), .SDcols = chr_cols
# ][
#   , (data_cols) := lapply(.SD, function(x) try_pad(x, 8, "left", "0")), .SDcols = data_cols
# ][
#   , cnae_95_classe := try_ext(cnae_95_classe, "\\d+")
# ]
}
