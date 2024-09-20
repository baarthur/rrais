#' Read Workers from RAIS dataset.
#' @inherit read_firms
#' @param vinculo_ativo Should only workers that were active at the end of the year be selected?
#'  Default is `TRUE`.
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

# read workers --------------------------------------------------------------------------------

read_workers <- function(file, firm_filter = NULL, muni_filter = NULL, delim = NULL, year = NULL,
                         vinculo_ativo = F, discard = T, discard_other = NULL, col_select = NULL, ...) {

  ## parameters
  delim <- ifelse(is.null(delim), ";", delim)

  . <- addr_tiddy <- cbo_2002 <- cei_vinculado <- cnpj_cei <- cpf <- data_nascimento <- escolaridade <- NULL
  genero <- horas_contr <- mes_desligamento <- municipio <- raca_cor <- street_name <- street_type <- NULL
  tempo_emprego <- vinculo_ativo_31_12 <- NULL


  undesired_cols <- c(
    discard_other,
    # 2003-2011
    "CLAS CNAE 95", "CAUSA DESLI", "IND ALVARA", "OCUPACAO 94", "TAMESTAB", "IND PAT",
    "PIS", "NUME CTPS", "RADIC CNPJ", "NOME", "DIA DESL", "TIPO ESTB ID", "CLAS CNAE 20", "SB CLAS 20",
    "TP DEFIC", "ANO CHEGADA2", "QT DIAS AFAS",
    "CAUS AFAST 1", "DIA INI AF 1", "MES INI AF 1", "DIA FIM AF 1", "MES FIM AF 1",
    "CAUS AFAST 2", "DIA INI AF 2", "MES INI AF 2", "DIA FIM AF 2", "MES FIM AF 2",
    "CAUS AFAST 3", "DIA INI AF 3", "MES INI AF 3", "DIA FIM AF 3", "MES FIM AF 3",
    # 2011 onwards
    "CNAE 95 Classe", "Motivo Desligamento", "Ind V\\u00c3\\u00adnculo Alvar\\u00c3\\u00a1", "CBO 94 Ocupa\\u00c3\\u00a7\\u00c3\\u00a3o",
    "Tamanho Estabelecimento", "Ind Estab Participa PAT", "N\\u00c3\\u00bamero CTPS", "CNPJ Raiz", "Nome Trabalhador",
    "CNAE 2.0 Classe", "CNAE 2.0 Subclasse", "Tipo Defic", "Qtd Dias Afastamento",
    "Causa Afastamento 1", "Dia Ini AF1", "M\\u00c3\\u00aas Ini AF1", "Dia Fim AF1", "M\\u00c3\\u00aas Fim AF1",
    "Causa Afastamento 2", "Dia Ini AF2", "M\\u00c3\\u00aas Ini AF2", "Dia Fim AF2", "M\\u00c3\\u00aas Fim AF2",
    "Causa Afastamento 3", "Dia Ini AF3", "M\\u00c3\\u00aas Ini AF3", "Dia Fim AF3", "M\\u00c3\\u00aas Fim AF3",
    "Dia de Desligamento", "IBGE Subsetor", "Ano Chegada Brasil", "CEP Estab", "Mun Trab", "Raz\\u00c3\\u00a3o Social",
    "Vl Rem Janeiro CC", "Vl Rem Fevereiro CC", "Vl Rem Mar\\u00c3\\u00a7o CC", "Vl Rem Abril CC", "Vl Rem Maio CC",
    "Vl Rem Junho CC", "Vl Rem Julho CC", "Vl Rem Agosto CC", "Vl Rem Setembro CC", "Vl Rem Outubro CC",
    "Vl Rem Novembro CC", "Ind Trab Intermitente", "Ind Trab Parcial", "Ind Sindical",
    "Vl Rem Janeiro SC", "Vl Rem Fevereiro SC", "Vl Rem Mar\\u00c3\\u00a7o SC", "Vl Rem Abril SC", "Vl Rem Maio SC",
    "Vl Rem Junho SC", "Vl Rem Julho SC", "Vl Rem Agosto SC", "Vl Rem Setembro SC", "Vl Rem Outubro SC",
    "Vl Rem Novembro SC"
  )

  new_names <- c(
    emp_em_31_12 = "vinculo_ativo_31_12", tp_vinculo = "tipo_vinculo", mes_deslig = "mes_desligamento",
    tipo_adm = "tipo_admissao", tipo_sal = "tipo_salario", nacionalidad = "nacionalidade",
    sexo = "genero", sexo_trabalhador = "genero",
    grau_instr = "escolaridade", gr_instrucao = "escolaridade", escolaridade_apos_2005 = "escolaridade",
    port_defic = "pcd", ind_portador_defic = "pcd",
    natur_jur = "natureza_juridica", nat_juridica = "natureza_juridica",
    ind_cei_vinc = "ind_cei_vinculado", tipo_estbl = "tipo_estab",
    dt_admissao = "data_admissao", data_admissao_declarada = "data_admissao",
    rem_med_r = "rem_med", vl_remun_media_nom = "rem_med",
    rem_media = "rem_med_fx", vl_remun_media_sm = "rem_med_fx",
    rem_dez_r = "rem_dez", vl_remun_dezembro_nom = "rem_dez",
    rem_dezembro = "rem_dez_fx", vl_remun_dezembro_sm = "rem_dez_fx",
    temp_empr = "tempo_emprego", qtd_hora_contr = "horas_contr",
    ult_rem = "rem_ult", vl_ultima_remuneracao_ano = "rem_ult",
    sal_contr = "salario_contr", vl_salario_contratual = "salario_contr",
    dt_nasciment = "data_nascimento", data_de_nascimento = "data_nascimento",
    cei_vinc = "cei_vinculado", identificad = "cnpj_cei",
    ocup_2002 = "cbo_2002", cbo_ocupacao_2002 = "cbo_2002"
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
  df <- df %>% clean_names()

  df <- df %>%
    rename_with(
      \(old_name) case_when(old_name %in% names(new_names) ~ new_names[old_name], .default = old_name),
      .cols = everything()
    )


  ## filters
  df <- as.data.table(df)[
    , `:=`(cnpj_cei = try_pad(cnpj_cei, 14, "left", "0"),
           vinculo_ativo_31_12 = as.numeric(vinculo_ativo_31_12))
  ]

  df <- if(is.null(firm_filter)) df else df[cnpj_cei %in% firm_filter, ]

  df <- if(vinculo_ativo) df else df[vinculo_ativo_31_12 == 1, ]

  df <- if(is.null(muni_filter)) df else df[municipio %in% muni_filter, ]


  ## tidying
  df <- df %>%
    select(-starts_with("ano_chegada")) %>%
    mutate(
      ano = year,
      across(c(starts_with("tipo"), escolaridade, raca_cor, genero,
               cei_vinculado, cbo_2002, mes_desligamento), as.double),
      across(starts_with("data_"), date_repair),
      cpf = str_pad(cpf, 11, "left", 0),
      tempo_emprego = try_dec_repair(tempo_emprego),
      horas_contr = try_dec_repair(horas_contr),
      across(starts_with("rem_"), try_dec_repair),
      .keep = "unused"
    ) %>%
    suppressWarnings()

  df <- if("idade" %in% colnames(df)) {df} else {
    df %>% mutate(idade = year - lubridate::year(data_nascimento))
  }

  return(df)
}
