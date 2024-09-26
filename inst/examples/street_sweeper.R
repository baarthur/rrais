df <- tibble::tibble(
  addr_full = c("'RUA SANTO ANTÔNIO, 687 APTO 1904 NESTA", "AV. PIRAJU-COMBO, 4159  TERREO",
                "RUA 13 DE MAIO, 7  NESTA", "-RUA VERBO DIVINO, 1830  TERREO",
                ",ALAMEDA VOVOZINHA, 1577  NESTA", "AV ALA ALAMOS, 173", "RUA Z  QUADRA X LOTE +++9",
                "R.DOIS, 15 APTO 1", "AV.21 DE ABRIL 345", "R. MARIA, 16 APTO 15",
                "AVENIDA 21DE ABRIL, 42 BL A AP 1", "R.15, 123", "PRAÇA XV, S/N", "BR-356 SN", "PC TENENTE JOSE, 98")
)

street_sweeper(df, addr_full)

street_sweeper(df, addr_full, split = F)

street_sweeper(df, addr_full, keep_original = F)
