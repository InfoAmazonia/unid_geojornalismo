dd <- here::here("202607_saneamento_tis/dados")

# ibge <- dd |>
#   here::here("tabela9849.csv") |>
#   readr::read_csv(skip = 3)

# # tidy
# ibge <- ibge |>
#   janitor::clean_names() |>
#   dplyr::select(-nivel) |>
#   purrr::set_names(
#     "cod", "ti", "esgoto", "lixo", "abastecimento", "canalizacao", "variavel",
#     "total"
#   ) |>
#   tidyr::fill(cod, ti, esgoto, lixo, abastecimento, canalizacao) |>
#   dplyr::filter(total != "-")


# De-para códigos TIs IBGE e FUNAI
depara <- dd |>
  here::here("raw") |>
  fs::dir_ls(regexp = "Tabela") |>
  readxl::read_excel(skip = 3) |>
  janitor::clean_names() |>
  dplyr::filter(!is.na(codigo_da_terra_indigena)) |>
  dplyr::select(3, 4, 8) |>
  purrr::set_names("cod", "code_indigenous_land", "total_pessoas") |>
  dplyr::mutate(total_pessoas = as.numeric(ifelse(total_pessoas == "-", "0", total_pessoas)))

amazonia <- geobr::read_amazon(2024)
tis_geral <- geobr::read_indigenous_land(2025)
tis <- tis_geral |>
  sf::st_intersection(amazonia) |>
  dplyr::filter_out(abbrev_state == "MS")
tis |>
  dplyr::left_join(depara, dplyr::join_by(code_indigenous_land))
tbl_tis <- tis |>
  tibble::as_tibble() |>
  dplyr::select(-geometry)

# UF
# Considera TIs em estados da Amazônia Legal para dados do IBGE
# ibge <- ibge |>
#   dplyr::select(-ti) |>
#   dplyr::mutate(cod = as.numeric(cod)) |>
#   dplyr::left_join(depara, dplyr::join_by(cod)) |>
#   dplyr::inner_join(tbl_tis, dplyr::join_by(code_indigenous_land))


# Abastecimento ----
abastecimento <- dd |>
  here::here("raw/abastecimento.csv") |>
  readr::read_csv(skip = 1) |>
  janitor::clean_names() |>
  dplyr::filter(
    cod != "0",
    !stringr::str_detect(cod, "[A-Za-z-\\.]")
  ) |>
  dplyr::transmute(
    cod = as.numeric(cod), terra_indigena_por_unidade_da_federacao,
    agua = principal_forma_de_abastecimento_de_agua,
    variavel, n = x10
  ) |>
  dplyr::mutate(
    n = as.numeric(ifelse(n == "-", "0", n)),
    variavel = dplyr::case_when(
      stringr::str_detect(variavel, "Domicílios") ~ "domicilios",
      stringr::str_detect(variavel, "Moradores") ~ "moradores"
    )
  ) |>
  dplyr::filter(!is.na(n)) |>
  dplyr::left_join(depara) |>
  dplyr::mutate(code_indigenous_land = dplyr::if_else(
    code_indigenous_land == 39401, 39402, code_indigenous_land
  )) |>
  dplyr::left_join(tbl_tis) |>
  dplyr::filter(!is.na(name_indigenous_land))
readr::write_csv(abastecimento, here::here(dd, "tidy/abastecimento.csv"))

abastecimento |>
  dplyr::summarise(.by = c(agua, variavel), n = sum(n)) |>
  tidyr::pivot_wider(names_from = variavel, values_from = n) |>
  dplyr::arrange(dplyr::desc(moradores)) |>
  dplyr::mutate(
    p_dom = scales::percent(domicilios /sum(domicilios), accuracy = 0.1),
    p_mor = scales::percent(moradores / sum(moradores), accuracy = 0.1)
  )

# Lixo ----

lixo <- dd |>
  here::here("raw/lixo.csv") |>
  readr::read_csv(skip = 1) |>
  janitor::clean_names() |>
  dplyr::filter(
    cod != "0",
    !stringr::str_detect(cod, "[A-Za-z-\\.]")
  ) |>
  dplyr::transmute(
    cod = as.numeric(cod), terra_indigena_por_unidade_da_federacao,
    lixo = destino_do_lixo, variavel, n = x10
  ) |>
  dplyr::mutate(
    n = as.numeric(ifelse(n == "-", "0", n)),
    variavel = dplyr::case_when(
      stringr::str_detect(variavel, "Domicílios") ~ "domicilios",
      stringr::str_detect(variavel, "Moradores") ~ "moradores"
    )
  ) |>
  dplyr::filter(!is.na(n)) |>
  dplyr::left_join(depara) |>
  dplyr::mutate(code_indigenous_land = dplyr::if_else(
    code_indigenous_land == 39401, 39402, code_indigenous_land
  )) |>
  dplyr::left_join(tbl_tis) |>
  dplyr::filter(!is.na(name_indigenous_land))
readr::write_csv(lixo, here::here(dd, "tidy/lixo.csv"))

# Esgoto ----

esgoto <- dd |>
  here::here("raw/esgoto.csv") |>
  readr::read_csv(skip = 1) |>
  janitor::clean_names() |>
  dplyr::filter(
    cod != "0",
    !stringr::str_detect(cod, "[A-Za-z-\\.]")
  ) |>
  dplyr::transmute(
    cod = as.numeric(cod), terra_indigena_por_unidade_da_federacao,
    esgoto = tipo_de_esgotamento_sanitario, variavel, n = x10
  ) |>
  dplyr::mutate(
    n = as.numeric(ifelse(n == "-", "0", n)),
    variavel = dplyr::case_when(
      stringr::str_detect(variavel, "Domicílios") ~ "domicilios",
      stringr::str_detect(variavel, "Moradores") ~ "moradores"
    )
  ) |>
  dplyr::filter(!is.na(n)) |>
  dplyr::left_join(depara) |>
  dplyr::mutate(code_indigenous_land = dplyr::if_else(
    code_indigenous_land == 39401, 39402, code_indigenous_land
  )) |>
  dplyr::left_join(tbl_tis) |>
  dplyr::filter(!is.na(name_indigenous_land))
readr::write_csv(esgoto, here::here(dd, "tidy/esgoto.csv"))

# Água canalizada ----

agua <- dd |>
  here::here("raw/agua.csv") |>
  readr::read_csv(skip = 1) |>
  janitor::clean_names() |>
  dplyr::filter(
    cod != "0",
    !stringr::str_detect(cod, "[A-Za-z-\\.]")
  ) |>
  dplyr::transmute(
    cod = as.numeric(cod), terra_indigena_por_unidade_da_federacao,
    agua = existencia_de_canalizacao_de_agua, variavel,
    n = x10
  ) |>
  dplyr::mutate(
    n = as.numeric(ifelse(n == "-", "0", n)),
    variavel = dplyr::case_when(
      stringr::str_detect(variavel, "Domicílios") ~ "domicilios",
      stringr::str_detect(variavel, "Moradores") ~ "moradores"
    )
  ) |>
  dplyr::filter(!is.na(n)) |>
  dplyr::left_join(depara) |>
  dplyr::mutate(code_indigenous_land = dplyr::if_else(
    code_indigenous_land == 39401, 39402, code_indigenous_land
  )) |>
  dplyr::left_join(tbl_tis) |>
  dplyr::filter(!is.na(name_indigenous_land))
readr::write_csv(agua, here::here(dd, "tidy/agua.csv"))
