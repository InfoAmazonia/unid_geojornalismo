# Os dados do censo são puxados da Base dos Dados (https://basedosdados.org/)
# Este script facilita apenas a criação da query para as variáveis pertinentes
# com o filtro dos municípios da Amazônia.
# Os dados são manualmente salvos em data-raw/censo2022

raw <- here::here("2025_06-vulneraveis/data-raw")
tidy <- here::here("2025_06-vulneraveis/data-tidy")

# CENSO

# CD_TIPO		Tipo do Setor Censitário
# 	0	Não especial
# 	1	Favela e Comunidade Urbana
# 	2	Quartel e base militar
# 	3	Alojamento / acampamento
# 	4	Setor com baixo patamar domiciliar
# 	5	Agrupamento indígena
# 	6	Unidade prisional
# 	7	Convento / hospital / ILPI / IACA
# 	8	Agrovila do PA
# 	9	Agrupamento quilombola

depara_tipo <- tibble::tribble(
  ~CD_TIPO, ~tipo_setor,
  "0", "Não especial",
  "1", "Favela e Comunidade Urbana",
  "2", "Quartel e base militar",
  "3", "Alojamento / acampamento",
  "4", "Setor com baixo patamar domiciliar",
  "5", "Agrupamento indígena",
  "6", "Unidade prisional",
  "7", "Convento / hospital / ILPI / IACA",
  "8", "Agrovila do PA",
  "9", "Agrupamento quilombola"
)
readr::write_rds(depara_tipo, here::here(raw, "depara_tipo.rds"))

# V0001		Total de pessoas
# V0002		Total de Domicílios (DPPO + DPPV + DPPUO + DPIO + DCCM + DCSM)
# V0003		Total de Domicílios Particulares (DPPO + DPPV + DPPUO + DPIO)
# V0004		Total de Domicílios Coletivos (DCCM + DCSM)
# V0005		Média de moradores em Domicílios Particulares Ocupados (Total pessoas em Domicílios Particulares Ocupados / DPPO + DPIO)
# V0006		Percentual de Domicílios Particulares Ocupados Imputados (Total DPO imputados / Total DPO)
# V0007		Total de Domicílios Particulares Ocupados (DPPO + DPIO)


dict <- raw |>
  here::here("dicionario_de_dados_agregados_por_setores_censitarios.xlsx") |>
  readxl::read_excel(sheet = 3) |>
  janitor::clean_names()

# IDADE E COR ----

dict_idade_cor <- dict |>
  dplyr::filter(
    stringr::str_detect(descricao, "^[0-9].+Cor ou raça é \\w+$"),
    !stringr::str_detect(variavel, "V013[89]"),
    !stringr::str_detect(variavel, "V0137[789]")
  ) |>
  dplyr::select(3:4)
gerar_query(dict_idade_cor$variavel)


# SEXO E COR ----
dict_sexo_cor <- dict |>
  dplyr::filter(
    stringr::str_detect(
      descricao,
      "^Cor ou raça da pessoa responsável pelo domicílio é \\w+, Sexo da pessoa responsável pelo domicílio é \\w+$"
    )
  ) |>
  dplyr::select(3:4)
gerar_query(dict_sexo_cor$variavel)

# ALFABETIZAÇÃO ----

dict_alfab_idade <- dict |>
  dplyr::filter(
    stringr::str_detect(
      descricao,
      "Pessoas alfabetizadas\\, [0-9]+.+ anos( ou mais)?$"
    )
  ) |>
  dplyr::select(3:4)
gerar_query(dict_alfab_idade$variavel)


# IDADE ----
dict_idade <- dict |>
  dplyr::filter(
    stringr::str_detect(variavel, "V010(3[1-9]|40|41)")
    # stringr::str_detect(variavel, "V006(4[4-9]|5[0-6])")
  )
gerar_query(dict_idade$variavel)

# TIPO ----

dict_tipo <- dict |>
  dplyr::select(3:4) |>
  dplyr::filter(
    stringr::str_detect(
      descricao,
      "Tipo de espécie é .+(?!\\, [A-Z])"
    )
  ) |>
  head(23)
gerar_query(dict_tipo$variavel)


# LIXO ----

dict_lixo <- dict |>
  dplyr::select(3:4) |>
  dplyr::filter(
    stringr::str_detect(
      descricao,
      "^[\\w ]+\\, [Ll]ixo(?!.+[A-Z])"
    )
  )
gerar_query(dict_lixo$variavel)

# ESGOTO ----

dict_esgoto <- dict |>
  dplyr::select(3:4) |>
  dplyr::filter(
    stringr::str_detect(
      descricao,
      "^[\\w ]+\\, Destinação do esgoto(?!.+[A-Z])"
    )
  )
gerar_query(dict_esgoto$variavel)


# Tipo setor ----

am <- sf::read_sf(here::here(raw, "am_legal_shp"))

malha <- here::here(raw, "malha_setor_ibge2022") |>
  fs::dir_ls() |>
  purrr::map(sf::read_sf) |>
  purrr::list_rbind() |>
  dplyr::filter(CD_MUN %in% c(am$cd_geocmu) | NM_MUN == "Manaus") |>
  sf::st_as_sf()
depara_tipo <- readr::read_rds(here::here(tidy, "depara_tipo.rds"))

malha <- malha |>
  tibble::as_tibble() |>
  dplyr::transmute(id_setor_censitario = CD_SETOR, CD_TIPO) |>
  dplyr::left_join(depara_tipo) |>
  dplyr::select(-CD_TIPO)
readr::write_csv(malha, here::here(raw, "censo2022/tipo_setor.csv"))
