# TIs com de-para IBGE

amazonia <- geobr::read_amazon(2024)
tis_geral <- geobr::read_indigenous_land(2025)
tis <- tis_geral |>
  sf::st_intersection(amazonia) |>
  dplyr::filter_out(abbrev_state == "MS")


# Abastecimento
abastecimento <- dd |>
  here::here("tidy/abastecimento.csv") |>
  readr::read_csv() |>
  dplyr::select(cod, code_indigenous_land, agua:n) |>
  dplyr::rename(abastecimento = agua)
tis |>
  dplyr::left_join(abastecimento) |>
  sf::write_sf(here::here(dd, "shp/abastecimento.shp"))

# Esgoto
esgoto <- dd |>
  here::here("tidy/esgoto.csv") |>
  readr::read_csv() |>
  dplyr::select(cod, code_indigenous_land, esgoto:n)
tis |>
  dplyr::left_join(esgoto) |>
  sf::write_sf(here::here(dd, "shp/esgoto.shp"))

# Lixo
lixo <- dd |>
  here::here("tidy/lixo.csv") |>
  readr::read_csv() |>
  dplyr::select(cod, code_indigenous_land, lixo:n)
tis |>
  dplyr::left_join(lixo) |>
  sf::write_sf(here::here(dd, "shp/lixo.shp"))
