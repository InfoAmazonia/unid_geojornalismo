dd <- here::here("ep_03/dados")
prodes <- sf::read_sf(here::here(dd, "yearly_deforestation"))

prodes <- prodes |>
  dplyr::filter(year %in% c(2021, 2023))

municipios <- geobr::read_municipality(year = 2021)

municipios_bolsonaro <- prodes |>
  dplyr::filter(year == 2021) |>
  sf::st_make_valid() |>
  sf::st_intersection(municipios) |>
  dplyr::mutate(area = sf::st_area(geometry))

area_muni <- municipios |>
  dplyr::mutate(area_muni = sf::st_area(geometry)) |>
  dplyr::as_tibble() |>
  dplyr::select(code_muni, area_muni)

municipios_bolsonaro |>
  dplyr::as_tibble() |>
  dplyr::summarise(
    area = sum(area),
    .by = c(code_muni, name_muni, abbrev_state)
  ) |>
  dplyr::left_join(area_muni, by = "code_muni") |>
  dplyr::mutate(pct = area / area_muni) |>
  # clipr::write_clip()
  dplyr::slice_max(pct, n = 10) |>
  readr::write_csv(here::here(dd, "dados-tidy/ranking_municipios_bolsonaro.csv"))


sf::sf_use_s2(FALSE)
municipios_lula <- prodes |>
  dplyr::filter(year == 2023) |>
  sf::st_intersection(municipios) |>
  dplyr::mutate(area = sf::st_area(geometry))

municipios_lula |>
  dplyr::as_tibble() |>
  dplyr::summarise(
    area = sum(area),
    .by = c(code_muni, name_muni, abbrev_state)
  ) |>
  dplyr::left_join(area_muni, by = "code_muni") |>
  dplyr::mutate(pct = area / area_muni) |>
  # clipr::write_clip()
  dplyr::slice_max(pct, n = 10) |>
  readr::write_csv(here::here(dd, "dados-tidy/ranking_municipios_lula.csv"))
