dd <- here::here("ep_03/dados")
amazonia <- geobr::read_amazon(year = 2024)

deter_nf <- sf::read_sf(here::here(dd, "deter-nf")) |>
  dplyr::filter(lubridate::year(VIEW_DATE) == 2026) |>
  sf::st_make_valid() |>
  sf::st_intersection(amazonia)

deter_amz <- sf::read_sf(here::here(dd, "deter-amz")) |>
  dplyr::filter(lubridate::year(VIEW_DATE) == 2026) |>
  sf::st_make_valid() |>
  sf::st_intersection(amazonia)

deter_cerrado <- sf::read_sf(here::here(dd, "deter-cerrado")) |>
  dplyr::filter(lubridate::year(VIEW_DATE) == 2026) |>
  sf::st_make_valid() |>
  sf::st_intersection(amazonia)

deter_pantanal <- sf::read_sf(here::here(dd, "deter-pantanal")) |>
  dplyr::filter(lubridate::year(VIEW_DATE) == 2026) |>
  sf::st_make_valid() |>
  sf::st_intersection(amazonia)

deter2026 <- dplyr::bind_rows(
  deter_nf, deter_amz, deter_cerrado, deter_pantanal
)

deter2026_union <- sf::st_union(deter2026)

area <- deter2026_union |>
  sf::st_make_valid() |>
  sf::st_area()

deter_ano <- tibble::tibble(
  year = 2026,
  area_km2 = units::set_units(area, km^2),
  area_ha = units::set_units(area, ha),
  arvores = 565 * area_ha
)

readr::write_csv(deter_ano, here::here(dd, "deter_ano_2026.csv"))

