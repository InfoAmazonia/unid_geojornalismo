dd <- here::here("ep_03/dados")

# 565 árvores para cada hectare da floresta

prodes <- dd |>
  fs::dir_ls(regexp = "terra") |>
  readr::read_csv2() |>
  janitor::clean_names()

prodes_ano <- prodes |>
  dplyr::summarise(.by = year, area_km2 = sum(area_km2)) |>
  dplyr::mutate(
    area_km2 = units::set_units(area_km2, km^2),
    area_ha = units::set_units(area_km2, ha),
    arvores = 565 * area_ha
  )

deter_ano <- readr::read_csv(here::here(dd, "deter_ano_2026.csv"))

desmatamento_ano <- prodes_ano |>
  dplyr::mutate(dplyr::across(area_km2:arvores, units::drop_units)) |>
  dplyr::bind_rows(deter_ano) |>
  dplyr::arrange(year) |>
  dplyr::filter(year > 2001)

readr::write_csv(desmatamento_ano, here::here(dd, "desmatamento_ano.csv"))
