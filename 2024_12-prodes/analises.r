dr <- here::here("202411_prodes/data-raw")
dt <- here::here("202411_prodes/data-tidy")

prodes <- sf::read_sf(here::here(dr, "yearly_deforestation_2024_pri"))
historico <- sf::read_sf(here::here(dr, "prodes_2008_2023"))


shp_uf <- geobr::read_state() |>
  dplyr::filter(
    abbrev_state %in% c("AM", "MT", "PA", "AC", "RO", "RR", "MA", "AP", "TO")
  )

# Geral ----

uf24 <- prodes |>
  tibble::as_tibble() |>
  dplyr::group_by(state) |>
  dplyr::summarise(area = sum(area_km)) |>
  dplyr::mutate(year = 2024)

## Redução do desmatamento geral
historico |>
  tibble::as_tibble() |>
  dplyr::bind_rows(tibble::as_tibble(prodes)) |>
  dplyr::group_by(year, state) |>
  dplyr::summarise(area = sum(area_km), .groups = "drop") |>
  # tidyr::pivot_wider(values_from = area, names_from = year)
  dplyr::bind_rows(uf24) |>
  ggplot2::ggplot(ggplot2::aes(x = year, y = area, fill = state)) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_viridis_d() +
  ggplot2::theme_minimal()

## Mapa 2023 x 2024

m_2023 <- historico |>
  dplyr::filter(year == 2023) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = shp_uf) +
  ggplot2::geom_sf(fill = "red", colour = "red")
m_2024 <- prodes |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = shp_uf) +
  ggplot2::geom_sf(fill = "red", colour = "red")

## Opção 1
patchwork::wrap_plots(m_2023, m_2024, ncol = 2)

## Opção 2
pcts_uf <- prodes |>
  tibble::as_tibble() |>
  dplyr::group_by(state) |>
  dplyr::summarise(area = sum(area_km)) |>
  dplyr::mutate(
    pct = area / sum(area),
    pct_label = scales::percent(pct, .1),
    label = glue::glue("{scales::number(area)} ({pct_label})")
  )
shp_uf |>
  dplyr::inner_join(pcts_uf, dplyr::join_by(abbrev_state == state)) |>
  ggplot2::ggplot(ggplot2::aes(fill = pct, label = label)) +
  ggplot2::geom_sf() +
  ggplot2::geom_sf_text()

# UF com maior redução na taxa de desmatamento de 2023 a 2024
# Roraima foi o único estado com aumento de desmatamento

historico |>
  tibble::as_tibble() |>
  dplyr::filter(year == 2023) |>
  dplyr::group_by(state) |>
  dplyr::summarise(area = sum(area_km), .groups = "drop") |>
  dplyr::left_join(uf24, dplyr::join_by(state)) |>
  dplyr::transmute(
    state, area23 = area.x, area24 = area.y, dif = area24 - area23,
    dif_pct = area24 / area23 - 1
  ) |>
  dplyr::arrange(dif_pct)

# RORAIMA

mun <- geobr::read_municipality()
am_legal <- geobr::read_amazon()
mun <- mun |>
  sf::st_make_valid() |>
  sf::st_crop(am_legal)

# CNFP RR
cnfp_mt <- readr::read_rds(here::here(dt, "cnfp/cnfp_mt.rds"))
cnfp_prodes_mt <- cnfp_mt |>
  sf::st_make_valid() |>
  sf::st_intersection(cnfp_mt)


mun |>
  dplyr::filter(abbrev_state == "RR") |>
  ggplot2::ggplot() +
  ggplot2::geom_sf() +
  ggplot2::geom_sf(
    data = cnfp_rr, ggplot2::aes(fill = classe_tidy)
  ) +
  ggplot2::geom_sf(
    data = dplyr::filter(prodes, state == "RR"), color = "red", fill = "red"
  )


mt <- geobr::read_state("MT")

mt |>
  ggplot2::ggplot() +
  ggplot2::geom_sf() +
  ggplot2::geom_sf(data = cnfp_mt, ggplot2::aes(fill = classe_tidy), alpha = .2) +
  ggplot2::geom_sf(data = prodes, color = "red", fill = "red")

# Amacro
amacro <- readr::read_csv("202408_amacro/data-raw/AMACRO.csv")
mun <- geobr::read_municipality()
mun_amacro <- mun |>
  dplyr::semi_join(amacro, dplyr::join_by(code_muni == GEOCODIGO)) |>
  sf::st_make_valid()
prodes_amacro <- prodes |>
  dplyr::filter(state %in% unique(mun_amacro$abbrev_state)) |>
  sf::st_make_valid() |>
  sf::st_intersection(mun_amacro)

prodes_amacro |>
  sf::st_area() |>
  sum() |>
  units::set_units(km^2)

hist_amacro <- historico |>
  dplyr::filter(state %in% unique(mun_amacro$abbrev_state), year == 2023) |>
  sf::st_make_valid() |>
  sf::st_intersection(mun_amacro)

hist_amacro |>
  sf::st_area() |>
  sum() |>
  units::set_units(km^2)

mun_amacro_union <- sf::st_union(mun_amacro) |>
  sf::st_make_valid()

cnfp_amacro <- cnfp |>
  dplyr::filter(uf %in% c("AM", "AC", "RO")) |>
  sf::st_make_valid() |>
  sf::st_crop(mun_amacro_union)

rodovias <- sf::read_sf(here::here(dr, "rodovias"))
rodovias <- rodovias |>
  sf::st_crop(mun_amacro_union)
buf_rod <- sf::st_buffer(rodovias, 10000) |>
  sf::st_union()
sf::sf_use_s2(FALSE)
buf_rod <- buf_rod |> sf::st_intersection(mun_amacro_union)
buf_rod_prodes <- prodes_amacro |>
  sf::st_intersection(buf_rod)
sf::sf_use_s2(TRUE)

buf_rod_prodes |>
  dplyr::pull(area_km) |>
  sum()
buf_rod_prodes |>
  dplyr::mutate(area = sf::st_area(geometry)) |>
  dplyr::pull(area) |>
  sum() |>
  units::set_units(km^2)
prodes_amacro |>
  dplyr::pull(area_km) |>
  sum()

rodovias |>
  tibble::as_tibble() |>
  dplyr::count(vl_br)
  dplyr::glimpse()


mun_amacro_union |>
  ggplot2::ggplot() +
  ggplot2::geom_sf() +
  ggplot2::geom_sf(data = buf_rod) +
  ggplot2::geom_sf(data = int_rod) +
  ggplot2::geom_sf(data = buf_rod_prodes, color = "red", fill = "red")


cnfp_amacro |>
  sf::st_intersection(prodes)



p1 <- mun_amacro |>
  ggplot2::ggplot() +
  ggplot2::geom_sf() +
  ggplot2::geom_sf(data = prodes_amacro, fill = "red", color = "red")

p2 <- mun_amacro |>
  ggplot2::ggplot() +
  ggplot2::geom_sf() +
  ggplot2::geom_sf(data = hist_amacro, fill = "purple", color = "purple")

patchwork::wrap_plots(p1, p2, nrow = 1)


# Maior polígono

maior2023 <- prodes |>
  dplyr::slice_max(area_km) |>
  sf::st_intersection(dplyr::filter(historico, year == 2023))

maior <- prodes |>
  dplyr::slice_max(area_km)
maior_buff <- maior |>
  sf::st_buffer(units::set_units(10, km))

hist_crop <- sf::st_crop(hh, maior_buff)


hh <- historico |>
  dplyr::filter(year == 2023, state == "MT")

ggplot2::ggplot() +
  ggplot2::geom_sf(data = hist_crop, color = "blue", fill = "blue") +
  # ggplot2::geom_sf(data = maior_buff) +
  ggplot2::geom_sf(data = maior, color = "red", fill = "red")

sf::st_crop(maior)

historico |>
  dplyr::filter(year == 2023) |>
  sf::st_write(here::here(dt, "prodes2023.geojson"))
