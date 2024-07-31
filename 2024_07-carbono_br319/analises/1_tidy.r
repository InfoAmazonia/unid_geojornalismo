# Buffer ao redor da BR-319
buf <- sf::read_sf("202407_carbono/data-raw/buffer_br319")
buf_union <- buf |>
  sf::st_union()

ggplot2::ggplot(buf_union) +
  ggplot2::geom_sf()

# Dados CAR
# Propriedades privadas
sicar <- sf::read_sf("202407_carbono/data-raw/sicar_am") |>
  dplyr::filter(des_condic != "Cancelado por duplicidade") |>
  sf::st_make_valid() |>
  sf::st_transform(sf::st_crs(buf_union))
sicar_union <- sf::st_union(sicar)
sicar_buffer <- sf::st_intersection(sicar, buf_union)
sf::st_write(sicar_buffer, dsn = "202407_carbono/geojson/0_sicar.geojson")

# Municípios no buffer
mun <- geobr::read_municipality()
mun <- sf::st_transform(mun, sf::st_crs(buf_union))
mun_buffer <- mun |>
  sf::st_intersection(buf_union)
sf::st_write(mun_buffer, dsn = "202407_carbono/geojson/0_muni_buffer.geojson")

# Área de CAR por município, área por município
tbl_area_mun_buffer <- mun_buffer |>
  dplyr::mutate(area_muni_buffer = units::set_units(sf::st_area(geom), hectare)) |>
  tibble::as_tibble() |>
  dplyr::select(-geom)
readr::write_rds(tbl_area_mun_buffer, "202407_carbono/data-tidy/tbl_area_mun_buffer.rds")
mun_sicar_buffer <- mun_buffer |>
  sf::st_intersection(sicar_buffer)
tbl_mun_sicar_buffer <- mun_sicar_buffer |>
  dplyr::mutate(area_sicar_muni = units::set_units(sf::st_area(geom), hectare)) |>
  tibble::as_tibble() |>
  dplyr::select(-geom)
readr::write_rds(tbl_mun_sicar_buffer, "202407_carbono/data-tidy/tbl_mun_sicar_buffer.rds")

# Dados desmatamento Prodes
prodes <- sf::read_sf("202407_carbono/data-raw/prodes")
prodes <- prodes |>
  sf::st_make_valid() |>
  dplyr::filter(state %in% c("RO", "AM"))
prodes <- sf::st_transform(prodes, sf::st_crs(buf_union))
prodes_buffer <- sf::st_intersection(prodes, buf_union)
readr::write_rds(prodes_buffer, "202407_carbono/data-tidy/prodes_buffer.rds")
sf::st_write(prodes_buffer, dsn = "202407_carbono/geojson/0_prodes_buffer.geojson")

prodes_buffer |>
  dplyr::group_by(year) |>
  dplyr::summarise(area = sum(area_km)) |>
  dplyr::mutate(area = units::set_units(units::set_units(area, km^2), hectare))

# Área desmatada em sicar por ano
sicar_buffer_prodes <- prodes_buffer |>
  sf::st_intersection(sicar_union)
sicar_buffer_prodes |>
  dplyr::group_by(year) |>
  dplyr::summarise(area = sum(area_km)) |>
  dplyr::mutate(area = units::set_units(units::set_units(area, km^2), hectare))
readr::write_rds(sicar_buffer_prodes, "202407_carbono/data-tidy/sicar_buffer_prodes.rds")

# Área desmatada por município e ano
mun_prodes_buffer <- prodes_buffer |>
  sf::st_intersection(mun_buffer)
mun_prodes_buffer |>
  ggplot2::ggplot() +
  ggplot2::geom_sf()
mun_prodes_buffer <- mun_prodes_buffer |>
  dplyr::mutate(area_desmatada = units::set_units(sf::st_area(geometry), hectare)) |>
  dplyr::group_by(year, code_muni, name_muni) |>
  dplyr::summarise(
    area_desmatada = sum(area_desmatada), .groups = "drop"
  )
sf::st_write(mun_prodes_buffer, dsn = "202407_carbono/geojson/1_desmatamento_municipio_buffer.geojson")

# Área desmatada em propriedades rurais por município e ano
mun_prodes_sicar_buffer <- mun_prodes_buffer |>
  sf::st_intersection(sicar_union)
mun_prodes_sicar_buffer <- mun_prodes_sicar_buffer |>
  dplyr::mutate(area_desmatada_sicar = units::set_units(
    sf::st_area(geometry), hectare
  ))
sf::st_write(mun_prodes_sicar_buffer, "202407_carbono/geojson/1_mun_prodes_sicar_buffer.geojson")

# Carbono
depara_mun <- tbl_mun_prodes_buffer |>
  dplyr::distinct(code_muni, name_muni) |>
  dplyr::mutate(name_join = toupper(stringi::stri_trans_general(name_muni, "Latin-ASCII")))

carbono <- readxl::read_excel("202407_carbono/data-raw/carbono_co2.xlsx", "carbono")
carbono <- carbono |>
  janitor::clean_names() |>
  tidyr::pivot_longer(2:18, names_to = "ano", values_to = "carbono_k_ton") |>
  dplyr::mutate(ano = stringr::str_extract(ano, "20[0-9]{2}")) |>
  dplyr::mutate(ano = as.integer(ano)) |>
  dplyr::filter(ano > 2009) |>
  dplyr::mutate(name_join = stringi::stri_trans_general(municipio, "Latin-ASCII")) |>
  dplyr::inner_join(depara_mun) |>
  dplyr::select(ano, carbono_k_ton, code_muni, name_muni)
readr::write_rds(carbono, "202407_carbono/data-tidy/carbono.rds")

# CO2
co2 <- readxl::read_excel("202407_carbono/data-raw/carbono_co2.xlsx", "co2_desmatamento")
co2 <- co2 |>
  janitor::clean_names() |>
  tidyr::pivot_longer(2:18, names_to = "ano", values_to = "co2_k_ton") |>
  dplyr::mutate(ano = stringr::str_extract(ano, "20[0-9]{2}")) |>
  dplyr::mutate(ano = as.integer(ano)) |>
  dplyr::filter(ano > 2009) |>
  dplyr::mutate(name_join = stringi::stri_trans_general(municipio, "Latin-ASCII")) |>
  dplyr::inner_join(depara_mun) |>
  dplyr::select(ano, co2_k_ton, code_muni, name_muni)
readr::write_rds(co2, "202407_carbono/data-tidy/co2.rds")


# Juntando tudo numa tabela
tbl_mun_prodes_buffer <- tibble::as_tibble(mun_prodes_buffer)
tbl_mun_prodes_sicar_buffer <- tibble::as_tibble(mun_prodes_sicar_buffer)

tbl_final <- tbl_mun_prodes_buffer |>
  dplyr::left_join(tbl_mun_prodes_sicar_buffer, dplyr::join_by(
    year, code_muni, name_muni
  )) |>
  dplyr::transmute(
    ano = year, code_muni, name_muni, area_desmatada = area_desmatada.x,
    area_desmatada_sicar
  ) |>
  dplyr::left_join(tbl_area_mun_buffer) |>
  dplyr::left_join(tbl_mun_sicar_buffer) |>
  dplyr::left_join(carbono) |>
  dplyr::left_join(co2) |>
  dplyr::filter(ano > 2009) |>
  tidyr::replace_na(list(
    area_desmatada_sicar = units::set_units(0, hectare),
    area_sicar_muni = units::set_units(0, hectare)
  )) |>
  dplyr::mutate(
    area_desmatada_fora_sicar = area_desmatada - area_desmatada_sicar,
    pct_area_sicar = area_sicar_muni / area_muni_buffer,
    pct_desmat_sicar = area_desmatada_sicar / area_desmatada
  ) |>
  dplyr::glimpse()

readr::write_rds(tbl_final, "202407_carbono/data-tidy/tbl_final.rds")
