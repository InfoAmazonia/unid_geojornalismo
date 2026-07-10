dir <- here::here("202606_agua_tis/dados")
dataviz <- here::here("202606_agua_tis/dataviz")
fs::dir_create(dataviz)

bhae <- sf::read_sf(here::here(dir, "geoft_bhae_curso_dagua.gpkg"))
drenagem <- sf::read_sf(here::here(dir, "geoft_bhae_trecho_drenagem.gpkg"))

alto_juruena <- sf::read_sf(here::here(dir, "micro_rh")) |>
  dplyr::filter(nm_microRH == "Alto Juruena")

fs::dir_create(here::here(dataviz, "alto_juruena"))
sf::write_sf(alto_juruena, here::here(dataviz, "alto_juruena", "alto_juruena.shp"))

# sf::st_crs(alto_juruena) == sf::st_crs(drenagem)
drenagem <- drenagem |>
  sf::st_intersection(alto_juruena)
# drenagem <- drenagem |>
  # dplyr::select(cocursodag, nogenerico, noespecif, noriocomp, nooriginal)
bhae <- bhae |>
  sf::st_intersection(alto_juruena)

bhae |>
  ggplot2::ggplot() +
  ggplot2::geom_sf()

juruena <- drenagem |>
  dplyr::filter(
    noespecif %in% "Juruena"
    # stringr::str_detect(nooriginal, "Juruena")
  ) |>
  dplyr::distinct(cocursodag)


# Buriti, Papagaio, Sacre e Juruena
rios <- drenagem |>
  dplyr::as_tibble() |>
  dplyr::filter(
    noespecif %in% c("Buriti", "Papagaio", "Sacre", "Juruena")
  ) |>
  dplyr::select(noespecif, cocursodag) |>
  dplyr::distinct(cocursodag, noespecif)

rios_shp <- bhae |>
  dplyr::inner_join(rios, dplyr::join_by(cocursodag))

readr::write_rds(rios_shp, here::here(dir, "rios_shp.rds"))

rios_shp |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(color = noespecif))

# Buffer de 1 km, 5 km e 10 km para análise de mata ciliar

rios_shp_1km <- sf::st_buffer(rios_shp, units::set_units(1, "km"))
rios_shp_5km <- sf::st_buffer(rios_shp, units::set_units(5, "km"))
rios_shp_10km <- sf::st_buffer(rios_shp, units::set_units(10, "km"))


# Cobertura - MapBiomas

dd <- here::here("202606_agua_tis/dados/mb_cobertura")

cobertura_2024 <- fs::dir_ls(dd, regexp = "2024_coverage") |>
  purrr::discard(\(x) stringr::str_detect(x, "buff_")) |>
  terra::rast()
alto_juruena_terra <- terra::vect(alto_juruena)
alto_juruena_terra <- terra::project(alto_juruena_terra, terra::crs(cobertura_2024))
cobertura_2024_alto_juruena_mask <- terra::crop(cobertura_2024, alto_juruena_terra, mask = TRUE)
cobertura_2024_alto_juruena_unmask <- terra::crop(cobertura_2024, alto_juruena_terra, mask = FALSE)

terra::writeRaster(
  cobertura_2024_alto_juruena_mask,
  filename = here::here(dataviz, "2024_coverage_alto_juruena_mask.tif"),
  overwrite = TRUE
)
terra::writeRaster(
  cobertura_2024_alto_juruena_unmask,
  filename = here::here(dataviz, "2024_coverage_alto_juruena_unmask.tif"),
  overwrite = TRUE
)

rios_shp_10km_terra <- terra::vect(rios_shp_10km)
teste <- terra::rast(fs::dir_ls(dd)[1])
rios_shp_10km_terra <- terra::project(rios_shp_10km_terra, terra::crs(teste))

buffer_save <- function(x, buffer) {

  ra <- terra::rast(x)
  buff <- sf::st_buffer(rios_shp, units::set_units(buffer, "km")) |>
    terra::vect() |>
    terra::project(terra::crs(ra))

  cropped <- terra::crop(ra, buff, mask = TRUE)
  terra::writeRaster(
    cropped,
    filename = here::here(
      "202606_agua_tis/dados/mb_cobertura",
      paste0("buff_", buffer, "km_", basename(x))
    ),
    overwrite = TRUE
  )
  Sys.sleep(1)
}

fs::dir_ls(here::here(dir, "mb_cobertura"), regexp = "/\\d{4}_coverage") |>
  purrr::walk(buffer_save, 10, .progress = TRUE)

legenda <- readr::read_delim(here::here(dir, "legenda_mapbiomas_col10.csv"))

calc_area_cobertura <- function(x) {
  km <- stringr::str_extract(basename(x), "(?<=buff_)\\d+")
  ano <- stringr::str_extract(basename(x), "(?<=buff_\\d{1,2}_)\\d{4}")
  r <- terra::rast(x)
  area <- terra::expanse(r, unit = "ha", byValue = TRUE)
  area |>
    dplyr::mutate(ano = ano, buffer_km = km)
}

area_cob <- here::here(dd) |>
  fs::dir_ls() |>
  purrr::map(calc_area_cobertura, .progress = TRUE)


area_buffer <- here::here(dd) |>
  fs::dir_ls() |>
  basename() |>
  stringr::str_extract("^buff_\\d{1,2}") |>
  readr::parse_number()

ano <- here::here(dd) |>
  fs::dir_ls() |>
  basename() |>
  stringr::str_extract("^buff_\\d{1,2}km_\\d{4}") |>
  stringr::str_extract("\\d{4}")

area_cob <- purrr::map2(area_cob, area_buffer, \(x, y) dplyr::mutate(x, buffer_km = y))
area_cob <- purrr::map2(area_cob, ano, \(x, y) dplyr::mutate(x, ano = y))

area_cob <- purrr::list_rbind(area_cob) |>
  tibble::as_tibble()

area_cob <- area_cob |>
  dplyr::mutate(ano = as.numeric(ano), buffer_km = as.numeric(buffer_km)) |>
  dplyr::filter(!is.na(ano))

area_cob <- area_cob |>
  dplyr::left_join(legenda, dplyr::join_by(value == Class_ID))

readr::write_rds(area_cob, here::here(dir, "area_cobertura.rds"))
# area_cob <- readr::read_rds(here::here(dir, "area_cobertura.rds"))


area_cob |>
  dplyr::filter(
    Level == 2
    # Descricao %in% c(
    #   "Formação Florestal", "Formação Savânica", "Floresta Alagável", "Formação Campestre"
    # )
  ) |>
  ggplot2::ggplot(ggplot2::aes(x = ano, y = area, color = Descricao)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~buffer_km, scales = "free_y", nrow = 3) +
  ggplot2::scale_y_continuous(labels = scales::label_number()) +
  ggplot2::theme_minimal()
  dplyr::distinct(Descricao, Level)

area_cob |>
  dplyr::filter(
    Level == 2
    # Descricao %in% c(
    #   "Formação Florestal", "Formação Savânica", "Floresta Alagável", "Formação Campestre"
    # )
  ) |>
  tidyr::pivot_wider(names_from = ano, values_from = area) |>
  dplyr::select(-c(layer, value, Level, Description, Color)) |>
  dplyr::mutate(dif = `2024` - `1985`, dif_pct = dif / `1985`) |>
  clipr::write_clip()

fs::dir_create(here::here(dataviz, "rios_bhae_alto_juruena"))
rios_drenagem <- drenagem |>
  tibble::as_tibble() |>
  dplyr::select(cocursodag, noespecif, noriocomp)
bhae |>
  dplyr::left_join(rios_drenagem) |>
  sf::write_sf(here::here(dataviz, "rios_bhae_alto_juruena", "bhae.shp"))
