dir <- here::here("202606_agua_tis/dados")
dd <- here::here("202606_agua_tis/dados/mb_agua")

tis <- geobr::read_indigenous_land(2025)
amazonia <- geobr::read_amazon(2024)
tis_amazonia <- sf::st_intersection(tis, amazonia)

# checagem polígonos
# amazonia |>
#   ggplot2::ggplot() +
#   ggplot2::geom_sf() +
#   ggplot2::geom_sf(data = tis_amazonia, fill = "red", alpha = 0.5)


# Intersecção Água x Amazônia Legal

teste <- terra::rast(here::here(dd, "1985_water_water_surface_1-1-1_059dac8f-f27c-49a8-8ab2-0561f009ed5f.tif"))
tis_amazonia_terra <- terra::vect(tis_amazonia)
tis_amazonia_terra <- terra::project(tis_amazonia_terra, terra::crs(teste))

crop_save <- function(x) {
  if (fs::file_exists(here::here("202606_agua_tis/dados/mb_agua_amazonia", basename(x)))) {
    message(paste0("Arquivo ", basename(x), " já existe. Pulando..."))
    return(NULL)
  }
  x |>
    terra::rast() |>
    terra::crop(tis_amazonia_terra) |>
    terra::writeRaster(
      filename = here::here(
        "202606_agua_tis/dados/mb_agua_amazonia", basename(x)
      ),
      overwrite = TRUE
    )
}

fs::dir_ls(dd) |>
  purrr::walk(crop_save, .progress = TRUE)


# Intersecção Água na Amazônia Legal x TIs Alto Juruena

# TIs Alto Juruena
alto_juruena <- sf::read_sf(here::here(dir, "micro_rh")) |>
  dplyr::filter(nm_microRH == "Alto Juruena")

tis_alto_juruena <- sf::st_intersection(tis, alto_juruena)
tis_alto_juruena_terra <- terra::vect(tis_alto_juruena)
tis_alto_juruena_terra <- terra::project(
  tis_alto_juruena_terra, terra::crs(teste)
)
tis |>
  dplyr::filter(code_indigenous_land %in% tis_alto_juruena$code_indigenous_land) |>
sf::write_sf(here::here(dataviz, "tis_alto_juruena", "tis_alto_juruena.shp"))

crop_save <- function(x) {
  ra <- terra::rast(x)
  cropped <- terra::crop(ra, tis_alto_juruena_terra, mask = TRUE)
  terra::writeRaster(
    cropped,
    filename = here::here(
      "202606_agua_tis/dados/mb_agua_tis_alto_juruena", basename(x)
    ),
    overwrite = TRUE
  )
  Sys.sleep(1)
}
fs::dir_ls(here::here(dir, "mb_agua_tis_alto_juruena")) |>
  purrr::walk(crop_save, .progress = TRUE)

# Calcula área (ha) por ano em TIs (total)
calc_water_area <- function(x) {
  r <- terra::rast(x)
  area <- terra::expanse(r, unit = "ha", byValue = TRUE)
  area$file <- basename(x)
  area
}

area_agua_tis <- fs::dir_ls(here::here(dir, "mb_agua_tis_alto_juruena")) |>
  purrr::map_dfr(calc_water_area, .progress = TRUE)

area_agua_tis <- area_agua_tis |>
  dplyr::mutate(ano = as.numeric(stringr::str_extract(file, "^\\d{4}")))

readr::write_csv(area_agua_tis, here::here(dir, "agua_tis_alto_juruena.csv"))


# Água por TI em 1985 e em 2025

## Dentro da bacia do Alto Juruena

agua_ti_1985_2025 <- function(code) {
  ano1985 <- here::here(
    dir, "mb_agua_tis_alto_juruena",
    "1985_water_water_surface_1-1-1_059dac8f-f27c-49a8-8ab2-0561f009ed5f.tif"
  ) |>
    terra::rast()
  ano2025 <- here::here(
    dir, "mb_agua_tis_alto_juruena",
    "2025_water_water_surface_1-1-1_eeb58198-228a-4dc0-8b79-7e3f9da81385.tif"
  ) |>
    terra::rast()
  ti <- tis_alto_juruena |>
    dplyr::filter(code_indigenous_land == code) |>
    terra::vect() |>
    terra::project(terra::crs(ano1985))
  ti_1985 <- terra::crop(ano1985, ti, mask = TRUE)
  ti_2025 <- terra::crop(ano2025, ti, mask = TRUE)
  area_1985 <- terra::expanse(ti_1985, unit = "ha", byValue = TRUE)
  area_2025 <- terra::expanse(ti_2025, unit = "ha", byValue = TRUE)
  Sys.sleep(1)
  tibble::tibble(
    code_indigenous_land = code,
    area_1985 = area_1985[area_1985$value == 1, "area"],
    area_2025 = area_2025[area_2025$value == 1, "area"]
  )
}

agua_por_tis_1985_2025 <- tis_alto_juruena |>
  dplyr::pull(code_indigenous_land) |>
  purrr::map_dfr(agua_ti_1985_2025, .progress = TRUE)

agua_por_tis_1985_2025 <- tis_alto_juruena |>
  dplyr::as_tibble() |>
  dplyr::select(1:2, area_ha:fase_ti) |>
  dplyr::left_join(agua_por_tis_1985_2025, by = "code_indigenous_land")

agua_por_tis_1985_2025 <- agua_por_tis_1985_2025 |>
  dplyr::mutate(
    dif = area_2025 - area_1985,
    dif_pct = dif / area_1985
  )
readr::write_csv(
  agua_por_tis_1985_2025,
  here::here(dir, "agua_por_tis_1985_2025.csv")
)

mb_agua_tis_1985_2025 <- tis |>
  dplyr::filter(code_indigenous_land %in% agua_por_tis_1985_2025$code_indigenous_land) |>
  dplyr::left_join(agua_por_tis_1985_2025, dplyr::join_by(code_indigenous_land, name_indigenous_land, area_ha, fase_ti))

sf::write_sf(
  mb_agua_tis_1985_2025,
  here::here(dataviz, "mb_agua_tis_1985_2025.shp")
)

mb_agua_2025 <- terra::rast(here::here(dir, "mb_agua_amazonia", "2025_water_water_surface_1-1-1_eeb58198-228a-4dc0-8b79-7e3f9da81385.tif"))
mb_agua_2025_alto_juruena <- terra::crop(mb_agua_2025, alto_juruena_terra, mask = TRUE)
terra::writeRaster(
  mb_agua_2025_alto_juruena,
  filename = here::here(dataviz, "mb_agua_2025_alto_juruena.tif"),
  overwrite = TRUE
)
mb_agua_1985 <- terra::rast(here::here(dir, "mb_agua_amazonia", "1985_water_water_surface_1-1-1_059dac8f-f27c-49a8-8ab2-0561f009ed5f.tif"))
mb_agua_1985_alto_juruena <- terra::crop(mb_agua_1985, alto_juruena_terra, mask = TRUE)
terra::writeRaster(
  mb_agua_1985_alto_juruena,
  filename = here::here(dataviz, "mb_agua_1985_alto_juruena.tif"),
  overwrite = TRUE
)
