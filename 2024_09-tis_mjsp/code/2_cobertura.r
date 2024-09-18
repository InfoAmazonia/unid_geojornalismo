read_cob <- function(arq) {
  arq |>
    readr::read_csv() |>
    dplyr::mutate(terrai_cod = stringr::str_extract(basename(arq), "[0-9]+"))
}

cob <- fs::dir_ls("202408_tis_mjsp/data-raw/cobertura") |>
  purrr::map(read_cob) |>
  purrr::list_rbind() |>
  dplyr::relocate(terrai_cod) |>
  tidyr::pivot_longer(3:40)

readr::write_rds(cob, "202408_tis_mjsp/data-tidy/cobertura.rds")


dplyr::glimpse(mb)

mb |>
  dplyr::filter(origem == "Total (1985)") |>
  dplyr::group_by(terrai_nom) |>
  dplyr::summarise(area = sum(area_ha))
t <- sf::read_sf("202408_secatis/data-raw/tis_shp")
t |>
  dplyr::filter(terrai_cod %in% mb$terrai_cod) |>
  tibble::as_tibble() |>
  dplyr::select(terrai_nom, superficie)
    dplyr::glimpse()

mb |>
  dplyr::filter(origem == "1. Floresta (1985)", destino == "Total (2022)") |>
  dplyr::group_by(terrai_nom) |>
  dplyr::summarise(s = sum(area_ha))
  dplyr::glimpse()

mb |>
  # dplyr::filter(destino == "Total (2022)") |>
  dplyr::filter(origem == "Total (1985)", destino == "Total (2022)") |>
  dplyr::group_by(terrai_nom) |>
  dplyr::summarise(area = sum(area_ha)) |>
  clipr::write_clip()
  dplyr::glimpse()
