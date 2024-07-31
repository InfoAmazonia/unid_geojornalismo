# CNFP
# fonte: https://www.gov.br/florestal/pt-br/assuntos/cadastro-nacional-de-florestas-publicas/cadastro-nacional-de-florestas-publicas-atualizacao-2022/cnfp-2022
cnfp_am <- sf::read_sf("202407_carbono/data-raw/cnfp_am")
cnfp_ro <- sf::read_sf("202407_carbono/data-raw/cnfp_ro")
cnfp <- rbind(cnfp_am, cnfp_ro)
buf_union <- "dados/buffer_br319" |>
  sf::read_sf() |>
  sf::st_union() |>
  sf::st_transform(sf::st_crs(cnfp))

sf::sf_use_s2(FALSE)
cnfp_buf <- cnfp |>
  sf::st_make_valid() |>
  sf::st_intersection(buf_union)

dif_buf_cnpf <- sf::st_difference(buf_union, cnfp)

st_erase <- function(x, y) {
  sf::st_difference(x, sf::st_union(sf::st_combine(y)))
}
dif_buf_cnpf <- st_erase(buf_union, cnfp)

sf::st_write(cnfp_buf, dsn = "202407_carbono/geojson/cnfp_buffer.geojson")

# Desmatamento
prodes_buffer <- "202407_carbono/data-tidy/prodes_buffer.rds" |>
  readr::read_rds() |>
  dplyr::filter(year == 2023)
cnfp_buf <- sf::st_transform(cnfp_buf, sf::st_crs(prodes_buffer))
prodes_cnfp <- cnfp_buf |>
  sf::st_intersection(prodes_buffer)
prodes_cnfp <- prodes_cnfp |>
  dplyr::mutate(
    area_desmatada_ha = units::set_units(sf::st_area(geometry), hectare),
    area_ha = units::set_units(area_ha, hectare)
  )
sf::st_write(prodes_cnfp, dsn = "202407_carbono/geojson/desmatamento_perfil.geojson")

# Desmatamento total buffer
prodes_buffer <- readr::read_rds("202407_carbono/data-tidy/prodes_buffer.rds")
prodes_buffer_23 <- prodes_buffer |>
  dplyr::filter(year == 2023) |>
  dplyr::summarise(area = sum(area_km)) |>
  dplyr::mutate(
    area = units::set_units(area, km^2),
    area_ha = units::set_units(area, hectare)
  ) |>
  tibble::as_tibble() |>
  dplyr::select(-geometry)
readr::write_rds(prodes_buffer_23, "202407_carbono/data-tidy/area_desmatada_buffer_23.rds")

desmat_classe <- prodes_cnfp |>
  tibble::as_tibble() |>
  dplyr::mutate(
    classe_tidy = dplyr::case_when(
      stringr::str_detect(classe, "ASSEN") ~ "Assentamento",
      stringr::str_detect(classe, "GLEBA") ~ "FPND",
      stringr::str_detect(classe, "TI") ~ "TI",
      stringr::str_detect(classe, "UC") ~ "UC",
      TRUE ~ classe
    )
  ) |>
  dplyr::group_by(classe_tidy) |>
  dplyr::summarise(
    area_desmatada_ha = sum(area_desmatada_ha),
    area_ha = sum(area_ha)
  )
pct_desmat_classe <- desmat_classe |>
  dplyr::mutate(
    area_desmatada_ha = as.numeric(area_desmatada_ha),
    area_ha = as.numeric(area_ha)
  ) |>
  janitor::adorn_percentages("col") |>
  janitor::adorn_pct_formatting(1)
tbl_desmat_classe <- desmat_classe |>
  dplyr::left_join(pct_desmat_classe, "classe_tidy") |>
  dplyr::transmute(
    classe_tidy,
    area_desmatada_ha = glue::glue("{round(area_desmatada_ha.x, 1)} ({area_desmatada_ha.y})"),
    area_total_desmatada_buffer = prodes_buffer_23$area_ha,
    pct_area_desmatada_buffer = scales::percent(as.numeric(area_desmatada_ha.x/area_total_desmatada_buffer))
  )
readr::write_rds(tbl_desmat_classe, "202407_carbono/data-tidy/tbl_desmat_classe.rds")

desmatamento_total_uf <- prodes |>
  dplyr::filter(year == 2023, state %in% c("AM", "RO")) |>
  dplyr::group_by(state) |>
  dplyr::summarise(area_km = sum(area_km)) |>
  dplyr::mutate(
    area_km = units::set_units(area_km, km^2),
    area_ha = units::set_units(area_km, hectare)
  ) |>
  tibble::as_tibble() |>
  dplyr::select(-geometry)
readr::write_rds(desmatamento_total_uf, "202407_carbono/data-tidy/tbl_desmat_total_uf.rds")

# Número de FPND no buffer
cnfp_buf <- sf::read_sf("202407_carbono/geojson/cnfp_buffer.geojson")
cnfp_buf |>
  tibble::as_tibble() |>
  dplyr::mutate(
    classe_tidy = dplyr::case_when(
      stringr::str_detect(classe, "ASSEN") ~ "Assentamento",
      stringr::str_detect(classe, "GLEBA") ~ "FPND",
      stringr::str_detect(classe, "TI") ~ "TI",
      stringr::str_detect(classe, "UC") ~ "UC",
      TRUE ~ classe
    )
  ) |>
  dplyr::count(classe_tidy)

