# Municípios na Amazônia Legal
al <- readxl::read_excel("202408_seca/data-raw/amazonialegal.xlsx") |>
  dplyr::pull(SIGLA_UF) |>
  unique() |>
  paste(collapse = "|")
tis23 <- readxl::read_excel("202408_secatis/data-raw/Terras_Indigenas_2023_07.xlsx") |>
  janitor::clean_names()
tis24 <- readxl::read_excel("202408_secatis/data-raw/Terras_Indigenas_2024_07.xlsx") |>
  janitor::clean_names() |>
  dplyr::distinct()
categorias <- readr::read_rds("202408_seca/data-raw/iis_classe.rds")

tis_geo <- sf::read_sf("202408_secatis/data-raw/tis_shp", options = "ENCODING=WINDOWS-1252")

al <- geobr::read_amazon()
tis_geo <- tis_geo |>
  sf::st_make_valid() |>
  sf::st_intersection(al)

# tis23 |>
#   dplyr::group_by(terrai_nom, etnia_nome) |>
#   dplyr::summarise(n = dplyr::n_distinct(`SECA jul/23`)) |>
#   dplyr::ungroup() |>
#   dplyr::arrange(dplyr::desc(n))

tis_seca <- tis23 |>
  dplyr::inner_join(tis24) |>
  dplyr::left_join(categorias, dplyr::join_by(seca_jul_23 == iis)) |>
  dplyr::mutate(
    seca_jul_23 = factor(classe, levels = c(
      "Normal", "Seca Fraca", "Seca Moderada", "Seca Severa", "Seca Extrema"
    )),
    seca_jul_24 = factor(seca_jul_24, levels = c(
      "Normal", "Seca Fraca", "Seca Moderada", "Seca Severa", "Seca Extrema"
    ))
  ) |>
  dplyr::select(-classe) |>
  dplyr::inner_join(tis_geo, dplyr::join_by(gid, terrai_cod)) |>
  dplyr::transmute(
    gid, terrai_cod, terrai_nom = dplyr::coalesce(terrai_nom.y, terrai_nom.x),
    etnia_nome = dplyr::coalesce(etnia_nome.y, etnia_nome.x),
    municipio = municipio_, uf = dplyr::coalesce(uf_sigla.y, uf_sigla.x),
    seca_jul_23, seca_jul_24
  )

readr::write_rds(tis_seca, "202408_secatis/data-tidy/tis_seca.rds")

tis_geo_seca <- tis_geo |>
  dplyr::inner_join(tis_seca, dplyr::join_by(gid, terrai_cod))
readr::write_rds(tis_geo_seca, "202408_secatis/data-tidy/tis_geo_seca.rds")

sf::st_write(tis_geo_seca, dsn = "202408_secatis/geojson/tis_geo_seca.geojson")
