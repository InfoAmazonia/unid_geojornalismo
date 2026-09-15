dd <- here::here("ep_02/dados")

locais <- geobr::read_polling_places(year = 2022, code_muni = c(
  "AC", "AM", "AP", "MT", "MA", "RR", "RO", "TO", "PA"
)) |>
  sf::st_crop(xmin = -80, xmax = -35, ymin = -20, ymax = 10)

apply_voronoi <- function(ano, uf) {
  locais <- geobr::read_polling_places(year = ano, code_muni = uf) |>
    sf::st_crop(xmin = -80, xmax = -35, ymin = -20, ymax = 10)
  vor <- locais |>
    dplyr::pull(geometry) |>
    sf::st_combine() |>
    sf::st_voronoi() |>
    sf::st_collection_extract("POLYGON")
  sf::st_sf(geom = vor) |>
    sf::st_join(locais, sf::st_intersects) |>
    sf::st_intersection(geobr::read_state(year = ano, code_state = uf))

}


# Resultados ----

resultados <- readr::read_csv(here::here(dd, "resultado_zona_presidente.csv"))
notas_partido <- dd |>
  fs::path() |>
  dirname() |>
  dirname() |>
  here::here("dados_obl", "obl_notas_partido.csv") |>
  readr::read_csv()

resultados <- resultados |>
  dplyr::group_by(ano, id_municipio, zona) |>
  dplyr::mutate(
    total_votos = sum(votos_nominais),
    pct = votos_nominais / total_votos
  ) |>
  dplyr::ungroup() |>
  dplyr::left_join(notas_partido, dplyr::join_by(sigla_partido))

notas_zona <- resultados |>
  dplyr::mutate(nota = nota * pct) |>
  dplyr::summarise(
    nota_zona = sum(nota, na.rm = TRUE),
    .by = c(ano, id_municipio, id_municipio_tse, zona)
  )
readr::write_csv(notas_zona, here::here(dd, "notas_zona.csv"))

# Juntando os dados geográficos das zonas com as suas respectivas notas

get_resultado_zona <- function(ano_eleicao) {
  c(
    "AC", "AM", "AP", "MT", "MA", "RR", "RO", "TO", "PA"
  ) |>
    purrr::map(apply_voronoi, ano = ano_eleicao) |>
    dplyr::bind_rows() |>
    dplyr::group_by(code_muni_tse, nr_zona) |>
    dplyr::summarise(
      geom = sf::st_union(geom),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      dplyr::filter(notas_zona, ano == ano_eleicao), dplyr::join_by(
        code_muni_tse == id_municipio_tse, nr_zona == zona
      )
    )
}

geo_resultado_zona <- c(2010, 2022) |>
  purrr::map(get_resultado_zona) |>
  purrr::list_rbind()

geo_resultado_zona <- dplyr::filter(geo_resultado_zona, !is.na(ano))
geo_resultado_zona_2022 <- get_resultado_zona(2022)

readr::write_rds(geo_resultado_zona_2022, here::here(dd, "geo_resultado_zona_2022.rds"))
readr::write_rds(geo_resultado_zona, here::here(dd, "geo_resultado_zona.rds"))


geo_resultado_zona |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = nota_zona)) +
  ggplot2::scale_fill_viridis_c(option = "magma") +
  ggplot2::theme_minimal() +
  ggplot2::facet_wrap(~ano, ncol = 2)

