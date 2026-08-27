dd <- here::here("2026_09-eleicoes_em_mapas/ep_01")

clima <- fs::dir_ls(here::here(dd, "dados"), regexp = "ranking.*csv$") |>
  purrr::map_dfr(readr::read_csv)

clima <- clima |>
  dplyr::summarise(nota = mean(nota), .by = partido) |>
  dplyr::rename(sigla_partido = partido)

readr::write_csv(clima, here::here(dd, "obl_notas_partido.csv"))

clima |>
  dplyr::arrange(nota) |>
  print(n = 40)


geo_muni |>
  dplyr::left_join(
    dplyr::distinct(pres2, id_municipio, nota_eleitorado, ano),
    dplyr::join_by(code_muni == id_municipio)
  ) |>
  dplyr::filter(!is.na(ano)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = nota_eleitorado)) +
  ggplot2::facet_wrap(~ano, nrow = 2)
