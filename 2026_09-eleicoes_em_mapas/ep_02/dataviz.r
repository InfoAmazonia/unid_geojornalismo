rr <- rel_zona |>
  dplyr::filter(religiao_1 == "EVANGÉLICAS")

rr |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = nota_zona)) +
  ggplot2::scale_fill_gradient(low = "white", high = "red") +
  ggplot2::geom_sf(ggplot2::aes(fill = proporcao_pct), .alpha = .5)

rr$faixas_pct_evangelicos <- cut(rr$proporcao_pct, breaks = 3)
rr$faixas_notas <- cut(rr$nota_zona, breaks = 3)

rr <- rr |>
  dplyr::mutate(pc
    label_faixas = dplyr::case_when(
      faixas_pct_evangelicos == "(0.0707,0.213]" & faixas_notas == "(-1.95,-0.249]" ~ "low_low",
      faixas_pct_evangelicos == "(0.0707,0.213]" & faixas_notas == "(-0.249,1.44]" ~ "low_mid",
      faixas_pct_evangelicos == "(0.0707,0.213]" & faixas_notas == "(1.44,3.14]" ~ "low_high",
      faixas_pct_evangelicos == "(0.213,0.354]" & faixas_notas == "(-1.95,-0.249]" ~ "mid_low",
      faixas_pct_evangelicos == "(0.213,0.354]" & faixas_notas == "(-0.249,1.44]" ~ "mid_mid",
      faixas_pct_evangelicos == "(0.213,0.354]" & faixas_notas == "(1.44,3.14]" ~ "mid_high",
      faixas_pct_evangelicos == "(0.354,0.496]" & faixas_notas == "(-1.95,-0.249]" ~ "high_low",
      faixas_pct_evangelicos == "(0.354,0.496]" & faixas_notas == "(-0.249,1.44]" ~ "high_mid",
      faixas_pct_evangelicos == "(0.354,0.496]" & faixas_notas == "(1.44,3.14]" ~ "high_high",
    )
  )


dd <- here::here("eleicoes2026", "ep_02_demografia", "dados")
sf::st_write(rr, here::here(dd, "dados_notas_evangelicos.geojson"), delete_dsn = TRUE)

notas <- tbl_zonas2022 |>
  dplyr::select(nr_zona, id_municipio, nota_zona)

notas_brancos <- zona_setor_2022_raca |>
  dplyr::filter(raca_cor == "branca") |>
  dplyr::left_join(notas)

notas_brancos |>
  ggplot2::ggplot(ggplot2::aes(x = pct, y = nota_zona)) +
  ggplot2::geom_point()

notas_brancos$faixa_bcos <- cut(notas_brancos$pct, breaks = 5)

notas_brancos |>
  dplyr::group_by(faixa_bcos) |>
  dplyr::summarise(nota_media = mean(nota_zona, na.rm = TRUE)) |>
  ggplot2::ggplot(ggplot2::aes(x = faixa_bcos, y = nota_media)) +
  ggplot2::geom_col() +
  ggplot2::labs(x = "Faixa de % de brancos", y = "Nota média da zona") +
  ggplot2::theme_minimal()

melhores <- notas |>
  dplyr::slice_max(nota_zona, n = 10) |>
  dplyr::mutate(tipo = "melhores")
piores <- notas |>
  dplyr::slice_min(nota_zona, n = 10) |>
  dplyr::mutate(tipo = "piores")
mp <- dplyr::bind_rows(melhores, piores)

geobr::read_municipality(year = 2022) |>
  dplyr::filter(code_muni %in% mp$id_municipio) |>
  tibble::as_tibble() |>
  dplyr::select(code_muni, name_muni, abbrev_state) |>
  dplyr::left_join(mp, dplyr::join_by(code_muni == id_municipio)) |>
  dplyr::arrange(tipo)



brancos <- notas_brancos |>
  dplyr::inner_join(mp)

brancos |>
  ggplot2::ggplot(ggplot2::aes(x = pct, y = nota_zona, color = tipo)) +
  ggplot2::geom_point()

ev <- tbl_rel_zona |>
  dplyr::inner_join(mp) |>
  dplyr::filter(religiao_1 == "EVANGÉLICAS") |>
  dplyr::select(id_municipio, nr_zona, proporcao_pct)


teste_resumo |>
  readr::write_csv(here::here(dd, "nota_media_evangelicos.csv"))
