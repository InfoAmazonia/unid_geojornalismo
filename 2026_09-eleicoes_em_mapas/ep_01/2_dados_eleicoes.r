dd <- here::here("2026_09-eleicoes_em_mapas/ep_01/dados")

resultados <- fs::dir_ls(regexp = "bq-") |>
  readr::read_csv()

resultados <- resultados |>
  dplyr::filter(
    sigla_uf %in% c("AC", "AM", "PA", "RR", "RO", "MT", "MA", "TO", "AP")
  )


# PRESIDENCIA: 2010, 2014, 2018, 2022

get_pres <- function(turn = 1) {
  resultados |>
    dplyr::filter(cargo == "presidente", turno == turn) |>
    dplyr::mutate(
      total_muni = sum(votos_nominais), .by = c(id_municipio, ano)
    ) |>
    dplyr::mutate(pct = votos_nominais / total_muni, .by = ano) |>
    dplyr::left_join(clima, dplyr::join_by(sigla_partido)) |>
    dplyr::filter(!is.na(nota)) |>
    dplyr::mutate(
      nota_eleitorado = nota * pct,
      nota_eleitorado = sum(nota_eleitorado), .by = c(id_municipio, ano)
    ) |>
    dplyr::distinct()
}
presidente <- get_pres(1)
pres2 <- get_pres(2)

readr::write_csv(presidente, here::here(dd, "presidente.csv"))
presidente <- readr::read_csv(here::here(dd, "presidente.csv"))

geo_muni <- geobr::read_municipality(year = 2022)
geo_muni <- geo_muni |>
  dplyr::filter(
    abbrev_state %in% c("AC", "AM", "PA", "RR", "RO", "MT", "MA", "TO", "AP")
  )

geo_muni |>
  dplyr::left_join(
    dplyr::distinct(presidente, id_municipio, nota_eleitorado, ano),
    dplyr::join_by(code_muni == id_municipio)
  ) |>
  dplyr::filter(!is.na(ano)) |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = nota_eleitorado)) +
  ggplot2::facet_wrap(~ano, nrow = 2)

munis_presidente <- presidente |>
  dplyr::distinct(id_municipio, nota_eleitorado, ano) |>
  tidyr::pivot_wider(names_from = ano, values_from = nota_eleitorado) |>
  dplyr::mutate(dif_2010_2022 = `2010` - `2022`)
munis_presidente <- geo_muni |>
  dplyr::inner_join(munis_presidente, dplyr::join_by(code_muni == id_municipio)) |>
  dplyr::as_tibble() |>
  dplyr::arrange(dif_2010_2022)
tbl_notas_municipios <- munis_presidente
tbl_notas_municipios |>
  dplyr::select(code_muni, name_muni, abbrev_state, `2010`, `2014`, `2018`, `2022`) |>
  readr::write_csv(here::here(dd, "tbl_notas_municipios.csv"))
munis_presidente <- munis_presidente |>
  dplyr::select(code_muni, name_muni, abbrev_state, `2010`, `2022`, dif_2010_2022)
readr::write_csv(munis_presidente, here::here(dd, "tbl_munis_presidente.csv"))

# Dataviz
indice_por_municipio <- geo_muni |>
  dplyr::inner_join(
    munis_presidente, dplyr::join_by(code_muni == id_municipio)
  ) |>
  dplyr::select(
    code_muni, name_muni, abbrev_state, `2010`, `2014`, `2018`, `2022`
  )
sf::write_sf(indice_por_municipio, here::here(dd, "indice_por_municipio.geojson"))


# Município com maior diferença

piorou_2014 <- presidente |>
  dplyr::distinct(id_municipio, nota_eleitorado, ano) |>
  tidyr::pivot_wider(names_from = ano, values_from = nota_eleitorado) |>
  dplyr::mutate(dif_2010_2014 = `2010` - `2014`) |>
  dplyr::arrange(dplyr::desc(dif_2010_2014)) |>
  dplyr::slice_head(n = 3) |>
  dplyr::transmute(id_municipio, ano_piora = 2014)
piora_2018 <- presidente |>
  dplyr::distinct(id_municipio, nota_eleitorado, ano) |>
  tidyr::pivot_wider(names_from = ano, values_from = nota_eleitorado) |>
  dplyr::mutate(dif = `2014` - `2018`) |>
  dplyr::arrange(dplyr::desc(dif)) |>
  dplyr::slice_head(n = 3) |>
  dplyr::transmute(id_municipio, ano_piora = 2018)
piora_2022 <- presidente |>
  dplyr::distinct(id_municipio, nota_eleitorado, ano) |>
  tidyr::pivot_wider(names_from = ano, values_from = nota_eleitorado) |>
  dplyr::mutate(dif = `2018` - `2022`) |>
  dplyr::arrange(dplyr::desc(dif)) |>
  dplyr::slice_head(n = 3) |>
  dplyr::transmute(id_municipio, ano_piora = 2022)
piora <- dplyr::bind_rows(piorou_2014, piora_2018, piora_2022)
readr::write_csv(piora, here::here(dd, "ids_municipios_piora.csv"))

piorou <- presidente |>
  dplyr::distinct(id_municipio, nota_eleitorado, ano) |>
  tidyr::pivot_wider(names_from = ano, values_from = nota_eleitorado) |>
  dplyr::mutate(dif_2010_2022 = `2010` - `2022`) |>
  dplyr::arrange(dplyr::desc(dif_2010_2022)) |>
  dplyr::slice_head(n = 10)

melhorou <- presidente |>
  dplyr::distinct(id_municipio, nota_eleitorado, ano) |>
  tidyr::pivot_wider(names_from = ano, values_from = nota_eleitorado) |>
  dplyr::mutate(dif_2010_2022 = `2010` - `2022`) |>
  dplyr::slice_min(dif_2010_2022, n = 10)

geo_muni |>
  dplyr::inner_join(piorou, dplyr::join_by(code_muni == id_municipio)) |>
  dplyr::as_tibble() |>
  dplyr::arrange(dif_2010_2022) |>
  dplyr::select(code_muni, name_muni, abbrev_state, `2010`, `2022`) |>
  readr::write_csv(here::here(dd, "tbl_piorou.csv"))

geo_muni |>
  dplyr::inner_join(melhorou, dplyr::join_by(code_muni == id_municipio)) |>
  dplyr::as_tibble() |>
  dplyr::arrange(dif_2010_2022) |>
  dplyr::select(code_muni, name_muni, abbrev_state, `2010`, `2022`) |>
  readr::write_csv(here::here(dd, "tbl_melhorou.csv"))

pres_2010_2 <- resultados |>
  dplyr::filter(cargo == "presidente", ano == 2010, turno == 2) |>
  dplyr::mutate(total_muni = sum(votos_nominais), .by = id_municipio) |>
  dplyr::mutate(pct = votos_nominais / total_muni)



# Agro IBGE

# https://sidra.ibge.gov.br/tabela/5457

agro <- readxl::read_excel(here::here(dd, "agro.xlsx"), skip = 3) |>
  janitor::clean_names() |>
  dplyr::filter(!is.na(x6)) |>
  tidyr::fill(nivel, cod, municipio)

# atualiza valores
# pak::pak("deflateBR")

agro <- agro |>
  dplyr::mutate(
    data = lubridate::ym(paste0(ano, "-12")),
    valor_nominal = x5 * 1000
  )

deflated <- deflateBR::deflate(
  nominal_values = agro$valor_nominal,
  nominal_dates = agro$data,
  real_date = "07/2026"
)

agro <- agro |>
  dplyr::mutate(valor_real = deflated)

agro |>
  dplyr::mutate(cod = as.numeric(cod)) |>
  dplyr::filter(cod %in% piorou$id_municipio) |>
  ggplot2::ggplot(ggplot2::aes(x = ano, y = valor_real, group = municipio)) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~municipio, scales = "free_y")

readr::write_csv(agro, here::here(dd, "agro.csv"))

agro_10 <- agro |>
  dplyr::filter(ano %in% c(2010, 2022)) |>
  dplyr::select(cod, municipio, ano, valor_real) |>
  tidyr::pivot_wider(names_from = ano, values_from = valor_real) |>
  dplyr::mutate(dif = `2022` - `2010`) |>
  dplyr::slice_max(dif, n = 10)


# Área Agro

area <- readxl::read_excel(here::here(dd, "agro_area.xlsx"), skip = 1) |>
  janitor::clean_names() |>
  dplyr::filter(!is.na(x6)) |>
  tidyr::fill(cod, municipio) |>
  dplyr::mutate(x6 = as.numeric(x6)) |>
  dplyr::mutate(x6 = tidyr::replace_na(x6, 0))

readr::write_csv(area, here::here(dd, "agro_area.csv"))

area_10 <- area |>
  dplyr::filter(ano %in% c(2010, 2022)) |>
  dplyr::select(cod, municipio, ano, x6) |>
  tidyr::pivot_wider(names_from = ano, values_from = x6) |>
  dplyr::mutate(dif = `2022` - `2010`) |>
  dplyr::slice_max(dif, n = 10)

areapct_10 <- area |>
  dplyr::filter(ano %in% c(2010, 2022)) |>
  dplyr::select(cod, municipio, ano, x6) |>
  tidyr::pivot_wider(names_from = ano, values_from = x6) |>
  dplyr::mutate(
    dif = `2022` - `2010`,
    pct_dif = dif / `2010`
  ) |>
  dplyr::filter(pct_dif < Inf) |>
  dplyr::slice_max(pct_dif, n = 10)

agro_10 |>
  dplyr::filter(
    cod %in% piorou$id_municipio,
    cod %in% area_10$cod
    # cod %in% areapct_10$cod
  )

area |>
  dplyr::filter(ano %in% c(2010, 2022)) |>
  dplyr::select(cod, municipio, ano, x6) |>
  tidyr::pivot_wider(names_from = ano, values_from = x6) |>
  dplyr::mutate(
    dif = `2022` - `2010`,
    pct_dif = dif / `2010`
  ) |>
  dplyr::filter(pct_dif < Inf) |>
  dplyr::filter(cod %in% piorou$id_municipio)



area_melhorou <- area |>
  dplyr::filter(ano %in% c(2010, 2022), cod %in% melhorou$id_municipio) |>
  dplyr::select(cod, municipio, ano, x6) |>
  tidyr::pivot_wider(names_from = ano, values_from = x6) |>
  dplyr::mutate(
    dif = `2022` - `2010`,
    pct_dif = dif / `2010`
  ) |>
  dplyr::rename(area_2010 = `2010`, area_2022 = `2022`) |>
  dplyr::transmute(
    cod, municipio, area_2010, area_2022 = stringr::str_glue(
      "{area_2022} ({scales::percent(pct_dif, 1, style_positive = 'plus')})"
    )
  )

valor_melhorou <- agro |>
  dplyr::filter(ano %in% c(2010, 2022), cod %in% melhorou$id_municipio) |>
  dplyr::select(cod, municipio, ano, valor_real) |>
  tidyr::pivot_wider(names_from = ano, values_from = valor_real) |>
  dplyr::mutate(
    dif = `2022` - `2010`,
    pct_dif = dif / `2010`
  ) |>
  dplyr::rename(valor_2010 = `2010`, valor_2022 = `2022`) |>
  dplyr::transmute(
    cod = as.numeric(cod), municipio,
    valor_2010 = scales::dollar(
      valor_2010, scale = 1e-6, suffix = " mi", accuracy = 0.1,
      big.mark = ".", decimal.mark = ","
    ),
    valor_2022 = scales::dollar(
      valor_2022, scale = 1e-6, suffix = " mi", accuracy = 0.1,
      big.mark = ".", decimal.mark = ","
    ),
    valor_2022 = stringr::str_glue(
      "{valor_2022} ({scales::percent(pct_dif, 1, style_positive = 'plus')})"
    )
  )

melhorou |>
  dplyr::left_join(area_melhorou, dplyr::join_by(id_municipio == cod)) |>
  dplyr::left_join(
    valor_melhorou, dplyr::join_by(id_municipio == cod, municipio)
  ) |>
  dplyr::select(
    municipio, `2010`, `2022`, dif_2010_2022,
    area_2022, valor_2022
  ) |>
  dplyr::filter(!is.na(municipio))

medias_nota <- presidente |>
  dplyr::distinct(id_municipio, nota_eleitorado, ano) |>
  dplyr::summarise(.by = ano, media = mean(nota_eleitorado, na.rm = TRUE)) |>
  dplyr::arrange(ano) |>
  dplyr::mutate(variavel = "nota")

medias_valor <- agro |>
  dplyr::filter(ano %in% c(2010, 2014, 2018, 2022)) |>
  dplyr::select(cod, municipio, ano, valor_real) |>
  dplyr::summarise(.by = ano, media = mean(valor_real, na.rm = TRUE)) |>
  dplyr::mutate(variavel = "valor")

medias_area <- area |>
  dplyr::select(cod, municipio, ano, x6) |>
  dplyr::summarise(.by = ano, media = mean(x6, na.rm = TRUE)) |>
  dplyr::mutate(variavel = "area")

medias_area |>
  ggplot2::ggplot(ggplot2::aes(ano, media, group = variavel)) +
  ggplot2::geom_line()


area |>
  dplyr::distinct(cod, x6, ano) |>
  ggplot2::ggplot(ggplot2::aes(x = ano, y = x6, group = cod)) +
  ggplot2::geom_line(alpha = .2) +
  ggplot2::scale_x_continuous(breaks = c(2010, 2014, 2018, 2022))




# Bases visualização
agro <- readr::read_csv("ep_01_eleitorado/agro.csv")
agro |>
  dplyr::summarise(.by = ano, valor = sum(valor_real, na.rm = TRUE)) |>
  readr::write_csv(here::here(dd, "tbl_agro_total.csv"))

agro |>
  dplyr::transmute(cod, ano, municipio, valor = valor_real) |>
  readr::write_csv(here::here(dd, "tbl_agro_municipio.csv"))
