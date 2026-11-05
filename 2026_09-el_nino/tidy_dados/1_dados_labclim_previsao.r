# Dados de previsão do LABCLIM - anomalias de temperatura e precipitação

dd <- here::here("2026_09-el_nino/dados/raw")
tidy <- here::here("2026_09-el_nino/dados/tidy")
if (!dir.exists(tidy)) dir.create(tidy)

create_tables <- function(mes) {

  if (mes == "11") {
    mes_label <- "NOVEMBRO"
  } else if (mes == "12") {
    mes_label <- "DEZEMBRO"
  } else {
    stop("Mês inválido. Use '11' para novembro ou '12' para dezembro.")
  }

  # Os arquivos PREV-*-novo.nc já trazem anomalias: T2M em K (= °C) e
  # TP em mm/mês (apesar do atributo de unidade "m"). Não há conversão.
  prev <- terra::rast(here::here(dd, "LABCLIM_AnomaliasChuvaTemperatura",
    stringr::str_glue("PREV-{mes_label}-novo.nc")
  ))
  prev <- terra::rotate(prev)

  # Amazônia Legal: os 9 estados inteiros (inclusive todo o Maranhão, e não
  # só a parte a oeste do meridiano de 44°), sem recorte por interseção
  ufs_al <- c("AC", "AM", "AP", "MA", "MT", "PA", "RO", "RR", "TO")

  sf_municipios <- geobr::read_municipality(year = 2025) |>
    dplyr::filter(abbrev_state %in% ufs_al)
  sf_ufs <- geobr::read_state(year = 2025) |>
    dplyr::filter(abbrev_state %in% ufs_al)

  # Média ponderada pela fração de área de cada célula dentro do polígono
  # (a grade é de 1°, então crop com mask deixaria municípios sem célula).
  # exact = TRUE em vez de weights = TRUE: weights dá peso zero a polígonos
  # muito pequenos (ex.: recortes de Axixá e São João do Soter no MA).
  get_tbl <- function(sf_poligonos, id_col) {
    v <- sf_poligonos |>
      terra::vect() |>
      terra::project(terra::crs(prev))
    terra::extract(prev, v, fun = mean, exact = TRUE, na.rm = TRUE) |>
      tibble::as_tibble() |>
      dplyr::mutate(!!id_col := v[[id_col]][[1]]) |>
      dplyr::select(-ID) |>
      tidyr::pivot_longer(
        cols = -dplyr::all_of(id_col), names_to = "layer", values_to = "value"
      ) |>
      dplyr::mutate(
        variavel = dplyr::case_when(
          stringr::str_detect(layer, "T2M") ~ "anomalia_temperatura_c",
          stringr::str_detect(layer, "TP") ~ "anomalia_precipitacao_mm"
        ),
        mes = lubridate::ym(stringr::str_glue("2026-{mes}"))
      ) |>
      dplyr::select(-layer)
  }

  tbl_projecao_uf <- get_tbl(sf_ufs, "abbrev_state") |>
    dplyr::rename(uf = abbrev_state)

  readr::write_csv(tbl_projecao_uf, here::here(tidy, stringr::str_glue(
    "tbl_projecao_uf_2026{mes}.csv"
  )))

  tbl_projecao_municipio <- get_tbl(sf_municipios, "code_muni")

  readr::write_csv(
    tbl_projecao_municipio, here::here(tidy, stringr::str_glue(
      "tbl_projecao_municipio_2026{mes}.csv"
    ))
  )
}


create_tables("11")
create_tables("12")


