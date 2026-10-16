# Recortes na Pan-Amazônia para dataviz. Depende de 4_comparacao_normais.r
# (anomalias projetadas em dados/tidy/comparacao_normais).

dd <- here::here("2026_09-el_nino/dados/raw")
tidy <- here::here("2026_09-el_nino/dados/tidy")

prev <- terra::rast(
  here::here(dd, "LABCLIM_AnomaliasChuvaTemperatura", "PREVISAO-CHUVA-TEMP.nc")
)
prev <- terra::rotate(prev)

fator_precip <- 2628000000

tempo_variavel <- tibble::tibble(
  layer = names(prev), mes = terra::time(prev)
) |>
  dplyr::mutate(
    mes_ord = as.integer(stringr::str_extract(layer, "[0-9]+$")),
    mes_actual = lubridate::add_with_rollback(
      lubridate::ym("2026-09"), months(mes_ord)
    ),
    mes_label = lubridate::month(
      mes_actual, label = TRUE, abbr = TRUE, locale = "pt_BR"
    )
  ) |>
  dplyr::distinct()

bacia_am <- sf::read_sf(here::here(dd, "RAISG_Cuencas_2025")) |>
  terra::vect() |>
  terra::project(terra::crs(prev))


precipitacao_total_previsao_outubro <- prev$TP_1 |>
  terra::crop(bacia_am, mask = TRUE)
terra::time(precipitacao_total_previsao_outubro) <- tempo_variavel |>
  dplyr::filter(layer == "TP_1") |>
  dplyr::pull(mes_actual)
precipitacao_total_previsao_outubro <- precipitacao_total_previsao_outubro * fator_precip
terra::writeCDF(
  precipitacao_total_previsao_outubro,
  here::here(tidy, "precipitacao_total_previsao_outubro.nc"), overwrite = TRUE
)
precipitacao_total_previsao_novembro <- prev$TP_2 |>
  terra::crop(bacia_am, mask = TRUE)
terra::time(precipitacao_total_previsao_novembro) <- tempo_variavel |>
  dplyr::filter(layer == "TP_2") |>
  dplyr::pull(mes_actual)
precipitacao_total_previsao_novembro <- precipitacao_total_previsao_novembro * fator_precip
terra::writeCDF(
  precipitacao_total_previsao_novembro,
  here::here(tidy, "precipitacao_total_previsao_novembro.nc"), overwrite = TRUE
)
precipitacao_total_previsao_dezembro <- prev$TP_3 |>
  terra::crop(bacia_am, mask = TRUE)
terra::time(precipitacao_total_previsao_dezembro) <- tempo_variavel |>
  dplyr::filter(layer == "TP_3") |>
  dplyr::pull(mes_actual)
precipitacao_total_previsao_dezembro <- precipitacao_total_previsao_dezembro * fator_precip
terra::writeCDF(
  precipitacao_total_previsao_dezembro,
  here::here(tidy, "precipitacao_total_previsao_dezembro.nc"), overwrite = TRUE
)


anomalia_precipitacao_previsto <- tidy |>
  here::here("comparacao_normais", "anomalia_mm.tif") |>
  terra::rast()
terra::plot(anomalia_precipitacao_previsto)

anomalia_temperatura_previsto <- tidy |>
  here::here("comparacao_normais", "anomalia_temp_c.tif") |>
  terra::rast()

# Anomalia precipitação outubro
anomalia_precipitacao_outubro <- anomalia_precipitacao_previsto$`2026-10`
terra::time(anomalia_precipitacao_outubro) <- tempo_variavel |>
  dplyr::filter(layer == "TP_1") |>
  dplyr::pull(mes_actual)
terra::writeCDF(
  anomalia_precipitacao_outubro,
  here::here(tidy, "anomalia_precipitacao_outubro.nc"),
  overwrite = TRUE
)
# Anomalia precipitação novembro
anomalia_precipitacao_novembro <- anomalia_precipitacao_previsto$`2026-11`
terra::time(anomalia_precipitacao_novembro) <- tempo_variavel |>
  dplyr::filter(layer == "TP_2") |>
  dplyr::pull(mes_actual)
terra::writeCDF(
  anomalia_precipitacao_novembro,
  here::here(tidy, "anomalia_precipitacao_novembro.nc"),
  overwrite = TRUE
)
# Anomalia precipitação dezembro
anomalia_precipitacao_dezembro <- anomalia_precipitacao_previsto$`2026-12`
terra::time(anomalia_precipitacao_dezembro) <- tempo_variavel |>
  dplyr::filter(layer == "TP_3") |>
  dplyr::pull(mes_actual)
terra::writeCDF(
  anomalia_precipitacao_dezembro,
  here::here(tidy, "anomalia_precipitacao_dezembro.nc"),
  overwrite = TRUE
)

# Anomalia temperatura outubro
anomalia_temperatura_outubro <- anomalia_temperatura_previsto$`2026-10`
terra::time(anomalia_temperatura_outubro) <- tempo_variavel |>
  dplyr::filter(layer == "T2M_1") |>
  dplyr::pull(mes_actual)
terra::writeCDF(
  anomalia_temperatura_outubro,
  here::here(tidy, "anomalia_temperatura_outubro.nc"),
  overwrite = TRUE
)
# Anomalia temperatura novembro
anomalia_temperatura_novembro <- anomalia_temperatura_previsto$`2026-11`
terra::time(anomalia_temperatura_novembro) <- tempo_variavel |>
  dplyr::filter(layer == "T2M_2") |>
  dplyr::pull(mes_actual)
terra::writeCDF(
  anomalia_temperatura_novembro,
  here::here(tidy, "anomalia_temperatura_novembro.nc"),
  overwrite = TRUE
)
# Anomalia temperatura dezembro
anomalia_temperatura_dezembro <- anomalia_temperatura_previsto$`2026-12`
terra::time(anomalia_temperatura_dezembro) <- tempo_variavel |>
  dplyr::filter(layer == "T2M_3") |>
  dplyr::pull(mes_actual)
terra::writeCDF(
  anomalia_temperatura_dezembro,
  here::here(tidy, "anomalia_temperatura_dezembro.nc"),
  overwrite = TRUE
)

# Anomalias observadas temperatura: setembro, agosto, julho
anomalia_observado_temperatura_setembro <- here::here(
  dd, "LABCLIM_AnomaliasChuvaTemperatura",
  "OBSERVADO-TEMP-Anomalia_Mensal_202609.nc"
) |>
  terra::rast() |>
  terra::crop(bacia_am, mask = TRUE)
terra::writeCDF(
  anomalia_observado_temperatura_setembro,
  here::here(tidy, "anomalia_observado_temperatura_setembro.nc"),
  overwrite = TRUE
)
anomalia_observado_temperatura_agosto <- here::here(
  dd, "LABCLIM_AnomaliasChuvaTemperatura",
  "OBSERVADO-TEMP-Anomalia_Mensal_202608.nc"
) |>
  terra::rast() |>
  terra::crop(bacia_am, mask = TRUE)
terra::writeCDF(
  anomalia_observado_temperatura_agosto,
  here::here(tidy, "anomalia_observado_temperatura_agosto.nc"),
  overwrite = TRUE
)
anomalia_observado_temperatura_julho <- here::here(
  dd, "LABCLIM_AnomaliasChuvaTemperatura",
  "OBSERVADO-TEMP-Anomalia_Mensal_202607.nc"
) |>
  terra::rast() |>
  terra::crop(bacia_am, mask = TRUE)
terra::writeCDF(
  anomalia_observado_temperatura_julho,
  here::here(tidy, "anomalia_observado_temperatura_julho.nc"),
  overwrite = TRUE
)

# Anomalias observadas precipitacao: setembro, agosto, julho
anomalia_observado_precipitacao_setembro <- here::here(
  dd, "LABCLIM_AnomaliasChuvaTemperatura",
  "OBSERVADO-CHUVA-Anomalia_Mensal_202609.nc"
) |>
  terra::rast() |>
  terra::crop(bacia_am, mask = TRUE)
terra::writeCDF(
  anomalia_observado_precipitacao_setembro,
  here::here(tidy, "anomalia_observado_precipitacao_setembro.nc"),
  overwrite = TRUE
)
anomalia_observado_precipitacao_agosto <- here::here(
  dd, "LABCLIM_AnomaliasChuvaTemperatura",
  "OBSERVADO-CHUVA-Anomalia_Mensal_202608.nc"
) |>
  terra::rast() |>
  terra::crop(bacia_am, mask = TRUE)
terra::writeCDF(
  anomalia_observado_precipitacao_agosto,
  here::here(tidy, "anomalia_observado_precipitacao_agosto.nc"),
  overwrite = TRUE
)
anomalia_observado_precipitacao_julho <- here::here(
  dd, "LABCLIM_AnomaliasChuvaTemperatura",
  "OBSERVADO-CHUVA-Anomalia_Mensal_202607.nc"
) |>
  terra::rast() |>
  terra::crop(bacia_am, mask = TRUE)
terra::writeCDF(
  anomalia_observado_precipitacao_julho,
  here::here(tidy, "anomalia_observado_precipitacao_julho.nc"),
  overwrite = TRUE
)
