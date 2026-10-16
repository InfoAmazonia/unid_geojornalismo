dd <- here::here("2026_09-el_nino/dados/raw")
tidy <- here::here("2026_09-el_nino/dados/tidy")
if (!dir.exists(tidy)) dir.create(tidy)

prev <- terra::rast(
  here::here(dd, "LABCLIM_AnomaliasChuvaTemperatura", "PREVISAO-CHUVA-TEMP.nc")
)
prev <- terra::rotate(prev)

fator_precip <- 2628000000
fator_temp <- 273.15

# Recorte Amazônia Legal
geral <- here::here("2026_09-el_nino/dados/raw")
municipios_amazonia <- readxl::read_excel(
  here::here(geral, "amazonia_legal.xls")
)
amazonia_legal <- geobr::read_amazon(year = 2024)

sf_municipios <- geobr::read_municipality(year = 2024) |>
  dplyr::filter(code_muni %in% municipios_amazonia$CD_MUN) |>
  sf::st_intersection(amazonia_legal)

sf_ufs <- geobr::read_state(year = 2024) |>
  dplyr::filter(abbrev_state %in% unique(municipios_amazonia$SIGLA_UF)) |>
  sf::st_intersection(amazonia_legal)

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

# Crop por UF

ufs <- sf_ufs |>
  terra::vect() |>
  terra::project(terra::crs(prev))




get_tbl_uf <- function(uf) {
  masked <- terra::crop(prev, ufs[ufs$abbrev_state == uf], mask = TRUE)
  masked |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(
      cols = dplyr::everything(), names_to = "layer", values_to = "value"
    ) |>
    dplyr::left_join(tempo_variavel, by = "layer") |>
    dplyr::mutate(
      variavel = dplyr::case_when(
        stringr::str_detect(layer, "T2M") ~ "temperatura_2m",
        stringr::str_detect(layer, "TP") ~ "precipitacao_total_m"
      ),
      uf = uf
    ) |>
    dplyr::select(-layer)
}

tbl_projecao_uf <- purrr::map_dfr(
  ufs$abbrev_state, get_tbl_uf
)

tbl_projecao_uf <- tbl_projecao_uf |>
  dplyr::mutate(
    value = dplyr::case_when(
      variavel == "precipitacao_total_m" ~ value * fator_precip,
      TRUE ~ value - fator_temp
    ),
    variavel = dplyr::case_when(
      variavel == "precipitacao_total_m" ~ "precipitacao_total_mm",
      variavel == "temperatura_2m" ~ "temperatura_convertida_c",
      TRUE ~ variavel
    )
  )

readr::write_csv(tbl_projecao_uf, here::here(tidy, "tbl_projecao_uf.csv"))

# Crop por município

get_tbl_municipio <- function(municipio) {
  shp_municipio <- sf_municipios |>
    dplyr::filter(code_muni == municipio) |>
    terra::vect() |>
    terra::project(terra::crs(prev))
  masked <- terra::crop(
    prev, shp_municipio, mask = TRUE
  )
  masked |>
    tibble::as_tibble() |>
    tidyr::pivot_longer(
      cols = dplyr::everything(), names_to = "layer", values_to = "value"
    ) |>
    dplyr::left_join(tempo_variavel, by = "layer") |>
    dplyr::mutate(
      variavel = dplyr::case_when(
        stringr::str_detect(layer, "T2M") ~ "temperatura_2m",
        stringr::str_detect(layer, "TP") ~ "precipitacao_total_m"
      ),
      code_muni = municipio
    ) |>
    dplyr::select(-layer)
}

tbl_projecao_municipio <- purrr::map_dfr(
  sf_municipios$code_muni, get_tbl_municipio
)
tbl_projecao_municipio <- tbl_projecao_municipio |>
  dplyr::mutate(
    value = dplyr::case_when(
      variavel == "precipitacao_total_m" ~ value * fator_precip,
      TRUE ~ value - fator_temp
    ),
    variavel = dplyr::case_when(
      variavel == "precipitacao_total_m" ~ "precipitacao_total_mm",
      variavel == "temperatura_2m" ~ "temperatura_convertida_c",
      TRUE ~ variavel
    )
  )

readr::write_csv(
  tbl_projecao_municipio, here::here(tidy, "tbl_projecao_municipio.csv")
)


# Dados observados

bacia_am <- sf::read_sf(here::here(dd, "RAISG_Cuencas_2025")) |>
  terra::vect() |>
  terra::project(terra::crs(prev))

obs_temp_09_bacia <- here::here(
  dd, "LABCLIM_AnomaliasChuvaTemperatura",
  "OBSERVADO-TEMP-Anomalia_Mensal_202609.nc"
) |>
  terra::rast() |>
  terra::crop(bacia_am, mask = TRUE)

# Anomalias de temperatura positiva na bacia amazonica
here::here(
  dd, "LABCLIM_AnomaliasChuvaTemperatura",
  "OBSERVADO-TEMP-Anomalia_Mensal_202606.nc"
) |>
  terra::rast() |>
  terra::crop(bacia_am, mask = TRUE) |>
  tibble::as_tibble() |>
  dplyr::count(anomalia_tmed >= 0) |>
  janitor::adorn_percentages("col") |>
  janitor::adorn_pct_formatting()

# check visual
# terra::plot(obs_temp_09_bacia)
# terra::plot(ufs, add = TRUE)

obs_precip_09 <- here::here(
  dd, "LABCLIM_AnomaliasChuvaTemperatura",
  "OBSERVADO-CHUVA-Anomalia_Mensal_202609.nc"
) |>
  terra::rast()

obs_precip_09_bacia <- obs_precip_09 |>
  terra::crop(bacia_am, mask = TRUE)

terra::writeCDF(
  obs_precip_09_bacia,
  here::here(tidy, "anomalias_precip_09.nc")
)
# terra::plot(obs_precip_09_bacia)
# terra::plot(ufs, add = TRUE)

tbl_precip_09_municipios <- sf_municipios$code_muni |>
  purrr::map_dfr(
    \(mun) {
      muni <- sf_municipios |>
        dplyr::filter(code_muni == mun)
      terra::crop(
        obs_precip_09,
        terra::project(terra::vect(muni), terra::crs(obs_precip_09)),
        mask = TRUE
      ) |>
        tibble::as_tibble() |>
        dplyr::mutate(
          code_muni = mun, municipio = muni$name_muni, uf = muni$abbrev_state
        )
    }
  )

readr::write_csv(tbl_precip_09_municipios, here::here(tidy, "tbl_precip_09_municipios.csv"))


# Normais meteorológicas INMET (1991-2020) - precipitação acumulada mensal
url <- "https://apiclima.inmet.gov.br/NormaisClimatologicas/1991-2020/prec_acum_mes"

resp <- httr2::request(url) |>
  httr2::req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0 Safari/537.36") |>
  httr2::req_headers(
    Accept  = "application/json, text/plain, */*",
    Origin  = "https://clima.inmet.gov.br",
    Referer = "https://clima.inmet.gov.br/"
  ) |>
  httr2::req_options(http_version = 2L) |>   # força HTTP/1.1 (CURL_HTTP_VERSION_1_1)
  httr2::req_retry(max_tries = 3) |>
  httr2::req_perform()

httr2::resp_status(resp)
mapas <- httr2::resp_body_json(resp)

save_normal_png <- function(x, i, dir) {
  if (!dir.exists(dir)) dir.create(dir)
  mes <- purrr::pluck(x, "mes")
  b64 <- x |>
    purrr::pluck("base64") |>
    stringr::str_remove("^data:image/png;base64,")
  writeBin(
    jsonlite::base64_dec(b64),
    file.path(dir, paste0("normal_inmet_", mes, ".png"))
  )
}
purrr::iwalk(mapas, \(x, y) save_normal_png(x, y, here::here(dd, "normais_inmet_temp")))


geotif_normais_precip <- tidy |>
  here::here("normais_inmet_precip") |>
  fs::dir_ls(regexp = "normal_inmet_1.\\.tif$") |>
  purrr::map(terra::rast)

terra::plot(geotif_normais_precip[[1]])

br <- geobr::read_country(year = 2025) |>
  terra::vect() |>
  terra::project(terra::crs(geotif_normais_precip[[1]]))

geotif_normais_precip[[1]] |>
  terra::crop(br, mask = TRUE) |>
  terra::plot()


# Normais meteorológicas INMET (1991-2020) - temperatura

url <- "https://apiclima.inmet.gov.br/NormaisClimatologicas/1991-2020/tmedia_mes"

