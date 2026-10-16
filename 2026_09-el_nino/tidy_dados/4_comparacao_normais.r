dd <- here::here("2026_09-el_nino/dados/raw")
tidy <- here::here("2026_09-el_nino/dados/tidy")
dir_comp <- here::here(tidy, "comparacao_normais")
if (!dir.exists(dir_comp)) dir.create(dir_comp)

# Previsão ECMWF -------------------------------------------------------------
# TP vem como taxa média (m/s) e T2M em K. Camada 1 = outubro/2026 ...
# camada 6 = março/2027, como em 1_dados_tbl.r.
mes_inicial <- lubridate::ym("2026-10")

# br <- geobr::read_country(year = 2020, showProgress = FALSE) |>
#   terra::vect() |>
#   terra::project("EPSG:4326")
br <- sf::read_sf(here::here(dd, "RAISG_Cuencas_2025")) |>
  terra::vect() |>
  terra::project("EPSG:4326")


prev <- terra::rast(here::here(
  dd, "LABCLIM_AnomaliasChuvaTemperatura", "PREVISAO-CHUVA-TEMP.nc"
))
prev <- terra::rotate(prev)
tp <- prev[[grep("^TP", names(prev))]] |>
  terra::crop(br, snap = "out")

meses <- mes_inicial + months(seq_len(terra::nlyr(tp)) - 1)
dias <- lubridate::days_in_month(meses)
prev_mm <- tp * (86400 * 1000 * dias) # m/s -> mm/mês
names(prev_mm) <- format(meses, "%Y-%m")

prev_temp <- prev[[grep("^T2M", names(prev))]] |>
  terra::crop(br, snap = "out")
prev_temp <- prev_temp - 273.15 # K -> °C
names(prev_temp) <- names(prev_mm)

# Normais INMET 1991-2020 ----------------------------------------------------
# Rasters categóricos (3_normais_inmet.r): cada classe vira o ponto médio do
# intervalo. A última classe é aberta: >420 mm recebe 460 mm e >30 °C, 31 °C.
quebras_precip <- c(0, 10, 20, 30, 40, 60, 80, 100, 140, 180, 220, 260, 300,
                    340, 380, 420, 500)
quebras_temp <- seq(8, 32, by = 2)
meio_classe <- function(quebras) {
  (utils::head(quebras, -1) + quebras[-1]) / 2
}

ler_normal <- function(variavel, mes, quebras) {
  meio <- meio_classe(quebras)
  arq <- here::here(
    tidy, paste0("normais_inmet_", variavel),
    sprintf("normal_inmet_%02d.tif", mes)
  )
  terra::rast(arq) |>
    terra::as.int() |>
    terra::classify(cbind(seq_along(meio), meio)) |>
    # média dos pixels (~5 km) dentro de cada célula de 1° da previsão
    terra::resample(prev_mm, method = "average")
}

normal_mm <- lubridate::month(meses) |>
  purrr::map(\(m) ler_normal("precip", m, quebras_precip)) |>
  terra::rast()
names(normal_mm) <- names(prev_mm)

normal_temp <- lubridate::month(meses) |>
  purrr::map(\(m) ler_normal("temp", m, quebras_temp)) |>
  terra::rast()
names(normal_temp) <- names(prev_temp)

# Comparação: precipitação ---------------------------------------------------
normal_mm <- terra::mask(normal_mm, br)
prev_mm <- terra::mask(prev_mm, normal_mm)
anomalia_mm <- prev_mm - normal_mm
anomalia_pct <- 100 * anomalia_mm / normal_mm

purrr::iwalk(
  list(
    previsao_mm = prev_mm, normal_mm = normal_mm,
    anomalia_mm = anomalia_mm, anomalia_pct = anomalia_pct
  ),
  \(r, nome) terra::writeRaster(
    r, here::here(dir_comp, paste0(nome, ".tif")), overwrite = TRUE
  )
)

# Comparação: temperatura ----------------------------------------------------
normal_temp <- terra::mask(normal_temp, br)
prev_temp <- terra::mask(prev_temp, normal_temp)
anomalia_temp <- prev_temp - normal_temp

purrr::iwalk(
  list(
    previsao_temp_c = prev_temp, normal_temp_c = normal_temp,
    anomalia_temp_c = anomalia_temp
  ),
  \(r, nome) terra::writeRaster(
    r, here::here(dir_comp, paste0(nome, ".tif")), overwrite = TRUE
  )
)

# Tabelas: Amazônia Legal, por UF e município --------------------------------
# Média ponderada pela fração de cada célula de 1° dentro do polígono.
geral <- here::here("2026_09-el_nino/dados/raw")
municipios_amazonia <- readxl::read_excel(
  here::here(geral, "amazonia_legal.xls")
)
amazonia_legal <- geobr::read_amazon(year = 2024)

sf_municipios <- geobr::read_municipality(year = 2024) |>
  dplyr::filter(code_muni %in% municipios_amazonia$CD_MUN) |>
  sf::st_intersection(amazonia_legal) |>
  # interseções que viram GEOMETRYCOLLECTION se perdem no terra::vect()
  sf::st_collection_extract("POLYGON")

sf_ufs <- geobr::read_state(year = 2024) |>
  dplyr::filter(abbrev_state %in% unique(municipios_amazonia$SIGLA_UF)) |>
  sf::st_intersection(amazonia_legal)

extrair_media <- function(sf_poly, id_cols, rasters) {
  v <- terra::project(terra::vect(sf_poly), "EPSG:4326")
  extrair <- function(r, nome) {
    terra::extract(r, v, fun = mean, weights = TRUE, na.rm = TRUE,
                   ID = FALSE) |>
      dplyr::bind_cols(sf::st_drop_geometry(sf_poly)[id_cols]) |>
      tidyr::pivot_longer(
        -dplyr::all_of(id_cols), names_to = "mes", values_to = nome
      )
  }
  purrr::imap(rasters, extrair) |>
    purrr::reduce(dplyr::left_join, by = c(id_cols, "mes"))
}

extrair_precip <- function(sf_poly, id_cols) {
  extrair_media(
    sf_poly, id_cols, list(previsao_mm = prev_mm, normal_mm = normal_mm)
  ) |>
    dplyr::mutate(
      anomalia_mm = previsao_mm - normal_mm,
      anomalia_pct = 100 * anomalia_mm / normal_mm
    )
}

extrair_temp <- function(sf_poly, id_cols) {
  extrair_media(
    sf_poly, id_cols, list(previsao_c = prev_temp, normal_c = normal_temp)
  ) |>
    dplyr::mutate(anomalia_c = previsao_c - normal_c)
}

id_uf <- "abbrev_state"
id_municipio <- c("code_muni", "name_muni", "abbrev_state")

readr::write_csv(
  extrair_precip(sf_ufs, id_uf),
  here::here(dir_comp, "tbl_comparacao_uf.csv")
)
readr::write_csv(
  extrair_precip(sf_municipios, id_municipio),
  here::here(dir_comp, "tbl_comparacao_municipio.csv")
)
readr::write_csv(
  extrair_temp(sf_ufs, id_uf),
  here::here(dir_comp, "tbl_comparacao_temp_uf.csv")
)
readr::write_csv(
  extrair_temp(sf_municipios, id_municipio),
  here::here(dir_comp, "tbl_comparacao_temp_municipio.csv")
)

# check visual
# terra::plot(anomalia_pct, col = hcl.colors(11, "BrBG"), range = c(-100, 100))
# terra::plot(anomalia_temp, col = rev(hcl.colors(11, "RdBu")), range = c(-5, 5))
