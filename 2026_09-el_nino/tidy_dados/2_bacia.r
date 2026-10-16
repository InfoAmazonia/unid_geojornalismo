dd <- here::here("2026_09-el_nino/dados/raw")

prev <- terra::rast(here::here(
  dd, "LABCLIM_AnomaliasChuvaTemperatura", "PREVISAO-CHUVA-TEMP.nc"
))
prev <- terra::rotate(prev)

bacia_am <- sf::read_sf(here::here(dd, "RAISG_Cuencas_2025")) |>
  terra::vect() |>
  terra::project(terra::crs(prev))

prev_bacia <- terra::crop(prev, bacia_am, mask = TRUE)

fator_precip <- 2628000000
fator_temp <- 273.15

prev_bacia |>
  tibble::as_tibble() |>
  tidyr::pivot_longer(
    cols = dplyr::everything(), names_to = "layer", values_to = "value"
  ) |>
  dplyr::mutate(
    variavel = dplyr::case_when(
      stringr::str_detect(layer, "T2M") ~ "temperatura_2m",
      stringr::str_detect(layer, "TP") ~ "precipitacao_total_m"
    ),
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

prev_bacia





# Observado

# 1. Qual é o percentual do território da Bacia Amazônica (Pan-Amazônia) com anomalia de precipitação mensal observada abaixo de -50mm em setembro?
# 2. Entre as bacias hidrográficas da Pan-Amazônia, qual foi o percentual do território afetado por anomalias de precipitação abaixo de -50mm para cada bacia em setembro?
# 3. Quais estados na Amazônia brasileira foram mais afetados, em proporção ao território, por anomalias de precipitação abaixo de -50 mm em setembro?
# 4. (apenas se for viável no tempo curto) Quantos municípios na Amazônia brasileira foram afetados, em percentual do território, por anomalias de precipitação abaixo de -50 mm em setembro?
# 5. Quais foram as anomalias de precipitação observadas nos municípios de Humaitá, Manicoré, Novo Aripuanã, Borba, Nova Olinda do Norte e Manaus em setembro?
