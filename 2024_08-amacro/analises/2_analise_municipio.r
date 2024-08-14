mapbiomas_tidy <- readr::read_csv("202408_amacro/data-tidy/mapbiomas_tidy.csv")

# Quantidade de municípios por classe predominante
mapbiomas_tidy |>
  dplyr::group_by(geocodigo, nm_municip, nm_estado, classe_principal, ano) |>
  dplyr::summarise(area = sum(area), .groups = "drop") |>
  dplyr::group_by(geocodigo, nm_municip, nm_estado, ano) |>
  dplyr::slice_max(area) |>
  dplyr::ungroup() |>
  dplyr::count(ano, classe_principal) |>
  ggplot2::ggplot(ggplot2::aes(x = ano, y = n, color = classe_principal)) +
  ggplot2::geom_line()

classe_predom_municipios <- mapbiomas_tidy |>
  dplyr::group_by(geocodigo, nm_municip, nm_estado, classe_principal, ano) |>
  dplyr::summarise(area = sum(area), .groups = "drop") |>
  dplyr::group_by(geocodigo, nm_municip, nm_estado, ano) |>
  dplyr::slice_max(area) |>
  dplyr::ungroup() |>
  dplyr::count(ano, classe_principal) |>
  dplyr::mutate(ano = lubridate::year(ano)) |>
  tidyr::pivot_wider(names_from = classe_principal, values_from = n) |>
  tidyr::replace_na(list(`3. Agropecuária` = 0))
readr::write_csv(classe_predom_municipios, "202408_amacro/data-tidy/classe_predom_municipios.csv")


# Comparação de floresta e pastagem por município
## Em municípios em que já tem mais pastagem que floresta, em que ano isso ocorreu
tbl_munis_pastagem <- mapbiomas_tidy |>
  dplyr::filter(classe %in% c("3.1. Pastagem", "1. Floresta")) |>
  dplyr::group_by(ano, classe, geocodigo, nm_municip, nm_estado) |>
  dplyr::summarise(area = sum(area), .groups = "drop") |>
  dplyr::mutate(ano = as.numeric(lubridate::year(ano))) |>
  tidyr::pivot_wider(names_from = classe, values_from = area) |>
  dplyr::filter(`1. Floresta` <= `3.1. Pastagem`) |>
  dplyr::group_by(geocodigo, nm_municip, nm_estado) |>
  dplyr::slice_min(ano) |>
  dplyr::ungroup() |>
  dplyr::arrange(ano)

qt_munis_pastagem <- tbl_munis_pastagem |>
  dplyr::count(ano) |>
  dplyr::mutate(n_acum = cumsum(n))
qt_munis_pastagem |>
  ggplot2::ggplot(ggplot2::aes(x = ano, y = n_acum)) +
  ggplot2::geom_col()
readr::write_csv(qt_munis_pastagem, "202408_amacro/data-tidy/qt_munis_pastagem.csv")

# Percentual da área total de pastagem e de floresta por município
municipios_amacro <- readr::read_csv("202408_amacro/data-raw/AMACRO.csv") |>
  janitor::clean_names()
mapbiomas_wide <- mapbiomas_tidy |>
  dplyr::filter(classe %in% c("3.1. Pastagem", "1. Floresta")) |>
  dplyr::group_by(ano, classe, geocodigo, nm_municip, nm_estado) |>
  dplyr::summarise(area = sum(area), .groups = "drop") |>
  dplyr::mutate(ano = as.numeric(lubridate::year(ano))) |>
  tidyr::pivot_wider(names_from = classe, values_from = area)

pct_2022 <- mapbiomas_wide |>
  dplyr::inner_join(municipios_amacro) |>
  dplyr::mutate(
    pct_floresta = `1. Floresta` / ha,
    pct_pastagem = `3.1. Pastagem` / ha
  ) |>
  dplyr::filter(ano == 2022)
geo <- geobr::read_municipality()
geo |>
  dplyr::inner_join(pct_2022, dplyr::join_by(code_muni == geocodigo)) |>
  ggplot2::ggplot(ggplot2::aes(fill = pct_pastagem)) +
  ggplot2::geom_sf() +
  ggplot2::scale_fill_viridis_c(option = "cividis")


# --------

# PREDIÇÃO PASTAGEM > FLORESTA

comp_pasto_floresta <- mapbiomas_tidy |>
  dplyr::filter(classe %in% c("3.1. Pastagem", "1. Floresta")) |>
  dplyr::group_by(ano, classe, geocodigo, nm_municip) |>
  dplyr::summarise(area = sum(area), .groups = "drop") |>
  dplyr::mutate(ano = as.numeric(lubridate::year(ano)), geocodigo = as.factor(geocodigo))

df_pasto <- comp_pasto_floresta |>
  dplyr::filter(classe == "3.1. Pastagem") |>
  dplyr::mutate(area_sqrt = sqrt(area))
df_pasto |>
  ggplot2::ggplot(ggplot2::aes(x = ano, y = area_sqrt)) +
  ggplot2::geom_line()

df_floresta <- comp_pasto_floresta |>
  dplyr::filter(classe == "1. Floresta")

df_floresta |>
  dplyr::mutate(area = area^3) |>
  ggplot2::ggplot(ggplot2::aes(x = ano, y = area)) +
  ggplot2::geom_line()

## Modelo pasto
fit1 <- glm(sqrt(area) ~ ano + geocodigo, data = df_pasto)
summary(fit1)
plot(fit1)
# Modelo floresta
fit2 <- glm(area^3 ~ ano, data = df_floresta)
summary(fit2)
plot(fit2)
df <- tibble::tibble(ano = 2022:2070)

p <- predict(fit1, df)
preds_pasto <- df |>
  dplyr::mutate(area = p^2, classe = "3.1. Pastagem", tipo = "predição")
p <- predict(fit2, df)
preds_floresta <- df |>
  dplyr::mutate(area = p^(1/3), classe = "1. Floresta", tipo = "predição")
preds <- dplyr::bind_rows(preds_pasto, preds_floresta)

# Tabela com valores observados e predições a partir de 2022
comp_pasto_floresta_pred <- comp_pasto_floresta |>
  dplyr::mutate(tipo = "observado") |>
  dplyr::bind_rows(preds)
readr::write_csv(comp_pasto_floresta_pred, "202408_amacro/data-tidy/comp_pasto_floresta_pred.csv")

## ano em que pastagem ultrapassa floresta
comp_pasto_floresta_pred |>
  dplyr::filter(ano > 2022) |>
  tidyr::pivot_wider(names_from = classe, values_from = area) |>
  dplyr::filter(`3.1. Pastagem` >= `1. Floresta`)
## gráfico das predições ao longo do tempo
comp_pasto_floresta_pred |>
  ggplot2::ggplot(ggplot2::aes(
    x = ano, y = area, color = classe, linetype = tipo
  )) +
  ggplot2::geom_line()

