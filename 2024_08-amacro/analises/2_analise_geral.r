mapbiomas_tidy <- readr::read_csv("202408_amacro/data-tidy/mapbiomas_tidy.csv")

# TOTAL DA REGIÃO
mapbiomas_tidy |>
  dplyr::group_by(classe_principal, ano) |>
  dplyr::summarise(area = sum(area), .groups = "drop") |>
  ggplot2::ggplot(ggplot2::aes(x = ano, y = area, color = classe_principal)) +
  ggplot2::geom_line()

## Comparação Floresta e Agropecuária
comp_floresta_agro <- mapbiomas_tidy |>
  dplyr::group_by(classe_principal, ano) |>
  dplyr::summarise(area = sum(area), .groups = "drop") |>
  dplyr::filter(classe_principal %in% c("1. Floresta", "3. Agropecuária")) |>
  tidyr::pivot_wider(names_from = classe_principal, values_from = area) |>
  dplyr::mutate(
    ano = lubridate::year(ano),
    variacao_floresta = `1. Floresta` / dplyr::lag(`1. Floresta`) - 1,
    variacao_agropecuaria = `3. Agropecuária` / dplyr::lag(`3. Agropecuária`) - 1
  )
readr::write_csv(comp_floresta_agro, "202408_amacro/data-tidy/comp_floresta_agro.csv")


# PREDIÇÃO PASTAGEM > FLORESTA

comp_pasto_floresta <- mapbiomas_tidy |>
  dplyr::filter(classe %in% c("3.1. Pastagem", "1. Floresta")) |>
  dplyr::group_by(ano, classe) |>
  dplyr::summarise(area = sum(area), .groups = "drop") |>
  dplyr::mutate(ano = as.numeric(lubridate::year(ano)))

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
fit1 <- glm(sqrt(area) ~ ano, data = df_pasto)
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

