mapbiomas_tidy <- readr::read_csv("202408_amacro/data-tidy/mapbiomas_tidy.csv")

pred_mun <- function(mun, fat_flo = 2, fat_pasto = 3) {
  ac <- mapbiomas_tidy |>
    dplyr::filter(nm_municip == mun, classe %in% c("3.1. Pastagem", "1. Floresta")) |>
    dplyr::mutate(ano = as.numeric(lubridate::year(ano))) |>
    dplyr::group_by(ano, classe) |>
    dplyr::summarise(area = sum(area), .groups = "drop") |>
    dplyr::mutate(tipo = "observado")
  df_ac <- tibble::tibble(ano = 2022:2070)
  fit_ac <- lm(area^fat_flo ~ ano, data = dplyr::filter(ac, classe == "1. Floresta"))
  summary(fit_ac)
  pred <- predict(fit_ac, df_ac)
  df <- df_ac |>
    dplyr::mutate(area = pred^(1/fat_flo), tipo = "predição", classe = "1. Floresta") |>
    dplyr::bind_rows(ac)
  fit_ac <- lm(area^(1/fat_pasto) ~ ano, data = dplyr::filter(ac, classe == "3.1. Pastagem"))
  pred <- predict(fit_ac, df_ac)
  df <- df_ac |>
    dplyr::mutate(area = pred^fat_pasto, tipo = "predição", classe = "3.1. Pastagem") |>
    dplyr::bind_rows(df)
  df
}

plot_preds <- function(mun, ...) {
  pred_mun(mun, ...) |>
    ggplot2::ggplot(ggplot2::aes(x = ano, y = area, linetype = tipo, color = classe)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = mun)
}

munis <- unique(mapbiomas_tidy$nm_municip)

munis[[1]] |>
  plot_preds(fat_flo = 10, fat_pasto = 3)
  # pred_mun(fat_flo = 10, fat_pasto = 3) |>
  # tidyr::pivot_wider(names_from = classe, values_from = area) |>
  # dplyr::filter(`3.1. Pastagem` >= `1. Floresta`)
munis_preds <- munis[[3]] |>
  # plot_preds(fat_flo = .75, fat_pasto = .75)
  pred_mun(fat_flo = .75, fat_pasto = .75) |>
  tidyr::pivot_wider(names_from = classe, values_from = area) |>
  dplyr::filter(`3.1. Pastagem` >= `1. Floresta`) |>
  dplyr::slice_min(ano) |>
  dplyr::mutate(muni = munis[[3]])
munis_preds <- munis[[4]] |>
  # plot_preds(fat_flo = 1.7, fat_pasto = 1.8)
  pred_mun(fat_flo = 1.7, fat_pasto = 1.8) |>
  tidyr::pivot_wider(names_from = classe, values_from = area) |>
  dplyr::filter(`3.1. Pastagem` >= `1. Floresta`) |>
  dplyr::slice_min(ano) |>
  dplyr::mutate(muni = munis[[4]]) |>
  dplyr::bind_rows(munis_preds)
munis_preds <- munis[[9]] |>
  # plot_preds(fat_flo = 1.5, fat_pasto = 2)
  pred_mun(fat_flo = 1.5, fat_pasto = 2) |>
  tidyr::pivot_wider(names_from = classe, values_from = area) |>
  dplyr::filter(`3.1. Pastagem` >= `1. Floresta`) |>
  dplyr::slice_min(ano) |>
  dplyr::mutate(muni = munis[[9]]) |>
  dplyr::bind_rows(munis_preds)
munis[[11]] |>
  # pred_mun(fat_flo = 5, fat_pasto = 2) |>
  # tidyr::pivot_wider(names_from = classe, values_from = area) |>
  # dplyr::filter(`3.1. Pastagem` >= `1. Floresta`)
  plot_preds(fat_flo = 5, fat_pasto = 2)
munis_preds <- munis[[13]] |>
  # plot_preds(fat_flo = 1.2, fat_pasto = 1.5)
  pred_mun(fat_flo = 1.2, fat_pasto = 1.5) |>
  tidyr::pivot_wider(names_from = classe, values_from = area) |>
  dplyr::filter(`3.1. Pastagem` >= `1. Floresta`) |>
  dplyr::slice_min(ano) |>
  dplyr::mutate(muni = munis[[13]]) |>
  dplyr::bind_rows(munis_preds)
munis[[15]] |>
  plot_preds(fat_flo = 10, fat_pasto = 3)
  # pred_mun(fat_flo = 10, fat_pasto = 3) |>
  # tidyr::pivot_wider(names_from = classe, values_from = area) |>
  # dplyr::filter(`3.1. Pastagem` >= `1. Floresta`)
munis[[17]] |>
  plot_preds(fat_flo = 5, fat_pasto = 2)
  # pred_mun(fat_flo = 5, fat_pasto = 2) |>
  # tidyr::pivot_wider(names_from = classe, values_from = area) |>
  # dplyr::filter(`3.1. Pastagem` >= `1. Floresta`)
munis_preds <- munis[[18]] |>
  # plot_preds(fat_flo = 2, fat_pasto = 2)
  pred_mun(fat_flo = 2, fat_pasto = 2) |>
  tidyr::pivot_wider(names_from = classe, values_from = area) |>
  dplyr::filter(`3.1. Pastagem` >= `1. Floresta`) |>
  dplyr::slice_min(ano) |>
  dplyr::mutate(muni = munis[[18]]) |>
  dplyr::bind_rows(munis_preds)
munis[[19]] |>
  plot_preds(fat_flo = 5, fat_pasto = 2)
  # pred_mun(fat_flo = 5, fat_pasto = 2) |>
  # tidyr::pivot_wider(names_from = classe, values_from = area) |>
  # dplyr::filter(`3.1. Pastagem` >= `1. Floresta`)
munis[[20]] |>
  plot_preds(fat_flo = 5, fat_pasto = 2)
  # pred_mun(fat_flo = 5, fat_pasto = 2) |>
  # tidyr::pivot_wider(names_from = classe, values_from = area) |>
  # dplyr::filter(`3.1. Pastagem` >= `1. Floresta`)
munis[[21]] |>
  plot_preds(fat_flo = 5, fat_pasto = 2)
  # pred_mun(fat_flo = 5, fat_pasto = 2) |>
  # tidyr::pivot_wider(names_from = classe, values_from = area) |>
  # dplyr::filter(`3.1. Pastagem` >= `1. Floresta`)
munis_preds <- munis[[22]] |>
  # plot_preds(fat_flo = 1.5, fat_pasto = 2)
  pred_mun(fat_flo = 1.5, fat_pasto = 2) |>
  tidyr::pivot_wider(names_from = classe, values_from = area) |>
  dplyr::filter(`3.1. Pastagem` >= `1. Floresta`) |>
  dplyr::slice_min(ano) |>
  dplyr::mutate(muni = munis[[22]]) |>
  dplyr::bind_rows(munis_preds)
munis_preds <- munis[[24]] |>
  # plot_preds(fat_flo = .75, fat_pasto = 1)
  pred_mun(fat_flo = .75, fat_pasto = 1) |>
  tidyr::pivot_wider(names_from = classe, values_from = area) |>
  dplyr::filter(`3.1. Pastagem` >= `1. Floresta`) |>
  dplyr::slice_min(ano) |>
  dplyr::mutate(muni = munis[[24]]) |>
  dplyr::bind_rows(munis_preds)
munis_preds <- munis[[27]] |>
  # plot_preds(fat_flo = 1.7, fat_pasto = 1.7)
  pred_mun(fat_flo = 1.7, fat_pasto = 1.7) |>
  tidyr::pivot_wider(names_from = classe, values_from = area) |>
  dplyr::filter(`3.1. Pastagem` >= `1. Floresta`) |>
  dplyr::slice_min(ano) |>
  dplyr::mutate(muni = munis[[27]]) |>
  dplyr::bind_rows(munis_preds)
munis[[29]] |>
  plot_preds(fat_flo = 2, fat_pasto = 2)
  # pred_mun(fat_flo = 2, fat_pasto = 2) |>
  # tidyr::pivot_wider(names_from = classe, values_from = area) |>
  # dplyr::filter(`3.1. Pastagem` >= `1. Floresta`)
munis_preds <- munis[[30]] |>
  pred_mun(fat_flo = 10, fat_pasto = 3) |>
  tidyr::pivot_wider(names_from = classe, values_from = area) |>
  dplyr::filter(`3.1. Pastagem` >= `1. Floresta`) |>
  # plot_preds(fat_flo = 10, fat_pasto = 3) |>
  dplyr::mutate(muni = munis[[30]]) |>
  dplyr::bind_rows(munis_preds)
munis_preds <- munis[[31]] |>
  pred_mun(fat_flo = 10, fat_pasto = 3) |>
  tidyr::pivot_wider(names_from = classe, values_from = area) |>
  dplyr::filter(`3.1. Pastagem` >= `1. Floresta`) |>
  dplyr::mutate(muni = munis[[31]]) |>
  dplyr::bind_rows(munis_preds)
  # plot_preds(fat_flo = 5, fat_pasto = 2) |
munis_preds <- munis[[32]] |>
  # plot_preds(fat_flo = 1.5, fat_pasto = 2)
  pred_mun(fat_flo = 1.5, fat_pasto = 2) |>
  tidyr::pivot_wider(names_from = classe, values_from = area) |>
  dplyr::filter(`3.1. Pastagem` >= `1. Floresta`) |>
  dplyr::slice_min(ano) |>
  dplyr::mutate(muni = munis[[31]]) |>
  dplyr::bind_rows(munis_preds)

readr::write_csv(munis_preds, "202408_amacro/data-tidy/predicoes_muni.csv")
