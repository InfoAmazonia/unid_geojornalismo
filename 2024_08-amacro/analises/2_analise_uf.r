mapbiomas_tidy <- readr::read_csv("202408_amacro/data-tidy/mapbiomas_tidy.csv")
uf <- mapbiomas_tidy |>
  dplyr::filter(classe %in% c("3.1. Pastagem", "1. Floresta")) |>
  dplyr::group_by(ano, nm_estado, classe) |>
  dplyr::summarise(area = sum(area), .groups = "drop")


pred_uf <- function(uf, fat_flo = 2, fat_pasto = 3) {
  ac <- mapbiomas_tidy |>
    dplyr::filter(nm_estado == uf, classe %in% c("3.1. Pastagem", "1. Floresta")) |>
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

# ACRE ----
ac <- pred_uf("ACRE", fat_pasto = 2)
readr::write_csv(ac, "202408_amacro/data-tidy/pred_ac.csv")
  # ggplot2::ggplot(ggplot2::aes(x = ano, y = area, linetype = tipo, color = classe)) +
  # ggplot2::geom_line()

# AMAZONAS ----
am <- pred_uf("AMAZONAS", fat_flo = 3)
readr::write_csv(am, "202408_amacro/data-tidy/pred_am.csv")
  # ggplot2::ggplot(ggplot2::aes(x = ano, y = area, linetype = tipo, color = classe)) +
  # ggplot2::geom_line()

# RONDÔNIA ----
ro <- pred_uf("RONDÔNIA", fat_flo = 1.2, fat_pasto = 1.5)
readr::write_csv(ro, "202408_amacro/data-tidy/pred_ro.csv")
  # ggplot2::ggplot(ggplot2::aes(x = ano, y = area, linetype = tipo, color = classe)) +
  # ggplot2::geom_line()
