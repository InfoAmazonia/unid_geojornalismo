library(tidymodels)
library(multilevelmod)

dd <- here::here("2026_09-eleicoes_em_mapas/ep_01/dados")

presidente <- readr::read_csv(here::here(dd, "presidente.csv"))
agro <- readr::read_csv(here::here(dd, "agro.csv"))
agro_s <- agro |>
  dplyr::distinct(cod, ano, valor_real) |>
  dplyr::mutate(cod = as.numeric(cod), ano = as.numeric(ano))

tbl_agro_nota <- presidente |>
  dplyr::distinct(id_municipio, nota_eleitorado, ano) |>
  dplyr::inner_join(agro_s, dplyr::join_by(id_municipio == cod, ano)) |>
  dplyr::filter(!is.na(valor_real))

tbl_agro_nota <- tbl_agro_nota |>
  dplyr::mutate(
    ano = as.factor(ano),
    valor_bilhoes = valor_real / 1e9
  )

lmer_spec <- linear_reg() |>
  set_engine("lmer")

lme_spec <-
  linear_reg() |>
  set_engine("lme", random = ~ 1 | id_municipio)

lme_fit <-
  lme_spec |>
  fit(nota_eleitorado ~ valor_bilhoes + ano, data = tbl_agro_nota)

summary(lme_fit$fit)


# Área

area <- readr::read_csv(here::here(dd, "agro_area.csv"))
area_s <- area |>
  dplyr::distinct(cod, ano, x6) |>
  dplyr::mutate(cod = as.numeric(cod), ano = as.numeric(ano))

tbl_area_nota <- presidente |>
  dplyr::distinct(id_municipio, nota_eleitorado, ano) |>
  dplyr::inner_join(area_s, dplyr::join_by(id_municipio == cod, ano)) |>
  dplyr::filter(!is.na(x6))

tbl_area_nota <- tbl_area_nota |>
  dplyr::mutate(
    ano = as.factor(ano),
    x6 = as.numeric(x6),
    km2 = x6 / 100,
    logkm2 = log(km2)
  ) |>
  dplyr::filter(logkm2 != -Inf)


lmer_spec <- linear_reg() |>
  set_engine("lmer")

lme_spec <-
  linear_reg() |>
  set_engine("lme", random = ~ 1 | id_municipio)

lme_fit <-
  lme_spec |>
  fit(nota_eleitorado ~ logkm2 + ano, data = tbl_area_nota)

summary(lme_fit$fit)
