get_result_cand <- function(cd) {
  url <- glue::glue(
    "https://divulgacandcontas.tse.jus.br/divulga/rest/v1/candidatura/",
    "listar/2024/{cd}/2045202024/13/candidatos"
  )
  url |>
    jsonlite::fromJSON() |>
    purrr::pluck("candidatos")
}

data_raw <- here::here("202410_icat/data-raw")
data_tidy <- here::here("202410_icat/data-tidy")

icat_partido <- readr::read_csv(here::here(data_raw, "icat_partido.csv"))
pts_corte <- function(x) {
  dplyr::case_when(
    is.na(x) ~ NA,
    x <= 25 ~ "1. péssimo",
    x <= 50 ~ "2. ruim",
    x <= 60 ~ "3. regular",
    x <= 80 ~ "4. bom",
    TRUE ~ "5. ótimo"
  )
}
amazonia_legal <- "202408_seca/data-raw/amazonialegal.xlsx" |>
  readxl::read_excel() |>
  janitor::clean_names() |>
  dplyr::mutate(cd_mun = as.numeric(cd_mun))

amazonia <- data_raw |>
  here::here("20241007_151828_eleicao24_prefeitos_vereadores_finalizados.rds") |>
  readr::read_rds() |>
  dplyr::distinct(cd_municipio_ibge, cd_municipio_tse, nome_municipio, uf) |>
  dplyr::mutate(cd_municipio_ibge = as.numeric(cd_municipio_ibge)) |>
  dplyr::semi_join(amazonia_legal, dplyr::join_by(cd_municipio_ibge == cd_mun))

result <- amazonia$cd_municipio_tse |>
  purrr::map(get_result_cand) |>
  purrr::list_rbind()

v_eleitos <- result |>
  dplyr::filter(stringr::str_detect(descricaoTotalizacao, "^Eleito")) |>
  dplyr::select(partido, ufCandidatura)

v_icat <- v_eleitos |>
  tidyr::unnest(partido) |>
  dplyr::transmute(sg_partido = sigla, sg_ue = ufCandidatura) |>
  dplyr::left_join(icat_partido)

vereadores_eleitos_am <- v_icat |>
  dplyr::left_join(amazonia, dplyr::join_by(sg_ue == cd_municipio_tse)) |>
  dplyr::mutate(cat_vereador = pts_corte(icat))

readr::write_csv(vereadores_eleitos_am, here::here(data_tidy, "vereadores_icat.csv"))

camara_icat <- vereadores_eleitos_am |>
  dplyr::group_by(sg_ue, cd_municipio_ibge, nome_municipio, uf) |>
  dplyr::summarise(icat_medio = mean(icat, na.rm = TRUE)) |>
  dplyr::ungroup() |>
  dplyr::mutate(cat_medio_camara = pts_corte(icat_medio))

readr::write_csv(camara_icat, here::here(data_tidy, "camara_icat.csv"))
