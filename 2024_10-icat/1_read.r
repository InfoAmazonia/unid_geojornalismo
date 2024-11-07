data_raw <- here::here("202410_icat/data-raw")
data_tidy <- here::here("202410_icat/data-tidy")
fs::dir_create(data_raw)
fs::dir_create(data_tidy)

icat_partido <- tibble::tribble(
  ~sg_partido, ~icat,
  "AVANTE", 33.86,
  "CIDADANIA", 42.14,
  "MDB", 17.41,
  "NOVO", 6.94,
  "PC do B", 94.23,
  "PDT", 56.75,
  "PL", 7.85,
  "PODE", 20.7,
  "PP", 16.94,
  "PRD", 20.83,
  "PSB", 60.65,
  "PSD", 23.02,
  "PSDB", 15.21,
  "PSOL", 98.9,
  "PT", 73.72,
  "PV", 64.62,
  "REDE", 83.33,
  "REPUBLICANOS", 14.1,
  "SOLIDARIEDADE", 33.33,
  "UNIÃO", 15.68
)
readr::write_csv(icat_partido, here::here(data_raw, "icat_partido.csv"))

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

# result <- data_raw |>
#   here::here("20241007_151828_eleicao24_prefeitos_vereadores_finalizados.rds") |>
#   readr::read_rds() |>
#   dplyr::distinct()
# result_amazonia <- result |>
#   dplyr::mutate(cd_municipio_ibge = as.numeric(cd_municipio_ibge)) |>
#   dplyr::semi_join(amazonia_legal, dplyr::join_by(cd_municipio_ibge == cd_mun))

result <- readr::read_csv("202410_indigenas_eleitos/data-raw/resultados.csv")
amazonia <- data_raw |>
  here::here("20241007_151828_eleicao24_prefeitos_vereadores_finalizados.rds") |>
  readr::read_rds() |>
  dplyr::distinct(cd_municipio_ibge, cd_municipio_tse, nome_municipio, uf) |>
  dplyr::mutate(cd_municipio_ibge = as.numeric(cd_municipio_ibge)) |>
  dplyr::semi_join(amazonia_legal, dplyr::join_by(cd_municipio_ibge == cd_mun))

result_amazonia <- result |>
  dplyr::inner_join(amazonia, dplyr::join_by(sg_ue == cd_municipio_tse))


# PREFEITOS ----

pref_eleitos <- result_amazonia |>
  dplyr::filter(ds_cargo == "PREFEITO", ds_sit_tot_turno == "ELEITO")

pref_2turno <- result_amazonia |>
  dplyr::filter(ds_cargo == "PREFEITO", ds_sit_tot_turno == "2º TURNO")


turno2 <- readr::read_rds(here::here(data_raw, "turno2.rds")) |>
  dplyr::rename(sg_uf = uf, sg_ue = cd_municipio_tse) |>
  dplyr::mutate(dplyr::across(
    dplyr::matches("^(cd|sq)"), as.numeric
  )) |>
  dplyr::select(-cd_eleicao)
res_2turno <- pref_2turno |>
  dplyr::distinct() |>
  dplyr::inner_join(turno2) |>
  dplyr::filter(situacao_candidato_turno == "Eleito")

pref_eleitos <- pref_eleitos |>
  dplyr::bind_rows(res_2turno)


pref_eleitos_icat <- pref_eleitos |>
  dplyr::left_join(icat_partido, dplyr::join_by(sg_partido)) |>
  dplyr::transmute(
    ano_eleicao, sg_uf, cd_municipio_tse = as.numeric(sg_ue),
    cd_municipio_ibge = as.numeric(cd_municipio_ibge),
    nome_municipio = nm_ue, cargo = ds_cargo, sg_partido,
    nm_urna_candidato, icat, cat_prefeito = pts_corte(icat)
  )

pref_eleitos_icat <- pref_eleitos_icat |>
  dplyr::mutate(cat_prefeito = ifelse(is.na(icat), NA, cat_prefeito))


geo <- geobr::read_municipality() |>
  dplyr::semi_join(amazonia_legal, dplyr::join_by(code_muni == cd_mun)) |>
  dplyr::left_join(
    pref_eleitos_icat, dplyr::join_by(code_muni == cd_municipio_ibge)
  ) |>
  dplyr::mutate(segundo_turno = code_muni %in% pref_2turno$cd_municipio_ibge)
sf::write_sf(geo, here::here(data_tidy, "prefeitos_icat.geojson"))
geo |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = icat)) +
  ggplot2::scale_fill_distiller(palette = "Blues", direction = 1)

pref_eleitos_icat |>
  dplyr::group_by(sg_uf) |>
  dplyr::summarise(
    media = mean(icat, na.rm = TRUE),
    mediana = median(icat, na.rm = TRUE),
    max = max(icat, na.rm = TRUE),
    min = min(icat, na.rm = TRUE),
    sd = sd(icat, na.rm = TRUE)
  ) |>
  dplyr::arrange(dplyr::desc(max))

pref_eleitos_icat |>
  dplyr::count(cat_prefeito)

pref_eleitos_icat |>
  ggplot2::ggplot(ggplot2::aes(x = uf, y = icat, color = uf)) +
  ggplot2::geom_boxplot()

lm(icat ~ 0 + uf, data = pref_eleitos_icat) |>
  broom::tidy() |>
  dplyr::arrange(estimate)

# - Na Amazônia Legal, prefeitos de partidos alinhados a pautas ambientais se concentram principalmente no Maranhão e Roraima
# - Por outro lado, Rondônia e Amapá não elegeram nenhum prefeito ambientalista: todos os eleitos pertencem a partidos com índice abaixo de 50%, considerado ruim ou péssimo

pref_eleitos_icat |>
  dplyr::filter(!is.na(icat)) |>
  dplyr::mutate(icat_bom = icat > 50) |>
  dplyr::count(uf, icat_bom) |>
  dplyr::filter(!is.na(icat_bom)) |>
  dplyr::group_by(uf) |>
  dplyr::mutate(pct_bom = n/sum(n)) |>
  dplyr::ungroup() |>
  dplyr::filter(icat_bom) |>
  dplyr::arrange(pct_bom)


# VEREADORES ----

vereadores_eleitos <- electionsBR::elections_tse(
  2024, "candidate", br_archive = TRUE
) |>
  janitor::clean_names() |>
  dplyr::filter(
    ds_cargo == "VEREADOR",
    stringr::str_detect(ds_sit_tot_turno, "^ELEITO")
  ) |>
  dplyr::inner_join(amazonia, dplyr::join_by(sg_ue == cd_municipio_tse))

vereadores_geral <- electionsBR::elections_tse(
  2024, "candidate", br_archive = TRUE
) |>
  janitor::clean_names() |>
  dplyr::filter(
    ds_cargo == "VEREADOR",
    stringr::str_detect(ds_sit_tot_turno, "^ELEITO")
  )

vereadores_geral |>
  dplyr::filter(nm_ue == "TESOURO") |>
  dplyr::count(ds_sit_tot_turno)
vereadores_eleitos_icat |>
  dplyr::filter(nome_municipio == "TESOURO") |>
  dplyr::count(cat_vereador)


vereadores_eleitos_icat <- vereadores_eleitos |>
  dplyr::left_join(icat_partido, dplyr::join_by(sg_partido)) |>
  dplyr::transmute(
    ano_eleicao, sg_uf, cd_municipio_tse = as.numeric(sg_ue),
    cd_municipio_ibge = as.numeric(cd_municipio_ibge),
    nome_municipio = nm_ue, cargo = ds_cargo, sg_partido,
    nm_urna_candidato, icat_vereador = icat
  ) |>
  dplyr::mutate(cat_vereador = pts_corte(icat_vereador))
readr::write_csv(vereadores_eleitos_icat, here::here(data_tidy, "vereadores_icat.csv"))

camara_icat <- vereadores_eleitos |>
  dplyr::left_join(icat_partido, dplyr::join_by(sg_partido)) |>
  dplyr::transmute(
    ano_eleicao, sg_uf, cd_municipio_tse = as.numeric(sg_ue),
    cd_municipio_ibge = as.numeric(cd_municipio_ibge),
    nome_municipio = nm_ue, cargo = ds_cargo, sg_partido,
    nm_urna_candidato, icat
  ) |>
  dplyr::group_by(cd_municipio_ibge) |>
  dplyr::summarise(icat_medio = mean(icat, na.rm = TRUE)) |>
  dplyr::mutate(cat_medio_camara = pts_corte(icat_medio))



geo_camara <- geobr::read_municipality() |>
  dplyr::semi_join(amazonia_legal, dplyr::join_by(code_muni == cd_mun)) |>
  dplyr::left_join(
    camara_icat, dplyr::join_by(code_muni == cd_municipio_ibge)
  )
geo_camara |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = icat_medio)) +
  ggplot2::scale_fill_distiller(palette = "Greens", direction = 1)
sf::write_sf(geo_vereadores, here::here(data_tidy, "camara_icat.geojson"))

tbl_geo_vereadores <- geo_vereadores |>
  tibble::as_tibble()

lm(icat_medio ~ -1 + abbrev_state, data = tbl_geo_vereadores) |>
  broom::tidy() |>
  dplyr::arrange(estimate)

tbl_geo_vereadores |>
  dplyr::slice_min(icat_medio, n = 10)
tbl_geo_vereadores |>
  dplyr::group_by(abbrev_state) |>
  dplyr::summarise(
    media = mean(icat_medio, na.rm = TRUE)
  ) |>
  dplyr::arrange(media)
tbl_geo_vereadores |>
  dplyr::mutate(bom = icat_medio >= 50) |>
  dplyr::count(abbrev_state, bom) |>
  dplyr::filter(bom) |>
  janitor::adorn_totals()


# DIFERENÇA ENTRE VEREADORES E PREFEITOS

dif_prefeitos_vereadores <- pref_eleitos_icat |>
  dplyr::transmute(
    sg_uf, cd_municipio_ibge, nome_municipio, sg_partido_prefeito = sg_partido,
    icat_prefeito = icat,
  ) |>
  dplyr::inner_join(
    camara_icat, dplyr::join_by(cd_municipio_ibge)
  ) |>
  dplyr::mutate(
    dist = icat_prefeito - icat_medio,
  )
readr::write_csv(dif_prefeitos_vereadores, here::here(data_tidy, "dif_prefeitos_camara.csv"))


dif_prefeitos_vereadores |>
  dplyr::slice_max(icat_prefeito, n = 10)

dif_prefeitos_vereadores |>
  dplyr::slice_min(icat_prefeito, n = 10)


# icat prefeitos
pref_eleitos_icat |>
  dplyr::slice_max(icat, n = 10) |>
  dplyr::select(uf, nome_municipio, sg_partido, nome_urna_candidato, icat)

# icat vereadores
geo_vereadores |>
  tibble::as_tibble() |>
  dplyr::slice_max(icat_medio, n = 10) |>
  dplyr::select(abbrev_state, name_muni, icat_medio) |>
  knitr::kable()

# contraste prefeitos - vereadores
dif_prefeitos_vereadores |>
  dplyr::slice_min(dist, n = 10) |>
  knitr::kable()
dif_prefeitos_vereadores |>
  dplyr::slice_max(dist, n = 10) |>
  knitr::kable()

dif_prefeitos_vereadores |>
  dplyr::glimpse()


top10 <- pref_eleitos_icat |>
  dplyr::slice_min(icat, n = 10, with_ties = FALSE) |>
  dplyr::pull(cd_municipio_ibge)
vereadores_eleitos |>
  dplyr::left_join(icat_partido, dplyr::join_by(sg_partido)) |>
  dplyr::filter(cd_municipio_ibge %in% top10) |>
  dplyr::mutate(bom = icat > 50) |>
  dplyr::group_by(cd_municipio_ibge) |>
  dplyr::summarise(pct_bom = sum(bom, na.rm = TRUE)/dplyr::n())
vereadores_eleitos |>
  dplyr::filter(cd_municipio_ibge == 2110039) |>
  dplyr::glimpse()

pref_eleitos_icat |>
  dplyr::filter(icat >= 50) |>
  dplyr::count(sg_uf) |>
  janitor::adorn_totals() |>
  janitor::adorn_percentages("col") |>
  janitor::adorn_pct_formatting()



dif_prefeitos_vereadores |>
  ggplot2::ggplot(ggplot2::aes(x = icat_prefeito, y = icat_medio)) +
  ggplot2::geom_point()


geo_dif <- geobr::read_municipality() |>
  dplyr::semi_join(amazonia_legal, dplyr::join_by(code_muni == cd_mun)) |>
  dplyr::left_join(
    dif_prefeitos_vereadores, dplyr::join_by(code_muni == cd_municipio_ibge)
  )
geo_dif |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = dist)) +
  ggplot2::scale_fill_gradient2(
    low = "red",
    high = "blue",
    mid = "white",
    na.value = "grey50"
  )

dif_prefeitos_vereadores |>
  dplyr::mutate(
    cat_prefeito = pts_corte(icat_prefeito),
    cat_camara = pts_corte(icat_medio)
  )
