am <- readxl::read_excel("202408_seca/data-raw/amazonialegal.xlsx") |>
  dplyr::mutate(geo_cod = as.numeric(CD_MUN))
iis <- readr::read_csv("202408_seca/data-raw/IIS Brasil 2024 06.csv")

iis6 <- iis |>
  janitor::clean_names() |>
  dplyr::select(geo_cod, nm_municip, uf, regiao, dplyr::starts_with("iis6")) |>
  dplyr::semi_join(am, dplyr::join_by(geo_cod)) |>
  tidyr::pivot_longer(5:46) |>
  dplyr::mutate(
    mes = stringr::str_extract(name, "(?<=_)[0-9]{2}"),
    ano = paste0("20", stringr::str_extract(name, "[0-9]{2}$")),
    ano_mes = lubridate::ym(paste(ano, mes)),
    iis6 = value
  )
readr::write_rds(iis6, "202408_seca/data-tidy/iis6.rds")

iis6_resto <- iis |>
  janitor::clean_names() |>
  dplyr::select(geo_cod, nm_municip, uf, regiao, dplyr::starts_with("iis6")) |>
  dplyr::anti_join(am, dplyr::join_by(geo_cod)) |>
  tidyr::pivot_longer(5:46) |>
  dplyr::mutate(
    mes = stringr::str_extract(name, "(?<=_)[0-9]{2}"),
    ano = paste0("20", stringr::str_extract(name, "[0-9]{2}$")),
    ano_mes = lubridate::ym(paste(ano, mes)),
    iis6 = value
  )
readr::write_rds(iis6_resto, "202408_seca/data-tidy/iis6_br_sem_amazonia.rds")

iis3 <- iis |>
  janitor::clean_names() |>
  dplyr::select(geo_cod, nm_municip, uf, regiao, dplyr::starts_with("iis3")) |>
  dplyr::semi_join(am, dplyr::join_by(geo_cod)) |>
  tidyr::pivot_longer(5:46) |>
  dplyr::mutate(
    mes = stringr::str_extract(name, "(?<=_)[0-9]{2}"),
    ano = paste0("20", stringr::str_extract(name, "[0-9]{2}$")),
    ano_mes = lubridate::ym(paste(ano, mes)),
    iis3 = value
  )
readr::write_rds(iis3, "202408_seca/data-tidy/iis3.rds")

# classes de seca
iis_classe <- tibble::tibble(iis = 1:6, classe = c(
  "Seca Excepcional", "Seca Extrema", "Seca Severa", "Seca Moderada",
  "Seca Fraca", "Normal"
))
readr::write_rds(iis_classe, "202408_seca/data-raw/iis_classe.rds")

# Diferença entre junho23 e junho24 IIS6
iis |>
  janitor::clean_names() |>
  dplyr::select(geo_cod, nm_municip, uf, regiao, iis6_0623, iis6_0624) |>
  dplyr::semi_join(am, dplyr::join_by(geo_cod)) |>
  dplyr::mutate(dif = iis6_0624 - iis6_0623) |>
  dplyr::slice_min(dif)

# Municípios com Seca Excepcional
iis |>
  janitor::clean_names() |>
  dplyr::select(geo_cod, nm_municip, uf, regiao, iis6_0623, iis6_0624) |>
  dplyr::semi_join(am, dplyr::join_by(geo_cod)) |>
  dplyr::filter(iis6_0624 == 3)

# TIs
tis <- geobr::read_indigenous_land()
muns_amaz <- muns |>
  dplyr::inner_join(iis6, dplyr::join_by(code_muni == geo_cod)) |>
  dplyr::filter(ano %in% c("2023", "2024"), mes == "06")

tis <- tis |>
  dplyr::filter(abbrev_state %in% muns_amaz$abbrev_state)
muns_amaz <- muns_amaz |>
  sf::st_make_valid()
tis <- tis |>
  sf::st_make_valid()
tbl_tis <- tis |>
  tibble::as_tibble()
readr::write_rds(tbl_tis, "202408_seca/data-tidy/tbl_tis.rds")

tis_iis <- tis |>
  sf::st_intersection(muns_amaz)
readr::write_rds(tis_iis, "202408_seca/data-tidy/tis_iis.rds")

tbl_tis_iis <- tis_iis |>
  tibble::as_tibble() |>
  dplyr::group_by(code_terrai, terrai_nom, ano) |>
  dplyr::summarise(iis6_media = mean(iis6), .groups = "drop") |>
  tidyr::pivot_wider(names_from = ano, values_from = iis6_media)
readr::write_rds(tbl_tis_iis, "202408_seca/data-tidy/tbl_tis_iis.rds")

