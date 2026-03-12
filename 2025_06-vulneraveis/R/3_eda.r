library(sf)

raw <- here::here("2025_06-vulneraveis/data-raw")
tidy <- here::here("2025_06-vulneraveis/data-tidy")

censo <- readr::read_rds(here::here(tidy, "censo.rds"))
depara_tipo <- readr::read_rds(here::here(raw, "depara_tipo.rds"))
risco <- readr::read_rds(here::here(tidy, "risco.rds"))

# Área de risco por município

# Áreas dos municípios (para %)
areas_mun <- geobr::read_municipality(year = 2022)
areas_mun <- areas_mun |>
  dplyr::filter(code_muni %in% as.numeric(unique(censo$CD_MUN))) |>
  dplyr::mutate(area_mun = sf::st_area(geom)) |>
  dplyr::transmute(cd_geocmu = code_muni, area_mun)

risco_mun <- risco |>
  dplyr::group_by(munic, uf, cd_geocmu, grau_risco) |>
  dplyr::summarise() |>
  dplyr::ungroup() |>
  dplyr::mutate(
    area = sf::st_area(geometry),
    cd_geocmu = as.numeric(cd_geocmu)
  )

# Total da área de risco alto + muito alto por município
risco_mun |>
  dplyr::as_tibble() |>
  dplyr::summarise(
    area_risco_total = sum(area), .by = c(munic, uf, cd_geocmu)
  ) |>
  dplyr::left_join(areas_mun, dplyr::join_by(cd_geocmu)) |>
  dplyr::mutate(pct = area_risco_total / area_mun) |>
  readr::write_rds(here::here(tidy, "risco_total_municipio.rds"))
# Área de risco muito alto por município
risco_mun |>
  dplyr::as_tibble() |>
  dplyr::filter(grau_risco == "Muito alto") |>
  dplyr::summarise(
    area_risco_muito_alto = sum(area), .by = c(munic, uf, cd_geocmu)
  ) |>
  dplyr::left_join(areas_mun, dplyr::join_by(cd_geocmu)) |>
  dplyr::mutate(pct = area_risco_muito_alto / area_mun) |>
  readr::write_rds(here::here(tidy, "risco_muito_alto_municipio.rds"))

# Interesecção com censo ----
risco_censo <- fs::dir_ls(here::here(raw, "risco_censo")) |>
  purrr::map(readr::read_rds) |>
  purrr::list_rbind()
risco_censo <- risco_censo |>
  dplyr::mutate(area_risco = sf::st_area(geometry))
readr::write_rds(risco_censo, here::here(tidy, "risco_censo.rds"))

# Quantidade de setores por risco
risco_censo |>
  dplyr::as_tibble() |>
  dplyr::summarise(
    .by = grau_risco, setores = dplyr::n_distinct(CD_SETOR)
  )

# Área em risco
risco_censo |>
  dplyr::as_tibble() |>
  dplyr::summarise(
    .by = grau_risco, area = sum(area_risco)
  ) |>
  dplyr::mutate(area_ha = units::set_units(area, ha))

# Total de pessoas em risco
risco_censo |>
  tibble::as_tibble() |>
  dplyr::summarise(
    .by = grau_risco, total_pessoas = sum(v0001)
  )

# Tipo de setor censitário em risco
tipo_risco <- risco_censo |>
  tibble::as_tibble() |>
  dplyr::summarise(
    n_risco = dplyr::n_distinct(CD_SETOR),
    area_risco = sum(area_risco), .by = c(CD_TIPO, grau_risco)
  ) |>
  dplyr::left_join(depara_tipo, dplyr::join_by(CD_TIPO))

# tipo de setor censitário total
tipo_geral <- censo |>
  sf::st_make_valid() |>
  dplyr::mutate(area = sf::st_area(geometry)) |>
  tibble::as_tibble() |>
  dplyr::summarise(
    n = dplyr::n_distinct(CD_SETOR),
    area = sum(area), .by = CD_TIPO
  ) |>
  dplyr::left_join(depara_tipo, dplyr::join_by(CD_TIPO)) |>
  dplyr::select(-CD_TIPO) |>
  dplyr::relocate(tipo_setor)

tipo_geral |>
  dplyr::left_join(tipo_risco, dplyr::join_by(tipo_setor)) |>
  dplyr::mutate(
    pct_area_risco = area_risco / area,
    pct_n_risco = n_risco / n
  ) |>
  readr::write_rds(here::here(tidy, "tipo_setor_risco.rds"))

