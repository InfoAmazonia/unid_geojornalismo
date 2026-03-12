library(sf)

raw <- here::here("2025_06-vulneraveis/data-raw")
tidy <- here::here("2025_06-vulneraveis/data-tidy")

dict <- raw |>
  here::here("dicionario_de_dados_agregados_por_setores_censitarios.xlsx") |>
  readxl::read_excel(sheet = 3) |>
  janitor::clean_names()

mun <- geobr::read_municipality(year = 2022)
censo <- readr::read_rds(here::here(tidy, "censo2022.rds"))

risco <- readr::read_rds(here::here(tidy, "risco.rds")) |>
  dplyr::mutate(munic = ifelse(munic == "Manaus", "MANAUS", munic))
readr::write_rds(risco, here::here(tidy, "risco.rds"))

risco_censo <- fs::dir_ls(here::here(raw, "risco_censo")) |>
  purrr::map_dfr(readr::read_rds) |>
  dplyr::mutate(
    munic = stringr::str_to_title(munic),
    pct_area_risco = as.numeric(area_risco / area_setor)
  ) |>
  dplyr::select(CD_SETOR, uf, cd_geocmu, v0001, grau_risco, tipolo_g1, area_risco, pct_area_risco)
readr::write_rds(risco_censo, here::here(tidy, "risco_censo.rds"))


gdb <- sf::st_read(here::here(raw, "risco.gdb"))
gdb <- gdb |>
  dplyr::filter(!cd_geocmu %in% risco$cd_geocmu)
gdb <- dplyr::rename(gdb, geometry = shape)

gdb |>
  dplyr::as_tibble() |>
  dplyr::distinct(cd_geocmu) |>
  dplyr::slice(301:1250) |>
  dplyr::pull(cd_geocmu) |>
  clipr::write_clip()


censo_outros <- raw |>
  here::here("censo2022_outros") |>
  fs::dir_ls() |>
  purrr::map_dfr(readr::read_csv)
readr::write_csv(censo_outros, here::here(raw, "censo2022_outros/censo_outros.csv"))
censo_outros <- censo_outros |>
  dplyr::rename(geometry = geometria) |>
  dplyr::mutate(geometry = as.list(geometry)) |>
  sf::st_as_sf(sf_column_name = "geometry", crs = sf::st_crs(gdb))

cria_risco_censo <- function(cd_mun, tbl, risco, dir = ".") {
  message(cd_mun)
  tbl_mun <- tbl |>
    dplyr::filter(id_municipio == cd_mun)
  risco_mun <- dplyr::filter(risco, cd_geocmu == cd_mun)
  risco_censo <- risco_mun |>
    sf::st_intersection(sf::st_make_valid(tbl_mun)) |>
    dplyr::mutate(area_risco = sf::st_area(geometry))
  readr::write_rds(
    risco_censo, here::here(dir, stringr::str_glue("risco_censo_{cd_mun}.rds"))
  )
}
codigos <- unique(gdb$cd_geocmu)

risco_censo_outros <- purrr::map_dfr(
  codigos,
  \(x) cria_risco_censo(x, censo_outros, gdb, here::here(raw, "risco_censo_outros"))
)
risco_censo_outros <- here::here(raw, "risco_censo_outros") |>
  fs::dir_ls() |>
  purrr::map_dfr(readr::read_rds)

# ESGOTO
vars_esgoto <- c("V00309", "V00310", "V00311", "V00312", "V00313", "V00314", "V00315", "V00316")

esgoto_risco <- risco_censo_outros |>
  tibble::as_tibble() |>
  dplyr::mutate(
    area = area * 1000000,
    area_risco = as.numeric(area_risco),
    pct = area_risco / area
  ) |>
  dplyr::select(
    uf:cd_geocmu, tipolo_g1, grau_risco, id_setor_censitario, pessoas,
    dplyr::all_of(vars_esgoto), pct
  ) |>
  tidyr::pivot_longer(cols = dplyr::all_of(vars_esgoto)) |>
  dplyr::left_join(dict, dplyr::join_by(name == variavel)) |>
  dplyr::filter(!is.na(value)) |>
  dplyr::mutate(n = value * pct, pessoas = pessoas * pct) |>
  dplyr::summarise(n = sum(n), .by = descricao) |>
  dplyr::mutate(
    pct = n / sum(n),
    descricao = stringr::str_remove(descricao, "^.+Destinação do esgoto( .+é)? ")
  )
readr::write_csv(esgoto_risco, here::here(raw, "censo2022_outros/esgoto_outros.csv"))

# LIXO
vars_lixo <- c("V00397", "V00398", "V00399", "V00400", "V00401")

lixo_risco <- risco_censo_outros |>
  tibble::as_tibble() |>
  dplyr::mutate(
    area = area * 1000000,
    area_risco = as.numeric(area_risco),
    pct = area_risco / area
  ) |>
  dplyr::select(
    uf:cd_geocmu, tipolo_g1, grau_risco, id_setor_censitario, pessoas,
    dplyr::all_of(vars_lixo), pct
  ) |>
  tidyr::pivot_longer(cols = dplyr::all_of(vars_lixo)) |>
  dplyr::left_join(dict, dplyr::join_by(name == variavel)) |>
  dplyr::filter(!is.na(value)) |>
  dplyr::mutate(n = value * pct, pessoas = pessoas * pct) |>
  dplyr::summarise(n = sum(n), .by = descricao) |>
  dplyr::mutate(
    pct = n / sum(n),
    descricao = stringr::str_extract(descricao, "Lixo.+")
  )
readr::write_csv(lixo_risco, here::here(raw, "censo2022_outros/lixo_outros.csv"))

# TIPO

depara_tipo <- readr::read_rds(here::here(raw, "depara_tipo.rds"))

malhas <- sf::read_sf(here::here(raw, "malhas_br"))
malhas <- malhas |>
  tibble::as_tibble() |>
  dplyr::filter(CD_MUN %in% c(risco_censo_outros$cd_geocmu)) |>
  dplyr::select(CD_SETOR, CD_TIPO) |>
  dplyr::left_join(depara_tipo, dplyr::join_by(CD_TIPO))

tipo_setor_outros <- risco_censo_outros |>
  tibble::as_tibble() |>
  dplyr::mutate(
    area = area * 1000000,
    area_risco = as.numeric(area_risco),
    pct = area_risco / area,
    CD_SETOR = as.character(id_setor_censitario)
  ) |>
  dplyr::select(
    uf:cd_geocmu, tipolo_g1, grau_risco, id_setor_censitario, CD_SETOR, pessoas,
    pct
  ) |>
  dplyr::left_join(malhas, dplyr::join_by(CD_SETOR)) |>
  dplyr::mutate(n = pessoas * pct)

readr::write_csv(tipo_setor_outros, here::here(tidy, "tipo_setor_outros.csv"))
