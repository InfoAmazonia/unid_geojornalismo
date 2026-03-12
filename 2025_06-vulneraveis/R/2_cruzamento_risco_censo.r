# Dados

raw <- here::here("202504_risco/data-raw")
tidy <- here::here("202504_risco/data-tidy")
am <- sf::read_sf(here::here(raw, "am_legal_shp"))

mun <- geobr::read_municipality(year = 2022)


# Intersecção de áreas de risco e setor censitário
# Traz informações do censo para as áreas de risco

censo <- here::here(raw, "malha_setor_ibge2022") |>
  fs::dir_ls() |>
  purrr::map(sf::read_sf) |>
  purrr::list_rbind() |>
  dplyr::filter(CD_MUN %in% c(am$cd_geocmu)) |>
  sf::st_as_sf()
readr::write_rds(censo, here::here(raw, "censo.rds"))

cria_risco_censo <- function(cd_mun, tbl, risco, dir = ".") {
  message(cd_mun)
  tbl_mun <- tbl |>
    dplyr::filter(CD_MUN == cd_mun) |>
    dplyr::mutate(area_setor = AREA_KM2 * 1e6)
  risco_mun <- dplyr::filter(risco, cd_geocmu == cd_mun)
  risco_censo <- risco_mun |>
    sf::st_intersection(sf::st_make_valid(tbl_mun)) |>
    dplyr::mutate(area_risco = sf::st_area(geometry))
  readr::write_rds(
    risco_censo, here::here(dir, stringr::str_glue("risco_censo_{cd_mun}.rds"))
  )
}

codigos <- unique(am$cd_geocmu)
codigos_ok <- fs::dir_ls(here::here(raw, "risco_censo")) |>
  basename() |>
  stringr::str_extract("[0-9]+")
codigos_erro <- c("1301852", "1304401", "1505403", "1508100", "1300409")

codigos <- setdiff(codigos, codigos_ok)
# codigos <- setdiff(codigos, codigos_erro)

risco_censo <- purrr::map_dfr(
  codigos,
  \(x) cria_risco_censo(x, censo, am, here::here(raw, "risco_censo"))
)

readr::write_rds(am, here::here(tidy, "risco.rds"))

risco_censo <- fs::dir_ls(here::here(raw, "risco_censo")) |>
  purrr::map_dfr(readr::read_rds)