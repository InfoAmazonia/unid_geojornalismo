dd <- here::here("ep_02/dados")

get_setor <- function(uf, dir) {
  uf <- tolower(uf)
  fs::dir_ls(dir, regexp = paste0("\\b", uf, "\\b")) |>
    sf::read_sf(options = "ENCODING=WINDOWS-1252") |>
    dplyr::mutate(sigla_uf = toupper(uf))
}

setores2010 <- c("AC", "AM", "AP", "PA", "RO", "RR", "TO", "MA", "MT") |>
  purrr::map(get_setor, dir = here::here(dd, "setores2010")) |>
  purrr::list_rbind()

readr::write_rds(setores2010, here::here(dd, "setores2010.rds"))



setores2022 <- c("AC", "AM", "AP", "PA", "RO", "RR", "TO", "MA", "MT") |>
  purrr::map(get_setor, dir = here::here(dd, "setores2022")) |>
  purrr::list_rbind()

readr::write_rds(setores2022, here::here(dd, "setores2022.rds"))

setores2022 |>
  tibble::as_tibble() |>
  dplyr::distinct(CD_SETOR, CD_MUN, NM_MUN, sigla_uf) |>
  readr::write_csv(here::here(dd, "ids_municipios.csv"))
