municipios_amacro <- readr::read_csv("202408_amacro/data-raw/AMACRO.csv")

read_name <- function(x) {
  x |>
    readr::read_csv() |>
    dplyr::mutate(file = x)
}
mapbiomas <- "202407_amacro/data-raw/mapbiomas_amacro" |>
  fs::dir_ls() |>
  purrr::map(read_name) |>
  purrr::list_rbind()
depara_munis <- mapbiomas |>
  dplyr::distinct(file) |>
  dplyr::mutate(ordem = as.integer(stringr::str_extract(file, "(?<=\\()[0-9]+"))) |>
  tidyr::replace_na(list(ordem = 0)) |>
  dplyr::arrange(ordem) |>
  dplyr::bind_cols(municipios_amacro) |>
  dplyr::select(-ordem)

mapbiomas <- mapbiomas |>
  dplyr::left_join(depara_munis, "file")

mapbiomas_tidy <- mapbiomas |>
  tidyr::pivot_longer(
    2:39,
    names_to = "ano", values_to = "area"
  ) |>
  janitor::clean_names() |>
  dplyr::mutate(
    classe_principal = stringr::str_extract(classe, "^[0-9]\\. .+"),
    sub1 = stringr::str_extract(classe, "^[0-9]\\.[0-9]\\. .+"),
    sub2 = stringr::str_extract(classe, "^[0-9]\\.[0-9]\\.[0-9]\\. .+"),
    sub3 = stringr::str_extract(classe, "^[0-9]\\.[0-9]\\.[0-9]\\.[0-9]\\. .+"),
    ano = lubridate::ym(paste0(ano, "-01"))
  )

readr::write_csv(mapbiomas_tidy, "202407_amacro/data-tidy/mapbiomas_tidy.csv")


