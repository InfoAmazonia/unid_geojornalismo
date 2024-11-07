data_raw <- here::here("202410_infratores_doadores/data-raw")
data_tidy <- here::here("202410_infratores_doadores/data-tidy")
fs::dir_create(data_raw)
fs::dir_create(data_tidy)


# DOAÇÕES
receita <- readr::read_csv2(
  here::here(data_raw, "receita.csv"),
  locale = readr::locale(encoding = "Latin1")
) |>
  janitor::clean_names(receita)

# INFRAÇÕES

depara_amazonia <- readr::read_csv("depara_ibge_tse.csv")
amazonia_legal <- "202408_seca/data-raw/amazonialegal.xlsx" |>
  readxl::read_excel() |>
  janitor::clean_names() |>
  dplyr::mutate(cod_municipio = as.numeric(cd_mun)) |>
  dplyr::inner_join(
    depara_amazonia, dplyr::join_by(cod_municipio == cd_municipio_ibge)
  )

parse_infracoes <- function(xml) {
  xml |>
    xml2::read_xml() |>
    xml2::xml_find_all("itemRelatorio") |>
    purrr::map(xml2::xml_contents) |>
    purrr::map(\(x) tibble::tibble(
      name = xml2::xml_name(x), value = xml2::xml_text(x)
    )) |>
    purrr::map(tidyr::pivot_wider) |>
    purrr::list_rbind() |>
    janitor::clean_names() |>
    dplyr::mutate(cod_municipio = as.numeric(cod_municipio))
}

arqs <- data_raw |>
  here::here("auto_infracao_xml") |>
  fs::dir_ls()

arqs |>
  purrr::map(parse_infracoes) |>
  purrr::list_rbind() |>
  readr::write_rds(here::here(data_tidy, "infracoes_total.rds"))

infracoes <- readr::read_rds(here::here(data_tidy, "infracoes_total.rds"))

check_cpf <- function(cpf) {
  len <- stringr::str_length(cpf)
  if (len == 14) {
    TRUE
  } else if (len != 11) {
    FALSE
  } else {
    dig1 <- cpf |>
      stringr::str_remove(".{2}$") |>
      stringr::str_split_1("") |>
      as.numeric() |>
      purrr::map2_vec(1:9, `*`) |>
      sum() |>
      (\(x) x %% 11)() |>
      (\(x) x %% 10)()
    if (dig1 == stringr::str_sub(cpf, 10, 10)) {
      dig2 <- cpf |>
        stringr::str_remove(".$") |>
        stringr::str_split_1("") |>
        as.numeric() |>
        purrr::map2_vec(0:9, `*`) |>
        sum() |>
        (\(x) x %% 11)() |>
        (\(x) x %% 10)()
      dig2 == stringr::str_sub(cpf, 11, 11)
    } else {
      FALSE
    }
  }
}

infracoes_tidy <- infracoes |>
  dplyr::mutate(
    dat_hora_auto_infracao = lubridate::ymd_hms(dat_hora_auto_infracao),
    val_auto_infracao = readr::parse_number(
      val_auto_infracao, locale = readr::locale(decimal_mark = ",")
    ),
    cpf_cnpj = stringr::str_squish(cpf_cnpj_infrator),
    cpf_cnpj = stringr::str_remove_all(cpf_cnpj, "[^0-9]"),
    len_cpf = stringr::str_length(cpf_cnpj),
    cpf_valido = purrr::map_vec(cpf_cnpj, check_cpf)
  )
readr::write_csv(infracoes_tidy, here::here(data_tidy, "infracoes_tidy.csv"))

infracoes_cpf <- dplyr::filter(
  infracoes_tidy,
  stringr::str_length(cpf_cnpj) != 14
)
readr::write_csv(infracoes_cpf, here::here(data_tidy, "infracoes_cpf.csv"))
