tidy <- here::here("202501_financiamento_climatico/data-tidy")
if (!dir.exists(tidy)) {
  dir.create(tidy)
}

download_tbl <- function(page) {
  url <- paste0(
    "https://www.fundoamazonia.gov.br/pt/projetos/busca/index.html?reloaded&page=",
    page
  )
  Sys.sleep(.5)
  req <- httr::GET(url)
  orgs <- req |>
    xml2::read_html() |>
    xml2::xml_find_all("//div[@class='table-org']") |>
    xml2::xml_text()
  projs <- req |>
    xml2::read_html() |>
    xml2::xml_find_all("//div[@class='table-name']/a") |>
    xml2::xml_attrs("href") |>
    paste0("https://www.fundoamazonia.gov.br", ... = _)
  req |>
    xml2::read_html() |>
    xml2::xml_find_all("//table") |>
    rvest::html_table() |>
    purrr::pluck(1) |>
    janitor::clean_names() |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("valor"),
        \(x) readr::parse_number(x, locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
      ),
      dplyr::across(dplyr::where(is.character), stringr::str_squish),
      organizacao = orgs,
      url_proj = projs
    )
}

famazonia <- purrr::map(1:24, download_tbl)
famazonia <- purrr::list_rbind(famazonia)

readr::write_csv(famazonia, here::here(tidy, "fundo_amazonia.csv"))


# Datas
famazonia <- readr::read_csv(here::here(tidy, "fundo_amazonia.csv"))


get_datas <- function(url) {
  req <- httr::GET(url)
  req |>
    xml2::read_html() |>
    xml2::xml_find_all("//table[@class='tabela-evolucao']") |>
    rvest::html_table() |>
    purrr::pluck(1) |>
    janitor::clean_names() |>
    tidyr::pivot_wider(names_from = x, values_from = x_2) |>
    janitor::clean_names() |>
    dplyr::mutate(dplyr::across(dplyr::everything(), lubridate::dmy))
}
poss_get_datas <- purrr::possibly(get_datas, otherwise = tibble::tibble())

# famazonia <- famazonia |>
#   dplyr::mutate(datas = purrr::map(url_proj, get_datas))
datas <- purrr::map(famazonia$url_proj, poss_get_datas)

df_datas <- dplyr::bind_rows(datas)
famazonia <- dplyr::bind_cols(famazonia, df_datas)

readr::write_csv(famazonia, here::here(tidy, "fundo_amazonia.csv"))
# clipr::write_clip(famazonia)

famazonia |>
  dplyr::mutate(ano_contratacao = lubridate::year(data_da_contratacao)) |>
  dplyr::group_by(ano_contratacao, natureza) |>
  dplyr::summarise(n = dplyr::n(), valor = sum(valor_do_apoio)) |>
  dplyr::select(-n) |>
  tidyr::pivot_wider(names_from = natureza, values_from = valor, values_fill = 0) |>
  clipr::write_clip()


