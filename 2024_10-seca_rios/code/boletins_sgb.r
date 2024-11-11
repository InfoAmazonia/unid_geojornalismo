estacoes <- tibble::tribble(
  ~estacao,	~codigo,
  "Barcelos (Negro)",	14480002,
  "Beruri (Purus)",	13990000,
  "Boa Vista (Branco)",	14620000,
  "Caracaraí (Branco)",	14710000,
  "Careiro (P. Careiro)",	15040000,
  "Fonte Boa (Solimões)",	12351000,
  "Humaitá (Madeira)",	15630000,
  "Itacoatiara (Amazonas)",	16030000,
  "Itapeuá (Solimões)",	13150000,
  "Itapéua (Solimões)",	13150000,
  "Manacapuru (Solimões)",	14100000,
  "Manaus (Negro)",	14990000,
  "Parintins (Amazonas)", 16350002,
  "Rio Branco (Acre)",	13600002,
  "S. G. C. (Negro)",	14320001,
  "Tabatinga (Solimões)",	10100000,
  "S.I.N.Tapuruquara (Negro)", 14400000
)

url <- "https://www.sgb.gov.br/sace/amazonas_boletins.php"

boletins_pdf <- url |>
  httr::GET() |>
  xml2::read_html() |>xml2::xml_find_all("..//html/body") |>
  xml2::xml_find_all("..//a") |>
  xml2::xml_attr("href") |>
  purrr::keep(stringr::str_detect, pattern = "boletins/Amazonas/2024") |>
  stringr::str_replace_all(" ", "%20") |>
  paste0("https://www.sgb.gov.br/sace/", ... = _)

rx_estacoes_tabela <- estacoes |>
  dplyr::pull(estacao) |>
  stringr::str_extract("^[\\S\\s{1:2}]+") |>
  stringr::str_remove(" \\(.+") |>
  paste0(collapse = "|") |>
  stringr::str_replace_all("\\.", "\\\\.") |>
  paste0("^(", ... = _, "|\\s*Tabela)") |>
  stringr::regex()

rx_estacoes <- estacoes |>
  dplyr::pull(estacao) |>
  stringr::str_extract("^[\\S\\s{1:2}]+") |>
  stringr::str_remove(" \\(.+") |>
  paste0(collapse = "|") |>
  stringr::str_replace_all("\\.", "\\\\.") |>
  paste0("^(", ... = _, ")") |>
  stringr::regex()


parse_boletim <- function(url_pdf) {
  url_pdf |>
    pdftools::pdf_text() |>
    readr::read_lines() |>
    tibble::as_tibble() |>
    dplyr::filter(stringr::str_detect(value, rx_estacoes_tabela)) |>
    dplyr::mutate(
      keep = ifelse(
        stringr::str_detect(dplyr::lag(value), "Tabela 03"), TRUE, NA
      )
    ) |>
    tidyr::fill(keep) |>
    dplyr::filter(keep, stringr::str_detect(value, rx_estacoes)) |>
    dplyr::select(-keep) |>
    tidyr::separate(
      value, c(
        "estacao", "data", "cota_atual", "data_da_minima", "cota_minima",
        "relacao_cota_atual", "data_ano", "cota_periodo",
        "relacao_cota_atual_ano"
      ),
      sep = "  +"
    ) |>
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("data"), lubridate::dmy),
      dplyr::across(
        dplyr::matches("cota"),
        ~readr::parse_number(.x, locale = readr::locale(decimal_mark = ","))
      ),
      rio = stringr::str_extract(estacao, "(?<=\\().+(?=\\))")
    ) |>
    dplyr::relocate(rio)
}

boletins_parsed <- purrr::map(boletins_pdf, parse_boletim)
boletins_parsed <- boletins_parsed |>
  purrr::list_rbind() |>
  dplyr::filter(!is.na(relacao_cota_atual_ano))

cotas_2024 <- boletins_parsed |>
  dplyr::left_join(estacoes)

readr::write_csv(cotas_2024, "cotas_2024.csv")

