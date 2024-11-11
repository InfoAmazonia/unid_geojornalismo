# Função para pegas dados da série histórica da ANA

get_serie <- function(estacao, inicio = "01/01/1920", fim = "01/10/2024") {
  url <- glue::glue(
    "http://telemetriaws1.ana.gov.br/ServiceANA.asmx/HidroSerieHistorica?",
    "codEstacao={estacao}",
    "&dataInicio={inicio}", "&dataFim={fim}",
    "&tipoDados=1&nivelConsistencia=1"
  )
  httr::GET(url) |>
    httr::content() |>
    xml2::xml_find_all("..//SerieHistorica") |>
    purrr::map(parse_cotas) |>
    purrr::list_rbind()
}

# Organiza dados da série histórica em tabelas

parse_cotas <- function(node) {
  name <- node |>
    xml2::xml_contents() |>
    xml2::xml_name()
  value <- node |>
    xml2::xml_contents() |>
    xml2::xml_text()
  tibble::tibble(name, value) |>
    tidyr::pivot_wider()
}

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

dados <- estacoes$codigo |>
  purrr::map(get_serie) |>
  purrr::list_rbind() |>
  dplyr::mutate(
    data_hora = lubridate::ymd_hms(DataHora),
    ano = lubridate::year(data_hora),
    mes = lubridate::month(data_hora)
  )

readr::write_csv(dados, "dados_serie.csv")
