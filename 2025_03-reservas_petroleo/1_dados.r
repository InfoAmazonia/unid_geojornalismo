data_raw <- here::here("202411_petroleo/data-raw")
data_tidy <- here::here("202411_petroleo/data-tidy")

# Glossário:
# https://fgvenergia.fgv.br/sites/fgvenergia.fgv.br/files/glossarios/pdf/glossario_og_2a_versao_26_06_20.pdf
# https://dicionariopetroleoegas.com.br/dictionary/volume-de-oleo-in-situ/

# VOIP = volume de óleo in place
# VGIP = volume de gás in place
# Fração recuperada = Produção acumulada/Volume de óleo in place
# bbl = barris = 158.9873 litros (ou m^3)
# barris de óleo equivalente (1.000m3 de gás ≈ 6,28981 boe)
# 1 boe = 0.0061178632 TJ
# 1 TJ = 163.4558 boe
# 1 EJ = 163452108.5322 boe

# Fontes
# Consumo: https://www.iea.org/countries/brazil/oil#how-is-oil-used-in-brazil
# Exportação: https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-abertos/importacoes-e-exportacoes
# Reservas: https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-estatisticos/reservas-nacionais-de-petroleo-e-gas-natural


# tidy
depara_mes <- function(mes) {
  if (is.numeric(mes)) {
    dplyr::case_when(
      mes == 1 ~ "janeiro",
      mes == 2 ~ "fevereiro",
      mes == 3 ~ "março",
      mes == 4 ~ "abril",
      mes == 5 ~ "maio",
      mes == 6 ~ "junho",
      mes == 7 ~ "julho",
      mes == 8 ~ "agosto",
      mes == 9 ~ "setembro",
      mes == 10 ~ "outubro",
      mes == 11 ~ "novembro",
      mes == 12 ~ "dezembro"
    )
  } else {
    dplyr::case_when(
      stringr::str_detect(mes, stringr::regex("janeiro", TRUE)) ~ 1,
      stringr::str_detect(mes, stringr::regex("fevereiro", TRUE)) ~ 2,
      stringr::str_detect(mes, stringr::regex("março", TRUE)) ~ 3,
      stringr::str_detect(mes, stringr::regex("marco", TRUE)) ~ 3,
      stringr::str_detect(mes, stringr::regex("abril", TRUE)) ~ 4,
      stringr::str_detect(mes, stringr::regex("maio", TRUE)) ~ 5,
      stringr::str_detect(mes, stringr::regex("junho", TRUE)) ~ 6,
      stringr::str_detect(mes, stringr::regex("julho", TRUE)) ~ 7,
      stringr::str_detect(mes, stringr::regex("agosto", TRUE)) ~ 8,
      stringr::str_detect(mes, stringr::regex("setembro", TRUE)) ~ 9,
      stringr::str_detect(mes, stringr::regex("outubro", TRUE)) ~ 10,
      stringr::str_detect(mes, stringr::regex("novembro", TRUE)) ~ 11,
      stringr::str_detect(mes, stringr::regex("dezembro", TRUE)) ~ 12
    )
  }
}

# Reservas
# https://www.gov.br/anp/pt-br/centrais-de-conteudo/dados-estatisticos/reservas-nacionais-de-petroleo-e-gas-natural
reservas <- readxl::read_excel(here::here(data_raw, "reservas.xlsx"))

# Situação em 2023
restante_2023 <- reservas |>
  janitor::clean_names() |>
  dplyr::filter(!is.na(voip_bbl)) |>
  dplyr::mutate(
    volume_restante = voip_bbl - petroleo_acumulado_bbl,
    pct_restante = volume_restante/ voip_bbl
  ) |>
  dplyr::group_by(estado) |>
  dplyr::summarise(
    volume_restante = sum(volume_restante)
  )
readr::write_csv(restante_2023, here::here(data_tidy, "reservas_2023.csv"))

# milhões bbl
reservas_painel <- tibble::tribble(
  ~ano, ~reserva_1p, ~reserva_2p, ~reserva_3p,
  2023, 15894, 22779, 27531
)
readr::write_csv(reservas_painel, here::here(data_tidy, "reservas_painel.csv"))

# Produção
# producao <- readxl::read_excel(here::here(data_raw, "producao-pocos-2023/2023_01_producao_Mar.xlsx"))


# Exportação - ANP
exportacao <- here::here(data_raw, "exportacoes.csv") |>
  readr::read_csv(skip = 1) |>
  dplyr::select(-27) |>
  tidyr::pivot_longer(2:26, names_to = "ano", values_to = "bbl") |>
  janitor::clean_names() |>
  dplyr::mutate(
    mes_num = depara_mes(mes),
    data = lubridate::ym(paste0(ano, "-", mes_num)),
    bbl = readr::parse_number(
      bbl, locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
    )
  )

exportacao_mes <- dplyr::filter(exportacao, !is.na(mes_num))
exportacao_ano <- dplyr::filter(exportacao, is.na(data))
readr::write_csv(exportacao_ano, here::here(data_tidy, "exportacao_ano.csv"))

exportacao_ano |>
  dplyr::mutate(
    milhoes_bbl = bbl / 1e6,
    label = round(milhoes_bbl)
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(x = ano, y = milhoes_bbl, label = label)
  ) +
  ggplot2::geom_col() +
  ggplot2::geom_label()


# Exportação - IEA

iea_export <- readr::read_csv(here::here(data_raw, "iea_export.csv"))
iea_export <- iea_export |>
  janitor::clean_names() |>
  dplyr::filter(crude_oil_brazil == "Exports") |>
  dplyr::transmute(
    ano = year, value_tj = -value,
    bbl = value_tj * 163.4558
  )
readr::write_csv(iea_export, here::here(data_tidy, "iea_export.csv"))


# Consumo e vendas

vendas <- readr::read_csv2(here::here(data_raw, "vendas.csv")) |>
  janitor::clean_names() |>
  dplyr::mutate(
    vendas_bbl = vendas / 158.9873
  ) |>
  dplyr::filter(produto != "ETANOL HIDRATADO")

vendas_ano <- vendas |>
  dplyr::group_by(ano) |>
  dplyr::summarise(total_bbl = sum(vendas_bbl))
readr::write_csv(vendas_ano, here::here(data_tidy, "vendas_ano.csv"))

vendas_ano |>
  ggplot2::ggplot(ggplot2::aes(x = ano, y = total_bbl)) +
  ggplot2::geom_col()

iea_consumo <- readr::read_csv(here::here(data_raw, "iea_consumo.csv")) |>
  dplyr::rename(setor = 1) |>
  janitor::clean_names() |>
  dplyr::transmute(
    ano = year, setor, tj = value,
    boe = tj * 163.4558
  ) |>
  dplyr::group_by(ano) |>
  dplyr::summarise(tj = sum(tj), boe = sum(boe))
readr::write_csv(iea_consumo, here::here(data_tidy, "iea_consumo.csv"))


# Projeção
# https://iea.blob.core.windows.net/assets/493a4f1b-c0a8-4bfc-be7b-b9c0761a3e5e/Oil2024.pdf
# milhão de barris por dia
proj_iea <- tibble::tibble(
  ano = 2023:2030,
  milhao_bbl = c(3.25, 3.32, 3.33, 3.35, 3.35, 3.36, 3.38, 3.39)
) |>
  dplyr::mutate(bbl = milhao_bbl * 1e6)
readr::write_csv(proj_iea, here::here(data_tidy, "projecao_iea.csv"))

prod_iea <- tibble::tibble(
  ano = 2022:2030,
  milhao_bbl = c(3.12, 3.49, 3.56, 3.80, 4.25, 4.25, 4.46, 4.57, 4.26)
) |>
  dplyr::mutate(bbl = milhao_bbl * 1e6, total_ano = bbl * 365)
readr::write_csv(prod_iea, here::here(data_tidy, "producao_iea.csv"))


# NZE

nze <- readr::read_csv(here::here(data_raw, "nze.csv"))
nze |>
  dplyr::filter(
    stringr::str_detect(Product, "Oil"),
    Flow == "Total final consumption"
  ) |>
  dplyr::select(Category, Year, Value, Unit)
  dplyr::glimpse()

#  oil declines by 75% to 24 million barrels per day (mb/d), from around 90 mb/d in 2020


~Ano, ~Valor
2020,154
2021,151.6
2022,149.2
2023,146.8
2024,144.4
2025,142
2026,139.6
2027,137.2
2028,134.8
2029,132.4
2030,130
2031,125.1
2032,120.2
2033,115.3
2034,110.4
2035,105.5
2036,100.6
2037,95.7
2038,90.8
2039,85.9
2040,81
2041,77.2
2042,73.4
2043,69.6
2044,65.8
2045,62
2046,58.2
2047,54.4
2048,50.6
2049,46.8
2050,43