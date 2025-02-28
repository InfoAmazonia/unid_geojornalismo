# remotes::install_github("tchiluanda/rsiconfi")

# DAVID ANTONIO ABISAI PEREIRA DE ALMEIDA - MANAUS
# EDMILSON BRITO RODRIGUES - BELÉM
# LUIZ CARLOS - RIO BRANCO
# ARTHUR HENRIQUE BRANDÃO MACHADO - BOA VISTA
# ANTONIO PAULO DE OLIVEIRA FURLAN - MACAPÁ
# EDUARDO SALIM BRAIDE - SÃO LUÍS

library(rsiconfi)

municipios <- c(
  manaus = 1302603,
  belem = 1501402,
  riobranco = 1200401,
  boavista = 1400100,
  macapa = 1600303,
  saoluis = 2111300
)
ano <- 2021:2023
mun_ano <- tidyr::expand_grid(municipios, ano)

# Gastos ----

rreo <- purrr::map2(
  mun_ano$ano, mun_ano$municipios,
  rsiconfi::get_rreo,
  period = 6, report_tp = 1, annex = "02"
) |>
  purrr::list_rbind() |>
  dplyr::filter(stringr::str_detect(coluna, "LIQUIDADAS ATÉ O BIMESTRE"))
  # dplyr::distinct(conta) |>
  # clipr::write_clip()

rreo2024 <- purrr::map(
  municipios, rsiconfi::get_rreo, year = 2024, period = 3, report_tp = 1,
  annex = "02"
) |>
  purrr::list_rbind() |>
  dplyr::filter(stringr::str_detect(coluna, "LIQUIDADAS ATÉ O BIMESTRE"))

rreo <- dplyr::bind_rows(rreo, rreo2024)
readr::write_csv(rreo, "202409_reeleicao_gastos/data-tidy/rreo.csv")

# Filtro das contas de interesse

contas <- c(
  "Gestão Ambiental",
  "Assistência aos Povos Indígenas"
  # "Promoção da Produção Agropecuária"
)

rreo <- readr::read_csv("202409_reeleicao_gastos/data-tidy/rreo.csv")
rreo <- dplyr::mutate(
  rreo,
  ambiental = conta %in% contas,
  agropecuaria = conta == "Promoção da Produção Agropecuária"
)
rreo_filtrado <- dplyr::filter(rreo, ambiental)
readr::write_csv(rreo_filtrado, "202409_reeleicao_gastos/data-tidy/rreo_filtrado.csv")
readr::write_csv(rreo, "202409_reeleicao_gastos/data-tidy/rreo.csv")


# Função principal

# depara <- "202409_reeleicao_gastos/data-raw/depara_funcao.csv" |>
#   readr::read_csv() |>
#   janitor::clean_names()
# rreo <- rreo |>
#   dplyr::left_join(depara, dplyr::join_by(conta == subfuncao))
# readr::write_csv(rreo, "202409_reeleicao_gastos/data-tidy/rreo.csv")

# Receita ----

rcl <- purrr::map2(
  mun_ano$ano, mun_ano$municipios,
  rsiconfi::get_rreo,
  period = 6, report_tp = 1, annex = "03"
) |>
  purrr::list_rbind()

rcl2024 <- purrr::map(
  municipios, rsiconfi::get_rreo,
  year = 2024, period = 3, report_tp = 1, annex = "03"
) |>
  purrr::list_rbind()

rcl <- dplyr::bind_rows(rcl, rcl2024) |>
  dplyr::filter(conta == "RECEITA CORRENTE LÍQUIDA (III) = (I - II)")
readr::write_csv(rcl, "202409_reeleicao_gastos/data-tidy/rcl.csv")

