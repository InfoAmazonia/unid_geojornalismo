tidy <- here::here("202502_perdas_publicas/data-tidy")
df <- readr::read_csv(here::here(tidy, "danos_materiais.csv")) |>
  dplyr::filter(ano_evento < 2025)
df <- df |>
  dplyr::mutate(
    valor_perdas_danos_ensino = dm_inst_ensino_valor + pepl_ensino_r,
    valor_perdas_danos_saude = pepl_assis_med_e_emergen_r + dm_inst_saude_valor
  )
da24 <- readr::read_rds(here::here(tidy, "dados_2024.rds"))
am <- readxl::read_excel("amazonialegal.xlsx") |>
  dplyr::mutate(codigo_ibge = as.numeric(CD_MUN))
shp_mun <- geobr::read_municipality(year = 2022) |>
  dplyr::semi_join(am, dplyr::join_by(code_muni == codigo_ibge))

df |>
  dplyr::summarise(valor = sum(valor_perdas_danos_ensino), .by = ano_evento) |>
  readr::write_csv(here::here(tidy, "valor_perdas_ensino_ano.csv"))

df |>
  dplyr::filter(valor_perdas_danos_ensino > 0) |>
  dplyr::count(descricao_tipologia, sort = TRUE) |>
  janitor::adorn_totals() |>
  readr::write_csv(here::here(tidy, "causas_danos_perdas_ensino.csv"))
