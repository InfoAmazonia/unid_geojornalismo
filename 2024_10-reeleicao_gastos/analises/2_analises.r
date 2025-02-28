rreo_filtrado <- readr::read_csv("202409_reeleicao_gastos/data-tidy/rreo_filtrado.csv")
dplyr::glimpse(rreo_filtrado)
rcl <- readr::read_csv("202409_reeleicao_gastos/data-tidy/rcl.csv")
rcl <- rcl |>
  dplyr::filter(coluna == "TOTAL (ÚLTIMOS 12 MESES)") |>
  dplyr::select(-rotulo, -conta) |>
  dplyr::rename(valor_receita = valor)


despesas <- rreo_filtrado |>
  dplyr::group_by(exercicio, instituicao) |>
  dplyr::summarise(valor_despesas = sum(valor), .groups = "drop")

df <- despesas |>
  dplyr::left_join(rcl, dplyr::join_by(exercicio, instituicao)) |>
  dplyr::mutate(
    prop_despesa = valor_despesas/valor_receita,
    prop_despesa_txt = scales::percent(prop_despesa, .1)
  )
dplyr::glimpse(df)
readr::write_csv(df, "202409_reeleicao_gastos/data-tidy/df.csv")


df_total <- df |>
  dplyr::group_by(instituicao) |>
  dplyr::summarise(valor_despesas = sum(valor_despesas), valor_receita = sum(valor_receita)) |>
  janitor::adorn_totals() |>
  dplyr::mutate(
    prop_despesa = valor_despesas/valor_receita,
    prop_despesa_txt = scales::percent(prop_despesa, .01),
    valor_despesas = scales::dollar(valor_despesas, big.mark = ".", decimal.mark = ","),
    valor_receita = scales::dollar(valor_receita, big.mark = ".", decimal.mark = ",")
  ) |>
  dplyr::select(-prop_despesa)
readr::write_csv(df_total, "202409_reeleicao_gastos/data-tidy/df_total.csv")


df_ano <- df |>
  dplyr::group_by(exercicio) |>
  dplyr::summarise(valor_despesas = sum(valor_despesas), valor_receita = sum(valor_receita)) |>
  janitor::adorn_totals() |>
  dplyr::mutate(
    prop_despesa = valor_despesas/valor_receita,
    prop_despesa_txt = scales::percent(prop_despesa, .01),
    valor_despesas = scales::dollar(valor_despesas, big.mark = ".", decimal.mark = ","),
    valor_receita = scales::dollar(valor_receita, big.mark = ".", decimal.mark = ",")
  ) |>
  dplyr::select(-prop_despesa)
readr::write_csv(df_ano, "202409_reeleicao_gastos/data-tidy/df_ano.csv")

df_ano_mun <- df |>
  dplyr::group_by(exercicio, instituicao) |>
  dplyr::summarise(
    valor_despesas = sum(valor_despesas), valor_receita = sum(valor_receita),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    prop_despesa = valor_despesas/valor_receita,
    prop_despesa_txt = scales::percent(prop_despesa, .01),
    valor_despesas = scales::dollar(valor_despesas, big.mark = ".", decimal.mark = ","),
    valor_receita = scales::dollar(valor_receita, big.mark = ".", decimal.mark = ",")
  ) |>
  dplyr::select(-prop_despesa)
readr::write_csv(df_ano_mun, "202409_reeleicao_gastos/data-tidy/df_ano_mun.csv")

df_ano_mun |>
  tidyr::pivot_wider(
    names_from = exercicio,
    values_from = c(valor_despesas, valor_receita, prop_despesa_txt)
  ) |>
  dplyr::select(instituicao, dplyr::contains("prop")) |>
  dplyr::select(instituicao, prop_despesa_txt_2021)
  # dplyr::select(instituicao, dplyr::contains("valor_despesas"))
  # dplyr::glimpse()
