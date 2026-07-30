dd <- here::here("202607_saneamento_tis/dados/tidy")

# Abastecimento
abastecimento <- dd |>
  here::here("abastecimento.csv") |>
  readr::read_csv()

abastecimento |>
  dplyr::summarise(n = sum(n), .by = c(agua, variavel)) |>
  tidyr::pivot_wider(names_from = variavel, values_from = n) |>
  dplyr::arrange(dplyr::desc(moradores)) |>
  dplyr::mutate(
    pct_dom = domicilios / sum(domicilios),
    pct_mor = moradores / sum(moradores)
  ) |>
  clipr::write_clip()


## TIs - acesso a rede geral
abastecimento_rede_geral <- abastecimento |>
  dplyr::mutate(uf = dplyr::coalesce(abbrev_state, stringr::str_extract(
    terra_indigena_por_unidade_da_federacao, "[A-Z]{2}"
  ))) |>
  dplyr::summarise(
    n = sum(n),
    .by = c(
      code_indigenous_land, name_indigenous_land, uf, fase_ti, agua, variavel
    )
  ) |>
  tidyr::pivot_wider(names_from = variavel, values_from = n) |>
  dplyr::group_by(code_indigenous_land, name_indigenous_land, uf, fase_ti) |>
  dplyr::mutate(
    total_domicilios = sum(domicilios, na.rm = TRUE),
    total_moradores = sum(moradores, na.rm = TRUE)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(agua == "Rede geral de distribuição") |>
  dplyr::mutate(pct_dom = domicilios / total_domicilios, pct_mor = moradores / total_moradores)
readr::write_csv(abastecimento_rede_geral, here::here(dd, "abastecimento_rede_geral.csv"))

abastecimento_rede_geral |>
  dplyr::filter(total_domicilios > 0, total_moradores > 0, domicilios == 0, moradores == 0) |>
  dplyr::distinct(code_indigenous_land, name_indigenous_land, uf, fase_ti, total_moradores, total_domicilios)

## TIs sem acesso a poço, fonte, nascente ou mina
abastecimento |>
  dplyr::mutate(uf = dplyr::coalesce(abbrev_state, stringr::str_extract(
    terra_indigena_por_unidade_da_federacao, "[A-Z]{2}"
  ))) |>
  dplyr::filter(agua == "Poço, fonte, nascente ou mina") |>
  dplyr::summarise(
    n = sum(n),
    .by = c(code_indigenous_land, name_indigenous_land, uf, fase_ti)
  ) |>
  dplyr::filter(n == 0) |>
  dplyr::distinct(code_indigenous_land, name_indigenous_land, uf, fase_ti) |>
  clipr::write_clip()

# Esgoto ----

## TIs - acesso a rede geral
esgoto_geral <- esgoto |>
  dplyr::mutate(uf = dplyr::coalesce(abbrev_state, stringr::str_extract(
    terra_indigena_por_unidade_da_federacao, "[A-Z]{2}"
  ))) |>
  dplyr::summarise(
    n = sum(n),
    .by = c(
      code_indigenous_land, name_indigenous_land, uf, fase_ti, esgoto, variavel
    )
  ) |>
  tidyr::pivot_wider(names_from = variavel, values_from = n) |>
  dplyr::group_by(code_indigenous_land, name_indigenous_land, uf, fase_ti) |>
  dplyr::mutate(
    total_domicilios = sum(domicilios, na.rm = TRUE),
    total_moradores = sum(moradores, na.rm = TRUE)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(esgoto == "Rede geral, rede pluvial ou fossa ligada à rede") |>
  dplyr::mutate(pct_dom = domicilios / total_domicilios, pct_mor = moradores / total_moradores)

esgoto_geral |>
  clipr::write_clip()
