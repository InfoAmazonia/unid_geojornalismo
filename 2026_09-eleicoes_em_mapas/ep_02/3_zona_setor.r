# Calcula variáveis por ZONA a partir de SETOR CENSITÁRIO

zonas <- readr::read_rds(here::here(dd, "geo_resultado_zona.rds"))
zonas2022 <- dplyr::filter(zonas, ano == 2022)
readr::write_rds(zonas2022, here::here(dd, "geo_resultado_zona_2022.rds"))
setores <- readr::read_rds(here::here(dd, "setores2010.rds"))
setores2022 <- readr::read_rds(here::here(dd, "setores2022.rds"))

# sf::st_crs(setores) == sf::st_crs(zonas)

# Junta zona com setor censitário, calcula área de interseção e percentual de área do setor na zona

zona_setor <- function(uf, shp_zonas, shp_setor, year) {
  uf_code <- switch(
    uf,
    "AC" = 12,
    "AM" = 13,
    "AP" = 16,
    "PA" = 15,
    "RO" = 11,
    "RR" = 14,
    "TO" = 17,
    "MA" = 21,
    "MT" = 51
  )
  zona <- shp_zonas |>
    dplyr::filter(
      stringr::str_detect(id_municipio, paste0("^", uf_code)), ano == year
    )
  setor <- dplyr::filter(shp_setor, sigla_uf == uf) |>
    sf::st_make_valid()
  zona |>
    sf::st_make_valid() |>
    sf::st_intersection(setor) |>
    dplyr::mutate(area = sf::st_area(geom)) |>
    dplyr::group_by(ano, id_municipio, nr_zona) |>
    dplyr::mutate(
      pct_area_setor = as.numeric(area / sum(area))
    ) |>
    dplyr::ungroup()
}

# Zona x Setor Censitário - 2010
zona_setor_2010 <- purrr::map(
  c("AC", "AM", "AP", "PA", "RO", "RR", "TO", "MA", "MT"),
  \(x) zona_setor(x, zonas, setores, 2010)
)
zona_setor_2010 <- purrr::list_rbind(zona_setor_2010)
readr::write_rds(zona_setor_2010, here::here(dd, "zona_setor_2010.rds"))

# Zona x Setor Censitário - 2022
zona_setor_2022_ac <- zona_setor("AC", zonas, setores2022, 2022)
zona_setor_2022_am <- zona_setor("AM", zonas, setores2022, 2022)
zona_setor_2022_ap <- zona_setor("AP", zonas, setores2022, 2022)
zona_setor_2022_pa <- zona_setor("PA", zonas, setores2022, 2022)
zona_setor_2022_ro <- zona_setor("RO", zonas, setores2022, 2022)
zona_setor_2022_rr <- zona_setor("RR", zonas, setores2022, 2022)
zona_setor_2022_to <- zona_setor("TO", zonas, setores2022, 2022)
zona_setor_2022_ma <- zona_setor("MA", zonas, setores2022, 2022)
zona_setor_2022_mt <- zona_setor("MT", zonas, setores2022, 2022)
zona_setor_2022 <- dplyr::bind_rows(
  zona_setor_2022_ac, zona_setor_2022_am, zona_setor_2022_ap,
  zona_setor_2022_pa, zona_setor_2022_ro, zona_setor_2022_rr,
  zona_setor_2022_to, zona_setor_2022_ma, zona_setor_2022_mt
)
# zona_setor_2022 <- purrr::map(
#   c("AC", "AM", "AP", "PA", "RO", "RR", "TO", "MA", "MT"),
#   \(x) zona_setor(x, zonas, setores2022, 2022)
# )
# zona_setor_2022 <- purrr::list_rbind(zona_setor_2022)
readr::write_rds(zona_setor_2022, here::here(dd, "zona_setor_2022.rds"))
tbl_zona_setor_2022 <- dplyr::as_tibble(zona_setor_2022) |>
  dplyr::select(
    ano, sigla_uf, id_municipio, nr_zona, CD_SETOR, pct_area_setor
  )

# URBANIZAÇÃO ------------------------------------------------------------------
# % de urbanização por zona - 2010
zona_urbanizacao_2010 <- zona_setor_2010 |>
  dplyr::as_tibble() |>
  dplyr::filter(TIPO == "URBANO") |>
  dplyr::group_by(ano, id_municipio, nr_zona) |>
  dplyr::summarise(pct_urbano = sum(pct_area_setor), .groups = "drop")
# % de urbanização por zona - 2022
zona_urbanizacao_2022 <- zona_setor_2022 |>
  dplyr::as_tibble() |>
  dplyr::filter(SITUACAO == "Urbana") |>
  dplyr::group_by(ano, id_municipio, nr_zona) |>
  dplyr::summarise(pct_urbano = sum(pct_area_setor), .groups = "drop")

readr::write_rds(zona_urbanizacao_2010, here::here(dd, "zona_urbanizacao_2010.rds"))
readr::write_rds(zona_urbanizacao_2022, here::here(dd, "zona_urbanizacao_2022.rds"))


# IDADE ------------------------------------------------------------------------

ibge_2010_idade <- readr::read_csv(here::here(dd, "ibge_2010_idade.csv"), col_types = "c")
ibge_2010_idade_faixa <- ibge_2010_idade |>
  dplyr::mutate(faixa_idade = dplyr::case_when(
    idade < 16 ~ "até 15 anos",
    idade < 18 ~ "16 a 17 anos",
    idade < 25 ~ "18 a 24 anos",
    idade < 35 ~ "25 a 34 anos",
    idade < 45 ~ "35 a 44 anos",
    idade < 60 ~ "45 a 59 anos",
    idade < 70 ~ "60 a 69 anos",
    idade < 80 ~ "70 a 79 anos",
    TRUE ~ "80 anos ou mais"
  )) |>
  dplyr::group_by(id_setor_censitario, sigla_uf, faixa_idade) |>
  dplyr::summarise(pessoas = sum(pessoas), .groups = "drop")

zona_setor_2010_idade <- zona_setor_2010 |>
  dplyr::left_join(
    ibge_2010_idade, dplyr::join_by(sigla_uf, CD_GEOCODI == id_setor_censitario)
  )
readr::write_rds(zona_setor_2010_idade, here::here(dd, "zona_setor_2010_idade.rds"))

zona_setor_2022_idade <- tbl_zona_setor_2022 |>
  dplyr::left_join(
    ibge_2022_idade_faixa, dplyr::join_by(sigla_uf, CD_SETOR == id_setor_censitario)
  )
readr::write_rds(zona_setor_2022_idade, here::here(dd, "zona_setor_2022_idade.rds"))


# RENDA ------------------------------------------------------------------------

ibge_2010_renda <- readr::read_csv(here::here(dd, "ibge_2010_renda.csv"), col_types = "c")
zona_setor_2010_renda <- zona_setor_2010 |>
  tibble::as_tibble() |>
  dplyr::left_join(
    ibge_2010_renda, dplyr::join_by(sigla_uf, CD_GEOCODI == id_setor_censitario)
  )
zona_setor_2010_renda <- zona_setor_2010_renda |>
  dplyr::mutate(renda_media_pond = renda_media_ajustada * pct_area_setor) |>
  dplyr::group_by(ano, id_municipio, nr_zona) |>
  dplyr::summarise(renda_media_zona = sum(renda_media_pond, na.rm = TRUE), .groups = "drop")
readr::write_rds(zona_setor_2010_renda, here::here(dd, "zona_setor_2010_renda.rds"))

zona_setor_2022_renda <- tbl_zona_setor_2022 |>
  dplyr::left_join(
    ibge_2022_renda, dplyr::join_by(sigla_uf, CD_SETOR == id_setor_censitario)
  ) |>
  dplyr::mutate(renda_media_pond = renda_media * pct_area_setor) |>
  dplyr::group_by(ano, id_municipio, nr_zona) |>
  dplyr::summarise(renda_media_zona = sum(renda_media_pond, na.rm = TRUE), .groups = "drop")
readr::write_rds(zona_setor_2022_renda, here::here(dd, "zona_setor_2022_renda.rds"))


# RAÇA -------------------------------------------------------------------------

ibge_2010_raca <- readr::read_csv(here::here(dd, "ibge_2010_raca.csv"), col_types = "c")
zona_setor_2010_raca <- zona_setor_2010 |>
  dplyr::as_tibble() |>
  dplyr::select(ano, sigla_uf, id_municipio, nr_zona, CD_GEOCODI, pct_area_setor) |>
  dplyr::left_join(
    ibge_2010_raca, dplyr::join_by(sigla_uf, CD_GEOCODI == id_setor_censitario)
  ) |>
  dplyr::mutate(pessoas_pond = pessoas * pct_area_setor) |>
  dplyr::group_by(ano, id_municipio, nr_zona, raca_cor) |>
  dplyr::summarise(pessoas_zona = sum(pessoas_pond, na.rm = TRUE), .groups = "drop") |>
  dplyr::filter(!is.na(raca_cor))
readr::write_rds(zona_setor_2010_raca, here::here(dd, "zona_setor_2010_raca.rds"))

zona_setor_2022_raca <- tbl_zona_setor_2022 |>
  dplyr::left_join(
    ibge_2022_raca, dplyr::join_by(sigla_uf, CD_SETOR == id_setor_censitario)
  ) |>
  dplyr::filter(!is.na(pessoas)) |>
  dplyr::mutate(pessoas_pond = pessoas * pct_area_setor) |>
  dplyr::group_by(ano, id_municipio, nr_zona, raca_cor) |>
  dplyr::summarise(pessoas_zona = sum(pessoas_pond, na.rm = TRUE), .groups = "drop") |>
  dplyr::filter(!is.na(raca_cor))
readr::write_rds(zona_setor_2022_raca, here::here(dd, "zona_setor_2022_raca.rds"))


# ALFABETIZAÇÃO ----------------------------------------------------------------

ibge_2010_alfabetizacao <- readr::read_csv(here::here(dd, "ibge_2010_alfabetizacao.csv"), col_types = "c")
zona_setor_2010_alfabetizacao <- zona_setor_2010 |>
  dplyr::as_tibble() |>
  dplyr::select(ano, sigla_uf, id_municipio, nr_zona, CD_GEOCODI, pct_area_setor) |>
  dplyr::left_join(
    ibge_2010_alfabetizacao, dplyr::join_by(sigla_uf, CD_GEOCODI == id_setor_censitario)
  ) |>
  dplyr::mutate(pessoas_pond = alfabetizadas * pct_area_setor) |>
  dplyr::group_by(ano, id_municipio, nr_zona) |>
  dplyr::summarise(pessoas_zona = sum(pessoas_pond, na.rm = TRUE), .groups = "drop")
readr::write_rds(zona_setor_2010_alfabetizacao, here::here(dd, "zona_setor_2010_alfabetizacao.rds"))

zona_setor_2022_alfabetizacao <- tbl_zona_setor_2022 |>
  dplyr::left_join(
    ibge_2022_alfabetizacao, dplyr::join_by(sigla_uf, CD_SETOR == id_setor_censitario)
  ) |>
  dplyr::mutate(pessoas_pond = pessoas * pct_area_setor) |>
  dplyr::group_by(ano, id_municipio, nr_zona, variavel) |>
  dplyr::summarise(
    pessoas_zona = sum(pessoas_pond, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::filter(!is.na(variavel))
readr::write_rds(zona_setor_2022_alfabetizacao, here::here(dd, "zona_setor_2022_alfabetizacao.rds"))


