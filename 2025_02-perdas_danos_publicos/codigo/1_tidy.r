dt <- here::here("202502_perdas_publicas/data-tidy")

# Filtro Amazônia Legal
am <- readxl::read_excel("amazonialegal.xlsx") |>
  dplyr::mutate(codigo_ibge = as.numeric(CD_MUN))

# Histórico 1991-2023 Atlas Desastres Naturais
da_hist <- dt |>
  here::here("BD_Atlas_1991_2023_v1.0_2024.04.29.xlsx") |>
  readxl::read_excel(3) |>
  dplyr::semi_join(am, dplyr::join_by(Cod_IBGE_Mun == codigo_ibge)) |>
  janitor::clean_names() |>
  dplyr::filter(sigla_uf %in% unique(am$SIGLA_UF)) |>
  dplyr::mutate(ano_evento = lubridate::year(data_evento))
# readr::write_rds(da_hist, here::here(tidy, "dados_1991_2023.rds"))

# De-para cobrade
depara_cobrade <- da_hist |>
  dplyr::distinct(cod_cobrade, descricao_tipologia)

# Dados 2024 Danos Informados
da24 <- dt |>
  here::here("danos_informados_2024.csv") |>
  readr::read_csv(skip = 4, col_types = paste0(rep("c", 53), collapse = "")) |>
  janitor::clean_names() |>
  dplyr::mutate(
    municipio = stringr::str_replace(municipio, "`", "'"),
    municipio = dplyr::case_when(
      municipio == "Eldorado dos Carajás" ~ "Eldorado do Carajás",
      municipio == "Fortaleza do Tabocão" ~ "Tabocão",
      municipio == "Poxoréo" ~ "Poxoréu",
      municipio == "Mirassol D'Oeste" ~ "Mirassol d'Oeste",
      TRUE ~ municipio
    ),
    data = lubridate::dmy(registro),
    ano_evento = lubridate::year(data),
    cod_cobrade = as.numeric(stringr::str_extract(cobrade, "[0-9]+"))
  ) |>
  dplyr::inner_join(am, dplyr::join_by(municipio == NM_MUN, uf == SIGLA_UF)) |>
  dplyr::filter(uf %in% unique(am$SIGLA_UF)) |>
  dplyr::left_join(depara_cobrade, dplyr::join_by(cod_cobrade))
readr::write_rds(da24, here::here(dt, "dados_2024.rds"))

dm_hist <- da_hist |>
  dplyr::select(-dplyr::matches("^d[ha]_")) |>
  dplyr::rename(
    codigo_ibge = cod_ibge_mun, municipio = nome_municipio, uf = sigla_uf,
    data = data_evento, protocolo = protocolo_s2i_d
  )

dplyr::glimpse(dm_hist)

dm_24 <- da24 |>
  dplyr::select(-dplyr::matches("^d[ha]_")) |>
  dplyr::rename(
    dm_inst_ensino_danificadas = dm_instalacoes_publicas_de_ensino_danificadas,
    dm_inst_ensino_destruidas = dm_instalacoes_publicas_de_ensino_destruidas,
    dm_inst_ensino_valor = dm_instalacoes_publicas_de_ensino_valor,
    dm_inst_servicos_danificadas = dm_instalacoes_publicas_prestadoras_de_outros_servicos_danificadas,
    dm_inst_servicos_destruidas = dm_instalacoes_publicas_prestadoras_de_outros_servicos_destruidas,
    dm_inst_servicos_valor = dm_instalacoes_publicas_prestadoras_de_outros_servicos_valor,
    dm_inst_comuni_danificadas = dm_instalacoes_publicas_de_uso_comunitario_danificadas,
    dm_inst_comuni_destruidas = dm_instalacoes_publicas_de_uso_comunitario_destruidas,
    dm_inst_comuni_valor = dm_instalacoes_publicas_de_uso_comunitario_valor,
    dm_obras_de_infra_danificadas = dm_obras_de_infraestrutura_publica_danificadas,
    dm_obras_de_infra_destruidas = dm_obras_de_infraestrutura_publica_destruidas,
    dm_obras_de_infra_valor = dm_obras_de_infraestrutura_publica_valor,
    dm_uni_habita_danificadas = dm_unidades_habitacionais_danificadas,
    dm_uni_habita_destruidas = dm_unidades_habitacionais_destruidas,
    dm_uni_habita_valor = dm_unidades_habitacionais_valor,
    dm_inst_saude_danificadas = dm_instalacoes_publicas_de_saude_danificadas,
    dm_inst_saude_destruidas = dm_instalacoes_publicas_de_saude_destruidas,
    dm_inst_saude_valor = dm_instalacoes_publicas_de_saude_valor,
    pepl_assis_med_e_emergen_r = pepl_assistencia_medica_saude_publica_e_atendimento_de_emergencias_medicas_r,
    pepl_abast_de_agua_pot_r = pepl_abastecimento_de_agua_potavel_r,
    pepl_sist_de_esgotos_sanit_r = pepl_esgoto_de_aguas_pluviais_e_sistema_de_esgotos_sanitarios_r,
    pepl_sis_limp_e_rec_lixo_r = pepl_sistema_de_limpeza_urbana_e_de_recolhimento_e_destinacao_do_lixo_r,
    pepl_sis_cont_pragas_r = pepl_sistema_de_desinfestacao_desinfeccao_do_habitat_controle_de_pragas_e_vetores_r,
    pepl_distrib_energia_r = pepl_geracao_e_distribuicao_de_energia_eletrica_r,
    pepl_tran_loc_reg_l_curso_r = pepl_transportes_locais_regionais_e_de_longo_curso_r,
    pepl_distrib_combustiveis_r = pepl_distribuicao_de_combustiveis_especialmente_os_de_uso_domestico_r,
    regiao = NM_REGIAO
  ) |>
  dplyr::mutate(dplyr::across(
    dplyr::matches("^(dm|pe)"),
    \(x) readr::parse_number(x, locale = readr::locale(decimal_mark = ",", grouping_mark = "."))
  ))

dplyr::glimpse(dm_24)
danos_materiais <- dm_hist |>
  dplyr::bind_rows(dm_24) |>
  dplyr::select(-c(
    registro:SEDES_FORA_AMAZÔNIA, pepr_descricao, pepl_descricao
  ))

col_publico <- danos_materiais |>
  colnames() |>
  purrr::keep(~stringr::str_detect(.x, "pepl")) |>
  purrr::discard(~stringr::str_detect(.x, "total"))
clipr::write_clip(col_publico)
danos_materiais <- danos_materiais |>
  dplyr::mutate(pepl_total_publico = ifelse(
    is.na(pepl_total_publico),
    pepl_assis_med_e_emergen_r + pepl_abast_de_agua_pot_r + pepl_sist_de_esgotos_sanit_r + pepl_sis_limp_e_rec_lixo_r + pepl_sis_cont_pragas_r + pepl_distrib_energia_r + pepl_telecomunicacoes_r + pepl_tran_loc_reg_l_curso_r + pepl_distrib_combustiveis_r + pepl_seguranca_publica_r + pepl_ensino_r,
    pepl_total_publico
  ))

col_dm <- danos_materiais |>
  colnames() |>
  purrr::keep(~stringr::str_detect(.x, "dm_")) |>
  purrr::discard(~stringr::str_detect(.x, "total|descricao"))
clipr::write_clip(col_dm)
danos_materiais <- danos_materiais |>
  dplyr::mutate(dm_total_danos_materiais = ifelse(
    is.na(dm_total_danos_materiais),
    dm_uni_habita_danificadas + dm_uni_habita_destruidas + dm_uni_habita_valor + dm_inst_saude_danificadas + dm_inst_saude_destruidas + dm_inst_saude_valor + dm_inst_ensino_danificadas + dm_inst_ensino_destruidas + dm_inst_ensino_valor + dm_inst_servicos_danificadas + dm_inst_servicos_destruidas + dm_inst_servicos_valor + dm_inst_comuni_danificadas + dm_inst_comuni_destruidas + dm_inst_comuni_valor + dm_obras_de_infra_danificadas + dm_obras_de_infra_destruidas + dm_obras_de_infra_valor,
    dm_total_danos_materiais
  ))

readr::write_csv(danos_materiais, here::here(dt, "danos_materiais.csv"))

