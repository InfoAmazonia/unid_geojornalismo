data_raw <- here::here("202410_infratores_doadores/data-raw")
data_tidy <- here::here("202410_infratores_doadores/data-tidy")

infracoes_cpf <- readr::read_csv(here::here(data_tidy, "infracoes_cpf.csv"))
receita <- readr::read_csv2(
  here::here(data_raw, "receita.csv"),
  locale = readr::locale(encoding = "Latin1")
) |>
  janitor::clean_names()

receita <- receita |>
  dplyr::mutate(
    cpf_cnpj_valido_doacao = purrr::map_vec(nr_cpf_cnpj_doador, check_cpf)
  )

# infracoes_doc_valido <- dplyr::filter(infracoes_tidy, cpf_cnpj_valido)
# receita_doc_valido <- dplyr::filter(receita, cpf_cnpj_valido_doacao)

infracoes_select <- infracoes_cpf |>
  dplyr::select(
    seq_auto_infracao:num_auto_infracao, tipo_auto:gravidade_infracao,
    efeito_meio_ambiente:dat_hora_auto_infracao,
    cod_municipio:cpf_cnpj_infrator, classificacao_area:ds_biomas_atingidos,
    cpf_cnpj, cpf_valido
  )

df_join <- infracoes_select |>
  dplyr::inner_join(
    receita, dplyr::join_by(cpf_cnpj == nr_cpf_cnpj_doador),
    relationship = "many-to-many"
  )

df_join_validos <- df_join |>
  dplyr::filter(
    cpf_valido, cpf_cnpj != "00000000000"
  )

depara_amazonia <- readr::read_csv("depara_ibge_tse.csv")
amazonia_legal <- "202408_seca/data-raw/amazonialegal.xlsx" |>
  readxl::read_excel() |>
  janitor::clean_names() |>
  dplyr::mutate(cod_municipio = as.numeric(cd_mun)) |>
  dplyr::inner_join(
    depara_amazonia, dplyr::join_by(cod_municipio == cd_municipio_ibge)
  )

df_join_validos |>
  dplyr::filter(sit_cancelado == "N") |>
  dplyr::semi_join(amazonia_legal, dplyr::join_by(sg_ue == cd_municipio_tse)) |>
  dplyr::distinct() |>
  readr::write_csv(here::here(data_tidy, "infratores_doadores_filtrado.csv"))

df_join_validos |>
  dplyr::filter(sq_candidato == 110002252795) |>
  dplyr::group_by(nm_candidato, nome_infrator) |>
  dplyr::distinct(seq_auto_infracao, num_auto_infracao, .keep_all = TRUE) |>
  dplyr::glimpse()
  dplyr::summarise(
    val = sum(vr_receita)
  )

infracoes_tidy |>
  dplyr::filter(nome_infrator == "CARLOS ERNESTO AUGUSTIN") |>
  dplyr::select(seq_auto_infracao, num_auto_infracao, val_auto_infracao)

df_join_validos |>
  dplyr::filter(sq_candidato == 110002252795) |>
  dplyr::distinct(
    nome_infrator, vr_receita, sq_receita
  ) |>
  janitor::adorn_totals()
  
  
  dplyr::group_by(nm_candidato, nome_infrator) |>
  dplyr::distinct(seq_auto_infracao, num_auto_infracao, .keep_all = TRUE) |>
  dplyr::glimpse()
  dplyr::summarise(
    val = sum(vr_receita)
  )
