dd <- here::here("ep_02/dados")

# dados demográficos por setor censitário - IBGE

# religião - dida 31/08/2026

# idade
# renda
# raça/cor
# escolaridade (alfabetizacao)
# urbano/rural
# população
# saneamento

# 2010

# IDADE
ibge_2010_idade <- readr::read_csv(
  here::here(dd, "ibge_2010_idade.csv"), col_types = "c"
) |>
  dplyr::filter(
    !dplyr::if_all(v022:v134, is.na),
    sigla_uf %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO", "MA", "MT")
  ) |>
  purrr::set_names("id_setor_censitario", "sigla_uf", 0:100) |>
  tidyr::pivot_longer(
    cols = 3:103, names_to = "idade", values_to = "pessoas"
  ) |>
  dplyr::mutate(idade = as.integer(idade), ano_censo = 2010L)

ibge_2010_idade_faixa <- ibge_2010_idade |>
  dplyr::mutate(faixa_idade = dplyr::case_when(
    idade < 4 ~ "0 a 4 anos",
    idade < 10 ~ "5 a 9 anos",
    idade < 15 ~ "10 a 14 anos",
    idade < 20 ~ "15 a 19 anos",
    idade < 25 ~ "20 a 24 anos",
    idade < 30 ~ "25 a 29 anos",
    idade < 40 ~ "30 a 39 anos",
    idade < 50 ~ "40 a 49 anos",
    idade < 60 ~ "50 a 59 anos",
    idade < 70 ~ "60 a 69 anos",
    TRUE ~ "70 anos ou mais"
  )) |>
  dplyr::group_by(id_setor_censitario, sigla_uf, faixa_idade) |>
  dplyr::summarise(pessoas = sum(pessoas), .groups = "drop")

readr::write_csv(ibge_2010_idade_faixa, here::here(dd, "ibge_2010_idade.csv"))

# RENDA
# SELECT `id_setor_censitario`, `sigla_uf`, `v001`, `v002`, `v003`, `v004`,
# `v005`, `v006`, `v007`, `v008`, `v009`, `v010`, `v020` FROM
# `basedosdados.br_ibge_censo_demografico.setor_censitario_pessoa_renda_2010`

# SELECT `id_setor_censitario`, `sigla_uf`,
# v086 // Pessoas responsáveis moradoras em domicílios particulares permanentes com ou sem rendimento
# v088 // Total do rendimento nominal mensal das pessoas responsáveis moradoras em domicílios particulares permanentes
# `basedosdados.br_ibge_censo_demografico.setor_censitario_responsavel_renda_2010

ibge_2010_renda <- readr::read_csv(
  here::here(dd, "ibge_2010_renda.csv"), col_types = "c"
) |>
  dplyr::filter(!dplyr::if_all(v086:v088, is.na),) |>
  dplyr::transmute(
    id_setor_censitario, sigla_uf,
    responsaveis = v086,
    renda_nominal_mensal = v088,
    renda_media = renda_nominal_mensal / responsaveis,
    renda_media_ajustada = deflateBR::deflate(
      renda_media,
      nominal_dates = lubridate::as_date("2010-10-31"),
      real_date = "12/2022", index = "ipca"
    )
  )
readr::write_csv(ibge_2010_renda, here::here(dd, "ibge_2010_renda.csv"))

# RACA
# SELECT
# id_setor_censitario, sigla_uf, v001, v002, v003, v004, v005, v006
# FROM `basedosdados.br_ibge_censo_demografico.setor_censitario_raca_idade_genero_2010`
# where sigla_uf in ("AC", "AM", "AP", "PA", "RO", "RR", "TO", "MA", "MT")

ibge_2010_raca <- readr::read_csv(
  here::here(dd, "ibge_2010_raca.csv"), col_types = "c"
) |>
  dplyr::filter(!dplyr::if_all(v001:v006, is.na)) |>
  tidyr::pivot_longer(
    cols = 3:8, names_to = "raca_cor", values_to = "pessoas"
  ) |>
  dplyr::mutate(raca_cor = dplyr::recode_values(
    raca_cor,
    "v001" ~ "total residentes", "v002" ~ "branca", "v003" ~ "preta",
    "v004" ~ "amarela", "v005" ~ "parda", "v006" ~ "indígena"
  ))
readr::write_csv(ibge_2010_raca, here::here(dd, "ibge_2010_raca.csv"))

# ALFABETIZACAO - a partir de 15 anos (critério 2022)
# SELECT id_setor_censitario, sigla_uf,
# v012 + v013 + v014 + v015 + v016 + v017 + v018 + v019 + v020 + v021 + v022 + v023 + v024 + v025 + v026 + v027 + v028 + v029 + v030 + v031 + v032 + v033 + v034 + v035 + v036 + v037 + v038 + v039 + v040 + v041 + v042 + v043 + v044 + v045 + v046 + v047 + v048 + v049 + v050 + v051 + v052 + v053 + v054 + v055 + v056 + v057 + v058 + v059 + v060 + v061 + v062 + v063 + v064 + v065 + v066 + v067 + v068 + v069 + v070 + v071 + v072 + v073 + v074 + v075 + v076 + v077 as alfabetizadas
# FROM `basedosdados.br_ibge_censo_demografico.setor_censitario_alfabetizacao_total_2010`
# where sigla_uf in ("AC", "AM", "AP", "PA", "RO", "RR", "TO", "MA", "MT")

ibge_2010_alfabetizacao <- readr::read_csv(
  here::here(dd, "ibge_2010_alfabetizacao.csv"), col_types = "c"
) |>
  dplyr::filter(!is.na(alfabetizadas))
readr::write_csv(ibge_2010_alfabetizacao, here::here(dd, "ibge_2010_alfabetizacao.csv"))

# URBANO / RURAL: disponível pelo próprio shp de setores
# Calculado diretamente por zona em 3_zona_setor.r


# 2022 -----------

ufs <- tibble::tribble(
  ~sigla_uf, ~id_uf,
  "AC", 12,
  "AM", 13,
  "AP", 16,
  "PA", 15,
  "RO", 11,
  "RR", 14,
  "TO", 17,
  "MA", 21,
  "MT", 51
)

vars <- tibble::tribble(
  ~variavel, ~descricao, ~tipo,
  "V00001", "Total de pessoas", "geral",
  # Idade
  "V01031", "0 a 4 anos", "idade",
  "V01032", "5 a 9 anos", "idade",
  "V01033", "10 a 14 anos", "idade",
  "V01034", "15 a 19 anos", "idade",
  "V01035", "20 a 24 anos", "idade",
  "V01036", "25 a 29 anos", "idade",
  "V01037", "30 a 39 anos", "idade",
  "V01038", "40 a 49 anos", "idade",
  "V01039", "50 a 59 anos", "idade",
  "V01040", "60 a 69 anos", "idade",
  "V01041", "70 anos ou mais", "idade",
  # Alfabetização
  "V00644", "15 a 19 anos", "alfabetizacao",
  "V00645", "20 a 24 anos", "alfabetizacao",
  "V00646", "25 a 29 anos", "alfabetizacao",
  "V00647", "30 a 34 anos", "alfabetizacao",
  "V00648", "35 a 39 anos", "alfabetizacao",
  "V00649", "40 a 44 anos", "alfabetizacao",
  "V00650", "45 a 49 anos", "alfabetizacao",
  "V00651", "50 a 54 anos", "alfabetizacao",
  "V00652", "55 a 59 anos", "alfabetizacao",
  "V00653", "60 a 64 anos", "alfabetizacao",
  "V00654", "65 a 69 anos", "alfabetizacao",
  "V00655", "70 a 79 anos", "alfabetizacao",
  "V00656", "80 anos ou mais", "alfabetizacao",
  "V00748", "Pessoas alfabetizadas, 15 a 19 anos", "alfabetizacao",
  "V00749", "Pessoas alfabetizadas, 20 a 24 anos", "alfabetizacao",
  "V00750", "Pessoas alfabetizadas, 25 a 29 anos", "alfabetizacao",
  "V00751", "Pessoas alfabetizadas, 30 a 34 anos", "alfabetizacao",
  "V00752", "Pessoas alfabetizadas, 35 a 39 anos", "alfabetizacao",
  "V00753", "Pessoas alfabetizadas, 40 a 44 anos", "alfabetizacao",
  "V00754", "Pessoas alfabetizadas, 45 a 49 anos", "alfabetizacao",
  "V00755", "Pessoas alfabetizadas, 50 a 54 anos", "alfabetizacao",
  "V00756", "Pessoas alfabetizadas, 55 a 59 anos", "alfabetizacao",
  "V00757", "Pessoas alfabetizadas, 60 a 64 anos", "alfabetizacao",
  "V00758", "Pessoas alfabetizadas, 65 a 69 anos", "alfabetizacao",
  "V00759", "Pessoas alfabetizadas, 70 a 79 anos", "alfabetizacao",
  "V00760", "Pessoas alfabetizadas, 80 anos ou mais", "alfabetizacao",
  # Raça/cor
  "V01317", "Cor ou raça é branca", "raca_cor",
  "V01318", "Cor ou raça é preta", "raca_cor",
  "V01319", "Cor ou raça é amarela", "raca_cor",
  "V01320", "Cor ou raça é parda", "raca_cor",
  "V01321", "Cor ou raça é indígena", "raca_cor"
) |>
  dplyr::mutate(variavel = tolower(variavel))
vars$variavel |>
  tolower() |>
  paste0(collapse = ", ") |>
  clipr::write_clip()

ibge2022 <- readr::read_csv(here::here(dd, "ibge_2022.csv"), col_types = "c") |>
  tidyr::pivot_longer(
    cols = 3:45, names_to = "variavel", values_to = "pessoas"
  ) |>
  dplyr::left_join(vars, by = "variavel") |>
  dplyr::mutate(ano_censo = 2022L)

# Idade
ibge_2022_idade_faixa <- ibge2022 |>
  dplyr::filter(tipo == "idade") |>
  dplyr::left_join(ufs, dplyr::join_by(id_uf)) |>
  dplyr::transmute(
    id_setor_censitario, sigla_uf, faixa_idade = descricao, pessoas
  )
readr::write_csv(ibge_2022_idade_faixa, here::here(dd, "ibge_2022_idade_faixa.csv"))

# Raça/cor
ibge_2022_raca <- ibge2022 |>
  dplyr::filter(tipo == "raca_cor") |>
  dplyr::left_join(ufs, dplyr::join_by(id_uf)) |>
  dplyr::transmute(
    id_setor_censitario, sigla_uf,
    raca_cor = stringr::str_extract(descricao, "\\w+$"),
    pessoas
  )
readr::write_csv(ibge_2022_raca, here::here(dd, "ibge_2022_raca.csv"))

# Alfabetização
ibge_2022_alfabetizacao <- ibge2022 |>
  dplyr::filter(tipo == "alfabetizacao") |>
  dplyr::mutate(variavel = ifelse(
    stringr::str_detect(descricao, "alfabetizadas"), "alfabetizadas", "total"
  )) |>
  dplyr::summarise(
    pessoas = sum(pessoas),
    .by = c(id_setor_censitario, id_uf, ano_censo, variavel)
  ) |>
  dplyr::filter(!is.na(pessoas)) |>
  dplyr::left_join(ufs, dplyr::join_by(id_uf)) |>
  dplyr::transmute(id_setor_censitario, sigla_uf, variavel, pessoas)
readr::write_csv(ibge_2022_alfabetizacao, here::here(dd, "ibge_2022_alfabetizacao.csv"))

# Renda
vars <- tibble::tribble(
  ~variavel, ~descricao,
  "V06001", "Pessoas responsáveis em domicílios particulares permanentes ocupados",
  "V06002", "Moradores em domicílios particulares permanentes ocupados",
  "V06003", "Variância do número de moradores em domicílios particulares permanentes ocupados",
  "V06004", "Valor do rendimento nominal médio mensal das pessoas responsáveis com rendimentos por domicílios particulares permanentes ocupados",
  "V06005", "Variância do rendimento nominal mensal das pessoas responsáveis com rendimentos por domicílios particulares permanentes ocupados",
  "V06006", "Valor do rendimento nominal mediano mensal das pessoas responsáveis com rendimentos por domicílios particulares permanentes ocupados"
)
ibge_2022_renda <- dd |>
  here::here("Agregados_por_setores_renda_responsavel_BR.xlsx") |>
  readxl::read_excel(col_types = c("text", rep("numeric", 6))) |>
  dplyr::transmute(
    id_setor_censitario = CD_SETOR,
    id_uf = as.numeric(stringr::str_sub(id_setor_censitario, 1, 2)),
    renda_media = V06004
  ) |>
  dplyr::left_join(ufs, dplyr::join_by(id_uf)) |>
  dplyr::relocate(sigla_uf, .after = id_setor_censitario) |>
  dplyr::select(-id_uf)
readr::write_csv(ibge_2022_renda, here::here(dd, "ibge_2022_renda.csv"))
