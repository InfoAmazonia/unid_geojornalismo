dd <- here::here("2026_09-eleicoes_em_mapas/ep_05/dados")
tidy <- here::here("2026_09-eleicoes_em_mapas/ep_05/dados-tidy")

if (!dir.exists(tidy)) {
  dir.create(tidy)
}

# Despesas - Siconfi - BD ------------------------------------------------------
# SELECT * FROM `basedosdados.br_me_siconfi.uf_despesas_funcao`
# where ano >= 2023 and
# sigla_uf in ("AC", "AM", "AP", "MA", "MT", "PA", "RO", "RR", "TO")

siconfi <- readr::read_csv(here::here(
  dd, "job_EC-XBrUxSk0mqOoPvwwuXlH3HApO.csv"
))

siconfi |>
  dplyr::filter(estagio == "Despesas Empenhadas") |>
  dplyr::count(conta_bd, sort = TRUE) |>
  dplyr::pull(conta_bd) |>
  clipr::write_clip()

mapa_clima <- tibble::tribble(
  ~id_conta_bd, ~conta,                                  ~nivel, ~categoria,
  "3.18.122",   "FU18 - Administração Geral",            1,      "Ambiental",
  "3.18.999",   "FU18 - Demais Subfunções",              1,      "Ambiental",
  "3.18.541",   "Preservação e Conservação Ambiental",   1,      "Ambiental",
  "3.18.542",   "Controle Ambiental",                    1,      "Ambiental",
  "3.18.543",   "Recuperação de Áreas Degradadas",       1,      "Ambiental",
  "3.18.544",   "Recursos Hídricos",                     1,      "Ambiental",
  "3.18.545",   "Meteorologia",                          1,      "Ambiental",
  "3.06.182",   "Defesa Civil",                          2,      "Adaptação",
  "3.17.511",   "Saneamento Básico Rural",               2,      "Adaptação",
  "3.17.512",   "Saneamento Básico Urbano",              2,      "Adaptação",
  "3.04.127",   "Ordenamento Territorial",               2,      "Adaptação",
  "3.25.751",   "Conservação de Energia",                2,      "Energia",
  "3.21.631",   "Reforma Agrária",                       2,      "Uso da terra",
  "3.20.606",   "Extensão Rural",                        3,      "Agropecuária",
  "3.15.452",   "Serviços Urbanos",                      3,      "Urbano",
  "3.14.423",   "Assistência aos Povos Indígenas",       3,      "Outros"
)

cod_nivel <- c(
  `541` = 1, `542` = 1, `543` = 1, `544` = 1, `545` = 1,
  `182` = 2, `511` = 2, `512` = 2, `127` = 2, `751` = 2, `631` = 2,
  `606` = 3, `452` = 3, `423` = 3
)

siconfi <- siconfi |>
  dplyr::filter(estagio == "Despesas Empenhadas") |>
  dplyr::mutate(
    funcao = stringr::str_split_i(id_conta_bd, "\\.", 2),
    subfuncao = stringr::str_split_i(id_conta_bd, "\\.", 3),
    nivel = dplyr::if_else(funcao == "18", 1, cod_nivel[subfuncao]),
    clima = !is.na(nivel)
  ) |>
  dplyr::rename(uf = sigla_uf) |>
  dplyr::select(-c(estagio, portaria, estagio_bd))

readr::write_csv(siconfi, here::here(tidy, "bd_siconfi.csv"))

depara_codigos <- siconfi |>
  dplyr::distinct(conta, id_conta_bd, funcao, subfuncao) |>
  dplyr::mutate(conta = dplyr::case_when(
    conta == "Administração Geral" ~ paste0("FU", funcao, " - Administração Geral"),
    conta == "Demais Subfunções" ~ paste0("FU", funcao, " - Demais Subfunções"),
    TRUE ~ conta
  ))

finbra <- readr::read_csv2(
  here::here(dd, "finbraRREO.csv"),
  locale = readr::locale(encoding = "latin1"), skip = 6
) |>
  janitor::clean_names() |>
  dplyr::filter(
    coluna == "DESPESAS EMPENHADAS ATÉ O BIMESTRE (b)",
    uf %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO", "MT", "MA")
  ) |>
  dplyr::select(uf, conta, valor) |>
  dplyr::left_join(depara_codigos, by = "conta") |>
  dplyr::mutate(
    ano = 2026,
    clima = id_conta_bd %in% mapa_clima$id_conta_bd,
    conta = stringr::str_remove(conta, "FU18 - ")
  )

gastos <- siconfi |>
  dplyr::bind_rows(finbra)

excluir <- c(
  "DESPESAS (EXCETO INTRA-ORÇAMENTÁRIAS) (I)",
  "DESPESAS (INTRA-ORÇAMENTÁRIAS) (II)",
  "TOTAL (III) = (I + II)",
  "Transferências",
  "Despesas Intraorçamentárias"
)

gastos <- gastos |>
  dplyr::filter(!conta %in% excluir) |>
  dplyr::mutate(clima = dplyr::coalesce(clima, FALSE))


readr::write_csv(gastos, here::here(tidy, "gastos.csv"))


# Candidatos -------------------------------------------------------------------

cand <- dd |>
  here::here("candidatos") |>
  fs::dir_ls(regexp = "consulta_cand") |>
  purrr::map_dfr(readr::read_csv2, locale = readr::locale(encoding = "latin1"))

reeleicao <- c(
  "MAILZA ASSIS CAMELI",  # Acre
  "ROBERTO MAIA CIDADE FILHO",  # Amazonas
  "HANA GHASSAN TUMA",  # Pará
  "CLÉCIO LUÍS VILHENA VIEIRA",  # Amapá
  "OTAVIANO OLAVO PIVETTA"  # Mato Grosso
)
cand <- cand |>
  dplyr::filter(DS_CARGO == "GOVERNADOR") |>
  dplyr::select(
    SG_UF, NM_CANDIDATO, NM_URNA_CANDIDATO, SG_PARTIDO, NR_CPF_CANDIDATO,
    NR_CANDIDATO, SQ_CANDIDATO
  ) |>
  dplyr::mutate(reeleicao = NM_CANDIDATO %in% reeleicao)
readr::write_csv(cand, here::here(tidy, "candidatos.csv"))
