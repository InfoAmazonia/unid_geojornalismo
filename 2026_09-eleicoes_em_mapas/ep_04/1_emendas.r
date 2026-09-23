# https://portaldatransparencia.gov.br/download-de-dados/emendas-parlamentares-documentos/

dd <- here::here("ep_04/dados")
tidy <- here::here("ep_04/dados-tidy")

docs <- here::here(dd) |>
  fs::dir_ls(regexp = "EmendasParlamentares") |>
  purrr::map_dfr(
    readr::read_csv2, col_types = "cccccccccccccc",
    locale = readr::locale(encoding = "ISO-8859-1")
  ) |>
  janitor::clean_names()

emendas_individuais <- docs |>
  dplyr::filter(stringr::str_detect(tipo_de_emenda, "Individual"))

# https://inesc.org.br/wp-content/uploads/2025/12/nt-financiamento_climatico_e_emendas_parlamentares.pdf

# 1158 — Enfrentamento da Emergência Climática
# 20UF, 20VY, 21E4, 20G4
# não encontrados: 216W, 20VA

# 1189 — Bioeconomia para um Novo Ciclo de Prosperidade
# 00UD, 21F2
# não encontrados: 20VP, 21F1

# 1190 — Qualidade Ambiental nas Cidades e no Campo
# 21A9
# não encontrados: 6925, 10TT, 20WH,

# 2317 — Desenvolvimento Regional e Ordenamento Territorial
# não encontrados: 21HR

# 2318 — Gestão de Riscos e de Desastres
# 8865, 22BO, 00T5, 21HC, 00TK, 8172, 8348
# não encontrados: 000K, 20GB, 21HP, 21HQ
# adicionais: 14RL

# 2321 — Recursos Hídricos: Água em Quantidade e Qualidade para Sempre
# 00LX, 00T6, 00T7, 00T8, 00T9, 00TA, 00TB, 00TG, 00UN, 00VA, 00WP, 11AA, 12EP, 14VI, 152D, 15DX, 15XT, 15XV, 15XW, 15ZK, 162K, 166K, 169E, 1N64, 20N4, 20VR, 20VS, 20WI, 214T, 219H, 21DD, 21DE, 21DF, 21DG, 21HX, 2378, 4926, 5308, 5900, 7G88, 7M12, 7X91, 7XZ4
# adicionais: 7XK6, 10SC, 8948, 10SG

# 2322 — Saneamento Básico
# 00UO, 219R
# adicionais: 21CA, 00TN, 7Y06

# 5113 — Educação Superior: Qualidade, Democracia, Equidade e Sustentabilidade
# 4909
# adicionais: 218R

# 5601 — Cidades Melhores
# 00SY, 8874
# não encontrado: 8872

# 5602 — Periferia Viva
# 00SW, 00TJ, 00VD
# não encontrado: 00T2, 00TH, 00VC

# 6114 — Proteção e Recuperação da Biodiversidade e Combate ao Desmatamento e aos Incêndios
# 20W2, 20WM, 214M, 214N, 214O, 214P, 218R, 219K, 21F3,  2E87
# 168N, 20WN, 21F4, 21F5, 21HJ, 21HK, 21I5

acoes <- c(
  "20UF", "20VY", "21E4", "20G4", "216W", "20VA", "00UD", "21F2", "20VP",
  "21F1", "21A9", "6925", "10TT", "20WH", "21HR", "8865", "22BO", "00T5",
  "21HC", "00TK", "8172", "8348", "000K", "20GB", "21HP", "21HQ", "14RL",
  "00LX", "00T6", "00T7", "00T8", "00T9", "00TA", "00TB", "00TG", "00UN",
  "00VA", "00WP", "11AA", "12EP", "14VI", "152D", "15DX", "15XT", "15XV",
  "15XW", "15ZK", "162K", "166K", "169E", "1N64", "20N4", "20VR", "20VS",
  "20WI", "214T", "219H", "21DD", "21DE", "21DF", "21DG", "21HX", "2378",
  "4926", "5308", "5900", "7G88", "7M12", "7X91", "7XZ4", "7XK6", "10SC",
  "8948", "10SG", "00UO", "219R", "21CA", "00TN", "7Y06", "4909", "218R",
  "00SY", "8874", "8872", "00SW", "00TJ", "00VD", "00T2", "00TH", "00VC",
  "20W2", "20WM", "214M", "214N", "214O", "214P", "218R", "219K", "21F3",
  "2E87", "168N", "20WN", "21F4", "21F5", "21HJ", "21HK", "21I5"
)

emendas_individuais <- emendas_individuais |>
  dplyr::filter(codigo_acao %in% acoes)

readr::write_csv(
  emendas_individuais, here::here(tidy, "emendas_doc_filtradas.csv")
)
