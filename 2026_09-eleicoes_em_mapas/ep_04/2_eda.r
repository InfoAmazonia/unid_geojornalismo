dd <- here::here("ep_04/dados-tidy")

emendas <- readr::read_csv(here::here(dd, "emendas_doc_filtradas.csv"))
emendas <- emendas |>
  dplyr::mutate(
    valor_empenhado = readr::parse_number(
      valor_empenhado, locale = readr::locale(decimal_mark = ",")
    ),
    uf_favorecido = dplyr::case_when(
      uf_favorecido == "-1" | is.na(uf_favorecido) ~ uf_de_aplicacao_do_recurso,
      is.na(uf_favorecido) ~ "Sem informação",
      TRUE ~ uf_favorecido
    ),
    uf_amazonia = dplyr::case_when(
      uf_favorecido %in% c("AC", "AM", "AP", "PA", "RO", "RR", "TO", "MT", "MA") ~ "Sim",
      uf_favorecido == "Sem informação" ~ "Sem informação",
      TRUE ~ "Não"
    )
  )

readr::write_csv(emendas, here::here(dd, "emendas_tidy.csv"))

# por autor
emendas |>
  dplyr::summarise(
    total_empenhado = sum(valor_empenhado, na.rm = TRUE),
    emendas = dplyr::n_distinct(codigo_da_emenda),
    .by = nome_do_autor_da_emenda
  ) |>
  dplyr::arrange(dplyr::desc(total_empenhado))

# fora / dentro amazonia
emendas |>
  dplyr::summarise(
    total_empenhado = sum(valor_empenhado),
    emendas = dplyr::n_distinct(codigo_da_emenda),
    .by = uf_amazonia
  ) |>
  dplyr::arrange(dplyr::desc(total_empenhado))


## Lista ações
inesc <- c(
  "20G4", "20VA", "20VY", "216W", "21E4",
  "00UD", "20VP", "21F1", "21F2",
  "10TT", "20WH", "21A9", "6925",
  "21HR",
  "000K", "00T5", "00TK", "20GB", "21HC", "21HP", "21HQ", "22BO", "8172", "8348", "8865",
  "00LX", "00T6", "00T7", "00T8", "00T9", "00TA", "00TB", "00TG", "00UN", "00VA", "00WP", "11AA", "12EP", "14VI", "152D", "15DX", "15XT", "15XV", "15XW", "15ZK", "162K", "166K", "169E", "1N64", "20N4", "20VR", "20VS", "20WI", "214T", "219H", "21DD", "21DE", "21DF", "21DG", "21HX", "2378", "4926", "5308", "5900", "7G88", "7M12", "7X91", "7XZ4",
  "00UO", "219R", "4909",
  "00SY", "8872", "8874",
  "00SW", "00T2", "00TH", "00TJ", "00VC", "00VD",
  "168N", "20W2", "20WM", "20WN", "214M", "214N", "214O", "214P", "218R", "219K", "21F3", "21F4", "21F5", "21HJ", "21HK", "21I5", "2E87"
)


emendas |>
  dplyr::distinct(codigo_acao, acao) |>
  dplyr::mutate(inesc = codigo_acao %in% inesc) |>
  dplyr::filter(!inesc) |>
  dplyr::mutate(label = paste0("- ", codigo_acao, " — ", acao)) |>
  dplyr::pull(label) |>
  clipr::write_clip()
