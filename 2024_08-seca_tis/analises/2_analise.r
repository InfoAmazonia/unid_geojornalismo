# Dados wide
tis_seca <- readr::read_rds("202408_secatis/data-tidy/tis_seca.rds")
tis_geo_seca <- readr::read_rds("202408_secatis/data-tidy/tis_geo_seca.rds")
al <- readxl::read_excel("202408_seca/data-raw/amazonialegal.xlsx") |>
  dplyr::pull(SIGLA_UF) |>
  unique()
# TIs que passaram de Normal para Extrema
tis_seca |>
  dplyr::mutate(dif = as.numeric(seca_jul_24) - as.numeric(seca_jul_23)) |>
  dplyr::filter(dif > 3)

# Tis com algum grau de seca, 2023 v. 2024, por UF
total_tis <- "202408_secatis/data-raw/tis_shp" |>
  sf::read_sf(options = "ENCODING=WINDOWS-1252") |>
  tidyr::separate_longer_delim(uf_sigla, ",") |>
  dplyr::filter(uf_sigla %in% al) |>
  dplyr::group_by(uf_sigla) |>
  dplyr::summarise(total = dplyr::n_distinct(terrai_cod))

tis_seca |>
  tidyr::separate_longer_delim(uf, ",") |>
  tidyr::pivot_longer(c(seca_jul_23, seca_jul_24)) |>
  dplyr::mutate(seca = ifelse(value == "Normal", FALSE, TRUE)) |>
  dplyr::filter(!is.na(seca)) |>
  dplyr::group_by(name, uf) |>
  dplyr::summarise(tis_seca = sum(seca), .groups = "drop") |>
  tidyr::pivot_wider(names_from = name, values_from = tis_seca)



# Mapa
labels_petr <- glue::glue(
  "<strong>{dados_filtrados()$arayara$Nome}</strong><br>",
  "Cia: {dados_filtrados()$arayara$Operador}<br>",
  "Fase: {dados_filtrados()$arayara$Fase}"
) |>
  lapply(htmltools::HTML)
pal <- leaflet::colorFactor(
  palette = "Dark2",
  domain = dados_filtrados()$arayara$Fase
)
labels_anps <- glue::glue(
  "<strong>{dados_filtrados()$anps$nombre}</strong><br>",
  "Categoria: {dados_filtrados()$anps$categoria}<br>"
) |>
  lapply(htmltools::HTML)

tis_geo_seca |>
  leaflet::leaflet() |>
  leaflet::addTiles() |>
  leaflet::addPolygons()
