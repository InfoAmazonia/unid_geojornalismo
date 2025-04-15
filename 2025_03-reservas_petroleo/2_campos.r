data_raw <- here::here("202411_petroleo/data-raw")
data_tidy <- here::here("202411_petroleo/data-tidy")

campos <- sf::read_sf(here::here(data_raw, "campos"), options = "ENCODING=WINDOWS-1252")
campos <- campos |>
  dplyr::mutate(ETAPA = stringr::str_squish(ETAPA))

pal <- leaflet::colorFactor("Dark2", domain = campos$ETAPA)
labels <- glue::glue(
  "<strong>Campo/bacia:</strong> {campos$NOM_CAMPO} ({campos$NOM_BACIA})<br/>",
  "<strong>Operador:</strong> {campos$OPERADOR_C}<br/>",
  "<strong>Data início:</strong> {campos$DAT_INICIO}<br/>",
  "<strong>Data descoberta:</strong> {campos$DAT_DESCOB}<br/>",
  "<strong>ETAPA:</strong> {campos$ETAPA}<br/>"
) |> lapply(htmltools::HTML)

leaflet::leaflet() |>
  leaflet::addProviderTiles(leaflet::providers$CartoDB) |>
  leaflet::addPolygons(
    data = campos,
    weight = 1,
    color = "white",
    fillColor = ~pal(ETAPA), fillOpacity = .8,
    label = labels
  )
