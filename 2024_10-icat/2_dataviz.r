# Bases para visualização

data_tidy <- here::here("202410_icat/data-tidy")
dif_prefeitos_vereadores <- readr::read_csv(here::here(data_tidy, "dif_prefeitos_camara.csv"))
amazonia_legal <- "202408_seca/data-raw/amazonialegal.xlsx" |>
  readxl::read_excel() |>
  janitor::clean_names() |>
  dplyr::mutate(cd_mun = as.numeric(cd_mun))

geo_dif <- geobr::read_municipality() |>
  dplyr::semi_join(amazonia_legal, dplyr::join_by(code_muni == cd_mun)) |>
  dplyr::left_join(
    dif_prefeitos_vereadores, dplyr::join_by(code_muni == cd_municipio_ibge)
  )
geo_dif |>
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = dist)) +
  ggplot2::scale_fill_gradient2(
    low = "red",
    high = "blue",
    mid = "white",
    na.value = "grey50"
  )

# Prefeitos bons e ótimos
geo_dif |>
  dplyr::mutate(dist = ifelse(icat_prefeito > 60, dist, NA)) |>
  dplyr::mutate(dist = -dist) |>
  # sf::st_write(here::here(data_tidy, "prefs_bons.geojson"))

  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = dist))
  ggplot2::scale_fill_gradient2(
    low = "red",
    high = "blue",
    mid = "white",
    na.value = "grey70"
  )


# Prefeitos péssimos, ruins, regular
ruins <- geo_dif |>
  dplyr::mutate(dist = ifelse(icat_prefeito <= 60, dist, NA)) |>
  dplyr::mutate(
    dist_abs = abs(dist),
    cat_prefeito = pts_corte(icat_prefeito),
    sintonia = dplyr::case_when(
      cat_prefeito == "1. péssimo" & cat_medio_camara == "1. péssimo" ~ "1. Ambos péssimos",
      cat_prefeito == "1. péssimo" & cat_medio_camara == "2. ruim" ~ "2. Péssimo e ruim",
      cat_prefeito == "2. ruim" & cat_medio_camara == "1. péssimo" ~ "2. Péssimo e ruim",
      cat_prefeito == "2. ruim" & cat_medio_camara == "2. ruim" ~ "3. Ambos ruins",
      cat_prefeito == "2. ruim" & cat_medio_camara == "3. regular" ~ "4. Ruim e regular",
      cat_prefeito == "3. regular" & cat_medio_camara == "2. ruim" ~ "4. Ruim e regular",
      cat_prefeito == "3. regular" & cat_medio_camara == "1. péssimo" ~ "3. Pésismo e regular",
      cat_prefeito == "1. péssimo" & cat_medio_camara == "3. regular" ~ "3. Pésismo e regular",
      cat_prefeito == "3. regular" & cat_medio_camara == "3. regular" ~ "5. Ambos regulares"
    ),
    sintonia_num = as.integer(stringr::str_extract(sintonia, "[0-9]"))
  )
sf::st_write(ruins, here::here(data_tidy, "prefs_ruins.geojson"))
ruins |>
  tibble::as_tibble() |>
  dplyr::select(-geom) |>
  readr::write_csv(here::here(data_tidy, "prefs_ruins.csv"))

pal <- leaflet::colorBin("Reds", reverse = TRUE, domain = ruins$sintonia_num)
labels <- glue::glue(
  "<strong>{ruins$name_muni} ({ruins$sg_uf})</strong><br/>",
  "ICAt prefeito: {ruins$cat_prefeito}<br/>",
  "ICAt vereadores: {ruins$cat_medio_camara}<br/>",
  "{ruins$sintonia}<br/>",
  "Diferença: {round(ruins$dist)}<br/>"
) |> lapply(htmltools::HTML)

leaflet::leaflet() |>
  leaflet::addProviderTiles(leaflet::providers$CartoDB) |>
  leaflet::addPolygons(
    data = ruins,
    weight = 1,
    color = "white",
    fillColor = ~pal(sintonia_num), fillOpacity = .8,
    label = labels
  ) |>
  htmlwidgets::saveWidget(here::here(data_tidy, "mapa.html"), selfcontained = TRUE)

  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(fill = dist)) +
  ggplot2::scale_fill_gradient2(
    low = "red",
    high = "blue",
    mid = "white",
    na.value = "grey70"
  )

fs::dir_info(here::here(data_tidy))
