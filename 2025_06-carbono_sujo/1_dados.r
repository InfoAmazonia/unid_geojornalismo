# library(sf)
raw <- here::here("2025_06-carbono_sujo/data-raw")
tidy <- here::here("2025_06-carbono_sujo/data-tidy")
fs::dir_create(c(raw, tidy))

# Download dos processos minerários ativos | pasta data-raw/anm_ativos
# https://dados.gov.br/dados/conjuntos-dados/sistema-de-informacoes-geograficas-da-mineracao-sigmine
# "https://app.anm.gov.br/dadosabertos/SIGMINE/PROCESSOS_MINERARIOS/BRASIL.zip"

anm <- here::here(raw, "anm_ativos") |>
  sf::read_sf() |>
  sf::st_zm() |>
  dplyr::rename(anm_nome = NOME)

am <- geobr::read_amazon()

fs::dir_info("202506_creditos_carbono/data-raw/prodes") |>
  dplyr::arrange(dplyr::desc(size))
  head(2) |>
  dplyr::pull(path) |>
  clipr::write_clip()

sf::sf_use_s2(FALSE)
anm <- anm |>
  sf::st_crop(am, mask = TRUE)
sf::write_sf(anm, here::here(tidy, "mineracao.geojson"))

crs <- sf::st_crs(anm)
ids_projetos <- here::here(raw, "projetos_shp") |>
  fs::dir_ls() |>
  basename() |>
  stringr::str_extract(".+(?=\\.)") |>
  unique()

shp_projetos <- ids_projetos |>
  purrr::map({
    \(x) {
      here::here(raw, "projetos_shp", x) |>
        paste0(".shp") |>
        sf::read_sf() |>
        dplyr::mutate(id_projeto = readr::parse_number(x)) |>
        dplyr::select(id_projeto)
    }
  }) |>
  purrr::map(\(x) sf::st_transform(x, crs)) |>
  purrr::map(\(x) sf::st_zm(x)) |>
  purrr::list_rbind() |>
  sf::st_as_sf()

shp_projetos |>
  ggplot2::ggplot() +
  ggplot2::geom_sf()


area_projetos <- shp_projetos |>
  sf::st_make_valid() |>
  dplyr::group_by(id_projeto) |>
  dplyr::summarise() |>
  dplyr::mutate(
    area_projeto_hectares = units::set_units(sf::st_area(geometry), ha)
  ) |>
  tibble::as_tibble() |>
  dplyr::select(-geometry)
readr::write_rds(area_projetos, here::here(tidy, "area_projetos.rds"))

intersec <- shp_projetos |>
  sf::st_make_valid() |>
  sf::st_intersection(anm)



tbl_fase <- dados |>
  dplyr::group_by(FASE) |>
  dplyr::summarise(mineracao = dplyr::n_distinct(PROCESSO)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    area = as.numeric(units::set_units(sf::st_area(geometry), ha))
  ) |>
  tibble::as_tibble() |>
  dplyr::select(-geometry) |>
  dplyr::arrange(dplyr::desc(area))
readr::write_rds(tbl_fase, here::here(tidy, "tbl_fase.rds"))


# Dados auxiliares ----

# BD
bd_projetos <- readr::read_csv(here::here(raw, "projetos_dados/bd_carbonobr_projetos.csv"), skip = 1)
dados <- intersec |>
  dplyr::left_join(bd_projetos, dplyr::join_by(id_projeto))
dados |>
  ggplot2::ggplot() +
  ggplot2::geom_sf()

shp_projetos <- shp_projetos |>
  dplyr::left_join(bd_projetos, dplyr::join_by(id_projeto))

shp_projetos |>
  dplyr::glimpse()

# sf::write_sf(intersec, here::here(tidy, "intersec.geojson"))
# sf::write_sf(intersec, here::here(tidy, "intersec.shp"))

# fs::dir_info(here::here(tidy, "intersec"))
# fs::file_info(here::here(tidy, "dados.geojson"))
# fs::file_info(here::here(tidy, "intersec.geojson"))

# Degradação

deg2024 <- sf::read_sf("202502_degradacao/data-raw/degradacao2024")
deg2024 <- sf::st_transform(deg2024, sf::st_crs(dados))

dados_degradacao <- shp_projetos |>
  dplyr::filter(id_projeto %in% unique(dados$id_projeto)) |>
  sf::st_make_valid() |>
  sf::st_intersection(deg2024) |>
  dplyr::left_join(bd_projetos, dplyr::join_by(id_projeto))

dados_deg <- dados_degradacao |>
  dplyr::group_by(
    id_projeto, nome_projeto, empresa_certificadora, Class_Name,
    Mes, Ano, Mes_Ano
  ) |>
  dplyr::summarise() |>
  dplyr::ungroup()

readr::write_rds(dados_deg, here::here(tidy, "dados_degradacao.rds"))


tbl_projetos <- dados |>
  dplyr::select(-area_projeto_hectares) |>
  dplyr::left_join(area_projetos, dplyr::join_by(id_projeto)) |>
  dplyr::group_by(id_projeto, nome_projeto, uf, area_projeto_hectares) |>
  dplyr::summarise() |>
  dplyr::mutate(
    area = sf::st_area(geometry),
    area = units::set_units(area, ha)
  ) |>
  tibble::as_tibble() |>
  dplyr::select(-geometry) |>
  dplyr::mutate(pct_area = as.numeric(area / area_projeto_hectares))
readr::write_rds(tbl_projetos, here::here(tidy, "tbl_projetos.rds"))

# Prodes

dados <- readr::read_rds(here::here(tidy, "dados.rds"))
prodes <- here::here(raw, "prodes") |>
  sf::read_sf() |>
  dplyr::select(state, main_class:sub_class, year, area_km)

ids_intersec <- intersec$id_projeto |>
  unique()

projetos_comercializados <- dados |>
  dplyr::as_tibble() |>
  dplyr::filter(!is.na(vcu_serialNumbers)) |>
  dplyr::pull(id_projeto) |>
  unique()

shp_projetos_comercializados <- shp_projetos |>
  dplyr::filter(id_projeto %in% creditos)

shp_projetos |>
  dplyr::mutate(
    interseccao_mineracao = id_prjt %in% ids_intersec,
    comercializou = id_prjt %in% creditos
  ) |>
  dplyr::as_tibble() |>
  dplyr::count(comrclz)

  sf::write_sf(here::here(tidy, "shp_projetos.shp"))

shp_projetos |>
  dplyr::as_tibble() |>
  dplyr::count(comrclz)

shp_projetos <- sf::read_sf(here::here(tidy, "shp_projetos"))
intersec <- sf::read_sf(here::here(tidy, "intersec"))


sf::sf_use_s2(FALSE)
prodes_projetos <- prodes |>
  dplyr::filter(state != "AC") |>
  sf::st_make_valid() |>
  sf::st_intersection(shp_projetos_comercializados)

prodes |>
  dplyr::mutate(valid = sf::st_is_valid(geometry)) |>
  dplyr::filter(!valid)


geobr::read_state() |>
  ggplot2::ggplot() +
  ggplot2::geom_sf() +
  ggplot2::geom_sf(data = shp_projetos_comercializados)

