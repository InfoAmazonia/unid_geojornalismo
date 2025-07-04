# Download dos dados de créditos comercializados

library(sf)

raw <- here::here("2025_06-carbono_sujo/data-raw")
tidy <- here::here("2025_06-carbono_sujo/data-tidy")

# vcus <- here::here(raw, "vcus.csv") |>
#   readr::read_csv() |>
#   janitor::clean_names() |>
#   dplyr::rename_all(\(x) paste0("vcu_", x))

dados <- readr::read_rds(here::here(tidy, "dados.rds"))

get_vcus <- function(id, path = here::here(tidy, "vcus")) {
  Sys.sleep(1)
  url <- paste0(
    "https://registry.verra.org/uiapi/asset/asset/search?maxResults=2000&$",
    "count=true&$skip=0"
  )
  payload <- list(
    "program" = "VCS",
    "exactResourceIdentifier" = id,
    "issuanceTypeCodes" = list("ISSUE")
  )
  json_payload <- jsonlite::toJSON(payload, auto_unbox = TRUE)
  headers <- c(
    "Accept" = "application/json",
    "Content-Type" = "application/json",
    "Accept-Encoding" = "gzip, deflate, br, zstd",
    "Accept-Language" = "pt-BR,pt;q=0.9,en-US;q=0.8,en;q=0.7",
    "Connection" = "keep-alive",
    "Cookie" = "ASPSESSIONIDSEQSRCDB=JPIPCOGDHMMBJJNBMFDHILKF; ASPSESSIONIDSERSSCCB=MJCELJGDDHEGENHIABNMFCCN",
    "DNT" = "1",
    "Host" = "registry.verra.org",
    "Origin" = "https://registry.verra.org",
    "Referer" = paste0("https://registry.verra.org/app/search/VCS?programType=ISSUANCE&exactResId=", id),
    "User-Agent" = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/136.0.0.0 Safari/537.36",
    "sec-ch-ua" = '"Not.A/Brand";v="99", "Chromium";v="136"',
    "sec-ch-ua-mobile" = "?0",
    "sec-ch-ua-platform" = "macOS"
  )
  req <- httr::POST(
    url,
    body = json_payload,
    httr::add_headers(.headers = headers)
  )
  vcus <- httr::content(req, as = "parsed") |>
    purrr::pluck("value") |>
    purrr::map(tibble::enframe) |>
    purrr::map(tidyr::pivot_wider) |>
    purrr::map(tidyr::unnest) |>
    purrr::list_rbind() |>
    dplyr::mutate(id_projeto = id)
  filename <- paste0("vcus_", id, ".csv")
  if (nrow(vcus) > 0) {
    readr::write_csv(vcus, here::here(path, filename))
  }
  vcus
}

purrr::walk(
  unique(dados$id_projeto),
  get_vcus
)

vcus <- tidy |>
  here::here("vcus") |>
  fs::dir_ls() |>
  purrr::map(readr::read_csv) |>
  purrr::map(\(x) dplyr::mutate(x, id_projeto = as.numeric(id_projeto))) |>
  purrr::list_rbind() |>
  dplyr::rename_all(\(x) paste0("vcu_", x))

readr::write_rds(vcus, here::here(tidy, "vcus.rds"))

dados <- dados |>
  dplyr::left_join(vcus, dplyr::join_by(id_projeto == vcu_id_projeto))

readr::write_rds(dados, here::here(tidy, "dados.rds"))


# Projetos com VCU

dados |>
  tibble::as_tibble() |>
  dplyr::filter(!is.na(vcu_serialNumbers)) |>
  dplyr::distinct(id_projeto)

# Projetos (total, sem intersecção) com VCUs

ids_projetos <- here::here(raw, "projetos_shp") |>
  fs::dir_ls() |>
  basename() |>
  stringr::str_extract(".+(?=\\.)") |>
  unique() |>
  stringr::str_extract("[0-9]+")

ids_projetos_sem_anm <- setdiff(ids_projetos, unique(dados$id_projeto))

purrr::walk(
  ids_projetos_sem_anm[-1],
  get_vcus,
  path = here::here(tidy, "vcus_sem")
)

csvs_ok <- here::here(tidy, "vcus_sem") |>
  fs::dir_ls() |>
  stringr::str_extract("[0-9]+(?=\\.csv)")

ids_projetos_sem_anm |>
  setdiff(csvs_ok) |>
  purrr::walk(
    get_vcus,
    path = here::here(tidy, "vcus_sem")
  )

creditos_sem_anm <- here::here(tidy, "vcus_sem") |>
  fs::dir_ls() |>
  purrr::map(readr::read_csv) |>
  purrr::list_rbind()

creditos_sem_anm <- creditos_sem_anm |>
  dplyr::left_join(bd_projetos, dplyr::join_by(id_projeto))

readr::write_rds(creditos_sem_anm, here::here(tidy, "creditos_sem_anm.rds"))
