dd <- here::here("2026_09-el_nino/dados/raw")
tidy <- here::here("2026_09-el_nino/dados/tidy")

# Georreferenciamento --------------------------------------------------------
# Mapas do INMET em lat/lon (plate carrée): as linhas de grade estão a cada
# 210 px = 10°, ou seja, 21 px/grau. Linhas detectadas nos pixels:
#   colunas 106, 316, 526, 736 -> 70W, 60W, 50W, 40W
#   linhas  211, 421, 631, 841 -> 0, 10S, 20S, 30S
px_grau <- 21
col_ref <- 106; lon_ref <- -70
lin_ref <- 211; lat_ref <- 0

# Legendas: cores (RGB) e limites das classes, iguais em todos os meses
# Precipitação acumulada (mm): 16 classes
legenda_precip <- tibble::tribble(
  ~r,  ~g,  ~b,
  230,  63,  26,
  255, 102,   0,
  255, 153,   0,
  255, 204,   0,
  245, 226,  17,
  255, 255,   0,
  255, 255, 153,
  182, 251, 171,
  120, 245, 115,
   53, 210,  59,
   17, 160,  16,
  233, 254, 255,
  195, 224, 254,
  141, 162, 255,
   78,  91, 255,
    4,   0, 249
)
quebras_precip <- c(0, 10, 20, 30, 40, 60, 80, 100, 140, 180, 220, 260, 300,
                    340, 380, 420, Inf)

# Temperatura média compensada (°C): 12 classes de 2 °C
legenda_temp <- tibble::tribble(
  ~r,  ~g,  ~b,
    0,   1, 205,
   62, 150, 247,
  121, 186, 252,
  153, 211, 251,
  180, 240, 250,
  225, 255, 253,
  255, 247, 188,
  254, 238, 125,
  255, 204,  89,
  254, 167,  52,
  254,  93,   2,
  236,  24,   0
)
quebras_temp <- c(seq(8, 30, by = 2), Inf)

georref_inmet <- function(arq, legenda, quebras, nome, tol = 10) {
  img <- png::readPNG(arq) * 255
  nr <- dim(img)[1]; nc <- dim(img)[2]

  # Extent pelas bordas dos pixels (centro do pixel de referência = coordenada)
  xmin <- lon_ref - (col_ref - 0.5) / px_grau
  ymax <- lat_ref + (lin_ref - 0.5) / px_grau
  ext <- terra::ext(xmin, xmin + nc / px_grau, ymax - nr / px_grau, ymax)

  # Classifica cada pixel pela cor mais próxima da legenda
  rgb <- cbind(as.vector(t(img[, , 1])), as.vector(t(img[, , 2])),
               as.vector(t(img[, , 3])))
  pal <- as.matrix(legenda[, c("r", "g", "b")])
  dist <- sapply(seq_len(nrow(pal)), function(i) {
    sqrt(colSums((t(rgb) - pal[i, ])^2))
  })
  classe <- max.col(-dist, ties.method = "first")
  classe[apply(dist, 1, min) > tol] <- NA # fundo, fronteiras, grade, texto

  r <- terra::rast(nrows = nr, ncols = nc, extent = ext, crs = "EPSG:4326")
  terra::values(r) <- classe

  # Remove linhas de grade pontilhadas (a cada 10°, ±1 px)
  cols_grade <- outer(seq(col_ref %% 210, nc, by = 210), -1:1, "+")
  lins_grade <- outer(seq(lin_ref %% 210, nr, by = 210), -1:1, "+")
  r[, cols_grade[cols_grade >= 1 & cols_grade <= nc]] <- NA
  r[lins_grade[lins_grade >= 1 & lins_grade <= nr], ] <- NA

  # Recorta área do mapa (sem título/legenda) e preenche fronteiras/grade
  r <- terra::crop(r, terra::ext(-75, -33, -35, 6))
  r <- terra::focal(r, w = 5, fun = "modal", na.policy = "only", na.rm = TRUE)
  brasil <- geobr::read_country(year = 2020, showProgress = FALSE)
  r <- terra::mask(r, terra::vect(sf::st_transform(brasil, 4326)))

  min <- utils::head(quebras, -1)
  max <- quebras[-1]
  categorias <- data.frame(
    id = seq_along(min),
    classe = ifelse(is.infinite(max), paste0(">", min), paste0(min, "-", max))
  )
  names(categorias)[2] <- nome
  levels(r) <- categorias
  r
}

variaveis <- list(
  precip = list(legenda = legenda_precip, quebras = quebras_precip),
  temp = list(legenda = legenda_temp, quebras = quebras_temp)
)

purrr::iwalk(variaveis, function(v, nome) {
  dir_png <- here::here(dd, paste0("normais_inmet_", nome))
  dir_tif <- here::here(tidy, paste0("normais_inmet_", nome))
  if (!dir.exists(dir_tif)) dir.create(dir_tif, recursive = TRUE)
  arqs <- list.files(dir_png, pattern = "_[0-9]{2}\\.png$", full.names = TRUE)
  purrr::walk(arqs, function(arq) {
    r <- georref_inmet(arq, v$legenda, v$quebras, nome)
    saida <- file.path(dir_tif, sub("\\.png$", ".tif", basename(arq)))
    terra::writeRaster(r, saida, overwrite = TRUE, datatype = "INT1U")
  })
})
