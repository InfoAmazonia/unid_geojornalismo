# Dados em geojson

## Arquivos principais

- desmatamento_perfil.geojson
- cnfp_buffer.geojson

Colunas:

- classe_tidy: unidade de conservação (UC), TI, FPND (não destinadas), Assentamento, Uso militar
- area_desmatada_ha: Área desmatada em hectares
- area_ha: Área em hectares

## Arquivos 0_*

Arquivos recortados com o buffer de 50 km da BR-319.

- 0_muni_buffer.geojson (polígonos com nome e código do município, nome e código do estado, recortados no buffer)
- 0_sicar_geojson (união dos polígonos de propriedades privadas do CAR que estão no buffer)
- 0_prodes_buffer.geojson (desmatamento -todos os anos- no buffer)

## Arquivos 1_*

Arquivos com cálculos de áreas (hectare), já considerando o buffer

- 1_desmatamento_municipio_buffer.geojson (área desmatada por município e ano)
- 1_mun_prodes_sicar_buffer.geojson (área desmatada em propriedades privadas por município e ano)
