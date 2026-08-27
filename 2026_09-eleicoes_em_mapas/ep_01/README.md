# Eleições em Mapas - Episódio 01

Análise que cruza o posicionamento climático dos partidos eleitos para
presidente na Amazônia Legal (2010-2022) com a expansão da área e do valor
da produção agropecuária nos municípios, usando dados do Observatório do
Legislativo, do TSE e do IBGE (SIDRA).

## Estrutura

- `dados/`: dados brutos e processados utilizados na análise
- `1_dados_obl.r`, `2_dados_eleicoes.r`, `3_modelo.r`: scripts em R

## Scripts

1. **`1_dados_obl.r`**: calcula a nota climática média por partido a partir
   dos rankings do Observatório do Legislativo (`ranking_clima*.csv`),
   gerando `dados/obl_notas_partido.csv`.
2. **`2_dados_eleicoes.r`**: script principal. Carrega os resultados de
   presidente (exportados do BigQuery) para os estados da Amazônia Legal
   (AC, AM, PA, RR, RO, MT, MA, TO, AP), calcula a "nota do eleitorado" de
   cada município (média das notas dos partidos ponderada pelos votos),
   cruza com os dados geográficos dos municípios (`geobr`) e com dados de
   área plantada e valor real da produção agropecuária do IBGE (deflacionados
   com `deflateBR`). Gera as tabelas de municípios que mais pioraram/
   melhoraram a nota entre 2010 e 2022 e o `indice_por_municipio.geojson`
   usado nos mapas.
3. **`3_modelo.r`**: ajusta modelos multinível (`lme`, via `tidymodels`/
   `multilevelmod`) para testar a relação entre a nota do eleitorado e o
   valor e a área da produção agropecuária por município.

## Reprodução da análise

Os números dos scripts indicam a ordem de execução: primeiro
`1_dados_obl.r`, depois `2_dados_eleicoes.r` e, por fim, `3_modelo.r`.

## Dados (`dados/`)

- `bq-results-*.csv`: resultados eleitorais brutos exportados do BigQuery
- `ranking_clima*.csv`: notas climáticas por partido (Observatório do
  Legislativo)
- `obl_notas_partido.csv`: nota climática média por partido (saída do
  script 1)
- `presidente.csv`: nota do eleitorado por município/ano/turno (saída do
  script 2)
- `agro.xlsx`/`agro.csv`: valor da produção agropecuária por município
  (IBGE/SIDRA, bruto e tratado)
- `agro_area.xlsx`/`agro_area.csv`: área plantada por município (IBGE/SIDRA,
  bruto e tratado)
- `indice_por_municipio.geojson`: geometria dos municípios com a nota do
  eleitorado por ano, usada nos mapas
- `tbl_notas_municipios.csv`, `tbl_munis_presidente.csv`: notas do
  eleitorado por município nas eleições de 2010 a 2022
- `tbl_piorou.csv`, `tbl_melhorou.csv`, `ids_municipios_piora.csv`:
  municípios com maior piora/melhora na nota do eleitorado
- `tbl_agro_total.csv`, `tbl_agro_municipio.csv`: valor da produção
  agropecuária agregado por ano e por município
