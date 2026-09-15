# Eleições em Mapas - Episódio 02

Análise que aprofunda o índice de posicionamento climático do eleitorado
(construído no [episódio 01](../ep_01)) para o nível de zona eleitoral,
cruzando os resultados de presidente por zona nas eleições de 2010 e 2022
com dados demográficos do Censo (IBGE) por setor censitário — idade, renda,
raça/cor, alfabetização, urbanização e religião — nos estados da Amazônia
Legal (AC, AM, AP, MA, MT, PA, RO, RR, TO).

## Estrutura

- `dados/`: dados brutos e processados utilizados na análise
- `0_setores.r`, `1_resultado_zona.r`, `2_demografia.r`, `3_zona_setor.r`:
  scripts em R
- `dataviz.r`: exploração e visualizações a partir dos dados processados

## Scripts

1. **`0_setores.r`**: lê os shapefiles de setores censitários do IBGE
   (2010 e 2022) para os estados da Amazônia Legal e consolida em
   `setores2010.rds`/`setores2022.rds`, além de gerar `ids_municipios.csv`
   com os identificadores de setor/município.
2. **`1_resultado_zona.r`**: script principal. A partir dos locais de
   votação (`geobr::read_polling_places`), gera polígonos de Voronoi para
   aproximar a área de cada zona eleitoral, cruza com os limites estaduais
   e calcula a "nota do eleitorado" por zona (mesma lógica do episódio 01,
   ponderada pelos votos e pela nota climática dos partidos). Gera
   `notas_zona.csv` e as geometrias `geo_resultado_zona.rds`/
   `geo_resultado_zona_2022.rds`.
3. **`2_demografia.r`**: processa os microdados do Censo (IBGE, 2010 e
   2022) por setor censitário — idade, renda, raça/cor e alfabetização —,
   tratando e deflacionando (`deflateBR`) os valores de renda.
4. **`3_zona_setor.r`**: cruza a geometria das zonas eleitorais com os
   setores censitários (interseção espacial e percentual de área do setor
   em cada zona) para agregar as variáveis demográficas do script anterior
   ao nível de zona eleitoral — urbanização, idade, renda, raça/cor e
   alfabetização.

Além dos scripts numerados, `dataviz.r` reúne explorações e gráficos
soltos a partir dos dados já processados (ex.: nota da zona x proporção de
evangélicos, nota da zona x proporção de pessoas brancas, ranking dos
municípios com zonas de nota mais alta/mais baixa).

## Reprodução da análise

Os números dos scripts indicam a ordem de execução: `0_setores.r`,
`1_resultado_zona.r`, `2_demografia.r` e `3_zona_setor.r`. `1_resultado_zona.r`
depende dos dados de nota por partido gerados em `1_dados_obl.r` do
episódio 01. `dataviz.r` é exploratório e deve ser rodado por último, após
os demais.

## Dados (`dados/`)

- `ids_municipios.csv`: identificadores de setor censitário e município
  por UF (saída do script 0)
- `geo_resultado_zona_2022.rds`: geometria das zonas eleitorais (2022) com
  a nota do eleitorado (saída do script 1)
- `zona_setor_2022_idade.rds`, `zona_setor_2022_raca.rds`,
  `zona_setor_2022_renda.rds`: variáveis demográficas (idade, raça/cor,
  renda) agregadas por zona eleitoral em 2022 (saída do script 3)
- `religiao_zona.rds`: proporção de pessoas por religião agregada por
  zona eleitoral
- `mapa_zonas.png`: mapa das zonas eleitorais geradas a partir dos
  polígonos de Voronoi
- `setores2022.zip`: disponível para download no [release](https://github.com/InfoAmazonia/unid_geojornalismo/releases/download/prodes/setores2022.zip)
