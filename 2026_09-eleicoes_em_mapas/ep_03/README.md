# Eleições em Mapas - Episódio 03

Análise do desmatamento na Amazônia Legal (PRODES e DETER, INPE) e de sua
relação com o resultado das eleições presidenciais de 2022, identificando
os municípios com maior percentual da área desmatada em 2021 (governo
Bolsonaro) e em 2023 (governo Lula).

## Estrutura

- `dados/`: dados brutos utilizados na análise (parte não é versionada no
  repositório — ver [Dados geográficos externos](#dados-geográficos-externos-não-versionados))
- `dados-tidy/`: dados processados, saídas dos scripts
- `1_prodes.r`, `2_deter.r`, `3_ranking_municipios.r`: scripts em R

## Scripts

1. **`1_prodes.r`**: consolida a série histórica de área desmatada por ano
   a partir dos dados tabulares do PRODES por UF
   (`terrabrasilis_legal_amazon_*.csv`) e do total de 2026 calculado no
   script 2 (`deter_ano_2026.csv`), gerando `desmatamento_ano.csv` com a
   área desmatada (km²/ha) e a estimativa de árvores derrubadas (565
   árvores por hectare).
2. **`2_deter.r`**: a partir dos alertas de desmatamento do DETER (shapefiles
   `deter-nf`, `deter-amz`, `deter-cerrado`, `deter-pantanal`), filtra os
   alertas de 2026, recorta pela geometria da Amazônia Legal (`geobr`) e
   soma a área total desmatada no ano, gerando `deter_ano_2026.csv`
   (usado pelo script 1).
3. **`3_ranking_municipios.r`**: cruza a geometria anual de desmatamento do
   PRODES (`yearly_deforestation`) com os limites municipais (`geobr`) para
   os anos de 2021 e 2023, calcula o percentual da área de cada município
   desmatada em cada ano e gera os rankings dos 10 municípios com maior
   percentual desmatado em cada período — `ranking_municipios_bolsonaro.csv`
   (2021) e `ranking_municipios_lula.csv` (2023).

## Reprodução da análise

Rodar `2_deter.r` antes de `1_prodes.r`, pois o script 1 depende do
`deter_ano_2026.csv` gerado pelo script 2. `3_ranking_municipios.r` é
independente dos outros dois, mas depende dos dados do PRODES descritos
abaixo.

## Dados geográficos externos (não versionados)

Os shapefiles do PRODES e do DETER usados em `2_deter.r` e
`3_ranking_municipios.r` **não estão no repositório** (arquivos grandes) e
precisam ser baixados separadamente do portal TerraBrasilis do INPE:
<http://terrabrasilis.dpi.inpe.br/downloads/>

- **PRODES** — pasta `dados/yearly_deforestation/`: shapefile com os
  polígonos anuais de desmatamento da Amazônia Legal. No portal, procurar
  pela seção de desmatamento do PRODES ("Yearly deforestation" / taxas de
  desmatamento, Amazônia Legal) e extrair o shapefile baixado para essa
  pasta.
- **DETER** — pastas `dados/deter-nf/`, `dados/deter-amz/`,
  `dados/deter-cerrado/` e `dados/deter-pantanal/`: shapefiles de alertas
  de desmatamento do DETER, um por programa de monitoramento (Amazônia
  Legal, Cerrado, Pantanal e "Não Floresta"/NF). No mesmo portal, procurar
  pela seção de alertas do DETER e baixar o shapefile correspondente a
  cada programa para a respectiva pasta.

Ambos os shapefiles são atualizados periodicamente pelo INPE — vale
conferir no portal qual é a versão/data mais recente disponível antes de
rodar os scripts.

## Dados (`dados/` e `dados-tidy/`)

- `terrabrasilis_legal_amazon_*.csv`: área desmatada por ano e UF na
  Amazônia Legal, exportada do painel de estatísticas do TerraBrasilis
  (PRODES)
- `yearly_deforestation/`, `deter-nf/`, `deter-amz/`, `deter-cerrado/`,
  `deter-pantanal/`: shapefiles do PRODES e do DETER (ver seção acima —
  não versionados)
- `desmatamento_ano.csv`: série histórica de área desmatada e árvores
  derrubadas por ano, a partir de 2001 (saída do script 1)
- `deter_ano_2026.csv`: área total desmatada em 2026 segundo o DETER
  (saída do script 2)
- `ranking_municipios_bolsonaro.csv`: os 10 municípios com maior
  percentual da área desmatada em 2021 (saída do script 3)
- `ranking_municipios_lula.csv`: os 10 municípios com maior percentual da
  área desmatada em 2023 (saída do script 3)
