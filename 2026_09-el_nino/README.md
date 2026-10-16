
# El Niño: anomalias de chuva e temperatura na Amazônia

Os arquivos e scripts utilizados na análise das anomalias de precipitação e
temperatura observadas (junho a setembro de 2026) e projetadas (outubro a
dezembro de 2026) na Pan-Amazônia e na Amazônia Legal estão organizados nesta
pasta com a seguinte estrutura:

- `dados/raw`: dados brutos
  - `LABCLIM_AnomaliasChuvaTemperatura`: previsão sazonal ECMWF de chuva e
    temperatura (`PREVISAO-CHUVA-TEMP.nc`) e anomalias mensais observadas de
    chuva e temperatura (`OBSERVADO-*.nc`)
  - `normais_inmet_precip` e `normais_inmet_temp`: mapas das normais
    climatológicas 1991-2020 do INMET (precipitação acumulada e temperatura
    média mensal), baixados da API do INMET
  - `RAISG_Cuencas_2025`: bacias hidrográficas da Pan-Amazônia (RAISG)
  - `ibge_rh_meso`: mesorregiões hidrográficas (IBGE)
  - `amazonia_legal.xls`: lista de municípios da Amazônia Legal (IBGE)
- `dados/tidy`: dados tratados
  - `normais_inmet_precip` e `normais_inmet_temp`: normais do INMET
    georreferenciadas (GeoTIFF categórico)
  - `comparacao_normais`: rasters da previsão, da normal e da anomalia projetada
    (previsão − normal), além de tabelas por UF e por município da Amazônia
    Legal
  - `anomalia_*.nc`, `precipitacao_total_previsao_*.nc`: recortes mensais na
    Pan-Amazônia usados nas visualizações
  - `tbl_*.csv`: tabelas de previsão por UF e município e de anomalia observada
    de precipitação em setembro por município
- `tidy_dados`: scripts em R com arrumações dos dados
  - `1_dados_tbl.r`: recortes da previsão e das anomalias observadas e tabelas
    por UF e município
  - `2_bacia.r`: recorte da previsão na Pan-Amazônia
  - `3_normais_inmet.r`: georreferenciamento dos mapas de normais do INMET
  - `4_comparacao_normais.r`: cálculo das anomalias projetadas a partir da
    previsão ECMWF e das normais INMET
  - `5_dataviz.r`: recortes mensais na Pan-Amazônia para visualizações (usa
    os resultados de `4_comparacao_normais.r`)
- `analises`: relatório em Quarto com os principais achados, com resultados em
  html, docx e pdf.
