
# El Niño: anomalias de chuva e temperatura na Amazônia

Os arquivos e scripts utilizados na análise das anomalias de precipitação e
temperatura observadas (setembro e outubro de 2026) e projetadas (novembro e
dezembro de 2026) na Amazônia Legal estão organizados nesta pasta com a
seguinte estrutura:

- `dados/raw`: dados brutos
  - `LABCLIM_AnomaliasChuvaTemperatura`: dados fornecidos pelo Laboratório de
    Modelagem do Sistema Climático Terrestre da Universidade do Estado do
    Amazonas (Labclim-UEA)
    - `PREV-NOVEMBRO-novo.nc` e `PREV-DEZEMBRO-novo.nc`: anomalias de
      precipitação (`TP`, em mm/mês) e de temperatura a 2 metros (`T2M`, em K,
      equivalente a °C por se tratar de diferença) da previsão sazonal do
      ECMWF, rodada de outubro de 2026, em relação à climatologia do próprio
      modelo. Grade global de 1°, com longitude de 0° a 360°
    - `CHUVA_JUL-OUT`: anomalias mensais de precipitação observada (MERGE/Inpe,
      grade de 0,1°), de julho a outubro de 2026, em relação à normal de
      1998-2024. A primeira camada de cada arquivo é a anomalia
    - `TEMP_JUL-OUT`: anomalias mensais de temperatura média observada
      (SAMeT/Inpe, grade de 0,05°), de julho a outubro de 2026. A primeira
      camada de cada arquivo é a anomalia
  - `ibge_rh_meso`: mesorregiões hidrográficas (IBGE)
- `dados/tidy`: dados tratados
  - `tbl_projecao_uf_202611.csv` e `tbl_projecao_uf_202612.csv`: anomalia
    projetada média de precipitação e de temperatura por estado da Amazônia
    Legal, em novembro e dezembro
  - `tbl_projecao_municipio_202611.csv` e `tbl_projecao_municipio_202612.csv`:
    o mesmo, por município (809 municípios)
- `tidy_dados`: scripts em R com arrumações dos dados
  - `1_dados_labclim_previsao.r`: gera as tabelas de `dados/tidy`, com a média
    das anomalias projetadas em cada estado e município ponderada pela fração
    de cada célula da grade dentro do polígono
- `analises`: relatório em Quarto com os principais achados, com resultados em
  html e docx

A Amazônia Legal considerada aqui são os nove estados inteiros (AC, AM, AP, MA,
MT, PA, RO, RR e TO), incluindo todo o Maranhão. As bacias hidrográficas são as
mesorregiões hidrográficas do IBGE com mais da metade da área nesses estados.
Os limites de estados e municípios são obtidos com o pacote
[{geobr}](https://ipeagit.github.io/geobr/) (IBGE, 2025).
