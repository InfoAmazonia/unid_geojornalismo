# Conflitos entre projetos de carbono e mineração na Amazônia

Os arquivos e scripts utilizados na análise de projetos de carbono estão
organizados nesta pasta com a seguinte estrutura:

- `data-raw`: dados baixados diretamente das fontes
- `data-tidy`: dados intermediários utilizados para as análises após limpeza dos
  dados brutos
- `relatorio`: relatório em Quarto com os principais achados, com resultados em
  PDF, html e docx.

## Reprodução da análise

Os números dos scripts R indicam a ordem em que foram executados para no 
processo de obtenção, limpeza e análise dos dados.

### `1_dados.r`

- Download de processos minerários ativos, dados de degradação, etc
- Enriquecimento dos dados com bases auxiliares
- Intersecção das áreas

### `2_vcus.r`

Download e organização dos dados de créditos comercializados.