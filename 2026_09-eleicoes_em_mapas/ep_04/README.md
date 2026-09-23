# Eleições em Mapas - Episódio 04

Análise das emendas parlamentares individuais destinadas a ações
orçamentárias ligadas à agenda climática e ambiental entre 2023 e 2026
(Portal da Transparência), a partir do recorte de programas e ações
definido pelo Inesc, identificando os parlamentares que mais empenharam
recursos e a distribuição dos valores dentro e fora da Amazônia Legal.

## Estrutura

- `dados/`: dados brutos utilizados na análise (não versionados no
  repositório — ver [Dados externos](#dados-externos-não-versionados))
- `dados-tidy/`: dados processados, saídas dos scripts
- `1_emendas.r`, `2_eda.r`: scripts em R
- `programas_acoes_inesc.md`: lista dos programas e ações do recorte
  climático do Inesc usada como referência para o filtro

## Scripts

1. **`1_emendas.r`**: lê os arquivos anuais de emendas parlamentares por
   documento (`*_EmendasParlamentares_PorDocumento.csv`), mantém apenas as
   emendas individuais e filtra as ações orçamentárias do recorte climático
   do Inesc (ver `programas_acoes_inesc.md`), acrescido de algumas ações
   adicionais identificadas nos dados (ex.: 14RL, 7XK6, 10SC, 8948, 10SG,
   21CA, 00TN, 7Y06). Gera `emendas_doc_filtradas.csv`.
2. **`2_eda.r`**: converte o valor empenhado para numérico, padroniza a UF
   do favorecido (usando a UF de aplicação do recurso quando ausente) e
   classifica cada registro como dentro ou fora da Amazônia Legal, gerando
   `emendas_tidy.csv`. Em seguida, faz a análise exploratória: total
   empenhado e número de emendas por autor e por localização (Amazônia
   Legal ou não), e lista as ações presentes nos dados que não constam na
   lista original do Inesc.

## Reprodução da análise

Rodar `1_emendas.r` antes de `2_eda.r`, pois o script 2 depende do
`emendas_doc_filtradas.csv` gerado pelo script 1. Os scripts usam
`here::here("ep_04/...")`, então devem ser executados a partir da raiz do
projeto `2026_09-eleicoes_em_mapas`.

## Dados externos (não versionados)

Os arquivos brutos de emendas **não estão no repositório** (cerca de
300 MB cada) e precisam ser baixados do Portal da Transparência:
<https://portaldatransparencia.gov.br/download-de-dados/emendas-parlamentares-documentos/>

- **Emendas parlamentares por documento** — pasta `dados/`: baixar os
  arquivos de 2023, 2024, 2025 e 2026 e extrair os CSVs
  (`<ano>_EmendasParlamentares_PorDocumento.csv`) para essa pasta. Os
  arquivos vêm separados por `;` e com codificação ISO-8859-1.

Os dados são atualizados periodicamente pela CGU — vale conferir a data da
última atualização no portal antes de rodar os scripts.

A lista de programas e ações do recorte climático vem da nota técnica do
Inesc sobre financiamento climático e emendas parlamentares (PLOA 2026):
<https://inesc.org.br/wp-content/uploads/2025/12/nt-financiamento_climatico_e_emendas_parlamentares.pdf>

## Dados (`dados/` e `dados-tidy/`)

- `<ano>_EmendasParlamentares_PorDocumento.csv`: documentos de execução
  (empenho, liquidação, pagamento) das emendas parlamentares por ano,
  exportados do Portal da Transparência (ver seção acima — não versionados)
- `emendas_doc_filtradas.csv`: documentos de emendas individuais
  restritos às ações do recorte climático (saída do script 1)
- `emendas_tidy.csv`: mesma base com valor empenhado numérico, UF do
  favorecido padronizada e a coluna `uf_amazonia` (Sim/Não/Sem
  informação) indicando se a UF pertence à Amazônia Legal (saída do
  script 2)
