# Eleições em Mapas - Episódio 05

Análise dos gastos estaduais empenhados em funções e subfunções ligadas à
agenda climática e ambiental entre 2023 e 2026 nos estados da Amazônia
Legal (Siconfi/Tesouro Nacional), com foco nos estados em que o governador
é candidato à reeleição (AC, AM, AP, MT e PA), comparando a execução
orçamentária com as propostas de governo registradas no TSE.

## Estrutura

- `dados/`: dados brutos utilizados na análise (ver
  [Dados externos](#dados-externos))
- `dados-tidy/`: dados processados, saídas dos scripts
- `1_dados.r`: script em R de limpeza e preparação dos dados
- `2_analises.qmd`: relatório em Quarto com as análises (HTML e DOCX)

## Scripts

1. **`1_dados.r`**:
   - lê as despesas empenhadas por função/subfunção dos estados de 2023 a
     2025 (Siconfi, via Base dos Dados), identifica função e subfunção a
     partir do `id_conta_bd` e classifica cada conta como relacionada ou
     não ao clima (coluna `clima`), com um nível de prioridade (`nivel`):
     1 para toda a função 18 (Gestão Ambiental); 2 para Defesa Civil,
     Saneamento Básico Rural e Urbano, Ordenamento Territorial, Conservação
     de Energia e Reforma Agrária; e 3 para Extensão Rural, Serviços Urbanos
     e Assistência aos Povos Indígenas. Gera `bd_siconfi.csv`;
   - lê o RREO de 2026 (Anexo 02, até o 3º bimestre), mantém as despesas
     empenhadas até o bimestre dos estados da Amazônia Legal, associa os
     códigos de conta da base do Siconfi e junta as duas bases, removendo
     linhas de totais e intraorçamentárias. Gera `gastos.csv`;
   - lê os candidatos de 2026 do TSE, mantém apenas as candidaturas a
     governador e marca os que concorrem à reeleição (coluna `reeleicao`).
     Gera `candidatos.csv`.
2. **`2_analises.qmd`**: calcula o total empenhado em contas relacionadas
   ao clima e sua proporção sobre o total de despesas nos estados com
   candidato à reeleição e, para cada um desses estados, apresenta a
   evolução dos gastos por conta (tabela e gráfico) ao lado de um resumo
   das propostas ambientais dos candidatos ao governo.

## Reprodução da análise

Rodar `1_dados.r` antes de renderizar `2_analises.qmd`, pois o relatório
depende dos arquivos gerados em `dados-tidy/`. Os scripts usam
`here::here("2026_09-eleicoes_em_mapas/ep_05/...")`, então devem ser
executados a partir da raiz do repositório.

Os dados de 2026 são parciais (despesas empenhadas até o 3º bimestre).

## Dados externos

- **Despesas por função (2023-2025)** — `dados/job_*.csv`: exportado do
  BigQuery da [Base dos Dados](https://basedosdados.org/dataset/5a3dec52-8740-460e-b31d-0e0347979da0?table=0091d8c9-a2d0-4e6e-86f9-2b4c86aee683)
  com a consulta:

  ```sql
  SELECT * FROM `basedosdados.br_me_siconfi.uf_despesas_funcao`
  WHERE ano >= 2023
    AND sigla_uf IN ("AC", "AM", "AP", "MA", "MT", "PA", "RO", "RR", "TO")
  ```

- **RREO 2026** — `dados/finbraRREO.csv`: baixado na
  [consulta Finbra/RREO do Siconfi](https://siconfi.tesouro.gov.br/siconfi/pages/public/consulta_finbra_rreo/finbra_rreo_list.jsf)
  (exercício 2026, 3º bimestre, escopo Estados/DF, Anexo 02 -
  Demonstrativo da Execução das Despesas por Função/Subfunção). O arquivo
  vem separado por `;`, com codificação ISO-8859-1 e 6 linhas de
  cabeçalho antes da tabela.
- **Candidatos 2026** — `dados/candidatos/consulta_cand_2026_<UF>.csv`:
  [Portal de Dados Abertos do TSE](https://dadosabertos.tse.jus.br/dataset/candidatos-2026),
  arquivos por UF dos estados da Amazônia Legal (separados por `;`,
  ISO-8859-1).
- **Propostas de governo** — `dados/propostas/<UF>/`: PDFs das propostas
  dos candidatos a governador, também do
  [Portal de Dados Abertos do TSE](https://dadosabertos.tse.jus.br/dataset/candidatos-2026)
  (cerca de 120 MB). Os arquivos são nomeados pelo `SQ_CANDIDATO`; foram
  lidos para a elaboração dos resumos do relatório, mas não são processados
  pelos scripts.

Os dados do Siconfi e do TSE são atualizados periodicamente — vale conferir
a data da última atualização antes de rodar os scripts.

## Dados (`dados/` e `dados-tidy/`)

- `bd_siconfi.csv`: despesas empenhadas dos estados da Amazônia Legal por
  função/subfunção, 2023-2025, com as colunas `funcao`, `subfuncao`,
  `nivel` e `clima` (saída do script 1)
- `gastos.csv`: `bd_siconfi.csv` acrescido das despesas empenhadas de 2026
  (até o 3º bimestre) do RREO, sem linhas de totais e intraorçamentárias
  (saída do script 1)
- `candidatos.csv`: candidatos a governador em 2026 nos estados da Amazônia
  Legal, com a coluna `reeleicao` indicando quem concorre à reeleição
  (saída do script 1)
