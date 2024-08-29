# Como investigamos o impacto dos agrotóxicos na saúde infantil do Mato Grosso?

A Constituição brasileira garante o direito a um meio ambiente ecologicamente equilibrado, considerado um bem de uso comum e essencial para a qualidade de vida. Diz ainda que compete ao poder público e à coletividade o dever de defender e preservar o meio ambiente para a presente e a futura geração.

Ocorre que a área agrícola no país [aumentou](https://brasil.mapbiomas.org/2023/10/06/area-de-agropecuaria-no-brasil-cresceu-50-nos-ultimos-38-anos/) 41,9 milhões de hectares desde 1985 até 2022, equivalente a duas vezes a do estado do Paraná. As lavouras de grãos e cana triplicaram no período. Desse total, 35 milhões de hectares refletem apenas o avanço da soja. O aumento vertiginoso foi acompanhado pelo uso de agrotóxicos. O país é hoje o [campeão mundial no uso](https://www.fao.org/faostat/en/#data/RP) de pesticidas nas lavouras.

Nós queríamos entender como a expansão da produção agrícola e uso dos agrotóxicos têm impactado a saúde das crianças brasileiras. No ano passado, em um projeto gestado e executado no Laboratório de Geojornalismo (LabJor), [analisamos o impacto da soja nas taxas de leucemia, linfoma, anomalias congênitas e mortes fetais em crianças e adolescentes mato-grossenses](https://infoamazonia.org/2023/08/31/producao-de-soja-aumenta-risco-de-cancer-e-mortes-de-criancas-e-adolescentes-no-mato-grosso/).

Este ano resolvemos ampliar essa investigação, com recursos da International Women’s Media Foundation (IWMF), sediada nos Estados Unidos. Queríamos saber como a produção agrícola impacta a saúde de crianças nos principais estados produtores de commodities. Elegemos as anomalias congênitas e as mortes fetais, duas condições que são fortemente associadas aos agrotóxicos.


## Como investigamos

Buscamos as taxas anuais de óbitos fetais do [Sistema de Informação de Mortalidade (SIM)](https://svs.aids.gov.br/daent/centrais-de-conteudos/paineis-de-monitoramento/mortalidade/) e de anomalias congênitas no [Sistema Nacional de Nascidos Vivos (Sinasc)](https://svs.aids.gov.br/daent/centrais-de-conteudos/paineis-de-monitoramento/natalidade/). Com essa informação, estimamos as taxas dessas doenças por 1 mil habitantes em nível municipal, considerando a população estimada pelo Instituto Brasileiro de Geografia e Estatística (IBGE).

Posteriormente, calculamos a área municipal dedicada à produção agrícola a partir dos dados de uso da terra da plataforma MapBiomas. Criamos variáveis categóricas para identificar os municípios brasileiros com 5%, 30% e 50% do território com produção agrícola. Também calculamos quais estados tinham mais da metade dos municípios com 5% de área cultivada, o que mostrou quais são os principais estados agrícolas brasileiros. Entraram na amostra sete: Mato Grosso, Mato Grosso do Sul, Goiás, São Paulo, Santa Catarina, Paraná e Rio Grande do Sul. São Paulo e Paraná não apresentaram associação positiva. 

A partir do cruzamento desses dados, foi mensurado o risco da ocorrência dessas condições por município, conforme a área agrícola. 

A análise mostrou que estado do Mato Grosso tem um risco de anomalias congênitas e mortes fetais superior ao de outros estados analisados. Os índices eram bem maiores, 20% e 30% superiores, nos municípios com área agrícola maior de 5% do território na comparação com os municípios que não alcançam este percentual. 

## Bases de dados utilizadas

| Fonte | Base original (link) | Base analisada/ gerada (link) |
|:---|:---|:---|
| Excel completo com dados de agricultura dos municípios do MT (MapBiomas) e das taxas de anomalias congênitas e mortes fetais do Sistema Nacional de Nascidos Vivos (Sinasc) e Sistema de Informação de Mortalidade (SIM) do Datasus, Ministério da Saúde (2013-2021) | [https://docs.google.com/spreadsheets/d/1WDoMWBj2Z_EZpR5L6l7WB9ERSHvSY8tS/edit?usp=drive_link&ouid=101395816128045558546&rtpof=true&sd=true](https://docs.google.com/spreadsheets/d/1WDoMWBj2Z_EZpR5L6l7WB9ERSHvSY8tS/edit?usp=drive_link&ouid=101395816128045558546&rtpof=true&sd=true) |  |
| MapBiomas | [https://brasil.mapbiomas.org/estatisticas/](https://brasil.mapbiomas.org/estatisticas/) |  |
| Sinasc | [http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sinasc/Anomalias/anomabr.def](http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sinasc/Anomalias/anomabr.def) |  |
| SIM | [http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/fet10br.def](http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sim/cnv/fet10br.def) |  |


## Como analisamos e limitação dos dados

Após a seleção dos ser analisados, utilizamos o programa estatístico “R” para estimar o Risco Relativo (RR), que compara as taxas de incidência associadas a diferentes níveis de exposição. Assim, buscamos identificar se as taxas de casos de malformações congênitas e de óbitos fetais eram diferentes, com significância estatística, entre os municípios com área plantada superior a 5%.

Para a interpretação, valores de RR e seus intervalos de confiança superiores a 1 indicam que o aumento na taxa de casos de malformações congênitas e óbitos fetais podem estar associadas ao fato do município possuir uma área cultivada maior que 5%, 30% ou 50% em função da exposição aos agrotóxicos potencialmente usados nestas áreas.

Uma das limitações encontradas foi a falta de dados municipais sobre a comercialização de agrotóxicos. O IBGE apenas contabiliza o número de estabelecimentos rurais que usam agrotóxicos no Censo Agropecuário, sem estimar o volume. Usamos os dados do Ibama sobre comercialização de pesticidas (em volume) para mostrar que os estados de maior área agrícola são também os que mais usam, mas não cruzamos com os dados da pesquisa porque nossos dados eram de base municipal. Assim, inferimos haver associação entre a incidências de malformações congênitas e óbitos fetais com a maior proporção de área cultivada, mas, em uma etapa futura, seria interessante fazer análises com dados de volume de agrotóxicos comercializados e utilizados em nível municipal.

O estudo revela uma associação entre área agrícola e desfechos graves em crianças e estima um risco associado à área agrícola, que por sua vez está relacionada ao agrotóxicos. Mas são necessários mais estudos para entender por que o MT oferece mais risco às crianças do que outros estados do Centro-Oeste e da Região Sul.

## Fontes e cientistas consultados 

Toda a análise técnica desenvolvida para esta investigação foi realizada com a consultoria da pesquisadora Tatiane Moraes, pós-doutoranda na Universidade de São Paulo (USP). Moraes atua em pesquisas sobre epidemiologia ambiental, políticas de saúde, e o papel dos determinantes sociais e ambientais da saúde. Atualmente, integra a equipe de coordenação da Rede Solidária de Pesquisa em Políticas Públicas e do Observatório Clima e Saúde.

Também foram entrevistados pesquisadores, professores e profissionais de saúde para compreender melhor os resultados do levantamento.

Foi fundamental conversar com especialistas em anomalias congênitas e mortes fetais para entender se essas doenças podem ser influenciadas pela exposição aos agrotóxicos. Uma das fontes ouvidas, Lavinia Schuler-Faccini, professora da Universidade Federal do Rio Grande do Sul, explicou os diferentes mecanismos pelos quais os agrotóxicos afetam a saúde reprodutiva de homens e mulheres. Seu orientando de doutorado, Ricardo Rohweder está desenvolvendo estudos sobre agrotóxicos e hipospádia, um tipo de anomalia congênita que ocorre em meninos.

Sobre o contexto do Mato Grosso, ouvimos o biólogo Eduardo Darvin, coordenador do programa de Economias Sociais do Instituto Centro de Vida (ICV) para entender a dinâmica territorial de ocupação do estado e como a expansão agrícola agora avança sobre as regiões norte e noroeste, no bioma amazônico, onde estão situados territórios indígenas e importantes parques nacionais.

Também entrevistamos uma das maiores autoridades em agrotóxicos do Brasil, Larissa Bombardi, pesquisadora da Universidade de São Paulo (USP) e autora do livro “Colonialismo Químico”. Bombardi questionou o valor de produzir commodities em larga escala sem considerar o impacto na sociedade brasileira.

## Fontes consultadas

| Nome | Instituição | Lattes | Redes |
|:---|:---|:---|:---|
| Tatiane Moraes de Sousa | Universidade de São Paulo (USP) | [Lattes](https://buscatextual.cnpq.br/buscatextual/busca.do) | [LinkedIn](https://www.linkedin.com/in/tatianemoraes/?originalSubdomain=br) |
| Eduardo Darvin | Instituto Centro de Vida (ICV) |  |  |
| Lavinia Schuler-Faccini | Universidade Federal do Rio Grande do Sul (UFRGS) | [Lattes](https://buscatextual.cnpq.br/buscatextual/busca.do) | [Instagram](https://www.instagram.com/lavinietta/) |
| Mariana Soares | Universidade Federal do Mato Grosso (UFMT) |  | [LinkedIn](https://www.linkedin.com/in/mariana-soares-aa2155aa/?originalSubdomain=br) |
| Larissa Bombardi | Universidade de São Paulo (USP) | [Lattes](https://buscatextual.cnpq.br/buscatextual/busca.do) | [Twitter](https://twitter.com/mieslarissa) |
| Solange Garcia | Universidade Federal do Rio Grande do Sul (UFRGS) | [Lattes](https://buscatextual.cnpq.br/buscatextual/busca.do;jsessionid=488C50146E739D8F22381BA7C5184F10.buscatextual_0) | [LinkedIn](https://www.linkedin.com/in/solange-cristina-garcia-5a877355/?originalSubdomain=br) |
| Ricardo Rohweder | Universidade Federal do Rio Grande do Sul (UFRGS) | [Lattes](https://buscatextual.cnpq.br/buscatextual/busca.do) | [Twitter](https://twitter.com/RicardoRohweder) |
| Bruno Choary Cunha de Lima | Procurador do trabalho no MPT-MT |  |  |
