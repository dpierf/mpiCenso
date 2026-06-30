# MPI-LA Brasil: Censo Demográfico (1980-2010)

Pipeline reproduzível para cálculo, análise e visualização do **Índice de Pobreza Multidimensional para América Latina (MPI-LA)**, 
adaptado para os microdados do Censo Demográfico Brasileiro (1980-2010), se baseando na metodologia de Santos et al. (2015).

---

## Funcionalidades

- **Ingestão**: download e processamento dos microdados do Censo, a partir do pacote `censobr`
- **Cálculo**: construção dos indicadores e do score MPI-LA a nível de domicílio
- **Análise descritiva**: gráficos e tabelas de evolução, composição, decomposição, convergência, dominância e concentração
- **Machine Learning**: aplicação de análise de componentes principais (PCA), random forest (RF) e clusterização fuzzy (C-Means)
- **Modelagens Econométricas**: implementação de modelos de regressão logística, fracionária e quantílica
- **Análises de Convergência**: estudos de convergência beta e sigma (absoluta e condicional)
- **Econometria Espacial**: autocorrelação espacial (I de Moran, LISA), hotspots (Getis-Ord) e modelos SAR e SEM
- **Análises Espaciais**: análises de redes migratórias, modelos gravitacionais para fluxos migratórios

---

## Estrutura do repositório

```
mpiCenso/
├── output/                 # Resultados gráficos esperados
│   ├── 01_descriptive/     # Gráficos do código `04_graficos.R`
│   ├── 02_ml/              # Gráficos do código `05_modelagem1.R`
│   ├── 03_regressions/     # Gráficos do código `06_modelagem2.R`
│   ├── 04_decomposition/   # Gráficos do código `07_decomposicao.R`
│   └── 05_spatial/         # Gráficos do código `08_especial.R`
├── R/                      # Arquivos das etapas
│   ├── 00_packages.R       # Insalação e importação de pacotes
│   ├── 01_tratamento.R     # Tratamento de bases de dados e criação do MPI-LA
│   ├── 02_crosswalk.R      # Cruzamento de ID municipal com Regiões Imediatas
│   ├── 03_bases.R          # Bases agregadas por RIM e bases amostradas
│   ├── 04_graficos.R       # Análises descritivas
│   ├── 05_modelagem1.R     # Machine Learning
│   ├── 06_modelagem2.R     # Modelos Econométricos
│   ├── 07_decomposicao.R   # Decomposições de impacto
│   └── 08_espacial.R       # Métricas de rede e modelos gravitacionais
├── LICENSE
└── README
```

---

## Dados necessários

Os microdados do Censo não estão incluídos no repositório, devendo ser baixados por meio das funções disponíveis no pacote `censobr`.
O fluxo de download, join e tratamento está no código `01_tratamento.R`.

A pasta `output` contém os resultados gráficos que se espera obter a partir da execução do código no estado atual. Arquivos RDS não foram incluídos.

---

## Autoria

**Pier Francesco De Maria**  
[![Lattes](https://img.shields.io/badge/Lattes-CNPq-blue)](http://lattes.cnpq.br/8532403786219091)

[![ORCID](https://img.shields.io/badge/ORCID-0000--0003--1389--3082-green)](https://orcid.org/0000-0003-1389-3082)  

[![Email](https://img.shields.io/badge/Email-dpierf%40gmail.com-red)](mailto:dpierf@gmail.com)

[![LinkedIn](https://img.shields.io/badge/LinkedIn-Pier%20F.%20De%20Maria-blue?logo=linkedin)](https://www.linkedin.com/in/dpierf)

---

## Origem e referência teórica

Estes códigos foram desenvolvidos no âmbito de pesquisa acadêmica em continuidade à dissertação de mestrado:

> MARIA, Pier Francesco De. **Diferenciais sociodemográficos e espaciais da pobreza no Estado de São Paulo (1991-2015)**. 2018. Tese (Doutorado em Demografia) - Universidade Estadual de Campinas, Campinas, 2018.

O desenvolvimento e a implementação do MPI-LA a nível Brasil seguiu, como referencial, o seguinte trabalho do Oxford Poverty and Human Development Initiative (OPHI):

> SANTOS, Maria Emma et al. **A multidimensional poverty index for Latin America**. Oxford: OPHI, 2015. (OPHI Working Paper, 79). Disponível em: https://ophi.org.uk/wp-content/uploads/OPHIWP079.pdf. Acesso em: 27 abr. 2026.

---

## Uso de inteligência artificial generativa

O desenvolvimento deste pacote contou com o auxílio extensivo de inteligência artificial generativa ao longo de todo o processo — incluindo arquitetura do pipeline, escrita e revisão de código, decisões metodológicas e documentação. A ferramenta utilizada foi:

> ANTHROPIC. **Claude Sonnet 4.6**. San Francisco: Anthropic, 2025. Disponível em: https://claude.ai. Acesso em: 25 jun. 2026.

O uso de IA não substitui a responsabilidade intelectual do autor sobre as escolhas metodológicas, interpretações e resultados apresentados.
