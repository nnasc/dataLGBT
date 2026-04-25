
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dataLGBT

<p align="center">

<img src="https://raw.githubusercontent.com/nnasc/dataLGBT/master/images/dataLGBT_logo.png" width="300">
</p>

O **dataLGBT** é um pacote para o R que reúne funções para
processamento, vinculação (linkage) e geração de relatórios a partir de
dados de mortalidade de pessoas LGBT no Brasil.

O pacote implementa um pipeline estruturado e reprodutível, permitindo
integrar bases de dados de diferentes sistemas de informação em saúde e
gerar bancos prontos para análise com variáveis padronizadas e
indicadores epidemiológicos.

O pacote foi desenvolvido com o objetivo de fornecer autonomia a
técnicos de dados em saúde na produção de análises epidemiológicas em
diferentes níveis administrativos, utilizando dados públicos do DATASUS.

## Instalação

``` r
# install.packages("remotes")
remotes::install_github("nnasc/dataLGBT")
```

## Principais funcionalidades

- Vinculação probabilística utilizando Expectation-Maximization (EM) via
  fastLink
- Padronização automática de variáveis
- Deduplicação probabilística
- Construção de variáveis epidemiológicas em múltiplos domínios
- Estimativa de indicadores de carga de doença
- Geração automatizada de relatórios epidemiológicos
- Pipeline controlado para garantir reprodutibilidade

## Utilização

A utilização do pacote consiste, em geral, em três etapas:

1.  Preparação dos dados provenientes de diferentes sistemas (ex: SIM e
    SINAN)  
2.  Vinculação dos bancos de dados (linkage)  
3.  Geração de indicadores e relatórios

A imagem abaixo apresenta um resumo das funções do pacote:

<figure>
<img
src="https://raw.githubusercontent.com/nnasc/dataLGBT/master/images/dataLGBT_functions.png"
alt="Funções do pacote dataLGBT" />
<figcaption aria-hidden="true">Funções do pacote dataLGBT</figcaption>
</figure>

### Estrutura do Pipeline

O fluxo analítico do dataLGBT é composto por três etapas principais:

``` r
SINAN + SIM → data_link() → data_proc() → gen_report() → PDF
```

1.  Vinculação dos bancos

- Padronização de variáveis
- Deduplicação probabilística
- Blocking e estimação via EM
- Extração de pares com base em probabilidades posteriores

2.  Processamento epidemiológico

- Banco de dados limpo e padronizado
- Variáveis derivadas
- Classificação de óbitos

3.  Geração de relatórios

- Tabelas epidemiológicas padronizadas
- Indicadores de mortalidade e letalidade
- Carga de doença (APVP, AVCI, DALY)
- Gráficos temporais e por causa
- Exportação automática para PDF

### Exemplo

``` r
library(dataLGBT)

# 1. Linkage
link <- data_link(df_sinan, df_sim)

# 2. Processamento
proc <- data_proc(link)

# 3. Gerar relatório
report <- gen_report(proc, export = TRUE)
```

## Aplicação

O pacote foi desenvolvido para análises relacionadas à mortalidade de
pessoas LGBT, podendo ser utilizado em:

- Vigilância epidemiológica  
- Pesquisa em saúde pública  
- Monitoramento de indicadores de violência

## Notas Metodológicas

### Vinculação probabilística

A vinculação é baseada no modelo probabilístico de Fellegi-Sunter,
operacionalizado por meio do pacote fastLink:

- Estimação de parâmetros via algoritmo EM
- Comparação de strings para nomes
- Tratamento de concordância parcial
- Uso de probabilidades posteriores para definição de pares

### Fonte de dados

O pacote foi projetado para trabalhar com dados públicos do DATASUS,
sendo diretamente os sistemas:

- Sistema de Informação sobre Mortalidade (SIM)  
- Sistema de Informação de Agravos de Notificação (SINAN)

### Estrutura do relatório gerado

O relatório inclui:

*Tabela 1:* Caracterização dos casos

*Tabela 2:* Indicadores por população SGM

*Tabela 3:* Carga de doença (APVP, AVCI, DALY)

*Tabela 4:* Distribuição das causas de mortalidade

*Gráficos:*

- Óbitos ao longo do tempo
- Casos ao longo do tempo
- Letalidade por grupo SGM
- Distribuição de causas (CID-10)

*Principais achados* (gerados automaticamente)

## Como citar

Caso utilize este pacote, recomenda-se citar:

OLIVEIRA, Natan Nascimento de. dataLGBT: Ferramentas para análise de
mortalidade de pessoas LGBT no Brasil. \[R package\].

## Dúvidas e sugestões

Para reportar problemas ou sugerir melhorias, utilize a aba *Issues* no
repositório do GitHub.

![R](https://img.shields.io/badge/language-R-blue)
![status](https://img.shields.io/badge/status-development-yellow)
