# atlas_amaior

O objetivo deste repositório é disponibilizar os códigos e scripts em R utilizados na elaboração do artigo:

WONG, Laura Lídia Rodríguez; GIVISIEZ, Gustavo Henrique Naves; OLIVEIRA, Elzira Lucia de. A maior mudança demográfica na história moderna da humanidade: Brasil e o mundo. *In*: SANTOS, L. B.; OLIVEIRA, E. L. de; CINTRA, D. P. (org.). **O regional na interface da pesquisa e da extensão: Indicadores, métodos e ferramentas**. Jundiaí, SP: Paco Editorial, 2025. p. 11–46.

A iniciativa busca fortalecer a transparência metodológica e garantir a reprodutibilidade dos resultados apresentados, além de facilitar o uso, a adaptação e o aprimoramento dos procedimentos por outros pesquisadores interessados nas temáticas abordadas.

**Aviso:** Este repositório utiliza dados públicos da Organização das Nações Unidas (ONU) e do Instituto Brasileiro de Geografia e Estatística (IBGE), conforme permitido para uso não comercial. Os dados brutos não são redistribuídos aqui; o acesso é realizado diretamente pelas APIs oficiais. Para consulta às fontes originais, visite:

• <https://population.un.org/wpp/>

• <https://sidra.ibge.gov.br>

### Codificação

O script principal é R/chapter_figures.R, responsável por executar todo o processamento das figuras apresentadas no artigo. Para sua utilização, é necessário configurar previamente no ambiente R o *token* de acesso à API do **UN Data Portal** ([https://population.un.org/dataportal/about/dataapi)](https://population.un.org/dataportal/about/dataapi). O scrip cria uma banco de dados SQLite e o popula com dados da projeção da ONU. Os dados do IBGE são baixados diretamente do API do SIDRA.

#### Bibliotecas utlizadas

```{r eval=FALSE}
library(here)
library(dplyr)
library(ggplot2)
library(tibble)
library(ggsci)
library(readr)
library(stringr)
library(tidyr)
library(patchwork)
library(jsonlite)
library(httr)
library(readxl)
library(snakecase)
library(CGPfunctions)
library(ggrepel)
library(DBI)
library(RSQLite)
library(stringi) #Opcional
```
