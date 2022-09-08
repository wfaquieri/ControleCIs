
# ControleCIs

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

O objetivo do app ControleCIs é acrescentar à planilha de controle
existente, a nova demanda recebida, sinalizando duplicidades, quando
houver.

Resumo: Planilha de controle atualizada e mensagem, caso seja
necessário, sinalizando duplicidade de solicitação de elementar
(verificar se na planilha de controle o elementar já havia sido
solicitado e retornar o número da CI onde ele foi solicitado – coluna A:
Origem). Caso a coluna de elementar esteja vazia, fazer essa verificação
através do Item externo e, em casos em que ambos estejam vazias,
sinalizar que a planilha não tem o elementar e o externo.

## Instalação

Você pode instalar a versão de desenvolvimento do ControleCIs assim:

``` r
devtools::install_github('wfaquieri/ControleCIs')
```

## Deploy (Shiny apps)

A ferramenta também pode ser acessada diretamente:
<https://fgv-ibre.shinyapps.io/ControleCIs/>

## Demonstração

![](demo.gif) 

## DESCRIÇÃO

Package: ControleCIs

Title: App de controle de CIs.

Version: 0.0.0.9000

<Authors@R>: person(“winicius”, “faquieri”, ,
“<winicius.faquieri@fgv.com.br>”, role = c(“aut”, “cre”))

Description: o app ControleCIs PERMITE acrescentar à planilha de
controle existente, a nova demanda recebida, sinalizando duplicidades,
quando houver.

License: MIT + file LICENSE

Depends: R (\>= 2.10) Imports: bs4Dash, bslib, config (\>= 0.3.1),
dplyr, DT, golem (\>= 0.3.3), janitor, purrr, readxl, shiny (\>= 1.7.2),
shinyWidgets, stringr

Config/testthat/edition: 3

Encoding: UTF-8

LazyData: true

RoxygenNote: 7.2.1
