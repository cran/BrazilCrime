## ----setup, include=FALSE-----------------------------------------------------
#library(BrazilCrime)
#library(forecast)
#library(ggplot2)
#library(lubridate)
#library(bookdown)

knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----exemplo 1, tidy=TRUE, out.width='\\textwidth', fig.width=6, fig.height=4, results='asis'----
dados <- BrazilCrime::get_sinesp_vde_data(
  state = "PE",
  city = "Recife",
  typology = "Homicídio doloso",
  category = "vitimas",
  granularity = "month",
  year = 2015:2023
)

# Criar coluna de data (YYYY-MM)
dados <- dados|>
  dplyr::mutate(data = lubridate::ymd(paste0(ano, "-", mes, "-01"))) |>
  dplyr::arrange(data)

# Rodar previsão
BrazilCrime::br_crime_predict(dados = dados,ts_col = "total_vitima", log = TRUE)

