## ----setup, include=FALSE-----------------------------------------------------
#library(BrazilCrime)
#library(ggplot2)
#library(kableExtra)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----exemplo 1, tidy=TRUE, out.width='\\textwidth', fig.width=6, fig.height=4, results='asis'----
dados <- BrazilCrime::get_sinesp_vde_data()

kableExtra::kable(head(dados), format = "latex", booktabs = TRUE, caption = "Visualização dos primeiros registros") |>
  kableExtra::kable_styling(latex_options = c("scale_down", "hold_position"))

## ----exemplo2, tidy=TRUE, out.width='\\textwidth', fig.width=6, fig.height=4, results='asis'----
recife <- BrazilCrime::get_sinesp_vde_data(
  state = "PE",
  city = "Recife",
  typology = "Homicídio doloso",
  year = 2020:2022
)

kableExtra::kable(head(recife), format = "latex", booktabs = TRUE, caption = "Visualização dos primeiros registros") |>
  kableExtra::kable_styling(latex_options = c("scale_down", "hold_position"))

## ----exemplo3, tidy=TRUE, out.width='\\textwidth', fig.width=6, fig.height=4, results='asis'----
sp_anuais <- BrazilCrime::get_sinesp_vde_data(
  state = "SP",
  category = "ocorrencias",
  granularity = "year"
)


kableExtra::kable(head(sp_anuais), format = "latex", booktabs = TRUE, caption = "Visualização dos primeiros registros") |>
  kableExtra::kable_styling(latex_options = c("scale_down", "hold_position"))

## ----exemplo4, tidy=TRUE, out.width='\\textwidth', fig.width=6, fig.height=4, results='asis'----
#library(ggplot2)

ggplot2::ggplot(sp_anuais, ggplot2::aes(x = ano, y = total)) +
  ggplot2::geom_line(color = "#0072B2", size = 1.2) +
  ggplot2::geom_point(color = "#D55E00", size = 2) +
  ggplot2::facet_wrap(~evento) +
  ggplot2::labs(
    title = "Evolução anual de ocorrências no Estado de SP",
    y = "Total de ocorrências",
    x = "Ano"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 14),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
  )

## ----exemplo5, tidy=TRUE, out.width='\\textwidth', fig.width=6, fig.height=4, results='asis'----
#library(ggplot2)

sp_anuais_vit <- BrazilCrime::get_sinesp_vde_data(
  state = "SP",
  city = "SÃO PAULO",
  category = "vitimas",
  granularity = "year"
)

# Quebrar nomes longos nos títulos dos facetes
sp_anuais_vit$evento <- stringr::str_wrap(sp_anuais_vit$evento, width = 20)

ggplot2::ggplot(sp_anuais_vit, ggplot2::aes(x = ano, y = total_vitima)) +
  ggplot2::geom_line(color = "#0072B2", size = 1.2) +
  ggplot2::geom_point(color = "#D55E00", size = 2) +
  ggplot2::facet_wrap(~evento) +
  ggplot2::labs(
    title = "Evolução anual de vítimas na cidade de SP",
    y = "Total de vítimas",
    x = "Ano"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 14),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    strip.text = ggplot2::element_text(size = 8))

## ----exemplo6,tidy=TRUE, out.width='\\textwidth', fig.width=6, fig.height=4, results='asis'----
# Carregar dados
dados_var <- BrazilCrime::get_sinesp_vde_data(
  state = "BA",
  typology = "Homicídio doloso",
  city = "Salvador",
  category = "vitimas",
  granularity = "month",
  year = 2015:2023)

dados_var <- dados_var |>
  dplyr::mutate(data = as.Date(paste0(ano, "-", mes, "-01")))


# Carregar pacotes
#library(ggplot2)

# Criar gráfico com smoothing
ggplot2::ggplot(dados_var, ggplot2::aes(x = data)) +
  ggplot2::geom_line(ggplot2::aes(y = total_vitima, color = "Valores mensais"), size = 0.8) +
  ggplot2::geom_smooth(
    ggplot2::aes(y = total_vitima, color = "Tendência"),
    method = "loess",
    se = FALSE,
    size = 1.2,
    span = 0.3
  ) +
  ggplot2::scale_color_manual(
    name = "Série Temporal",
    values = c("Valores mensais" = "#0072B2", "Tendência" = "#D55E00")
  ) +
  ggplot2::labs(
    title = "Tendência de homicídios dolosos em Salvador - BA (2015–2023)",
    x = "Data",
    y = "Total de Vítimas"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", size = 10, hjust = 0.5),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = ggplot2::element_text(size = 9, face = "bold"),
    legend.text = ggplot2::element_text(size = 9)
  )



## ----exemplo7,tidy=TRUE, out.width='\\textwidth', fig.width=6, fig.height=4, results='asis'----
# Pacotes necessários
#library(BrazilCrime)
#library(tidyverse)
#library(ggcorrplot)

# Buscar dados para SP em 2022 (cross-section municipal)
dados_cross <- BrazilCrime::get_sinesp_vde_data(
  state = "SP",
  granularity = "year",
  year = 2022,
  category = "vitimas"
)

# Selecionar algumas tipologias comuns
tipos_desejados <- c("Feminicídio", "Homicídio doloso", "Suicídio",
                     "Lesão corporal seguida de morte")

# Agrupar e pivotar os dados corretamente
dados_filtrados <- dados_cross |>
  dplyr::filter(evento %in% tipos_desejados) |>
  dplyr::group_by(municipio, evento) |>
  dplyr::summarise(total_vitima = sum(total_vitima, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = evento, values_from = total_vitima) |>
  tidyr::drop_na() |>
  dplyr::mutate(across(-municipio, as.numeric))

# Calcular matriz de correlação
matriz_cor <- cor(dados_filtrados[-1], method = "pearson")

# Gerar gráfico de correlação
ggcorrplot::ggcorrplot(
  matriz_cor,
  lab = TRUE,
  lab_size = 3,
  colors = c("#D73027", "white", "#1A9850"),
  outline.color = "gray80"
) +
  ggplot2::ggtitle("Correlação entre diferentes crimes em SP (2022)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 14, hjust = 0.5),
    plot.margin = ggplot2::margin(t = 20, r = 10, b = 20, l = 10),
    axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1)
  )


