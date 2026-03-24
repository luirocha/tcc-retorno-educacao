# ==============================================================================
# SCRIPT: 07_descritiva_renda.R
# PROJETO: Evolução da taxa de retorno à educação em capitais brasileiras
#          via modelos lineares
# DADOS: PNAD Contínua — 2º trimestre de 2016, 2019, 2022 e 2025
# DESCRIÇÃO: Análise descritiva da distribuição do rendimento horário:
#            (1) boxplot por região e ano e (2) tabela cruzada de rendimento
#            médio por formalidade e contribuição previdenciária por região.
# AUTORA: Luí Rocha
# DATA: 2025
# DEPENDÊNCIAS: data/processed/pnadc_descritiva.rds
# SAÍDA: output/figures/07_boxplot_rendimento.png
#        output/tables/07_tabela_rendimento.csv
# PACOTES: dplyr, tidyr, ggplot2, scales, knitr, kableExtra
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. PACOTES E LEITURA
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(knitr)
library(kableExtra)

dadosPNADc <- readRDS("data/processed/pnadc_descritiva.rds")

if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)
if (!dir.exists("output/tables"))  dir.create("output/tables",  recursive = TRUE)

cores_regioes <- c(
  "Norte"        = "#7CAF5C",
  "Nordeste"     = "#E8954C",
  "Centro-Oeste" = "#E8D84C",
  "Sudeste"      = "#C87CC8",
  "Sul"          = "#7C7CCC"
)

tema_tcc <- theme_minimal(base_size = 16) +
  theme(
    axis.title.x       = element_text(face = "bold", size = 18, margin = margin(t = 15)),
    axis.title.y       = element_text(face = "bold", size = 18, margin = margin(r = 15)),
    axis.text.x        = element_text(size = 15, face = "bold"),
    axis.text.y        = element_text(size = 15),
    legend.title       = element_text(face = "bold", size = 16),
    legend.text        = element_text(size = 15),
    legend.position    = "right",
    legend.key.size    = unit(1.2, "cm"),
    strip.text         = element_text(face = "bold", size = 17),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85", linewidth = 0.5)
  )

formato_reais <- scales::number_format(prefix = "R$ ", decimal.mark = ",", big.mark = ".")

# ------------------------------------------------------------------------------
# 2. GRÁFICO 1: BOXPLOT DE RENDIMENTO HORÁRIO POR REGIÃO E ANO
#
# O filtro Rendimento <= 65 remove outliers extremos (menos de 1% da amostra)
# que, se mantidos, comprimiriam a escala e tornariam o boxplot ilegível.
# A escolha de 65 R$/hora corresponde aproximadamente ao percentil 99 da
# distribuição e é documentada como nota no TCC.
#
# O boxplot é preferível ao gráfico de barras para distribuições assimétricas
# como rendimento, pois expõe mediana, dispersão e assimetria simultaneamente.
# ------------------------------------------------------------------------------

dados_boxplot <- dadosPNADc |>
  filter(!is.na(Rendimento), Rendimento > 0, Rendimento <= 65, !is.na(Regiao))

g_boxplot <- ggplot(dados_boxplot,
                    aes(x = Regiao, y = Rendimento, fill = Regiao)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  facet_wrap(~Ano, ncol = 2) +
  labs(
    x    = "Região",
    y    = "Rendimento (R$/hora)",
    fill = "Região",
    caption = "Nota: outliers acima de R$ 65/hora omitidos para legibilidade (~1% da amostra)."
  ) +
  scale_fill_manual(values = cores_regioes) +
  scale_y_continuous(labels = formato_reais) +
  tema_tcc +
  theme(plot.caption = element_text(size = 12, hjust = 0.5, face = "italic"))

ggsave("output/figures/07_boxplot_rendimento.png",
       g_boxplot, width = 18, height = 10, dpi = 300)
cat("✔ Gráfico salvo: output/figures/07_boxplot_rendimento.png\n")

# ------------------------------------------------------------------------------
# 3. TABELA: RENDIMENTO MÉDIO, MEDIANO E DESVIO PADRÃO POR REGIÃO E ANO
# Complementa o boxplot com estatísticas numéricas precisas.
# A inclusão da mediana é importante porque o rendimento é assimétrico à direita
# (a média é sistematicamente mais alta que a mediana).
# ------------------------------------------------------------------------------

cat("\n=== TABELA: ESTATÍSTICAS DESCRITIVAS DO RENDIMENTO (R$/hora) ===\n\n")

tab_renda <- dadosPNADc |>
  filter(!is.na(Rendimento), Rendimento > 0, !is.na(Regiao)) |>
  group_by(Ano, Regiao) |>
  summarise(
    N           = n(),
    Media       = round(mean(Rendimento,   na.rm = TRUE), 2),
    Mediana     = round(median(Rendimento, na.rm = TRUE), 2),
    DP          = round(sd(Rendimento,     na.rm = TRUE), 2),
    P10         = round(quantile(Rendimento, 0.10, na.rm = TRUE), 2),
    P90         = round(quantile(Rendimento, 0.90, na.rm = TRUE), 2),
    .groups     = "drop"
  ) |>
  arrange(Ano, Regiao)

print(kable(tab_renda,
            digits = 2,
            format = "simple",
            col.names = c("Ano", "Região", "N",
                          "Média", "Mediana", "Desvio Padrão",
                          "P10 (R$/h)", "P90 (R$/h)")))

write.csv(tab_renda,
          "output/tables/07_tabela_rendimento.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
cat("✔ Tabela salva: output/tables/07_tabela_rendimento.csv\n")
