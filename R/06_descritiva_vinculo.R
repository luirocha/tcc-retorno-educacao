# ==============================================================================
# SCRIPT: 06_descritiva_vinculo.R
# PROJETO: Evolução da taxa de retorno à educação em capitais brasileiras
#          via modelos lineares
# DADOS: PNAD Contínua — 2º trimestre de 2016, 2019, 2022 e 2025
# DESCRIÇÃO: Análise descritiva da relação entre vínculo empregatício e
#            rendimento: (1) carteira assinada e (2) contribuição para
#            previdência, desagregados por região e ano. Inclui também
#            tabela de rendimento médio por formalidade × previdência.
# AUTORA: Luí Rocha
# DATA: 2025
# DEPENDÊNCIAS: data/processed/pnadc_descritiva.rds
# SAÍDA: output/figures/06_carteira_rendimento.png
#        output/figures/06_previdencia_rendimento.png
#        output/tables/06_formalidade_previdencia.csv
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
    axis.title.x       = element_text(face = "bold", size = 16, margin = margin(t = 15)),
    axis.title.y       = element_text(face = "bold", size = 16, margin = margin(r = 15)),
    axis.text.x        = element_text(size = 14, face = "bold"),
    axis.text.y        = element_text(size = 14),
    legend.title       = element_text(face = "bold", size = 15),
    legend.text        = element_text(size = 14),
    legend.position    = "right",
    legend.key.size    = unit(1.2, "cm"),
    strip.text         = element_text(face = "bold", size = 15),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85", linewidth = 0.5)
  )

formato_reais <- scales::number_format(prefix = "R$ ", decimal.mark = ",", big.mark = ".")

# ------------------------------------------------------------------------------
# 2. GRÁFICO 1: CARTEIRA ASSINADA × RENDIMENTO MÉDIO
# V4029 indica se o trabalhador assalariado possui carteira assinada.
# É uma proxy de proteção trabalhista e complementa a variável Formalidade
# (que cobre também autônomos e servidores).
# ------------------------------------------------------------------------------

dados_carteira <- dadosPNADc |>
  filter(!is.na(V4029), !is.na(Regiao), !is.na(Rendimento), Rendimento > 0) |>
  group_by(Ano, Regiao, V4029) |>
  summarise(Rendimento_Medio = mean(Rendimento, na.rm = TRUE),
            N = n(), .groups = "drop")

g_carteira <- ggplot(dados_carteira,
                     aes(x = V4029, y = Rendimento_Medio, fill = Regiao)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~Ano, ncol = 2) +
  labs(
    x    = "Possui Carteira Assinada?",
    y    = "Rendimento Médio (R$/hora)",
    fill = "Região"
  ) +
  scale_fill_manual(values = cores_regioes) +
  scale_y_continuous(labels = formato_reais) +
  tema_tcc

ggsave("output/figures/06_carteira_rendimento.png",
       g_carteira, width = 12, height = 7, dpi = 300)
cat("✔ Gráfico salvo: output/figures/06_carteira_rendimento.png\n")

# ------------------------------------------------------------------------------
# 3. GRÁFICO 2: CONTRIBUIÇÃO PARA PREVIDÊNCIA × RENDIMENTO MÉDIO
# VD4012 registra se o trabalhador contribui para algum regime previdenciário.
# Trabalhadores que contribuem tendem a ter empregos de melhor qualidade,
# o que se reflete no rendimento — mesmo dentro do setor informal.
# ------------------------------------------------------------------------------

dados_prev <- dadosPNADc |>
  filter(!is.na(VD4012), !is.na(Regiao), !is.na(Rendimento), Rendimento > 0) |>
  group_by(Ano, Regiao, VD4012) |>
  summarise(Rendimento_Medio = mean(Rendimento, na.rm = TRUE),
            N = n(), .groups = "drop")

g_previdencia <- ggplot(dados_prev,
                        aes(x = VD4012, y = Rendimento_Medio, fill = Regiao)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~Ano, ncol = 2) +
  labs(
    x    = "Contribui para Previdência?",
    y    = "Rendimento Médio (R$/hora)",
    fill = "Região"
  ) +
  scale_fill_manual(values = cores_regioes) +
  scale_y_continuous(labels = formato_reais) +
  tema_tcc

ggsave("output/figures/06_previdencia_rendimento.png",
       g_previdencia, width = 12, height = 7, dpi = 300)
cat("✔ Gráfico salvo: output/figures/06_previdencia_rendimento.png\n")

# ------------------------------------------------------------------------------
# 4. TABELA: RENDIMENTO MÉDIO POR FORMALIDADE × CONTRIBUIÇÃO PREVIDÊNCIA
# Cruzamento que revela a heterogeneidade dentro dos grupos formal e informal:
# trabalhadores informais que contribuem para a previdência têm rendimento
# sistematicamente distinto dos que não contribuem.
# ------------------------------------------------------------------------------

cat("\n=== TABELA: RENDIMENTO MÉDIO POR REGIÃO, FORMALIDADE E PREVIDÊNCIA ===\n\n")

tab_formalidade_prev <- dadosPNADc |>
  filter(!is.na(Formalidade), !is.na(VD4012), !is.na(Regiao),
         !is.na(Rendimento), Rendimento > 0) |>
  group_by(Regiao, Formalidade, VD4012) |>
  summarise(N = n(),
            Rendimento_Medio = round(mean(Rendimento, na.rm = TRUE), 2),
            .groups = "drop") |>
  group_by(Regiao) |>
  mutate(Percentual = round((N / sum(N)) * 100, 1)) |>
  ungroup() |>
  arrange(Regiao, Formalidade, VD4012) |>
  select(Regiao, Formalidade, VD4012, N, Percentual, Rendimento_Medio)

print(kable(tab_formalidade_prev,
            digits = 1,
            format = "simple",
            col.names = c("Região", "Formalidade", "Contribui Prev.",
                          "N", "% (dentro da região)", "Rend. Médio (R$/h)")))

write.csv(tab_formalidade_prev,
          "output/tables/06_formalidade_previdencia.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8")
cat("✔ Tabela salva: output/tables/06_formalidade_previdencia.csv\n")
