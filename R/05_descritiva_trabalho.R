# ==============================================================================
# SCRIPT: 05_descritiva_trabalho.R
# PROJETO: Evolução da taxa de retorno à educação em capitais brasileiras
#          via modelos lineares
# DADOS: PNAD Contínua — 2º trimestre de 2016, 2019, 2022 e 2025
# DESCRIÇÃO: Análise descritiva da relação entre características do trabalho
#            e rendimento: (1) formalidade e (2) grupamentos de atividade,
#            desagregados por região e ano.
# AUTORA: Luí Rocha
# DATA: 2025
# DEPENDÊNCIAS: data/processed/pnadc_descritiva.rds
# SAÍDA: output/figures/05_formalidade_rendimento.png
#        output/figures/05_atividades_rendimento.png
# PACOTES: dplyr, ggplot2, scales, stringr
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. PACOTES E LEITURA
# ------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

dadosPNADc <- readRDS("data/processed/pnadc_descritiva.rds")

if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)

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
# 2. GRÁFICO 1: FORMALIDADE × RENDIMENTO MÉDIO
# A distinção formal/informal é uma das variáveis de controle do modelo OLS
# (Formalidade). Este gráfico confirma que o diferencial de rendimento entre
# formais e informais persiste em todas as regiões e anos, justificando sua
# inclusão como variável de controle para isolar o retorno à educação.
# ------------------------------------------------------------------------------

dados_formalidade <- dadosPNADc |>
  filter(!is.na(Formalidade), !is.na(Regiao), !is.na(Rendimento), Rendimento > 0) |>
  group_by(Ano, Regiao, Formalidade) |>
  summarise(Rendimento_Medio = mean(Rendimento, na.rm = TRUE),
            N = n(), .groups = "drop")

g_formalidade <- ggplot(dados_formalidade,
                        aes(x = Formalidade, y = Rendimento_Medio, fill = Regiao)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~Ano, ncol = 2) +
  labs(
    x    = "Formalidade",
    y    = "Rendimento Médio (R$/hora)",
    fill = "Região"
  ) +
  scale_fill_manual(values = cores_regioes) +
  scale_y_continuous(labels = formato_reais) +
  tema_tcc

ggsave("output/figures/05_formalidade_rendimento.png",
       g_formalidade, width = 12, height = 7, dpi = 300)
cat("✔ Gráfico salvo: output/figures/05_formalidade_rendimento.png\n")

# ------------------------------------------------------------------------------
# 3. GRÁFICO 2: GRUPAMENTO DE ATIVIDADE × RENDIMENTO MÉDIO
# O gráfico usa reorder() para ordenar as atividades por rendimento médio
# dentro de cada faceta, facilitando a comparação visual entre grupos.
# O eixo é invertido com coord_flip() porque os labels de atividade são longos.
# str_wrap() com width = 35 quebra as linhas sem truncar o texto.
# ------------------------------------------------------------------------------

dados_atividades <- dadosPNADc |>
  filter(!is.na(Atividades), !is.na(Regiao), !is.na(Rendimento), Rendimento > 0) |>
  group_by(Ano, Regiao, Atividades) |>
  summarise(Rendimento_Medio = mean(Rendimento, na.rm = TRUE),
            N = n(), .groups = "drop")

g_atividades <- ggplot(dados_atividades,
                       aes(x = reorder(Atividades, Rendimento_Medio),
                           y = Rendimento_Medio,
                           fill = Regiao)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  facet_wrap(~Ano, ncol = 2) +
  labs(
    x    = "Grupamento de Atividade",
    y    = "Rendimento Médio (R$/hora)",
    fill = "Região"
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 35)) +
  scale_fill_manual(values = cores_regioes) +
  scale_y_continuous(labels = formato_reais) +
  tema_tcc +
  theme(
    axis.text.y        = element_text(size = 13, face = "bold"),
    axis.text.x        = element_text(size = 14),
    axis.title.x       = element_text(size = 20),
    axis.title.y       = element_text(size = 20),
    strip.text         = element_text(size = 18),
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.5),
    panel.grid.major.y = element_blank()
  )

ggsave("output/figures/05_atividades_rendimento.png",
       g_atividades, width = 18, height = 10, dpi = 300)
cat("✔ Gráfico salvo: output/figures/05_atividades_rendimento.png\n")
