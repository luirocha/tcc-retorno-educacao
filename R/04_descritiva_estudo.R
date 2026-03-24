# ==============================================================================
# SCRIPT: 04_descritiva_estudo.R
# PROJETO: Evolução da taxa de retorno à educação em capitais brasileiras
#          via modelos lineares
# DADOS: PNAD Contínua — 2º trimestre de 2016, 2019, 2022 e 2025
# DESCRIÇÃO: Análise descritiva da relação entre educação e rendimento.
#            Três visualizações: (1) alfabetização vs. rendimento médio,
#            (2) nível de escolaridade vs. rendimento médio,
#            (3) anos de estudo vs. rendimento médio (curva por região/ano).
# AUTORA: Luí Rocha
# DATA: 2025
# DEPENDÊNCIAS: data/processed/pnadc_descritiva.rds
# SAÍDA: output/figures/04_alfabetizacao_rendimento.png
#        output/figures/04_escolaridade_rendimento.png
#        output/figures/04_anos_estudo_rendimento.png
# PACOTES: dplyr, tidyr, ggplot2, scales, stringr
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. PACOTES E LEITURA
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)

dadosPNADc <- readRDS("data/processed/pnadc_descritiva.rds")

if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)

# Paleta de cores consistente para as regiões — usada em todos os scripts
# de visualização para garantir identidade visual uniforme ao longo do TCC.
cores_regioes <- c(
  "Norte"       = "#7CAF5C",
  "Nordeste"    = "#E8954C",
  "Centro-Oeste"= "#E8D84C",
  "Sudeste"     = "#C87CC8",
  "Sul"         = "#7C7CCC"
)

# Tema base: aplicado a todos os gráficos para garantir padronização visual.
# Centralizar o tema evita repetição de código e facilita ajustes futuros.
tema_tcc <- theme_minimal(base_size = 16) +
  theme(
    axis.title.x     = element_text(face = "bold", size = 16, margin = margin(t = 15)),
    axis.title.y     = element_text(face = "bold", size = 16, margin = margin(r = 15)),
    axis.text.x      = element_text(size = 14, face = "bold"),
    axis.text.y      = element_text(size = 14),
    legend.title     = element_text(face = "bold", size = 15),
    legend.text      = element_text(size = 14),
    legend.position  = "right",
    legend.key.size  = unit(1.2, "cm"),
    strip.text       = element_text(face = "bold", size = 15),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85", linewidth = 0.5)
  )

# Formação do eixo Y em R$/hora — padrão BRL
formato_reais <- scales::number_format(prefix = "R$ ", decimal.mark = ",", big.mark = ".")

# ------------------------------------------------------------------------------
# 2. GRÁFICO 1: ALFABETIZAÇÃO × RENDIMENTO MÉDIO
# Motivação: antes de explorar anos de estudo, verificar se a fronteira básica
# de literacia (saber ler e escrever) já produz diferencial de rendimento,
# e se esse diferencial varia entre regiões ao longo do tempo.
# ------------------------------------------------------------------------------

dados_alfa <- dadosPNADc |>
  filter(!is.na(V3001), !is.na(Regiao), !is.na(Rendimento), Rendimento > 0) |>
  group_by(Ano, Regiao, V3001) |>
  summarise(Rendimento_Medio = mean(Rendimento, na.rm = TRUE), .groups = "drop")

g_alfabetizacao <- ggplot(dados_alfa,
                          aes(x = V3001, y = Rendimento_Medio, fill = Regiao)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~Ano, ncol = 2) +
  labs(
    x    = "Sabe ler e escrever?",
    y    = "Rendimento Médio (R$/hora)",
    fill = "Região"
  ) +
  scale_fill_manual(values = cores_regioes) +
  scale_y_continuous(labels = formato_reais) +
  tema_tcc

ggsave("output/figures/04_alfabetizacao_rendimento.png",
       g_alfabetizacao, width = 12, height = 7, dpi = 300)
cat("✔ Gráfico salvo: output/figures/04_alfabetizacao_rendimento.png\n")

# ------------------------------------------------------------------------------
# 3. GRÁFICO 2: NÍVEL DE ESCOLARIDADE × RENDIMENTO MÉDIO
# Este gráfico é central para a motivação descritiva do TCC: mostra que o
# gradiente rendimento–escolaridade existe, é substancial e persiste ao longo
# do tempo, justificando a estimação formal dos retornos via OLS.
# ------------------------------------------------------------------------------

dados_esc <- dadosPNADc |>
  filter(!is.na(Escolaridade), !is.na(Regiao), !is.na(Rendimento), Rendimento > 0) |>
  group_by(Ano, Regiao, Escolaridade) |>
  summarise(Rendimento_Medio = mean(Rendimento, na.rm = TRUE), .groups = "drop")

g_escolaridade <- ggplot(dados_esc,
                         aes(x = Escolaridade, y = Rendimento_Medio, fill = Regiao)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~Ano, ncol = 2) +
  labs(
    x    = "Nível de Escolaridade",
    y    = "Rendimento Médio (R$/hora)",
    fill = "Região"
  ) +
  # str_wrap() quebra os labels longos do eixo X para evitar sobreposição
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  scale_fill_manual(values = cores_regioes) +
  scale_y_continuous(labels = formato_reais) +
  tema_tcc +
  theme(axis.text.x = element_text(size = 13, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        strip.text   = element_text(size = 18))

ggsave("output/figures/04_escolaridade_rendimento.png",
       g_escolaridade, width = 18, height = 10, dpi = 300)
cat("✔ Gráfico salvo: output/figures/04_escolaridade_rendimento.png\n")

# ------------------------------------------------------------------------------
# 4. GRÁFICO 3: ANOS DE ESTUDO × RENDIMENTO MÉDIO (curva de rendimento)
# Aqui a escolaridade é tratada como variável contínua (0–16 anos).
# O gráfico revela a forma funcional da relação — se é linear, convexa ou
# apresenta descontinuidades (ex.: efeito de diploma em 11 e 16 anos).
# O filtro N >= 30 garante que células com amostras muito pequenas não
# distorçam as curvas.
# ------------------------------------------------------------------------------

dados_anos <- dadosPNADc |>
  filter(!is.na(anos_estudo_num), !is.na(Regiao), !is.na(Rendimento), Rendimento > 0) |>
  group_by(Ano, Regiao, anos_estudo_num) |>
  summarise(Rendimento_Medio = mean(Rendimento, na.rm = TRUE),
            N = n(), .groups = "drop") |>
  filter(N >= 30)

g_anos_estudo <- ggplot(dados_anos,
                        aes(x = anos_estudo_num, y = Rendimento_Medio,
                            color = Regiao, group = Regiao)) +
  geom_line(linewidth = 2.5, alpha = 0.9) +
  geom_point(size = 4.5, alpha = 0.8) +
  facet_wrap(~Ano, ncol = 2) +
  labs(
    x     = "Anos de Estudo",
    y     = "Rendimento Médio (R$/hora)",
    color = "Região"
  ) +
  scale_color_manual(values = cores_regioes) +
  scale_x_continuous(breaks = seq(0, 16, 4)) +
  scale_y_continuous(labels = formato_reais) +
  tema_tcc +
  theme(
    panel.grid.major.x = element_line(color = "gray85", linewidth = 0.5),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    strip.text   = element_text(size = 18)
  )

ggsave("output/figures/04_anos_estudo_rendimento.png",
       g_anos_estudo, width = 16, height = 12, dpi = 300)
cat("✔ Gráfico salvo: output/figures/04_anos_estudo_rendimento.png\n")
