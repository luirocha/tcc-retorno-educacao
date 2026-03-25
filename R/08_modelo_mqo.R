# ==============================================================================
# SCRIPT: 08_modelo_mqo.R
# PROJETO: Evolução da taxa de retorno à educação em capitais brasileiras
#          via modelos lineares
# DADOS: PNAD Contínua — 2º trimestre de 2016, 2019, 2022 e 2025
# DESCRIÇÃO: Estimação dos modelos de Mincer (log-salário) via MQO para cada
#            ano. Inclui: (1) preparação e separação por ano, (2) ajuste dos
#            modelos, (3) coeficientes de determinação, (4) análise gráfica de
#            resíduos, (5) testes de Breusch-Pagan e Lilliefors, (6) tabela
#            completa de estimativas com IC95% e (7) gráfico de retornos
#            cumulativos da escolaridade com bandas de confiança,
#            (8) mapas coropléticos dos diferenciais regionais por capital.
# AUTORA: Luí Rocha
# DATA: 2025
# DEPENDÊNCIAS: data/processed/pnadc_modelo.rds
# SAÍDA: output/figures/08_residuos_XXXX.png  (um por ano)
#        output/figures/08_retornos_escolaridade.png
#        output/figures/08_mapa_diferenciais.png
#        output/tables/08_coeficientes_completos.csv
# PACOTES: tidyverse, lmtest, nortest, broom, geobr, sf, patchwork
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. PACOTES E LEITURA
# ------------------------------------------------------------------------------

library(tidyverse)
library(lmtest)    # bptest(): teste de Breusch-Pagan para homocedasticidade
library(nortest)   # lillie.test(): teste de Lilliefors para normalidade
library(broom)     # tidy(): extrai coeficientes em formato data frame
library(geobr)     # shapefiles do IBGE para os mapas
library(sf)        # manipulação de geometrias espaciais
library(patchwork) # composição de múltiplos ggplots

dadosPNADc_model <- readRDS("data/processed/pnadc_modelo.rds")

if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)
if (!dir.exists("output/tables"))  dir.create("output/tables",  recursive = TRUE)

# ------------------------------------------------------------------------------
# 2. SEPARAÇÃO POR ANO E CRIAÇÃO DAS VARIÁVEIS DO MODELO
#
# O modelo é ajustado separadamente para cada ano (2016, 2019, 2022, 2025)
# em vez de usar um modelo pooled com interações. Essa escolha é metodológica:
# permite que TODOS os coeficientes variem livremente entre os anos, o que é
# o objeto central do TCC — observar a evolução das taxas de retorno ao longo
# do tempo. Um modelo pooled restringiria essa flexibilidade.
#
# ln_rendimento = log do rendimento por hora (variável dependente)
# idade_quad    = idade² — captura a concavidade da curva de rendimento-idade
#                 (rendimento cresce com experiência mas a taxa diminui)
# ------------------------------------------------------------------------------

anos <- c(2016, 2019, 2022, 2025)

# Cria uma lista nomeada com um data frame por ano — facilita iteração
dados_por_ano <- lapply(anos, function(a) {
  dadosPNADc_model |>
    filter(Ano == as.character(a)) |>
    mutate(
      ln_rendimento = log(Rendimento),
      idade_quad    = V2009^2
    )
})
names(dados_por_ano) <- as.character(anos)

# ------------------------------------------------------------------------------
# 3. ESPECIFICAÇÃO E AJUSTE DOS MODELOS
#
# Equação de Mincer estendida:
#   ln(w) = β₀ + β₁·S + β₂·idade + β₃·idade² + β₄·sexo + β₅·raça
#           + β₆·chefe + β₇·formalidade + β₈·previdência
#           + β₉·atividade + β₁₀·capital + ε
#
# onde S = anos de estudo (especificado como factor discreto 0–16 para
# capturar não-linearidades e o chamado "efeito diploma").
#
# Capital entra como conjunto de dummies com Brasília como referência,
# permitindo estimar o prêmio/desconto salarial de cada capital em relação
# ao Distrito Federal, controlando por todas as demais variáveis.
# ------------------------------------------------------------------------------

formula_modelo <- ln_rendimento ~ anos_estudo_num + V2009 + idade_quad +
  V2007 + V2010 + Chefe + Formalidade + VD4012 + Atividades + Capital

modelos <- lapply(dados_por_ano, function(d) lm(formula_modelo, data = d))
names(modelos) <- as.character(anos)

# Exibir sumários (útil durante a análise, pode ser comentado na versão final)
for (a in anos) {
  cat("\n", strrep("=", 60), "\n")
  cat("MODELO", a, "\n")
  cat(strrep("=", 60), "\n")
  print(summary(modelos[[as.character(a)]]))
}

# ------------------------------------------------------------------------------
# 4. COEFICIENTES DE DETERMINAÇÃO
# R² e R² ajustado para os quatro modelos. O R² ajustado é mais adequado
# para comparação entre modelos com diferentes números de parâmetros.
# ------------------------------------------------------------------------------

r2_tab <- data.frame(
  Ano          = anos,
  R2           = sapply(modelos, function(m) round(summary(m)$r.squared,     4)),
  R2_Ajustado  = sapply(modelos, function(m) round(summary(m)$adj.r.squared, 4))
)

cat("\n=== COEFICIENTES DE DETERMINAÇÃO ===\n")
print(r2_tab)

# ------------------------------------------------------------------------------
# 5. GRÁFICOS DE DIAGNÓSTICO DE RESÍDUOS
#
# Três painéis por modelo:
#   (a) Resíduos padronizados vs. valores ajustados — verifica homocedasticidade
#       e linearidade. Idealmente: nuvem aleatória centrada em zero.
#   (b) QQ-plot dos resíduos — verifica normalidade. Desvios nas caudas são
#       esperados e, com N grande, não invalidam o MQO pelo TLC.
#   (c) Histograma dos resíduos — complementa o QQ-plot visualmente.
#
# Com amostras de N >> 10.000, desvios moderados de normalidade e
# homocedasticidade são esperados e tratados com erros-padrão robustos
# (discutido na seção de robustez do TCC).
# ------------------------------------------------------------------------------

graficos_residuos <- function(modelo, ano) {
  residuos_pad    <- rstandard(modelo)
  valores_aj      <- fitted(modelo)

  # Abre um dispositivo PNG diretamente (base graphics — mais simples para
  # painéis de diagnóstico do que ggplot2)
  png(filename = paste0("output/figures/08_residuos_", ano, ".png"),
      width = 1600, height = 1000, res = 150)

  par(mfrow  = c(1, 3),
      mar    = c(4, 4, 3, 2),
      cex.lab= 1.3, cex.axis = 1.2,
      font.lab = 2, font.axis = 2)

  # Painel (a): resíduos vs. ajustados
  plot(valores_aj, residuos_pad,
       xlab = "Valores Ajustados",
       ylab = "Resíduos Padronizados",
       pch  = 20, col = "black", cex = 0.6)
  abline(h = 0, col = "red", lwd = 2, lty = 2)
  title(main = paste("(a) Resíduos vs. Ajustados —", ano), cex.main = 1.2)

  # Painel (b): QQ-plot
  qqnorm(residuos_pad,
         xlab = "Quantis Teóricos",
         ylab = "Quantis dos Resíduos Padronizados",
         pch  = 20, col = "black", cex = 0.6,
         main = "")
  qqline(residuos_pad, col = "red", lwd = 2)
  title(main = "(b) QQ-Plot dos Resíduos", cex.main = 1.2)

  # Painel (c): histograma
  hist(residuos_pad,
       xlab   = "Resíduos Padronizados",
       ylab   = "Frequência",
       col    = "black",
       border = "white",
       breaks = 40,
       main   = "")
  title(main = "(c) Histograma dos Resíduos", cex.main = 1.2)

  dev.off()
  par(mfrow = c(1, 1))
  cat("✔ Gráfico de resíduos salvo: output/figures/08_residuos_", ano, ".png\n", sep = "")
}

for (a in anos) graficos_residuos(modelos[[as.character(a)]], a)

# ------------------------------------------------------------------------------
# 6. TESTES DE DIAGNÓSTICO FORMAIS
#
# Breusch-Pagan (bptest): H₀ = homocedasticidade.
#   Com N grande, é comum rejeitar H₀ mesmo com heterocedasticidade leve.
#   A rejeição motiva o uso de erros-padrão robustos (HC) na apresentação
#   dos resultados, ou ao menos sua menção como análise de sensibilidade.
#
# Lilliefors (lillie.test): H₀ = normalidade dos resíduos.
#   Com N >> 1.000, o TLC garante distribuição assintótica normal dos
#   estimadores mesmo com rejeição de H₀ aqui. O resultado é reportado
#   para completude metodológica.
# ------------------------------------------------------------------------------

realizar_testes <- function(modelo, ano) {
  bp     <- bptest(modelo)
  lillie <- lillie.test(residuals(modelo))

  data.frame(
    Ano               = ano,
    BP_estatistica    = round(as.numeric(bp$statistic), 2),
    BP_pvalor         = round(bp$p.value, 6),
    BP_conclusao      = ifelse(bp$p.value < 0.05, "Heterocedasticidade", "Homocedasticidade"),
    Lillie_estatistica= round(as.numeric(lillie$statistic), 4),
    Lillie_pvalor     = round(lillie$p.value, 6),
    Lillie_conclusao  = ifelse(lillie$p.value < 0.05, "Não-normal", "Normal")
  )
}

resultados_testes <- bind_rows(lapply(anos, function(a) realizar_testes(modelos[[as.character(a)]], a)))

cat("\n=== TESTES DE DIAGNÓSTICO ===\n")
print(resultados_testes)

# ------------------------------------------------------------------------------
# 7. TABELA COMPLETA DE ESTIMATIVAS
#
# Para cada coeficiente β, calculamos exp(β) − 1 (coluna exp_c_menos_1_pct),
# que representa o efeito percentual APROXIMADO sobre o rendimento horário.
# Ex.: β = 0.08 → exp(0.08) − 1 ≈ 8.3% de aumento no rendimento por unidade
# da variável (quando a variável é binária, é o diferencial daquela categoria
# em relação à referência).
# Essa interpretação é padrão em modelos log-lineares de salário (Mincer, 1974).
# ------------------------------------------------------------------------------

criar_tabela_coef <- function(modelo, ano) {
  coef_ci  <- confint(modelo, level = 0.95)
  coef_sum <- summary(modelo)$coefficients

  data.frame(
    Ano               = ano,
    Variavel          = rownames(coef_sum),
    Coeficiente       = round(coef_sum[, "Estimate"],     4),
    DP                = round(coef_sum[, "Std. Error"],   4),
    Valor_t           = round(coef_sum[, "t value"],      4),
    Valor_p           = round(coef_sum[, "Pr(>|t|)"],     4),
    IC95_inf          = round(coef_ci[, 1],               4),
    IC95_sup          = round(coef_ci[, 2],               4),
    exp_c_menos_1_pct = round((exp(coef_sum[, "Estimate"]) - 1) * 100, 2),
    stringsAsFactors  = FALSE,
    row.names         = NULL
  )
}

tabelas_coef <- lapply(anos, function(a) criar_tabela_coef(modelos[[as.character(a)]], a))
names(tabelas_coef) <- as.character(anos)

tabela_completa <- bind_rows(tabelas_coef)

# Exibir no console
for (a in anos) {
  cat("\n=== COEFICIENTES", a, "===\n")
  print(tabelas_coef[[as.character(a)]])
}

# Salvar em CSV para uso no LaTeX (pode ser importado com readr ou knitr::kable)
write.csv(tabela_completa, "output/tables/08_coeficientes_completos.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
cat("\n✔ Tabela salva em: output/tables/08_coeficientes_completos.csv\n")

# ------------------------------------------------------------------------------
# 8. GRÁFICO DE RETORNOS CUMULATIVOS DA ESCOLARIDADE
#
# Este é o gráfico central do TCC. Para cada ano S ∈ {1, …, 16}, o coeficiente
# β_S do factor anos_estudo_num mede o diferencial salarial de ter S anos de
# estudo em relação a 0 anos (a categoria de referência). Convertido para
# percentual via (exp(β_S) − 1) × 100, obtemos o retorno cumulativo bruto.
#
# As bandas de confiança (IC 95%) são incluídas para evidenciar a precisão
# das estimativas — especialmente relevante nos anos extremos da distribuição
# de escolaridade, onde o N é menor.
#
# A evolução das curvas entre 2016 e 2025 é o achado empírico central do TCC.
# ------------------------------------------------------------------------------

dados_grafico_retorno <- tabela_completa |>
  filter(grepl("^anos_estudo_num", Variavel)) |>
  mutate(
    anos_estudo        = as.numeric(gsub("anos_estudo_num", "", Variavel)),
    IC_inf_pct         = (exp(IC95_inf) - 1) * 100,
    IC_sup_pct         = (exp(IC95_sup) - 1) * 100,
    efeito_pct         = exp_c_menos_1_pct,
    Ano                = as.character(Ano)
  ) |>
  select(Ano, anos_estudo, efeito_pct, IC_inf_pct, IC_sup_pct)

# Adiciona o ponto âncora (0 anos de estudo = 0% de retorno, por definição)
base_zero <- data.frame(
  Ano         = c("2016", "2019", "2022", "2025"),
  anos_estudo = 0, efeito_pct = 0, IC_inf_pct = 0, IC_sup_pct = 0
)

dados_grafico_retorno <- bind_rows(base_zero, dados_grafico_retorno) |>
  arrange(Ano, anos_estudo)

cores_anos <- c("2016" = "#5470C6", "2019" = "#91CC75",
                "2022" = "#FAC858", "2025" = "#EE6666")

g_retornos <- ggplot(dados_grafico_retorno,
                     aes(x = anos_estudo, y = efeito_pct,
                         color = Ano, fill = Ano, group = Ano)) +
  geom_ribbon(aes(ymin = IC_inf_pct, ymax = IC_sup_pct),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  labs(
    x     = "Anos de Estudo",
    y     = "Incremento Percentual na Renda (%)",
    color = "Ano",
    fill  = "Ano"
  ) +
  scale_x_continuous(breaks = seq(0, 16, 2)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_manual(values = cores_anos) +
  scale_fill_manual(values  = cores_anos) +
  theme_minimal(base_size = 16) +
  theme(
    axis.title.x     = element_text(face = "bold", size = 16, margin = margin(t = 15)),
    axis.title.y     = element_text(face = "bold", size = 16, margin = margin(r = 15)),
    axis.text.x      = element_text(size = 14, face = "bold"),
    axis.text.y      = element_text(size = 14),
    legend.title     = element_text(face = "bold", size = 15),
    legend.text      = element_text(size = 14),
    legend.position  = "right",
    legend.key.size  = unit(1.2, "cm"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.5)
  )

ggsave("output/figures/08_retornos_escolaridade.png",
       g_retornos, width = 12, height = 7, dpi = 300)
cat("✔ Gráfico salvo: output/figures/08_retornos_escolaridade.png\n")

# ------------------------------------------------------------------------------
# 9. MAPAS COROPLÉTICOS: DIFERENCIAL SALARIAL DAS CAPITAIS
#
# Os coeficientes das dummies de Capital estimam o prêmio (ou desconto)
# salarial de trabalhar em cada capital em relação a Brasília (DF), após
# controlar por educação, experiência, sexo, raça, formalidade e atividade.
# O mapa visualiza como esse diferencial evoluiu entre 2016 e 2025.
#
# Capitais com coeficiente negativo oferecem rendimentos sistematicamente
# menores que Brasília, mesmo para trabalhadores com o mesmo perfil observável.
# Isso reflete diferenças no nível de desenvolvimento do mercado de trabalho
# local que não são captadas pelas variáveis de controle disponíveis.
# ------------------------------------------------------------------------------

estados_br <- read_state(year = 2020, showProgress = FALSE)

# Tabela de correspondência: sigla UF do código IBGE → code_state numérico
# (necessária porque os labels da PNAD usam siglas com sufixo de capital)
mapa_siglas <- data.frame(
  padrao     = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
                 "MA", "MS", "MT", "MG", "PA", "PB", "PR", "PE", "PI",
                 "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
  code_state = c(12, 27, 16, 13, 29, 23, 53, 32, 52,
                 21, 50, 51, 31, 15, 25, 41, 26, 22,
                 33, 24, 43, 11, 14, 42, 35, 28, 17)
)

dados_mapa <- tabela_completa |>
  filter(grepl("^Capital", Variavel)) |>
  mutate(
    # Extrai a sigla do estado do label da capital, ex: "Capital(SP)" → "SP"
    padrao = str_extract(Variavel, "\\(([A-Z]{2})\\)") |> str_remove_all("[()]")
  ) |>
  left_join(mapa_siglas, by = "padrao") |>
  select(Ano, code_state, exp_c_menos_1_pct)

mapa_sf <- estados_br |>
  left_join(dados_mapa, by = "code_state")

# Cria um mapa por ano e os combina em grade 2×2
criar_mapa_ano <- function(ano_str, letra) {
  dados_ano <- mapa_sf |> filter(Ano == ano_str)

  ggplot(dados_ano) +
    geom_sf(aes(fill = exp_c_menos_1_pct), color = "white", linewidth = 0.1) +
    scale_fill_viridis_c(
      option   = "viridis",
      name     = "Efeito (%)",
      na.value = "grey90",
      limits   = c(-40, 10)
    ) +
    labs(caption = paste0("(", letra, ") Ano ", ano_str)) +
    theme_minimal(base_size = 16) +
    theme(
      axis.text.x = element_text(size = 15, face = "bold"),
      axis.text.y = element_text(size = 18),
      axis.title.x = element_text(face = "bold", size = 24, margin = margin(t = 15)),
      axis.title.y = element_text(face = "bold", size = 24, margin = margin(r = 15)),
      plot.caption = element_text(size = 20, hjust = 0.5, face = "bold"),
      legend.title = element_text(face = "bold", size = 20),
      legend.text = element_text(size = 19),
      legend.position = "right",
      legend.key.size = unit(1.2, "cm"),
      strip.text = element_text(face = "bold", size = 20),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray85", linewidth = 0.5)
    )
}

m1 <- criar_mapa_ano("2016", "a")
m2 <- criar_mapa_ano("2019", "b")
m3 <- criar_mapa_ano("2022", "c")
m4 <- criar_mapa_ano("2025", "d")

mapa_combinado <- (m1 | m2) / (m3 | m4)

ggsave("output/figures/08_mapa_diferenciais.png",
       mapa_combinado, width = 18, height = 10, dpi = 300)
cat("✔ Mapa salvo: output/figures/08_mapa_diferenciais.png\n")
