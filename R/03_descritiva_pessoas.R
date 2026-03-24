# ==============================================================================
# SCRIPT: 03_descritiva_pessoas.R
# PROJETO: Evolução da taxa de retorno à educação em capitais brasileiras
#          via modelos lineares
# DADOS: PNAD Contínua — 2º trimestre de 2016, 2019, 2022 e 2025
# DESCRIÇÃO: Tabelas descritivas do perfil sociodemográfico da amostra:
#            sexo, faixa etária, cor/raça e chefia de domicílio,
#            desagregados por ano e macrorregião.
# AUTORA: Luí Rocha
# DATA: 2025
# DEPENDÊNCIAS: data/processed/pnadc_descritiva.rds
# SAÍDA: output/tables/03_sexo_por_ano_regiao.csv
#        output/tables/03_faixa_etaria_por_ano_regiao.csv
#        output/tables/03_raca_por_ano_regiao.csv
#        output/tables/03_chefe_domicilio_por_ano_regiao.csv
# PACOTES: dplyr, tidyr, knitr, kableExtra
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. PACOTES E LEITURA
# ------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)

dadosPNADc <- readRDS("data/processed/pnadc_descritiva.rds")

if (!dir.exists("output/tables"))  dir.create("output/tables",  recursive = TRUE)

# Função auxiliar: monta tabela de frequência e percentual em formato largo.
# Recebe o data frame, as colunas de agrupamento (Ano, Regiao) e a variável
# de interesse; retorna uma tabela wide com N e % para cada categoria.
tabela_freq_wide <- function(dados, var_interesse, nome_var) {
  dados |>
    filter(!is.na(.data[[var_interesse]])) |>
    count(Ano, Regiao, .data[[var_interesse]]) |>
    group_by(Ano, Regiao) |>
    mutate(Percentual = round((n / sum(n)) * 100, 1)) |>
    ungroup() |>
    rename(Categoria = all_of(var_interesse)) |>
    pivot_wider(
      names_from  = Categoria,
      values_from = c(n, Percentual),
      names_glue  = "{Categoria}_{.value}"
    ) |>
    arrange(Ano, Regiao)
}

# ------------------------------------------------------------------------------
# 2. SEXO POR ANO E REGIÃO
# ------------------------------------------------------------------------------

cat("=== TABELA 1: COMPOSIÇÃO POR SEXO ===\n\n")

tab_sexo <- dadosPNADc |>
  filter(!is.na(V2007)) |>
  count(Ano, Regiao, Sexo = V2007) |>
  group_by(Ano, Regiao) |>
  mutate(Percentual = round((n / sum(n)) * 100, 1)) |>
  ungroup() |>
  pivot_wider(
    names_from  = Sexo,
    values_from = c(n, Percentual),
    names_glue  = "{Sexo}_{.value}"
  ) |>
  arrange(Ano, Regiao) |>
  select(Ano, Regiao,
         starts_with("Homem"), starts_with("Mulher"))

print(kable(tab_sexo,
            digits  = 1,
            format  = "simple",
            col.names = c("Ano", "Região",
                          "Homens (N)", "Homens (%)",
                          "Mulheres (N)", "Mulheres (%)")))

write.csv(tab_sexo, "output/tables/03_sexo_por_ano_regiao.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
cat("✔ Tabela salva: output/tables/03_sexo_por_ano_regiao.csv\n")

# ------------------------------------------------------------------------------
# 3. FAIXA ETÁRIA POR ANO E REGIÃO
# ------------------------------------------------------------------------------

cat("\n\n=== TABELA 2: COMPOSIÇÃO POR FAIXA ETÁRIA ===\n\n")

tab_idade <- dadosPNADc |>
  filter(!is.na(Faixa_Etaria)) |>
  count(Ano, Regiao, Faixa_Etaria) |>
  group_by(Ano, Regiao) |>
  mutate(Percentual = round((n / sum(n)) * 100, 1)) |>
  ungroup() |>
  pivot_wider(
    names_from  = Faixa_Etaria,
    values_from = c(n, Percentual),
    names_glue  = "{Faixa_Etaria}_{.value}"
  ) |>
  arrange(Ano, Regiao) |>
  select(Ano, Regiao,
         starts_with("Jovens"), starts_with("Adultos"), starts_with("Idosos"))

print(kable(tab_idade,
            digits  = 1,
            format  = "simple",
            col.names = c("Ano", "Região",
                          "Jovens (N)", "Jovens (%)",
                          "Adultos (N)", "Adultos (%)",
                          "Idosos (N)", "Idosos (%)")))

write.csv(tab_idade, "output/tables/03_faixa_etaria_por_ano_regiao.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
cat("✔ Tabela salva: output/tables/03_faixa_etaria_por_ano_regiao.csv\n")

# ------------------------------------------------------------------------------
# 4. COR OU RAÇA POR ANO E REGIÃO
# ------------------------------------------------------------------------------

cat("\n\n=== TABELA 3: COMPOSIÇÃO POR COR OU RAÇA ===\n\n")

tab_raca <- dadosPNADc |>
  filter(!is.na(Raca_Agrupada)) |>
  count(Ano, Regiao, Raca_Agrupada) |>
  group_by(Ano, Regiao) |>
  mutate(Percentual = round((n / sum(n)) * 100, 1)) |>
  ungroup() |>
  pivot_wider(
    names_from  = Raca_Agrupada,
    values_from = c(n, Percentual),
    names_glue  = "{Raca_Agrupada}_{.value}"
  ) |>
  arrange(Ano, Regiao) |>
  select(Ano, Regiao,
         starts_with("Branco"), starts_with("Preto/Pardo"))

print(kable(tab_raca,
            digits  = 1,
            format  = "simple",
            col.names = c("Ano", "Região",
                          "Branco (N)", "Branco (%)",
                          "Preto/Pardo (N)", "Preto/Pardo (%)")))

write.csv(tab_raca, "output/tables/03_raca_por_ano_regiao.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
cat("✔ Tabela salva: output/tables/03_raca_por_ano_regiao.csv\n")

# ------------------------------------------------------------------------------
# 5. CHEFIA DE DOMICÍLIO POR SEXO, ANO E REGIÃO
# Cruzamento triplo: permite analisar a evolução da chefia feminina ao longo
# do período e as diferenças regionais nessa dimensão.
# ------------------------------------------------------------------------------

cat("\n\n=== TABELA 4: CHEFIA DE DOMICÍLIO POR SEXO, ANO E REGIÃO ===\n\n")

tab_chefe <- dadosPNADc |>
  filter(!is.na(Chefe) & !is.na(V2007)) |>
  mutate(Chefe_Sim_Nao = ifelse(Chefe == "Chefe", "Sim", "Não"),
         Chefe_Sim_Nao = factor(Chefe_Sim_Nao, levels = c("Sim", "Não"))) |>
  count(Ano, Regiao, Chefe_Sim_Nao, Sexo = V2007) |>
  group_by(Ano, Regiao, Chefe_Sim_Nao) |>
  mutate(Percentual = round((n / sum(n)) * 100, 1)) |>
  ungroup() |>
  arrange(Ano, Regiao, Chefe_Sim_Nao, Sexo)

print(kable(tab_chefe,
            digits  = 1,
            format  = "simple",
            col.names = c("Ano", "Região", "É chefe?", "Sexo", "N", "%")))

write.csv(tab_chefe, "output/tables/03_chefe_domicilio_por_ano_regiao.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
cat("✔ Tabela salva: output/tables/03_chefe_domicilio_por_ano_regiao.csv\n")
