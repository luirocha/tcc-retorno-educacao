# ==============================================================================
# SCRIPT: 01_importacao.R
# PROJETO: Evolução da taxa de retorno à educação em capitais brasileiras
#          via modelos lineares
# DADOS: PNAD Contínua — 2º trimestre de 2016, 2019, 2022 e 2025
# DESCRIÇÃO: Leitura dos microdados brutos, seleção das variáveis de interesse
#            e filtragem para municípios de capital. Salva um objeto .rds
#            intermediário com os quatro anos empilhados.
# AUTORA: Luí Rocha
# DATA: 2025
# DEPENDÊNCIAS: arquivos brutos em data/raw/ (ver README para instruções
#               de download no site do IBGE)
# SAÍDA: data/processed/pnadc_raw.rds
# PACOTES: PNADcIBGE, tidyverse
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. PACOTES
# ------------------------------------------------------------------------------

# PNADcIBGE: leitura e rotulagem dos microdados da PNAD Contínua
# tidyverse: manipulação e transformação de dados (dplyr, tidyr, etc.)
if (!require(PNADcIBGE)) install.packages("PNADcIBGE")
if (!require(tidyverse))  install.packages("tidyverse")

library(PNADcIBGE)
library(tidyverse)

# ------------------------------------------------------------------------------
# 2. LEITURA DOS MICRODADOS
# Os arquivos brutos devem estar em data/raw/ conforme descrito no README.
# A função read_pnadc() do pacote PNADcIBGE lê o layout fixo da PNAD Contínua
# usando o arquivo de dicionário input_PNADC_trimestral.txt fornecido pelo IBGE.
# ------------------------------------------------------------------------------

dadosPNADc2016_ <- read_pnadc(
  microdata = "data/raw/PNADC_022016.txt",
  input_txt  = "data/raw/input_PNADC_trimestral.txt"
)

dadosPNADc2019_ <- read_pnadc(
  microdata = "data/raw/PNADC_022019.txt",
  input_txt  = "data/raw/input_PNADC_trimestral.txt"
)

dadosPNADc2022_ <- read_pnadc(
  microdata = "data/raw/PNADC_022022.txt",
  input_txt  = "data/raw/input_PNADC_trimestral.txt"
)

dadosPNADc2025_ <- read_pnadc(
  microdata = "data/raw/PNADC_022025.txt",
  input_txt  = "data/raw/input_PNADC_trimestral.txt"
)

# ------------------------------------------------------------------------------
# 3. SELEÇÃO DE VARIÁVEIS E FILTRAGEM POR CAPITAL (V1023 == "1")
#
# As variáveis selecionadas cobrem quatro dimensões analíticas:
#   - Identificação geográfica: Ano, Trimestre, UF, Capital, V1023
#   - Características pessoais: V2005 (condição no domicílio), V2007 (sexo),
#     V2009 (idade), V2010 (cor ou raça)
#   - Educação: V3001 (sabe ler/escrever), VD3004 (nível de escolaridade),
#     VD3005 (anos de estudo)
#   - Trabalho e rendimento: V4001 (trabalhou), V4003 (trabalho adicional),
#     V4012 (posição na ocupação), V4029 (carteira assinada),
#     V403312 (rendimento efetivo do trabalho principal),
#     V4039 (horas habitualmente trabalhadas),
#     VD4009 (posição na ocupação — classificação detalhada),
#     VD4011 (grupamento ocupacional), VD4012 (contribuição previdência)
# ------------------------------------------------------------------------------

selecionar_e_filtrar <- function(dados) {
  dados |>
    select(
      Ano, Trimestre, UF, Capital,
      V1023, V2005, V2007, V2009, V2010,
      V3001, V4001, V4003, V4012, V4029, V403312, V4039,
      VD3004, VD3005, VD4009, VD4011, VD4012
    ) |>
    # V1023 == "1" identifica municípios de capital — foco da análise
    filter(V1023 == "1")
}

dadosPNADc2016 <- selecionar_e_filtrar(dadosPNADc2016_)
dadosPNADc2019 <- selecionar_e_filtrar(dadosPNADc2019_)
dadosPNADc2022 <- selecionar_e_filtrar(dadosPNADc2022_)
dadosPNADc2025 <- selecionar_e_filtrar(dadosPNADc2025_)

# Liberar os objetos brutos da memória após a filtragem
rm(dadosPNADc2016_, dadosPNADc2019_, dadosPNADc2022_, dadosPNADc2025_)

# ------------------------------------------------------------------------------
# 4. EMPILHAMENTO DOS QUATRO ANOS
# bind_rows() preserva todos os níveis de factor, o que é importante para
# a rotulagem feita na próxima etapa.
# ------------------------------------------------------------------------------

dadosPNADc <- bind_rows(dadosPNADc2016, dadosPNADc2019, dadosPNADc2022, dadosPNADc2025)
rm(dadosPNADc2016, dadosPNADc2019, dadosPNADc2022, dadosPNADc2025)

# ------------------------------------------------------------------------------
# 5. ROTULAGEM DAS VARIÁVEIS CATEGÓRICAS
# pnadc_labeller() converte os códigos numéricos brutos nas categorias textuais
# definidas no dicionário do IBGE (ex.: V2007: 1 → "Homem", 2 → "Mulher").
# ------------------------------------------------------------------------------

dadosPNADc <- pnadc_labeller(
  data_pnadc       = dadosPNADc,
  dictionary.file  = "data/raw/dicionario_PNADC_microdados_trimestral.xls"
)

# ------------------------------------------------------------------------------
# 6. VERIFICAÇÕES RÁPIDAS PÓS-IMPORTAÇÃO
# ------------------------------------------------------------------------------

cat("\n--- Dimensões do dataset empilhado ---\n")
glimpse(dadosPNADc)

cat("\n--- Contagem de observações por UF e Ano ---\n")
print(dadosPNADc |> count(UF, Ano) |> print(n = Inf))

cat("\n--- Percentual de NAs por variável ---\n")
print(
  dadosPNADc |>
    summarise(across(everything(), ~ round(mean(is.na(.)) * 100, 2))) |>
    pivot_longer(everything(), names_to = "variavel", values_to = "pct_na") |>
    arrange(desc(pct_na))
)

# ------------------------------------------------------------------------------
# 7. SALVAMENTO DO OBJETO INTERMEDIÁRIO
# Este .rds é a entrada do script 02_limpeza.R.
# Usamos saveRDS() em vez de write.csv() para preservar os tipos de dados
# (factors com labels, etc.) sem perda de informação.
# ------------------------------------------------------------------------------

if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)
saveRDS(dadosPNADc, "data/processed/pnadc_raw.rds")

cat("\n✔ Arquivo salvo em: data/processed/pnadc_raw.rds\n")
cat("  Dimensões:", nrow(dadosPNADc), "observações ×", ncol(dadosPNADc), "variáveis\n")
