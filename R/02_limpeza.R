# ==============================================================================
# SCRIPT: 02_limpeza.R
# PROJETO: Evolução da taxa de retorno à educação em capitais brasileiras
#          via modelos lineares
# DADOS: PNAD Contínua — 2º trimestre de 2016, 2019, 2022 e 2025
# DESCRIÇÃO: Filtragem da amostra, tratamento de missings, criação de variáveis
#            analíticas (rendimento/hora, escolaridade, formalidade, região,
#            etc.) e preparação da base para modelagem e análise descritiva.
#            Produz dois objetos: pnadc_descritiva.rds (análises descritivas)
#            e pnadc_modelo.rds (pronto para os modelos OLS).
# AUTORA: Luí Rocha
# DATA: 2025
# DEPENDÊNCIAS: data/processed/pnadc_raw.rds
# SAÍDA: data/processed/pnadc_descritiva.rds
#        data/processed/pnadc_modelo.rds
# PACOTES: tidyverse, forcats
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. PACOTES E LEITURA
# ------------------------------------------------------------------------------

library(tidyverse)
library(forcats)

dadosPNADc <- readRDS("data/processed/pnadc_raw.rds")

# ------------------------------------------------------------------------------
# 2. FILTRAGEM DA AMOSTRA
#
# Os critérios abaixo definem a população de interesse: trabalhadores ocupados
# com rendimento positivo nas capitais brasileiras, em idade ativa central
# (25–64 anos). Cada filtro é justificado individualmente.
# ------------------------------------------------------------------------------

# 2.1 Recorte etário: 25–64 anos
# Exclui jovens ainda em formação e pessoas próximas à aposentadoria,
# que têm padrões de participação e rendimento muito distintos.
dadosPNADc <- dadosPNADc |>
  filter(V2009 >= 25 & V2009 <= 64)

cat("Após filtro etário (25–64 anos):", nrow(dadosPNADc), "obs.\n")

# 2.2 Rendimento: remove NAs e zeros
# V403312 é o rendimento efetivo do trabalho principal.
# Zeros indicam ausência de rendimento monetário (ex.: trabalhadores em espécie),
# que são incompatíveis com a modelagem log-linear.
dadosPNADc <- dadosPNADc |>
  filter(!is.na(V403312) & V403312 > 0)

cat("Após filtro de rendimento positivo:", nrow(dadosPNADc), "obs.\n")

# 2.3 Condição no domicílio: remove categorias sem vínculo com o núcleo familiar
# Agrega, pensionistas e empregados domésticos têm lógica de rendimento diferente
# e representam parcela ínfima da amostra.
dadosPNADc <- dadosPNADc |>
  filter(
    !V2005 %in% c(
      "Agregado(a) - Não parente que não compartilha despesas",
      "Pensionista",
      "Empregado(a) doméstico(a)",
      "Parente do(a) empregado(a) doméstico(a)"
    )
  )

cat("Após filtro de condição no domicílio:", nrow(dadosPNADc), "obs.\n")

# 2.4 Cor ou raça: mantém apenas Branca, Preta e Parda
# Amarela e Indígena têm baixíssima frequência nas capitais e prejudicariam
# a estabilidade das estimativas. "Ignorado" é excluído por ser não-informativo.
dadosPNADc <- dadosPNADc |>
  filter(!V2010 %in% c("Amarela", "Indígena", "Ignorado"))

cat("Após filtro de cor/raça:", nrow(dadosPNADc), "obs.\n")

# 2.5 Posição na ocupação: remove trabalhador familiar não remunerado
# Incompatível com a análise de rendimento monetário.
dadosPNADc <- dadosPNADc |>
  filter(!V4012 %in% c("Trabalhador familiar não remunerado"))

cat("Após filtro de posição na ocupação:", nrow(dadosPNADc), "obs.\n")

# ------------------------------------------------------------------------------
# 3. CRIAÇÃO DE VARIÁVEIS ANALÍTICAS
# ------------------------------------------------------------------------------

dadosPNADc <- dadosPNADc |>
  mutate(

    # 3.1 Rendimento por hora (variável dependente nos modelos)
    # V403312 = rendimento mensal efetivo (R$)
    # V4039   = horas habitualmente trabalhadas por semana
    # A conversão semanas→mês usa o fator (30/28)*4 ≈ 4,286 semanas/mês.
    Rendimento = V403312 / (V4039 * ((30/28)*4)),

    # 3.2 Escolaridade — agrupamento em 5 níveis ordenados
    # A decisão de agrupar "Fundamental completo + Médio incompleto" e
    # "Médio completo + Superior incompleto" reflete transições de ciclo
    # que têm padrões de rendimento semelhantes na literatura brasileira.
    Escolaridade = case_when(
      VD3004 == "Sem instrução e menos de 1 ano de estudo"         ~ "Sem instrução e menos de 1 ano de estudo",
      VD3004 == "Fundamental incompleto ou equivalente"             ~ "Fundamental incompleto",
      VD3004 %in% c("Fundamental completo ou equivalente",
                    "Médio incompleto ou equivalente")              ~ "Fundamental completo",
      VD3004 %in% c("Médio completo ou equivalente",
                    "Superior incompleto ou equivalente")           ~ "Médio completo",
      VD3004 == "Superior completo"                                 ~ "Superior completo"
    ),

    # 3.3 Grupamentos de atividade — recodificação das ocupações do IBGE
    # Colapsa os 10 grupamentos originais em 5 categorias mais interpretáveis,
    # reduzindo o número de dummies no modelo sem perda substancial de informação.
    Atividades = case_when(
      VD4011 %in% c("Diretores e gerentes",
                    "Profissionais das ciências e intelectuais") ~
        "Diretores, Gerentes e Profissionais da Ciência/Intelectuais",
      VD4011 %in% c("Técnicos e profissionais de nível médio",
                    "Trabalhadores de apoio administrativo",
                    "Trabalhadores dos serviços, vendedores dos comércios e mercados") ~
        "Técnicos, Profissionais Administrativos, Serviços, Comércios e Mercados",
      VD4011 %in% c("Trabalhadores qualificados da agropecuária, florestais, da caça e da pesca",
                    "Trabalhadores qualificados, operários e artesões da construção, das artes mecânicas e outros ofícios",
                    "Operadores de instalações e máquinas e montadores") ~
        "Operários, Artesões da Construção, Agropecuária, Operadores de Máquinas e Outro Ofícios",
      VD4011 == "Membros das forças armadas, policiais e bombeiros militares" ~
        "Militares",
      VD4011 %in% c("Ocupações elementares", "Ocupações maldefinidas") ~
        "Outras ocupações"
    ),

    # 3.4 Chefe de domicílio (dummy)
    Chefe = case_when(
      V2005 == "Pessoa responsável pelo domicílio" ~ "Chefe",
      TRUE                                          ~ "Não chefe"
    ),

    # 3.5 Carteira assinada (proxy de formalidade simplificada)
    Carteira = case_when(
      VD4009 %in% c("Empregado no setor privado com carteira de trabalho assinada",
                    "Trabalhador doméstico com carteira de trabalho assinada",
                    "Empregado no setor público com carteira de trabalho assinada") ~ "Carteira",
      VD4009 %in% c("Empregado no setor privado sem carteira de trabalho assinada",
                    "Trabalhador doméstico sem carteira de trabalho assinada",
                    "Empregado no setor público sem carteira de trabalho assinada",
                    "Militar e servidor estatutário", "Empregador",
                    "Conta-própria", "Trabalhador familiar auxiliar")             ~ "Não carteira"
    ),

    # 3.6 Macrorregião geográfica
    Regiao = case_when(
      UF %in% c("Rondônia", "Acre", "Amazonas", "Roraima",
                "Pará", "Amapá", "Tocantins")                               ~ "Norte",
      UF %in% c("Maranhão", "Piauí", "Ceará", "Rio Grande do Norte",
                "Paraíba", "Pernambuco", "Alagoas", "Sergipe", "Bahia")     ~ "Nordeste",
      UF %in% c("Minas Gerais", "Espírito Santo",
                "Rio de Janeiro", "São Paulo")                               ~ "Sudeste",
      UF %in% c("Paraná", "Santa Catarina", "Rio Grande do Sul")            ~ "Sul",
      UF %in% c("Mato Grosso do Sul", "Mato Grosso",
                "Goiás", "Distrito Federal")                                 ~ "Centro-Oeste"
    ),

    # 3.7 Formalidade (conceito mais abrangente que carteira)
    # Inclui militares e servidores estatutários como formais,
    # pois têm proteção institucional equivalente à CLT.
    Formalidade = case_when(
      VD4009 %in% c("Empregado no setor privado com carteira de trabalho assinada",
                    "Trabalhador doméstico com carteira de trabalho assinada",
                    "Empregado no setor público com carteira de trabalho assinada",
                    "Militar e servidor estatutário")                         ~ "Formal",
      VD4009 %in% c("Empregado no setor privado sem carteira de trabalho assinada",
                    "Trabalhador doméstico sem carteira de trabalho assinada",
                    "Empregado no setor público sem carteira de trabalho assinada",
                    "Empregador", "Conta-própria")                            ~ "Informal"
    )
  )

# ------------------------------------------------------------------------------
# 4. ANOS DE ESTUDO: CONVERSÃO DE FACTOR PARA NUMÉRICO
#
# VD3005 é armazenado como factor com labels textuais ("3 anos de estudo").
# Para a modelagem, precisamos do valor numérico contínuo (ou discreto).
# A categoria "16 anos ou mais" é truncada em 16 — decisão conservadora
# que não distorce os resultados, dado que casos acima de 16 são raros.
# ------------------------------------------------------------------------------

dadosPNADc <- dadosPNADc |>
  mutate(
    anos_estudo_num = case_when(
      VD3005 == "Sem instrução e menos de 1 ano de estudo" ~ 0,
      VD3005 == "1 ano de estudo"   ~ 1,
      VD3005 == "2 anos de estudo"  ~ 2,
      VD3005 == "3 anos de estudo"  ~ 3,
      VD3005 == "4 anos de estudo"  ~ 4,
      VD3005 == "5 anos de estudo"  ~ 5,
      VD3005 == "6 anos de estudo"  ~ 6,
      VD3005 == "7 anos de estudo"  ~ 7,
      VD3005 == "8 anos de estudo"  ~ 8,
      VD3005 == "9 anos de estudo"  ~ 9,
      VD3005 == "10 anos de estudo" ~ 10,
      VD3005 == "11 anos de estudo" ~ 11,
      VD3005 == "12 anos de estudo" ~ 12,
      VD3005 == "13 anos de estudo" ~ 13,
      VD3005 == "14 anos de estudo" ~ 14,
      VD3005 == "15 anos de estudo" ~ 15,
      VD3005 == "16 anos ou mais de estudo" ~ 16,
      TRUE ~ NA_real_
    )
  )

# ------------------------------------------------------------------------------
# 5. FAIXA ETÁRIA (usada nas análises descritivas de perfil)
# ------------------------------------------------------------------------------

dadosPNADc <- dadosPNADc |>
  mutate(
    Faixa_Etaria = case_when(
      V2009 >= 25 & V2009 <= 29 ~ "Jovens",
      V2009 >= 30 & V2009 <= 59 ~ "Adultos",
      V2009 >= 60 & V2009 <= 64 ~ "Idosos"
    )
  )

# ------------------------------------------------------------------------------
# 6. RAÇA AGRUPADA (usada nas análises descritivas)
# Preto e Pardo são agrupados seguindo convenção consolidada na literatura
# de desigualdade racial brasileira (ex.: Osório 2009, Soares 2004).
# ------------------------------------------------------------------------------

dadosPNADc <- dadosPNADc |>
  mutate(
    Raca_Agrupada = case_when(
      V2010 == "Branca"                   ~ "Branco",
      V2010 %in% c("Preta", "Parda")      ~ "Preto/Pardo",
      TRUE                                 ~ NA_character_
    )
  )

# ------------------------------------------------------------------------------
# 7. CONVERSÃO DE VARIÁVEIS CATEGÓRICAS PARA FACTOR ORDENADO
# A ordem dos levels define a categoria de referência nos modelos OLS
# (primeiro level = referência) e a ordem nos gráficos.
# ------------------------------------------------------------------------------

dadosPNADc <- dadosPNADc |>
  mutate(
    Escolaridade = factor(Escolaridade,
      levels = c("Sem instrução e menos de 1 ano de estudo", "Fundamental incompleto",
                 "Fundamental completo", "Médio completo", "Superior completo")),

    Atividades = factor(Atividades,
      levels = c("Diretores, Gerentes e Profissionais da Ciência/Intelectuais",
                 "Técnicos, Profissionais Administrativos, Serviços, Comércios e Mercados",
                 "Operários, Artesões da Construção, Agropecuária, Operadores de Máquinas e Outro Ofícios",
                 "Militares",
                 "Outras ocupações")),

    Chefe      = factor(Chefe,      levels = c("Chefe", "Não chefe")),
    Carteira   = factor(Carteira,   levels = c("Carteira", "Não carteira")),
    Formalidade= factor(Formalidade,levels = c("Formal", "Informal")),

    Regiao = factor(Regiao,
      levels = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")),

    Faixa_Etaria = factor(Faixa_Etaria,
      levels = c("Jovens", "Adultos", "Idosos")),

    Raca_Agrupada = factor(Raca_Agrupada,
      levels = c("Branco", "Preto/Pardo")),

    # Remove o level "Trabalhador familiar não remunerado" que já foi filtrado
    V4012 = fct_drop(V4012, only = "Trabalhador familiar não remunerado")
  )

# ------------------------------------------------------------------------------
# 8. SALVAR BASE PARA ANÁLISES DESCRITIVAS
# Contém todas as variáveis criadas acima, sem restrições adicionais.
# ------------------------------------------------------------------------------

if (!dir.exists("data/processed")) dir.create("data/processed", recursive = TRUE)
saveRDS(dadosPNADc, "data/processed/pnadc_descritiva.rds")
cat("✔ Base descritiva salva em: data/processed/pnadc_descritiva.rds\n")
cat("  Dimensões:", nrow(dadosPNADc), "obs. ×", ncol(dadosPNADc), "variáveis\n\n")

# ------------------------------------------------------------------------------
# 9. PREPARAÇÃO ADICIONAL PARA MODELAGEM MQO
#
# Para os modelos de Mincer, fazemos dois ajustes:
#   a) Cor/raça: colapsa Preta+Parda → "Preto/Pardo" e torna Branco a referência.
#   b) Anos de estudo: mantido como factor discreto (0–16) para estimar
#      o retorno de CADA ano adicional separadamente (especificação não-linear),
#      em vez de assumir linearidade. Isso permite o gráfico de incremento
#      cumulativo de renda por ano de estudo.
#   c) Capital: Brasília (DF) como referência (maior PIB per capita entre
#      as capitais), para que os coeficientes expressem diferenciais em
#      relação ao mercado de trabalho mais desenvolvido.
# ------------------------------------------------------------------------------

dadosPNADc_model <- dadosPNADc |>
  mutate(
    V2010 = case_when(
      V2010 == "Branca"                  ~ "Branco",
      V2010 %in% c("Preta", "Parda")     ~ "Preto/Pardo",
      TRUE                                ~ NA_character_
    ),
    V2010 = factor(V2010, levels = c("Branco", "Preto/Pardo")),

    # Factor discreto: cada coeficiente mede o diferencial salarial
    # de ter exatamente S anos de estudo vs. 0 anos (referência)
    anos_estudo_num = factor(anos_estudo_num, levels = 0:16)
  )

# Redefine a referência de Capital para Brasília (DF)
dadosPNADc_model$Capital <- relevel(dadosPNADc_model$Capital,
                                     ref = "Município de Brasília (DF)")

saveRDS(dadosPNADc_model, "data/processed/pnadc_modelo.rds")
cat("✔ Base para modelagem salva em: data/processed/pnadc_modelo.rds\n")
cat("  Dimensões:", nrow(dadosPNADc_model), "obs. ×", ncol(dadosPNADc_model), "variáveis\n")
