# Evolução da taxa de retorno à educação em capitais brasileiras via modelos lineares

Trabalho de Conclusão de Curso apresentado ao Departamento de Estatística da
Universidade Federal Fluminense (UFF) como requisito parcial para obtenção do
grau de Bacharel em Estatística.

---

## Sobre o trabalho

A educação é frequentemente apontada como um dos principais determinantes da
renda individual, mas sua contribuição varia de acordo com o contexto regional,
o período histórico e o perfil socioeconômico do trabalhador. Este trabalho
estima e compara as **taxas de retorno à escolaridade** para trabalhadores
residentes nas capitais dos 26 estados brasileiros e do Distrito Federal,
utilizando a equação minceriana adaptada a um modelo log-normal.

A pergunta central é: *quanto vale, em termos de rendimento por hora, cada ano
adicional de estudo — e como esse valor evoluiu entre 2016 e 2025?*

---

## Metodologia

### Dados

Os microdados utilizados são da **PNAD Contínua** (Pesquisa Nacional por Amostra
de Domicílios Contínua), realizada pelo IBGE, referentes ao **2º trimestre** dos
anos de 2016, 2019, 2022 e 2025. A escolha do 2º trimestre garante
comparabilidade temporal, e os quatro anos permitem capturar efeitos do
período pré-pandemia (2016, 2019), do choque econômico (2022) e da recuperação
pós-pandemia (2025).

Após os filtros descritos abaixo, a base final conta com **184.264 observações**,
distribuídas da seguinte forma:

| Ano  | N observações |
|------|--------------|
| 2016 | 49.913       |
| 2019 | 47.784       |
| 2022 | 42.096       |
| 2025 | 44.471       |

**Critérios de inclusão e exclusão da amostra:**

- Moradores de municípios de capital (variável `V1023 == 1`)
- Faixa etária de 25 a 64 anos (população em idade ativa central)
- Rendimento efetivo do trabalho principal positivo e não nulo
- Exclusão de agregados, pensionistas, empregados domésticos e seus parentes
- Exclusão de pessoas que se declaram amarelas, indígenas ou com raça ignorada
  (representação insuficiente para estimativas robustas nas capitais)
- Exclusão de trabalhadores familiares não remunerados

### Modelo

A equação estimada é uma extensão da **equação minceriana** clássica
(Mincer, 1974), com a variável dependente sendo o logaritmo do rendimento
por hora trabalhada:

```
ln(rendimento/hora) = α + Σφⱼ·Aᵢⱼ + β₁·idade + β₂·idade²
                       + β₃·sexo + β₄·cor/raça + β₅·chefe
                       + β₆·formalidade + β₇·previdência
                       + Σθₖ·atividade + Σλₗ·capital + ε
```

onde `Aᵢⱼ` são dummies para cada nível discreto de anos de estudo (0 a 16),
permitindo capturar não-linearidades e o chamado *efeito diploma*. A
variável capital utiliza Brasília (DF) como referência, e os modelos são
estimados separadamente para cada ano — escolha metodológica que permite
observar a evolução livre de todos os coeficientes ao longo do tempo.

A interpretação dos coeficientes das variáveis dummy segue a transformação
`(exp{c} − 1) × 100`, que expressa o efeito percentual sobre o rendimento.

---

## Principais resultados

### Retorno à escolaridade

A educação tem retorno positivo e crescente com o nível de escolaridade em
todos os anos analisados. O padrão mais relevante é a **queda progressiva dos
retornos ao ensino superior** ao longo do período:

| Nível educacional         | 2016    | 2019    | 2022    | 2025    |
|---------------------------|---------|---------|---------|---------|
| Ensino médio completo (12 anos) | +40,5% | +39,6% | +43,1% | +26,1% |
| Início do ensino superior (13 anos) | +67,2% | +61,9% | +62,1% | +40,2% |
| Ensino superior completo (≥16 anos) | +184,4% | +156,0% | +153,5% | +125,7% |

*Todos os percentuais são em relação a indivíduos sem instrução ou com menos de
1 ano de estudo, controlando por idade, sexo, raça, formalidade, atividade
ocupacional e capital de residência.*

A redução observada em 2025 — de 184% para 126% no retorno ao nível superior —
pode estar associada a mudanças estruturais no mercado de trabalho no
período pós-pandemia.

### Diferenciais por características pessoais

Controlando por todas as demais variáveis do modelo, os resultados mostram
desigualdades persistentes ao longo do período:

- **Gênero:** ser mulher está associado a um rendimento entre 18% e 21% menor
  que o de homens com perfil equivalente, com leve piora em 2025 (−21,1%).
- **Raça:** trabalhadores pretos e pardos recebem entre 12% e 13% menos que
  brancos com mesmo perfil observável, padrão estável em todos os anos.
- **Formalidade:** o diferencial de informalidade cresceu substancialmente ao
  longo do período, passando de +5,5% em 2016 para +13,5% em 2025, sugerindo
  um mercado de trabalho que recompensa cada vez mais o vínculo informal.

### Diferenciais regionais (capitais vs. Brasília)

Comparadas a Brasília (DF), quase todas as capitais apresentam rendimentos
inferiores após controle das demais variáveis. As maiores diferenças negativas
concentram-se nas regiões Norte e Nordeste. Uma tendência relevante é a
**redução gradual das disparidades** ao longo do período — as diferenças em
relação a Brasília diminuíram entre 2016 e 2025 em praticamente todas as
capitais. Em 2025, dois casos excepcionais emergem: Florianópolis (SC, +8,1%)
e Vitória (ES, +1,2%) superaram Brasília no rendimento esperado.

### Qualidade do ajuste

Os modelos explicam entre 46% e 50% da variabilidade do log-rendimento (R²
ajustado entre 0,4571 e 0,5018). Os testes de Breusch-Pagan e Lilliefors
rejeitam homocedasticidade e normalidade dos resíduos em todos os anos,
o que é esperado em amostras de grande porte com dados de rendimento — a
assimetria da distribuição de renda e a presença de valores extremos
produzem sistematicamente esse resultado. Os estimadores de MQO permanecem
consistentes pelo Teorema Central do Limite, mas os erros-padrão devem ser
interpretados com cautela.

---

## Estrutura do repositório

```
tcc-retorno-educacao/
│
├── tcc-retorno-educacao.Rproj
├── README.md
├── .gitignore
│
├── R/
│   ├── 01_importacao.R          # Leitura dos microdados e seleção de variáveis
│   ├── 02_limpeza.R             # Filtragem, engenharia de features e geração dos .rds
│   ├── 03_descritiva_pessoas.R  # Tabelas: sexo, faixa etária, raça, chefia
│   ├── 04_descritiva_estudo.R   # Gráficos: alfabetização, escolaridade e anos de estudo vs. rendimento
│   ├── 05_descritiva_trabalho.R # Gráficos: formalidade e atividade ocupacional vs. rendimento
│   ├── 06_descritiva_vinculo.R  # Gráficos: carteira assinada e previdência vs. rendimento
│   ├── 07_descritiva_renda.R    # Boxplots e estatísticas descritivas da distribuição de rendimento
│   ├── 08_modelo_ols.R          # Estimação OLS, diagnóstico de resíduos, tabelas e mapas
│   └── 09_mapa_brasil.R         # Mapa temático do Brasil com capitais e regiões
│
├── output/
│   ├── figures/                 # Todos os gráficos gerados (.png)
│   └── tables/                  # Tabelas exportadas (08_coeficientes_completos.csv)
│
├── tex/
│   ├── main.tex                 # Código-fonte LaTeX do TCC
│   └── referencias.bib          # Referências bibliográficas
│
└── docs/
    └── tcc_final.pdf            # PDF compilado do trabalho
```

O fluxo de execução é sequencial: `01` → `02` → `03` a `09`. Os scripts `03`
a `09` são independentes entre si e dependem apenas dos arquivos `.rds`
gerados pelo `02_limpeza.R`.

---

## Como reproduzir a análise

Abra o arquivo tcc-retorno-educacao.Rproj no RStudio para garantir que os caminhos relativos funcionem corretamente.

### 1. Pré-requisitos

R (versão ≥ 4.2.0) com os seguintes pacotes instalados:

```r
install.packages(c(
  "PNADcIBGE", "tidyverse", "forcats",
  "lmtest", "nortest", "broom",
  "geobr", "sf", "patchwork", "ggspatial",
  "knitr", "kableExtra", "scales"
))
```

### 2. Download dos dados brutos

Os microdados da PNAD Contínua não estão incluídos neste repositório (arquivos
pesados, disponibilizados publicamente pelo IBGE). Acesse o portal do IBGE e
baixe os seguintes arquivos para cada ano, colocando-os em `data/raw/`:

**URL base:**
[https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/](https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/)

**Arquivos necessários (2º trimestre de cada ano):**

```
data/raw/
├── PNADC_022016.txt
├── PNADC_022019.txt
├── PNADC_022022.txt
├── PNADC_022025.txt
├── input_PNADC_trimestral.txt
└── dicionario_PNADC_microdados_trimestral.xls
```

O arquivo `input_PNADC_trimestral.txt` e o dicionário `.xls` são os mesmos
para todos os anos e estão disponíveis na mesma pasta do portal do IBGE.

### 3. Execução

Com os dados em `data/raw/`, execute os scripts em ordem a partir da raiz
do projeto:

```r
source("R/01_importacao.R")   # ~10–20 min (leitura dos 4 anos)
source("R/02_limpeza.R")      # ~2 min
source("R/03_descritiva_pessoas.R")
source("R/04_descritiva_estudo.R")
source("R/05_descritiva_trabalho.R")
source("R/06_descritiva_vinculo.R")
source("R/07_descritiva_renda.R")
source("R/08_modelo_ols.R")   # ~5–10 min (estimação + mapas)
source("R/09_mapa_brasil.R")
```

Todos os outputs (gráficos e tabelas) serão salvos automaticamente em
`output/figures/` e `output/tables/`.

### Compilação do documento LaTeX

O texto do TCC está em `tex/main.tex`. Para compilar localmente, é necessário
o template ABNT da UFF, que não está incluído neste repositório por ser de autoria
de terceiros. Baixe os arquivos do template em:

**[Projeto Final | Curso de Estatística da UFF](https://estatistica.uff.br/tcc/)**
(botão "Arquivos .tex")

Coloque os arquivos baixados na mesma pasta do `main.tex` antes de compilar.
O PDF já compilado está disponível em `docs/tcc_final.pdf`.

---

## Tecnologias utilizadas

O projeto foi desenvolvido inteiramente em **R**, com os seguintes pacotes
principais: `PNADcIBGE` para leitura dos microdados, `tidyverse` para
manipulação e visualização, `lmtest` e `nortest` para os testes de
diagnóstico, `geobr` e `sf` para os mapas coropléticos, e `patchwork` para
composição de múltiplos gráficos. O documento escrito foi produzido em
**LaTeX**.
