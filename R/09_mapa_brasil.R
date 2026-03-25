# ==============================================================================
# SCRIPT: 09_mapa_brasil.R
# PROJETO: Evolução da taxa de retorno à educação em capitais brasileiras
#          via modelos lineares
# DADOS: Shapefiles do IBGE via pacote geobr (não requer download manual)
# DESCRIÇÃO: Produz mapa temático do Brasil com estados coloridos por
#            macrorregião, siglas dos estados e localização das capitais.
#            Serve como figura de apresentação geográfica no TCC,
#            contextualizando a amostra (capitais das 27 UFs).
# AUTORA: Luí Rocha
# DATA: 2025
# DEPENDÊNCIAS: nenhuma (não usa dados da PNAD)
# SAÍDA: output/figures/09_mapa_brasil.png
# PACOTES: geobr, ggplot2, sf, dplyr, ggspatial
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. PACOTES
# ------------------------------------------------------------------------------

if (!require(geobr))     install.packages("geobr")
if (!require(ggspatial)) install.packages("ggspatial")

library(geobr)
library(ggplot2)
library(sf)
library(dplyr)
library(ggspatial)

if (!dir.exists("output/figures")) dir.create("output/figures", recursive = TRUE)

# ------------------------------------------------------------------------------
# 2. DADOS GEOGRÁFICOS
# read_state() e read_country() baixam shapefiles diretamente do servidor
# do IBGE via geobr — não é necessário nenhum arquivo local.
# showProgress = FALSE suprime a barra de progresso no console.
# ------------------------------------------------------------------------------

estados <- read_state(year = 2020, showProgress = FALSE)
pais    <- read_country(year = 2020, showProgress = FALSE)

# ------------------------------------------------------------------------------
# 3. PALETA DE CORES POR REGIÃO
# As mesmas cores usadas nos gráficos de análise descritiva (scripts 04–07),
# garantindo consistência visual ao longo de todo o TCC.
# ------------------------------------------------------------------------------

cores_regioes <- c(
  "Norte"        = "#7CAF5C",
  "Nordeste"     = "#E8954C",
  "Centro Oeste" = "#E8D84C",   # geobr usa "Centro Oeste" sem hífen
  "Sudeste"      = "#C87CC8",
  "Sul"          = "#7C7CCC"
)

# ------------------------------------------------------------------------------
# 4. COORDENADAS DAS CAPITAIS
# Coordenadas aproximadas do centroide urbano de cada capital.
# hjust e vjust controlam o alinhamento horizontal e vertical do label
# de nome da cidade em relação ao ponto plotado.
# ------------------------------------------------------------------------------

capitais <- data.frame(
  estado  = c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA",
              "MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN",
              "RS","RO","RR","SC","SP","SE","TO"),
  capital = c("Rio Branco","Maceió","Macapá","Manaus","Salvador","Fortaleza",
              "Brasília","Vitória","Goiânia","São Luís","Cuiabá","Campo Grande",
              "Belo Horizonte","Belém","João Pessoa","Curitiba","Recife",
              "Teresina","Rio de Janeiro","Natal","Porto Alegre","Porto Velho",
              "Boa Vista","Florianópolis","São Paulo","Aracaju","Palmas"),
  lon = c(-67.8,-35.73,-51.07,-60.02,-38.51,-38.54,-47.93,-40.34,
          -49.25,-44.30,-56.10,-54.65,-43.94,-48.50,-34.86,-49.27,
          -34.88,-42.80,-43.21,-35.21,-51.23,-63.90,-60.67,-48.55,
          -46.64,-37.07,-48.36),
  lat = c(-9.97,-9.67,0.04,-3.12,-12.97,-3.72,-15.78,-20.32,-16.68,
          -2.53,-15.60,-20.47,-19.92,-1.46,-7.12,-25.43,-8.05,-5.09,
          -22.91,-5.79,-30.03,-8.76,2.82,-27.60,-23.55,-10.91,-10.18),
  # hjust/vjust definem se o nome da capital aparece à direita (hjust = -0.15)
  # ou centrado (hjust = 0.5) em relação ao ponto marcador
  hjust = c(rep(-0.15, 12), 0.5, rep(-0.15, 14)),
  vjust = c(rep(0.5,   12), 1.5, rep(0.5,   14))
)

# ------------------------------------------------------------------------------
# 5. CENTROIDES DOS ESTADOS (para posicionamento das siglas)
# st_centroid() calcula o centroide geométrico de cada polígono.
# Alguns estados têm ajuste manual de latitude (ajustes) para evitar que
# o centroide do polígono fique muito próximo do ponto da capital.
# ------------------------------------------------------------------------------

centroides       <- st_centroid(estados)
centroides_coords <- st_coordinates(centroides)

estados <- estados |>
  mutate(lon_centro = centroides_coords[, 1],
         lat_centro = centroides_coords[, 2])

# Ajustes manuais: empurra a sigla para cima em estados onde o centroide
# coincide visualmente com a capital (evita sobreposição de texto)
ajustes <- data.frame(
  abbrev_state = c("TO", "CE", "DF", "ES", "MS"),
  ajuste_lat   = c( 1.5,  1.2,  0.8,  0.8,  1.5)
)

estados <- estados |>
  left_join(ajustes, by = "abbrev_state") |>
  mutate(lat_centro = ifelse(!is.na(ajuste_lat),
                             lat_centro + ajuste_lat,
                             lat_centro))

# ------------------------------------------------------------------------------
# 6. CONSTRUÇÃO DO MAPA
# ------------------------------------------------------------------------------

mapa_brasil <- ggplot() +

  # Camada 1: polígonos dos estados, preenchidos por região
  geom_sf(data = estados,
          aes(fill = name_region),
          color = "gray50", linewidth = 0.3) +

  # Camada 2: contorno do país em destaque
  geom_sf(data = pais,
          fill = NA, color = "#40E0D0", linewidth = 0.8) +

  # Cores das regiões com labels em português (geobr usa "Centro Oeste" sem hífen)
  scale_fill_manual(
    values = cores_regioes,
    name   = "Região",
    breaks = c("Norte", "Nordeste", "Centro Oeste", "Sudeste", "Sul"),
    labels = c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")
  ) +

  # Camada 3: siglas dos estados no centroide ajustado
  geom_text(data = estados,
            aes(x = lon_centro, y = lat_centro, label = abbrev_state),
            size = 4.5, fontface = "bold", color = "black") +

  # Camada 4: pontos das capitais (círculo branco com borda preta)
  geom_point(data = capitais,
             aes(x = lon, y = lat),
             size = 1.5, color = "black",
             shape = 21, fill = "white", stroke = 0.5) +

  # Camada 5: nomes das capitais (alinhamento individual via hjust/vjust)
  geom_text(data = capitais,
            aes(x = lon, y = lat, label = capital,
                hjust = hjust, vjust = vjust),
            size = 3.5, fontface = "bold", color = "black") +

  # Seta do norte e escala gráfica (pacote ggspatial)
  annotation_north_arrow(
    location    = "br",
    which_north = "true",
    style       = north_arrow_fancy_orienteering,
    height      = unit(1, "cm"),
    width       = unit(1, "cm"),
    pad_x       = unit(0.5, "cm"),
    pad_y       = unit(0.5, "cm")
  ) +
  annotation_scale(
    location   = "br",
    width_hint = 0.2,
    pad_x      = unit(3, "cm"),
    pad_y      = unit(0.5, "cm")
  ) +

  # Limita o enquadramento ao território continental + ilhas
  coord_sf(xlim = c(-72.3, -32.5), ylim = c(-34, 5)) +

  theme_void() +
  theme(
    panel.background = element_rect(fill = "#E8F4F8"),
    plot.background  = element_rect(fill = "white", color = NA),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold", size = 10),
    legend.text      = element_text(size = 10),
    panel.grid.major = element_line(color = "gray80", linewidth = 0.3),
    axis.text.x      = element_text(size = 9, color = "gray30"),
    axis.text.y      = element_text(size = 9, color = "gray30")
  )

# ------------------------------------------------------------------------------
# 7. SALVAMENTO
# ------------------------------------------------------------------------------

ggsave("output/figures/09_mapa_brasil.png",
       plot = mapa_brasil, width = 11, height = 11.5, dpi = 300)
cat("✔ Mapa salvo: output/figures/09_mapa_brasil.png\n")
