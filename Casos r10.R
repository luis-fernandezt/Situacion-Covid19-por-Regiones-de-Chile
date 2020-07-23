# Script guardado y codificado en Latin1 para evitar pérdida de caracteres
# cargamos librerias
require(raster)
require(tidyverse)
require(sf)
library(readr)
library(readxl)
library(ggplot2)
require(ggspatial)
library(ggrepel)
library(viridis) 
library(ggpubr)

# paso 1. cargamos mapa de La Región de Los Lagos ####
# polígonos dispnibles en: https://www.bcn.cl/siit/mapas_vectoriales

shp <- shapefile('./shp/comunas.shp')
shp@data$Region <- iconv(shp@data$Region, from = 'UTF-8', to = 'latin1')
shp@data$Provincia <- iconv(shp@data$Provincia, from = 'UTF-8', to = 'latin1')
shp@data$Comuna <- iconv(shp@data$Comuna, from = 'UTF-8', to = 'latin1')

r10 <- shp[shp@data$Region=="Región de Los Lagos" ,  ]

comunas_sf <- aggregate(r10, 'Comuna') #agregamos region por comuna
comunas_sf <- st_as_sf(comunas_sf)

comunas_sf$Comuna <- c("San Pablo", "Puqueldon", "Fresia", "Llanquihue", "Osorno", "Purranque", "Puyehue", "Cochamo", "Ancud",
             "Quellon", "Queilen", "Chonchi", "Puerto Montt", "Rio Negro", "Castro", "Dalcahue", "Quemchi", 
             "San Juan de la Costa", "Calbuco", "Chaiten", "Los Muermos", "Maullin", "Quinchao", "Curaco de Velez",
             "Puerto Octay", "Frutillar", "Puerto Varas", "Hualaihue", "Palena", "Futaleufu")

# paso 2. cargamos BD Covid_19_std agregada por comunas desde repositorio MinCiencia ####
Covid_19_std <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto1/Covid-19_std.csv")
Covid_19_std <- as.data.frame(Covid_19_std)
names(Covid_19_std) <- c("Region", "Codigo_region", "Comuna", "Codigo_comuna", "Poblacion", "Fecha", "Casos_confirmados")

Covid_19_std$Tasa_cont_100mil <- (Covid_19_std$Casos_confirmados/Covid_19_std$Poblacion)*100000
names(Covid_19_std)

# filtramos por última fecha para tener el dato más actualizado
tbl <- Covid_19_std %>%  filter(Codigo_region == 10)
tbl <- tbl %>% filter(Fecha == max(Fecha)) 
max(tbl$Fecha)

# paso 3. fortificamos la data ####
mps_ct <- tbl %>% 
  group_by(Comuna) %>% #agrupamos casos totales
  dplyr::summarise(Casos_confirmados = max(Casos_confirmados)) %>%  
  ungroup()

mps_tc <- tbl %>% 
  group_by(Comuna) %>% #agrupamos tasa de contagios cada 100 mil hab.
  dplyr::summarise(Tasa_cont_100mil = max(Tasa_cont_100mil)) %>% 
  ungroup()

# unimos la columna "municipio" del excel con el shp agregado por "Comuna"
sft_casos <- st_as_sf(comunas_sf) %>% 
  inner_join(., y = mps_ct, by = c('Comuna' = 'Comuna'))
sft_casos <- sft_casos %>% filter(Casos_confirmados > 0)

sft_tasa <- st_as_sf(comunas_sf) %>% 
  inner_join(., y = mps_tc, by = c('Comuna' = 'Comuna'))
sft_tasa <- sft_tasa %>% filter(Tasa_cont_100mil > 0)

# paso 4 calculamos quantiles ####
# para casos totales
labels <- c()
quantiles <- quantile(sft_casos$Casos_confirmados, probs = c(0, 0.4, 0.6, 0.8, 0.9, 1), type=6, names = FALSE)


labels <- c() # etiquetas de leyenda personalizada
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 0), 
                             "-", 
                             round(quantiles[idx + 1], 0)))}

labels <- labels[1:length(labels)-1] # se remueve la última etiqueta ya que incluye NA

sft_casos$Casos_confirmados_qt <- cut(sft_casos$Casos_confirmados, #guardamos como nueva variable
                                  breaks = quantiles, 
                                  labels = labels, 
                                  include.lowest = T)

#  calculamos los quantiles para tasa contagiados cada 100 mil habitantes
labels_tasa <- c()
quantiles_tasa <- quantile(sft_tasa$Tasa_cont_100mil, probs = c(0, 0.4, 0.6, 0.8, 0.9, 1), 
                           type=6, names = FALSE)

labels_tasa <- c()
for(idx in 1:length(quantiles_tasa)){
  labels_tasa <- c(labels_tasa, paste0(round(quantiles_tasa[idx], 1), 
                                       "-", 
                                       round(quantiles_tasa[idx + 1], 1)))}

labels_tasa <- labels_tasa[1:length(labels_tasa)-1]

sft_tasa$Tasa_cont_100mil_qt <- cut(sft_tasa$Tasa_cont_100mil, # guardamos los quantiles
                                    breaks = quantiles_tasa, 
                                    labels = labels_tasa, 
                                    include.lowest = T)

#paso 5 -  ploteamos ####
# gg1 - ploteamos los datos con los Casos_confirmados_qt

gg1 <- 
  ggplot() +
  geom_sf(data = comunas_sf, color= 'white', size=0.5, fill = 'grey') +
  geom_sf(data = sft_casos, color= 'white', size=0.5,
          aes(fill = Casos_confirmados_qt, colour = Casos_confirmados_qt)) +
  coord_sf() +
  theme_void() +
  theme(plot.title = element_text(hjust = 0, size = 14, face = "bold"),
        legend.position = c(0.5, 0.03),
        legend.direction = "horizontal") +
  
  labs(x = NULL, 
       y = NULL, 
       title = "", 
       subtitle = "Casos confirmados covid19", 
       caption = "") +
  
  scale_fill_viridis(option = "magma",
                     name = "Casos covid19",
                     alpha = 0.8,
                     begin = 0,
                     end = 0.9,
                     discrete = T,
                     direction = -1,
                     guide = guide_legend(direction = "horizontal",
                                          keyheight = unit(3, units = "mm"),
                                          keywidth = unit(70 / length(labels), units = "mm"),
                                          title.position = 'top',
                                          title.hjust = 0.5,
                                          label.hjust = 1,
                                          nrow = 1,
                                          byrow = T,
                                          reverse = F,
                                          label.position = "bottom")) +
  ylim(-5500000, -4900000)

# gg2 - ploteamos los datos con tasa_cont_100mil_qt


gg2 <-
  ggplot() +
  geom_sf(data = comunas_sf, color= 'white', size=0.5, fill = 'grey') +
  geom_sf(data = sft_tasa, color= 'white', size=0.5,
          aes(fill = Tasa_cont_100mil_qt, colour = Tasa_cont_100mil_qt)) +
  coord_sf() +
  theme_void() +
  theme(plot.title = element_text(hjust = 0, size = 14, face = "bold"),
        legend.position = c(0.5, 0.03),
        legend.direction = "horizontal") +
  
  labs(x = NULL, 
       y = NULL, 
       title = "", 
       subtitle = "Tasa de contagiados cada 100 mil habitantes", 
       caption = "") +
  
  annotation_north_arrow(location = "tr", 
                         which_north = "true", 
                         pad_x = unit(0.1, "cm"), 
                         pad_y = unit(0.1, "cm"),
                         style = north_arrow_fancy_orienteering) +
  
  scale_fill_viridis(option = "magma",
                     name = "Tasa contagiados cada 100 mil habitantes",
                     alpha = 0.8,
                     begin = 0,
                     end = 0.9,
                     discrete = T,
                     direction = -1,
                     guide = guide_legend(direction = "horizontal",
                                          keyheight = unit(3, units = "mm"),
                                          keywidth = unit(90 / length(labels_tasa), units = "mm"),
                                          title.position = 'top',
                                          title.hjust = 0.5,
                                          label.hjust = 1,
                                          nrow = 1,
                                          byrow = T,
                                          reverse = F,
                                          label.position = "bottom")) +
  ylim(-5500000, -4900000)

# guardamos gg1 y gg2 combinado como imagen
ggx <- ggarrange(gg1, gg2 + rremove("x.text"), 
                 #labels = c("A", "B", "C"),
                 ncol = 2, nrow = 1)

ggx1 <- annotate_figure(ggx,
                        top = text_grob("Región de Los Lagos \n 20-07-2020", 
                                        color = "black", face = "bold", size = 14),
                        bottom = text_grob("Autor: L. Fernández, Datos: MinCiencia", color = "grey",
                                           hjust = 1.05, x = 1, face = "italic", size = 10))

ggx1

# opcional, guardamos como imagen
ggsave(plot = ggx1, filename = './Map_R10_datos_MinCiencia.png', #guardamos como imagen
       units = 'mm', width = 279, height = 216, dpi = 300)
