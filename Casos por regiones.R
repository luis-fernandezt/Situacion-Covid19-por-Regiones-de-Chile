
# Script guardado y codificado en Latin1 para evitar pérdida de caracteres
# cargamos librerias
require(raster)
require(tidyverse)
require(sf)
library(readxl)
library(ggplot2)
require(ggspatial)
library(ggrepel)
library(ggpubr)
library(viridis)  

# paso 1. Cargamos el mapa ####
# descomprimir con 7-zip en la carpeta shp/ antes de ejecutar
shp <- shapefile('./shp/Regional.shp')
shp@data$Region <- iconv(shp@data$Region, from = 'UTF-8', to = 'latin1') 
shp <- shp[shp@data$Region != "Zona sin demarcar" ,  ]

regiones <- aggregate(shp, 'Region') # agregamos por regiones
regiones <- st_as_sf(regiones) # lento

# cambiamos los nombres de las regiones para que coincidan con los nombres del archivo csv
regiones$Region <- c("Arica y Parinacota",   "Tarapaca",   "Antofagasta",   "Magallanes",
                      "Aysen",   "Atacama",   "Coquimbo",   "Valparaiso",   "Metropolitana",
                      "Los Lagos",   "Los Rios",   "Araucania",   "Biobio",   "Nuble",
                      "Maule",   "O'Higgins")

# Paso 2 - cargamos casos desde excel ####
# hay que actualizar cada día
# cargamos casos en Excel editado manualmente con fuentes Minsal-MinCiencia e INE
# https://www.ine.cl/estadisticas/sociales/demografia-y-vitales/proyecciones-de-poblacion
# https://github.com/MinCiencia/Datos-COVID19/blob/master/output/producto4/2020-05-01-CasosConfirmados-totalRegional.csv

casos <- read_excel('./tbl/Reporte_Regional.xlsx')

# paso 3 - fortificamos la data por cada calumna del excel que necesitemos ####
mps_casos <- casos %>% 
  group_by(Region) %>%
  dplyr::summarise(Casos_totales_acumulados = max(Casos_totales_acumulados)) %>%  
  ungroup()

mps_tasa100mil <- casos %>%
  group_by(Region) %>%
  dplyr::summarise(tasa_cont_100mil = max(tasa_cont_100mil)) %>%  
  ungroup()

mps_tasa_fallec <- casos %>% 
  group_by(Region) %>%
  dplyr::summarise(tasa_fallec_100mil = max(tasa_fallec_100mil)) %>%  
  ungroup()

# Ahora unimos la columna "Region" del excel con el shp agregado"REGION" 
sft <- st_as_sf(regiones) %>%
  inner_join(., y = mps_casos, by = c('Region' = 'Region'))

sft_tasa100mil <- st_as_sf(regiones) %>% 
  inner_join(., y = mps_tasa100mil, by = c('Region' = 'Region'))

sft_tasa_fallec <- st_as_sf(regiones) %>%
  inner_join(., y = mps_tasa_fallec, by = c('Region' = 'Region'))

# paso 4 - quantiles #### 
# quantiles para Casos_totales
labels_casos <- c()
quantiles_casos <- quantile(sft$Casos_totales_acumulados, probs = c(0,0.2, 0.4, 0.6, 0.8, 0.9, 1),
                            type=6, names = FALSE)

labels_casos <- c()
for(idx in 1:length(quantiles_casos)){
  labels_casos <- c(labels_casos, paste0(round(quantiles_casos[idx], 0), 
                                         "-", 
                                         round(quantiles_casos[idx + 1], 0)))}

labels_casos <- labels_casos[1:length(labels_casos)-1]

sft$Casos_totales_qt <- cut(sft$Casos_totales_acumulados, # guardamos los quantiles  
                            breaks = quantiles_casos, 
                            labels = labels_casos, 
                            include.lowest = T)

# quantiles tasa sft_tasa100mil
labels_tc <- c()
quantiles_tc <- quantile(sft_tasa100mil$tasa_cont_100mil, probs = c(0,0.2, 0.4, 0.6, 0.8, 0.9, 1), type=6, names = FALSE) # ajuste manual

labels_tc <- c()
for(idx in 1:length(quantiles_tc)){
  labels_tc <- c(labels_tc, paste0(round(quantiles_tc[idx], 1), 
                                   "-", 
                                   round(quantiles_tc[idx + 1], 1)))}

labels_tc <- labels_tc[1:length(labels_tc)-1]

sft_tasa100mil$tasa_cont_100mil_qt <- cut(sft_tasa100mil$tasa_cont_100mil, # guardamos  
                                          breaks = quantiles_tc, 
                                          labels = labels_tc, 
                                          include.lowest = T)

# quantiles sft_tasa_fallec
labels_fall <- c()
quantiles_fall <- quantile(sft_tasa_fallec$tasa_fallec_100mil, probs = c(0, 0.4, 0.6, 0.8, 0.9, 1), type=6, names = FALSE) # ajuste manual

labels_fall <- c()
for(idx in 1:length(quantiles_fall)){
  labels_fall <- c(labels_fall, paste0(round(quantiles_fall[idx], 1), 
                                       "-", 
                                       round(quantiles_fall[idx + 1], 1)))}

labels_fall <- labels_fall[1:length(labels_fall)-1]

sft_tasa_fallec$tasa_fallec_100mil_qt <- cut(sft_tasa_fallec$tasa_fallec_100mil, # guardamos 
                                             breaks = quantiles_fall, 
                                             labels = labels_fall, 
                                             include.lowest = T)

# paso 5 - ploteamos ####
# ploteamos Casos_totales_qt por region
gg1 <- ggplot() +
  geom_sf(data = sft, color= 'white', size=0.2,
          aes(fill = Casos_totales_qt, colour = Casos_totales_qt)) +
  
  coord_sf() +
  theme_void() +
  theme(plot.subtitle = element_text(hjust = 0.5, vjust = 0.9, size = 12, face = "plain"),
        legend.position = c(0.9, 0.5),
        legend.key.size = unit(0.4, "cm"),
        legend.key.width = unit(0.4, "cm")) +
  
  annotation_north_arrow(location = "bl", 
                         which_north = "true", 
                         pad_x = unit(0.1, "cm"), 
                         pad_y = unit(0.1, "cm"),
                         height = unit(1, "cm"), 
                         width = unit(1, "cm"),
                         style = north_arrow_fancy_orienteering) +
  
  labs(x = NULL, 
       y = NULL, 
       title = "",
       subtitle ="Casos confirmados") +  
  
  scale_fill_viridis(option = "magma",
                     name = "Casos covid19",
                     alpha = 1, #0.8 para publicar
                     begin = 0,
                     end = 0.9,
                     discrete = T,
                     direction = -1,
                     guide = guide_legend(title.position = 'top',
                                          reverse = T)) +
  xlim(-8300000, -7200000)

## gg2 - ploteamos tasa_cont_100mil_qt por region
gg2 <- ggplot() +
  geom_sf(data = sft_tasa100mil, color= 'white', size=0.2,
          aes(fill = tasa_cont_100mil_qt, colour = tasa_cont_100mil_qt)) +
  
  coord_sf() +
  theme_void() +
  theme(plot.subtitle = element_text(hjust = 0.5, vjust = 0.9, size = 12, face = "plain"),
        legend.position = c(0.9, 0.5),
        legend.key.size = unit(0.4, "cm"),
        legend.key.width = unit(0.4, "cm")) +
  
  labs(x = NULL, 
       y = NULL, 
       title = "",
       subtitle ="Tasa de contagiados") + 
  
  scale_fill_viridis(option = "cividis",
                     name = "Tasa de\ncontagiados\ncada 100 mil\nhabitantes",
                     alpha = 1, #0.8 para publicar
                     begin = 0,
                     end = 0.9,
                     discrete = T,
                     direction = -1,
                     guide = guide_legend(title.position = 'top',
                                          reverse = T)) +
  xlim(-8300000, -7200000)

## gg3 - ploteamos tasa_fallec_100mil_qt por region y guardamos
gg3 <- ggplot() +
  geom_sf(data = sft_tasa_fallec, color= 'white', size=0.2,
          aes(fill = tasa_fallec_100mil_qt, colour = tasa_fallec_100mil_qt)) +
  
  coord_sf() +
  theme_void() +
  theme(plot.subtitle = element_text(hjust = 0.5, vjust = 0.9, size = 12, face = "plain"),
        legend.position = c(0.9, 0.5),
        legend.key.size = unit(0.4, "cm"),
        legend.key.width = unit(0.4, "cm")) +
  
  labs(x = NULL, 
       y = NULL, 
       title = "",
       subtitle ="Tasa de fallecidos") + 
  
  scale_fill_viridis(option = "cividis",
                     name = "Tasa de\nfallecidos\ncada 100 mil\nhabitantes",
                     alpha = 1, #0.8 para publicar
                     begin = 0,
                     end = 0.9,
                     discrete = T,
                     direction = -1,
                     guide = guide_legend(title.position = 'top',
                                          reverse = T)) +
  xlim(-8300000, -7200000)

## grafico combinado
ggx <- ggarrange(gg1, gg2, gg3 + rremove("x.text"), 
                 #labels = c("A", "B", "C"),
                 ncol = 3, nrow = 1)

ggx1 <- annotate_figure(ggx,
                        top = text_grob("Situación covid19 por regiones\n01 de mayo de 2020", 
                                        color = "black", face = "bold", size = 14),
                        bottom = text_grob("Autor: L. Fernández - Datos: MinCiencia - Mapa vectorial: bcn.cl", 
                                           color = "grey",
                                           hjust = 1.03, x = 1, face = "italic", size = 10))

# guardamos como imagen (opcional)
ggsave(plot = ggx1, filename = './Gráficos/Casos por regiones.png', 
       units = 'mm', width = 279, height = 216, dpi = 300)
