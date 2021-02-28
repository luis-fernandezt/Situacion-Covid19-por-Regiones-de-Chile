# Script guardado y codificado en Latin1 para evitar p√©rdida de caracteres
# cargamos librerias
require(raster)
require(tidyverse)
require(sf)
library(viridis)  
library(ggplot2)
library(ggplot2)
require(ggspatial)
library(ggrepel)
library(ggpubr)

# paso 1. Cargamos el poligono con las regiones####
shp <- shapefile('./shp/Regional.shp')
shp@data$Region <- iconv(shp@data$Region, from = 'UTF-8', to = 'latin1') 
shp <- shp[shp@data$Region != "Zona sin demarcar" ,  ]

regiones <- aggregate(shp, 'Region') # agregamos por regiones
regiones <- st_as_sf(regiones) # lento

# cambiamos los nombres de las regiones para que coincidan con los nombres del archivo .csv
regiones$Region <- c("Arica y Parinacota",   "Tarapac·",   "Antofagasta",   "Magallanes",
                     "AysÈn",   "Atacama",   "Coquimbo",   "ValparaÌso",   "Metropolitana",
                     "Los Lagos",   "Los RÌos",   "AraucanÌa",   "BiobÌo",   "—uble",
                     "Maule",   "O'Higgins") # <- cambiar apostrofe de O'Higgins para coincidir con producto4

# Paso 2 - cargamos reporte diario desde Minciencia (ACTUALIZAR FECHA)
producto4 <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto4/2021-02-26-CasosConfirmados-totalRegional.csv")
producto4 <- as.data.frame(producto4)

names(producto4)

# agregamos poblacion regional seg√∫n datos del INE
producto4$Poblacion <- c("252110", "382773", "691854", "314709", "836096", "1960170", "8125072", "991063",
                         "1131939", "511551", "1663696", "1014343", "405835", "891440", "107297", "178362",
                         NA, "19458310")
names(producto4)

#Procesamos la DF
Region <- producto4[1:16, c("Region")]
Fallecidos_totales <- producto4[1:16, c("Fallecidos totales")]
Fallecidos_totales <- as.numeric(Fallecidos_totales) 

Casos_activos_confirmados <- producto4[1:16, c("Casos activos confirmados")]
Casos_activos_confirmados <- as.numeric(Casos_activos_confirmados) 
Poblacion <- producto4[1:16, c("Poblacion")]
Poblacion <- as.numeric(Poblacion)  

tasa_fallec_100mil <- (Fallecidos_totales/Poblacion)*100000
tasa_activ_100mil <- (Casos_activos_confirmados/Poblacion)*100000

casos <- cbind(Region, Fallecidos_totales, Casos_activos_confirmados, Poblacion, tasa_fallec_100mil, tasa_activ_100mil)
casos <- as.data.frame(casos)

#
#view(regiones$Region)
#view(casos$Region)

# paso 3 - fortificamos la data por cada calumna del excel que necesitemos ####
mps_casos <- casos %>% 
  group_by(Region) %>%
  dplyr::summarise(Casos_activos_confirmados) %>%  
  ungroup()

mps_tasa100mil <- casos %>%
  group_by(Region) %>%
  dplyr::summarise(tasa_activ_100mil) %>%  
  ungroup()

mps_tasa_fallec <- casos %>% 
  group_by(Region) %>%
  dplyr::summarise(tasa_fallec_100mil) %>%  
  ungroup()

# Ahora unimos la columna "Region" del excel con el shp agregado"REGION" 
sft <- st_as_sf(regiones) %>%
  inner_join(., y = mps_casos, by = c('Region' = 'Region'))
sft$Casos_activos_confirmados <- as.numeric(as.character(sft$Casos_activos_confirmados))

sft_tasa100mil <- st_as_sf(regiones) %>% 
  inner_join(., y = mps_tasa100mil, by = c('Region' = 'Region'))
sft_tasa100mil$tasa_activ_100mil <- as.numeric(as.character(sft_tasa100mil$tasa_activ_100mil))


sft_tasa_fallec <- st_as_sf(regiones) %>%
  inner_join(., y = mps_tasa_fallec, by = c('Region' = 'Region'))
sft_tasa_fallec$tasa_fallec_100mil <- as.numeric(as.character(sft_tasa_fallec$tasa_fallec_100mil))

# paso 4 - quantiles #### 
# quantiles para Casos_totales
labels_casos <- c()
quantiles_casos <- quantile(sft$Casos_activos_confirmados, probs = c(0, 0.2, 0.4, 0.5, 0.6, 0.8, 0.9, 1),
                            type=6, names = FALSE)

labels_casos <- c()
for(idx in 1:length(quantiles_casos)){
  labels_casos <- c(labels_casos, paste0(round(quantiles_casos[idx], 0), 
                                         "-", 
                                         round(quantiles_casos[idx + 1], 0)))}

labels_casos <- labels_casos[1:length(labels_casos)-1]

sft$Casos_activos_confirmados_qt <- cut(sft$Casos_activos_confirmados, # guardamos los quantiles  
                                        breaks = quantiles_casos, 
                                        labels = labels_casos, 
                                        include.lowest = T)

# quantiles tasa sft_tasa100mil
labels_tc <- c()
quantiles_tc <- quantile(sft_tasa100mil$tasa_activ_100mil, probs = c(0,0.2, 0.4, 0.5, 0.6, 0.8, 0.9, 1), type=6, names = FALSE) # ajuste manual

labels_tc <- c()
for(idx in 1:length(quantiles_tc)){
  labels_tc <- c(labels_tc, paste0(round(quantiles_tc[idx], 1), 
                                   "-", 
                                   round(quantiles_tc[idx + 1], 1)))}

labels_tc <- labels_tc[1:length(labels_tc)-1]

sft_tasa100mil$tasa_activ_100mil_qt <- cut(sft_tasa100mil$tasa_activ_100mil, # guardamos  
                                           breaks = quantiles_tc, 
                                           labels = labels_tc, 
                                           include.lowest = T)

# quantiles sft_tasa_fallec
labels_fall <- c()
quantiles_fall <- quantile(sft_tasa_fallec$tasa_fallec_100mil, probs = c(0, 0.4, 0.5, 0.6, 0.8, 0.9, 1), type=6, names = FALSE) # ajuste manual

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

# limpiamos la BD
rm(Fallecidos_totales)
rm(Casos_activos_confirmados)
rm(Poblacion)
rm(tasa_fallec_100mil)
rm(tasa_activ_100mil)
rm(mps_casos)
rm(mps_tasa100mil)
rm(mps_tasa_fallec)
rm(idx)
rm(labels_casos)
rm(labels_fall)
rm(labels_tc)
rm(quantiles_casos)
rm(quantiles_fall)
rm(quantiles_tc)
rm(casos)

# paso 5 - ploteamos ####

# Casos_activos_confirmados_qt
gg1 <- ggplot() +
  geom_sf(data = sft, color= 'white', size=0.2,
          aes(fill = Casos_activos_confirmados_qt, colour = Casos_activos_confirmados_qt)) +
  
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
       subtitle ="Casos Activos Confirmados") +  
  
  scale_fill_viridis(option = "magma",
                     name = "Casos\nActivos",
                     alpha = 1, #0.8 para publicar
                     begin = 0,
                     end = 0.9,
                     discrete = T,
                     direction = -1,
                     guide = guide_legend(title.position = 'top',
                                          reverse = T)) +
  xlim(-8300000, -7200000)

# tasa_activ_100mil_qt

gg2 <- ggplot() +
  geom_sf(data = sft_tasa100mil, color= 'white', size=0.2,
          aes(fill = tasa_activ_100mil_qt, colour = tasa_activ_100mil_qt)) +
  
  coord_sf() +
  theme_void() +
  theme(plot.subtitle = element_text(hjust = 0.5, vjust = 0.9, size = 12, face = "plain"),
        legend.position = c(0.9, 0.5),
        legend.key.size = unit(0.4, "cm"),
        legend.key.width = unit(0.4, "cm")) +
  
  labs(x = NULL, 
       y = NULL, 
       title = "",
       subtitle ="Tasa de incidencia de Casos Activos\ncada 100 mil habitantes") + 
  
  scale_fill_viridis(option = "cividis",
                     name = "Incidenia de\ncasos activos",
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
       subtitle ="Tasa de fallecidos\ncada 100 mil habitantes") + 
  
  scale_fill_viridis(option = "cividis",
                     name = "Tasa de\nfallecidos",
                     alpha = 1, #0.8 para publicar
                     begin = 0,
                     end = 0.9,
                     discrete = T,
                     direction = -1,
                     guide = guide_legend(title.position = 'top',
                                          reverse = T)) +
  xlim(-8300000, -7200000)

## grafico combinado
ggx <- ggarrange(gg1, gg2, gg3 + rremove("x.text"), #lento
                 #labels = c("A", "B", "C"),
                 ncol = 3, nrow = 1)

ggx1 <- annotate_figure(ggx,
                        top = text_grob("SituaciÛn por regiones\n26 de febrero de 2021", 
                                        color = "black", face = "bold", size = 16),
                        bottom = text_grob("Fuente: Minsal.cl | Gob.cl", 
                                           color = "grey",
                                           hjust = 1.03, x = 1, face = "italic", size = 10))

# guardamos como imagen (opcional)
ggsave(plot = ggx1, filename = './Gr·ficos/Casos por regiones.png', 
       units = 'mm', width = 279, height = 216, dpi = 300)
 
