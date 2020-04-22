# https://www.ine.cl/estadisticas/sociales/demografia-y-vitales/proyecciones-de-poblacion
# https://www.minsal.cl/nuevo-coronavirus-2019-ncov/casos-confirmados-en-chile-covid-19/

## cargamos librerias ####
require(raster)
require(tidyverse)
require(sf)
library(readxl)
library(ggplot2)
require(ggspatial)
library(ggrepel)
library(ggpubr)
library(viridis) 

## cargamos el mapa ####
# descomprimir con 7-zip en la carpeta shp/ antes de ejecutar
shp <- shapefile('./shp/Regional.shp')
shp@data$Region <- iconv(shp@data$Region, from = 'UTF-8', to = 'latin1') 
shp <- shp[shp@data$Region != "Zona sin demarcar" ,  ]

regiones <- aggregate(shp, 'Region') # agregamos por regiones
regiones <- st_as_sf(regiones) # lento*

## cargamos casos en Excel ####
casos <- read_excel('./tbl/Reporte_Regional.xlsx')

## cambiamos los nombres de las regiones para que coincidan con los nombres del archivo *.xlsx  ####
regiones$REGION <- c("Arica y Parinacota", "Tarapacá", "Antofagasta",  "Magallanes", "Aysén", "Atacama",
                     "Coquimbo", "Valparaíso", "Metropolitana", "Los Lagos", "Los Ríos", "La Araucanía", 
                     "Biobío", "Ñuble", "Maule", "O'Higgins")

## fortificamos la data por cada calumna del excel que necesitemos  ####
mps_casos <- casos %>% 
  group_by(Region) %>%
  dplyr::summarise(Casos_totales = max(Casos_totales)) %>%  
  ungroup()

mps_tasa100mil <- casos %>%
  group_by(Region) %>%
  dplyr::summarise(tasa_cont_100mil = max(tasa_cont_100mil)) %>%  
  ungroup()

mps_tasa_fallec <- casos %>% 
  group_by(Region) %>%
  dplyr::summarise(tasa_fallec_100mil = max(tasa_fallec_100mil)) %>%  
  ungroup()

## Ahora unimos la columna "Region" del excel con el shp agregado"REGION"  ####
sft <- st_as_sf(regiones) %>%
  inner_join(., y = mps_casos, by = c('REGION' = 'Region'))

sft_tasa100mil <- st_as_sf(regiones) %>% 
  inner_join(., y = mps_tasa100mil, by = c('REGION' = 'Region'))

sft_tasa_fallec <- st_as_sf(regiones) %>%
  inner_join(., y = mps_tasa_fallec, by = c('REGION' = 'Region'))

## quantiles para Casos_totales, indicando los quiebres de forma manual ####
labels_casos <- c()
quantiles_casos <- quantile(sft$Casos_totales, probs = c(0,0.2, 0.4, 0.6, 0.8, 0.9, 1), type=6, names = FALSE)

labels_casos <- c()
for(idx in 1:length(quantiles_casos)){
  labels_casos <- c(labels_casos, paste0(round(quantiles_casos[idx], 0), 
                                         "-", 
                                         round(quantiles_casos[idx + 1], 0)))
}

labels_casos <- labels_casos[1:length(labels_casos)-1]

sft$Casos_totales_qt <- cut(sft$Casos_totales, # guardamos los quantiles  
                            breaks = quantiles_casos, 
                            labels = labels_casos, 
                            include.lowest = T)

## quantiles tasa sft_tasa100mil ####
labels_tc <- c()
quantiles_tc <- quantile(sft_tasa100mil$tasa_cont_100mil, probs = c(0,0.2, 0.4, 0.6, 0.8, 0.9, 1), type=6, names = FALSE) # ajuste manual

labels_tc <- c()
for(idx in 1:length(quantiles_tc)){
  labels_tc <- c(labels_tc, paste0(round(quantiles_tc[idx], 1), 
                                   "-", 
                                   round(quantiles_tc[idx + 1], 1)))
}

labels_tc <- labels_tc[1:length(labels_tc)-1]

sft_tasa100mil$tasa_cont_100mil_qt <- cut(sft_tasa100mil$tasa_cont_100mil, # guardamos  
                                          breaks = quantiles_tc, 
                                          labels = labels_tc, 
                                          include.lowest = T)

## quantiles sft_tasa_fallec ####
labels_fall <- c()
quantiles_fall <- quantile(sft_tasa_fallec$tasa_fallec_100mil, probs = c(0, 0.4, 0.6, 0.8, 0.9, 1), type=6, names = FALSE) # ajuste manual

labels_fall <- c()
for(idx in 1:length(quantiles_fall)){
  labels_fall <- c(labels_fall, paste0(round(quantiles_fall[idx], 1), 
                                       "-", 
                                       round(quantiles_fall[idx + 1], 1)))
}

labels_fall <- labels_fall[1:length(labels_fall)-1]

sft_tasa_fallec$tasa_fallec_100mil_qt <- cut(sft_tasa_fallec$tasa_fallec_100mil, # guardamos 
                                             breaks = quantiles_fall, 
                                             labels = labels_fall, 
                                             include.lowest = T)

## gg1 -plotiamos Casos_totales_qt por region y guardamos ####
gg1 <- ggplot() +
  geom_sf(data = sft, color= 'white', size=0.5,
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

## gg2 - plotiamos tasa_cont_100mil_qt por region y guardamos ####
gg2 <- ggplot() +
  geom_sf(data = sft_tasa100mil, color= 'white', size=0.5,
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

## gg3 - plotiamos tasa_fallec_100mil_qt por region y guardamos ####
gg3 <- ggplot() +
  geom_sf(data = sft_tasa_fallec, color= 'white', size=0.5,
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

## grafico combinado ####
ggx <- ggarrange(gg1, gg2, gg3 + rremove("x.text"), 
                 #labels = c("A", "B", "C"),
                 ncol = 3, nrow = 1)

ggx1 <- annotate_figure(ggx,
                        top = text_grob("Situación covid19 por regiones\n21 de abril de 2020", 
                                        color = "black", face = "bold", size = 14),
                        bottom = text_grob("Autor: L. Fernández, Fuente: Minsal.cl, Mapa vectorial: bcn.cl", 
                                           color = "grey",
                                           hjust = 1.03, x = 1, face = "italic", size = 10))
# guardamos grafico
ggsave(plot = ggx1, filename = './Map1_regiones_combinado.png', 
       units = 'mm', width = 279, height = 216, dpi = 300)

#Fin de línea