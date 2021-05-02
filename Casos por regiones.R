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

# Paso 2 - cargamos reporte diario desde Minciencia (ACTUALIZAR FECHA)
producto4 <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto4/2021-04-30-CasosConfirmados-totalRegional.csv")
producto4 <- as.data.frame(producto4)

# cambiamos los nombres de las regiones para que coincidan con los nombres del archivo .csv
regiones$Region <- c("Arica y Parinacota",   "Tarapac·",   "Antofagasta",   "Magallanes",
                     "AysÈn",   "Atacama",   "Coquimbo",   "ValparaÌso",   "Metropolitana",
                     "Los Lagos",   "Los RÌos",   "AraucanÌa",   "BiobÌo",   "—uble",
                     "Maule", as.character(producto4[8,1])) # <- cambiar apostrofe de O'Higgins para coincidir con producto4

# agregamos poblacion regional seg√∫n datos del INE
producto4$Poblacion <- c("252110", "382773", "691854", "314709", "836096", "1960170", "8125072", "991063",
                         "1131939", "511551", "1663696", "1014343", "405835", "891440", "107297", "178362",
                         NA, "19458310")




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

## #ubicamos el centroide
sft <- sft %>% mutate(centroid = map(geometry, st_centroid), 
                      coords = map(centroid, st_coordinates), 
                      coords_x = map_dbl(coords, 1), 
                      coords_y = map_dbl(coords, 2))

sft_tasa100mil <- sft_tasa100mil %>% mutate(centroid = map(geometry, st_centroid), 
                                            coords = map(centroid, st_coordinates), 
                                            coords_x = map_dbl(coords, 1), 
                                            coords_y = map_dbl(coords, 2))

sft_tasa_fallec <- sft_tasa_fallec %>% mutate(centroid = map(geometry, st_centroid), 
                                              coords = map(centroid, st_coordinates), 
                                              coords_x = map_dbl(coords, 1), 
                                              coords_y = map_dbl(coords, 2))

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
rm(Region)
rm(t)

# Paso 5 - ploteamos ####
# Grafico Casos Activos confirmados por region || sft$Casos_activos_confirmados
g1 <- ggplot() +
  geom_sf(data = sft, color= 'white', size=0.2,
          aes(fill = Casos_activos_confirmados, colour = Casos_activos_confirmados)) +
  
  scale_fill_gradientn(name = 'Casos activos', 
                       colours = c('#ffffd4', '#fed98e', '#fe9929', '#d95f0e', '#993404'), 
                       na.value = 'white') +
  scale_colour_gradientn(name = '', 
                         colours = c('#ffffd4', '#fed98e', '#fe9929', '#d95f0e', '#993404'), 
                         na.value = 'white',
                         guide = FALSE) + 
  geom_text(data= sft, aes(x = coords_x, y = coords_y, label = Region), hjust=0.9,
            color = "black", check_overlap = FALSE, size = 3) +
  geom_text_repel(data=sft, size= 3, color= 'black', fontface = 'bold', hjust=-0.5, segment.colour = NA,
                  aes(coords_x, coords_y, label=format(Casos_activos_confirmados, decimal.mark = ',', sep_mark = '.'))) +
  coord_sf() +
  theme_void() +
    theme(plot.subtitle = element_text(hjust = 0.5, vjust = 0.9, size = 12, face = "plain"),
        legend.position = c(0.9, 0.5),
        legend.key.size = unit(0.4, "cm"),
        legend.key.width = unit(0.4, "cm")) +

    labs(x = NULL, y = NULL, 
       title = "",
       subtitle ="Casos activos seg˙n regiÛn") +  
  xlim(-8700000, -6700000)

# Grafico Casos Activos cada 100 mil habitantes || sft_tasa100mil$tasa_activ_100mil
g2 <- ggplot() +
  geom_sf(data = sft_tasa100mil, color= 'white', size=0.2,
          aes(fill = tasa_activ_100mil, colour = tasa_activ_100mil)) +
  
  scale_fill_gradientn(name = 'Casos activos\n cada 100 mil\n habitantes', 
                       colours = c('#f0f9e8', '#bae4bc', '#7bccc4', '#43a2ca', '#0868ac'), 
                       na.value = 'white') +
  scale_colour_gradientn(name = '', 
                         colours = c('#f0f9e8', '#bae4bc', '#7bccc4', '#43a2ca', '#0868ac'), 
                         na.value = 'white',
                         guide = FALSE) +
  geom_text(data= sft_tasa100mil, aes(x = coords_x, y = coords_y, label = Region), hjust=0.9, #region name
            color = "black", check_overlap = FALSE, size = 3) +
  geom_text_repel(data=sft_tasa100mil, size= 3, color= 'black', fontface = 'bold', hjust=-0.5, segment.colour = NA, #cases count
                  aes(coords_x, coords_y, label=format(round(tasa_activ_100mil, 1), decimal.mark = ",", sep_mark = "."))) +
  coord_sf() +
  theme_void() +
  theme(plot.subtitle = element_text(hjust = 0.5, vjust = 0.9, size = 12, face = "plain"),
        legend.position = c(0.9, 0.5),
        legend.key.size = unit(0.4, "cm"),
        legend.key.width = unit(0.4, "cm")) +

  labs(x = NULL, y = NULL, 
       title = "",
       subtitle ="Casos activos cada 100 mil habitantes") +  
  xlim(-8700000, -6700000)

# Grafico  Tasa fallecidos cada 100 mil habitantes || sft_tasa_fallec$tasa_fallec_100mil
g3 <- ggplot() +
  geom_sf(data = sft_tasa_fallec, color= 'white', size=0.2,
          aes(fill = tasa_fallec_100mil, colour = tasa_fallec_100mil)) +
  
  scale_fill_gradientn(name = 'Tasa fallecidos\n cada 100 mil\n habitantes', 
                       colours = c('#f7f7f7', '#cccccc', '#969696', '#636363', '#252525'), 
                       na.value = 'white') +
  scale_colour_gradientn(name = '', 
                         colours = c('#f7f7f7', '#cccccc', '#969696', '#636363', '#252525'), 
                         na.value = 'white',
                         guide = FALSE) +
  geom_text(data= sft_tasa_fallec, aes(x = coords_x, y = coords_y, label = Region), hjust= 1, 
            color = "red", check_overlap = FALSE, size = 3) +
  
  geom_text_repel(data=sft_tasa_fallec, size= 3, color= 'red', fontface = 'bold', hjust=-0.5, segment.colour = NA,
                  aes(coords_x, coords_y, label=format(round(tasa_fallec_100mil, 1), decimal.mark = ",", sep_mark = "."))) +
  coord_sf() +
  theme_void() +
  theme(plot.subtitle = element_text(hjust = 0.5, vjust = 0.9, size = 12, face = "plain"),
        legend.position = c(0.9, 0.5),
        legend.key.size = unit(0.4, "cm"),
        legend.key.width = unit(0.4, "cm")) +
  
  labs(x = NULL, y = NULL, 
       title = "",
       subtitle ="Tasa de fallecidos cada 100 mil habitantes") +  
  xlim(-8700000, -6700000)

# Grafico combinado
t <- proc.time()
ggx <- ggarrange(g1, g2, g3 + rremove("x.text"), #lento
                 #labels = c("A", "B", "C"),
                 ncol = 3, nrow = 1)

ggx1 <- annotate_figure(ggx,
                        top = text_grob("SituaciÛn por regiones\n30 de abril de 2021", #cambiar fecha manualmente
                                        color = "black", face = "bold", size = 14),
                        bottom = text_grob("Fuente: Minsal.cl | Gob.cl", 
                                           color = "grey",
                                           hjust = 1.03, x = 1, face = "italic", size = 10))

# guardamos como imagen (opcional)
ggsave(plot = ggx1, filename = './Gr·ficos/Casos por regiones.png', 
       units = 'mm', width = 279, height = 216, dpi = 300)
proc.time() - t