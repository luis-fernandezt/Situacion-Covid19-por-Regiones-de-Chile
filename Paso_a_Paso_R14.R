# Script guardado y codificado en Latin1 para evitar pérdida de caracteres
# paso 1.cargamos librerias ####
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

# paso 2. cargamos poligonos de La Región de Los Lagos ####
# polígonos dispnibles en: https://www.bcn.cl/siit/mapas_vectoriales

shp <- shapefile('./shp/comunas.shp')
shp@data$Region <- iconv(shp@data$Region, from = 'UTF-8', to = 'latin1')
shp@data$Provincia <- iconv(shp@data$Provincia, from = 'UTF-8', to = 'latin1')
shp@data$Comuna <- iconv(shp@data$Comuna, from = 'UTF-8', to = 'latin1')

r14 <- shp[shp@data$Region=="Región de Los Ríos" ,  ]

comunas_sf <- aggregate(r14, 'Comuna') #agregamos region por comuna
comunas_sf <- st_as_sf(comunas_sf)
comunas_paso <- st_as_sf(comunas_sf)

# guardar poligono borde regional como .shp
#r10_sf <- aggregate(r10, 'Region') #agregamos por region
#r10_sf <- st_as_sf(r10_sf)
#plot(r10_sf)
# st_write(r10_sf, dsn = 'shp', layer = 'shp_r10', driver = 'ESRI Shapefile')

comunas_sf$Comuna <- c("La Union", "Los Lagos", "Mafil", "Paillaco", "Lanco", "Panguipulli", "Rio Bueno",
                       "Mariquina", "Corral", "Valdivia", "Lago Ranco", "Futrono")

comunas_paso$Comuna <- c("La Unión", "Los Lagos", "Máfil", "Paillaco", "Lanco", "Panguipulli", "Río Bueno",
                         "Mariquina", "Corral", "Valdivia", "Lago Ranco", "Futrono")



# paso 3. cargamos polígono con las Masas Lacustres #### 
#objetivo: mostrar grandes masas de agua como lagos en el mapa
shp_masas_lacustres <- shapefile('./shp/Masas_Lacustres/masas_lacustres.shp')
shp_masas_lacustres@data$Nombre <- iconv(shp_masas_lacustres@data$Nombre, from = 'UTF-8', to = 'latin1')
shp_masas_lacustres@data$Tipo <- iconv(shp_masas_lacustres@data$Tipo, from = 'UTF-8', to = 'latin1')

#extraemos por tipo masa de agua
Lago <- subset(shp_masas_lacustres, Tipo=="Lago")
Glaciar <- subset(shp_masas_lacustres, Tipo=="Glaciar")
Ventisquero <- subset(shp_masas_lacustres, Tipo=="Ventisquero")

Lago <- aggregate(Lago, "Nombre") #agregamos por nombre de masa de agua
Lago <- st_as_sf(Lago)
Glaciar <- aggregate(Glaciar, "Nombre")
Glaciar <- st_as_sf(Glaciar)
Ventisquero <- aggregate(Ventisquero, "Nombre")
Ventisquero <- st_as_sf(Ventisquero)

# paso 3. cargamos archivo *.csv "producto 19" agregada por comunas desde repositorio MinCiencia ####
producto19 <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto19/CasosActivosPorComuna_std.csv")
producto19 <- as.data.frame(producto19)
names(producto19) <- c("Region", "Codigo_region", "Comuna", "Codigo_comuna", "Poblacion", "Fecha", "Casos_activos")
max(producto19$Fecha) #verificar que es el ultimo reporte

# agregamos columna con tasa de incidencia de casos activos:
# (casos activos comuna / poblacion comunal) x 100.000
producto19$Tasa_cont_100mil <- (producto19$Casos_activos/producto19$Poblacion)*100000
names(producto19)

# Importamos plan paso a paso por comunas
Hoja_1 <- read_csv("Paso/Hoja 1.csv", col_types = cols(Paso = col_number()))
Hoja_1 <- read_excel("Paso/Hoja 1.xlsx")


#paso4. preparacion de la base de datos ####

# creamos df para serie de tiempo grandes comunas de la macro zona sur
Temuco <- producto19 %>%  filter(Codigo_comuna %in% c("09101"))
Valdivia <- producto19 %>%  filter(Codigo_comuna %in% c("14101"))
Osorno <- producto19 %>%  filter(Codigo_comuna %in% c("10301"))
PuertoMontt <- producto19 %>%  filter(Codigo_comuna %in% c("10101"))
sdt_comunas <- rbind(Temuco, Valdivia, Osorno,  PuertoMontt)

#removemos
rm(Temuco)
rm(Valdivia)
rm(Osorno)
rm(PuertoMontt)

# para mapas
# filtramos por region y ultima fecha reportada
tbl <- producto19 %>%  filter(Codigo_region == 14)
tbl <- tbl %>% filter(Fecha == max(Fecha)) 
max(tbl$Fecha)

# fortificamos la data
mps_activo <- tbl %>% 
  group_by(Comuna) %>% #agrupamos casos totales
  dplyr::summarise(Casos_activos = max(Casos_activos)) %>%  
  ungroup()

mps_tc <- tbl %>% 
  group_by(Comuna) %>% #agrupamos tasa de activos cada 100 mil hab.
  dplyr::summarise(Tasa_cont_100mil = max(Tasa_cont_100mil)) %>% 
  ungroup()

mps_paso <- Hoja_1 %>% 
  group_by(COMUNA) %>% #agrupamos casos totales
  dplyr::summarise(ESTADO) %>%  
  ungroup()

# unimos la columna "Comuna" del df producto 19 con en shp agregado por "Comuna"
sft_casos <- st_as_sf(comunas_sf) %>% 
  inner_join(., y = mps_activo, by = c('Comuna' = 'Comuna'))
sft_casos <- sft_casos %>% filter(Casos_activos > 0)

sft_tasa <- st_as_sf(comunas_sf) %>% 
  inner_join(., y = mps_tc, by = c('Comuna' = 'Comuna'))
sft_tasa <- sft_tasa %>% filter(Tasa_cont_100mil > 0)

sft_paso <- st_as_sf(comunas_paso) %>% 
  inner_join(., y = mps_paso, by = c('Comuna' = 'COMUNA'))

# ubicamos los centroides
sft_casos <- sft_casos %>% mutate(centroid = map(geometry, st_centroid), 
                                  coords = map(centroid, st_coordinates), 
                                  coords_x = map_dbl(coords, 1), 
                                  coords_y = map_dbl(coords, 2))

sft_tasa <- sft_tasa %>% mutate(centroid = map(geometry, st_centroid), 
                                  coords = map(centroid, st_coordinates), 
                                  coords_x = map_dbl(coords, 1), 
                                  coords_y = map_dbl(coords, 2))

#paso 5 -  ploteamos ####

#Grafico 1. sdt casos activos grandes comunas macro zona sur
gg1 <- ggplot(sdt_comunas, aes(x=Fecha, y=Tasa_cont_100mil, group=Comuna, color=Comuna)) +
  geom_line(size = 1.3, data = sdt_comunas) +
  geom_point(size = 1.7, data = sdt_comunas) +
  scale_colour_viridis_d(name = "", option = 'B', begin = 0, end = 0.8, direction = -1, alpha=0.9) +
  geom_text(aes(label=format(round(Tasa_cont_100mil, 1), decimal.mark = ",", sep_mark = ".")), 
            size= 6, hjust = -0.1, data = sdt_comunas 
            %>% filter(Fecha == max(Fecha)), show.legend = F) +
  theme_classic() +
  theme(legend.position = c(0.2,0.8), 
        plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic"),
        legend.text = element_text(color = "black", size = 14)) +
  scale_x_date(date_breaks = '3 day', date_labels = "%b %d") +
  scale_y_continuous(trans = 'sqrt', breaks = c(1, 5, 10, 20, 50, 100, 150, 200)) +
  
  geom_hline(aes(yintercept = 50), color="red", linetype = 'dashed') +
  
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Fecha", 
       y = "Tasa de incidencia de casos activos", 
       title = "Tasa de incidencia Casos Activos de Covid-19\nTemuco, Valdivia, Osorno y Puerto Montt", 
       subtitle = "28 de septiembre de 2020", 
       caption = "Fuente: Minsal.cl, Gob.cl")

gg1

# Situación Comunal con Casos Activos de covid-19 y etapa del Plan Paso a Paso ####

colors <- c("Cuarentena" = "#f75c5c", 
            "Transición" = "#ffbf00", 
            "Preparación" = "#fff200",
            "Apertura Inicial" = "#3389d0",
            "Apertura avanzada" = "#a7d1f2")

gg2 <- 
ggplot() +
  geom_sf(data = sft_paso, color= 'transparent', size=0.5, aes(fill = ESTADO, colour = ESTADO)) +
  scale_fill_manual(values = colors, name= "")+

  geom_sf(data = Lago, color= 'transparent', fill = '#D6F1FF', alpha =0.8) +
  geom_sf(data = Glaciar, color= 'transparent', fill = '#D6F1FF', alpha =0.8) +
  geom_sf(data = Ventisquero, color= 'transparent', size=0.5, fill = '#D6F1FF', alpha =0.8) +
  
  geom_sf(data = comunas_sf, color= 'white', size=0.5, fill = 'transparent') +
  
  geom_text_repel(data=sft_tasa, size= 5, color= 'black', fontface = 'bold',  segment.color = NA,
                  hjust = 0.5, vjust  = 0.51, aes(coords_x, coords_y, label= Comuna)) +
  
  geom_label_repel(data=sft_tasa, size= 5, color= 'white', fontface = 'bold', fill = 'red', segment.color = NA,
                   hjust = 0.5, vjust  = 0.49, aes(coords_x, 
                                                   coords_y, 
                                                   label=format(round(Tasa_cont_100mil, 1), 
                                                                decimal.mark = ",", 
                                                                sep_mark = "."))) + 
  coord_sf() +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic"),
        legend.position = c(0.5, 0.05), #"none" #c(0.5, 0.97) 
        legend.direction = "horizontal",
        legend.key.size = unit(0.5, "cm"), #alto leyenda
        legend.key.width = unit(0.5, "cm"), #ancho leyenda 
        legend.text= element_text(hjust = 0, size = 14, face = "bold"),
        legend.background = element_rect(fill = alpha('white', 0),colour = alpha('white', 0.0))) + 
  
  labs(x = NULL, 
       y = NULL, 
       title = "Región de Los Ríos,\nTasa de Incidencia de Casos Activos por comunas\ny etapa del Plan Paso a Paso", 
       subtitle = "28 de septiembre de 2020", 
       caption = "Fuente: Minsal.cl | gob.cl   ") +
  
  annotation_north_arrow(location = "tr", 
                         which_north = "true", 
                         pad_x = unit(0.1, "cm"), 
                         pad_y = unit(0.1, "cm"),
                         style = north_arrow_fancy_orienteering) +
  
  ylim(-4990000, -4770000) +
  xlim(-8200000, -7970000)

gg2
