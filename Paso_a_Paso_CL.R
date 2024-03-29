# PLAN PASO A PASO
# cargamos librer�as
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

# paso 2. cargamos poligonos dispnibles en: https://www.bcn.cl/siit/mapas_vectoriales
shp <- shapefile('./shp/comunas.shp')
shp@data$Region <- iconv(shp@data$Region, from = 'UTF-8', to = 'latin1')
shp@data$Provincia <- iconv(shp@data$Provincia, from = 'UTF-8', to = 'latin1')
shp@data$Comuna <- iconv(shp@data$Comuna, from = 'UTF-8', to = 'latin1')
#shp <- shp[shp@data$Region != "Zona sin demarcar" ,  ]

shp_sf <- aggregate(shp, 'Comuna') #agregamos por comuna
shp_sf <- st_as_sf(shp_sf)

shp_reg <- aggregate(shp, 'Region') #agregamos por region (lento)
shp_reg <- st_as_sf(shp_reg)

shp_sf$Comuna <- c("Marchihue",
                   "Codegua",
                   "Coinco",
                   "Coltauco",
                   "Cholchol",
                   "Cunco",
                   "Freire",
                   "Galvarino",
                   "Graneros",
                   "Malloa",
                   "Nueva Imperial",
                   "Perquenco",
                   "Pitrufqu�n",
                   "Temuco",
                   "Teodoro Schmidt",
                   "Vilc�n",
                   "Angol",
                   "Collipulli",
                   "Curacaut�n",
                   "Ercilla",
                   "Los Sauces",
                   "Lumaco",
                   "Pur�n",
                   "Renaico",
                   "Traigu�n",
                   "Victoria",
                   "San Joaqu�n",
                   "Peumo",
                   "Pichidegua",
                   "San Pablo",
                   "La Uni�n",
                   "Los Lagos",
                   "M�fil",
                   "Paillaco",
                   "Curepto",
                   "Empedrado",
                   "San Miguel",
                   "Pencahue",
                   "Rancagua",
                   "Requ�noa",
                   "Mostazal",
                   "San Vicente",
                   "Nancagua",
                   "Peralillo",
                   "Placilla",
                   "Pumanque",
                   "Andacollo",
                   "San Ram�n",
                   "Gorbea",
                   "Lautaro",
                   "Puqueld�n",
                   "Fresia",
                   "Llanquihue",
                   "Osorno",
                   "Purranque",
                   "Laguna Blanca",
                   "Zona sin demarcar",
                   "Independencia",
                   "Rinconada",
                   "Cabildo",
                   "Petorca",
                   "La Cisterna",
                   "Panquehue",
                   "Ca�ete",
                   "Contulmo",
                   "Curanilahue",
                   "Lebu",
                   "Los �lamos",
                   "Pe�alol�n",
                   "Olmu�",
                   "Cartagena",
                   "Catemu",
                   "Llaillay",
                   "San Felipe",
                   "Santa Maria",
                   "Villa Alemana",
                   "Laja",
                   "Los �ngeles",
                   "Mulch�n",
                   "Nacimiento",
                   "Linares",
                   "Longav�",
                   "Providencia",
                   "Negrete",
                   "Quilaco",
                   "Quilleco",
                   "La Reina",
                   "R�o Ib��ez",
                   "San Rosendo",
                   "Tucapel",
                   "Calera de Tango",
                   "Chile Chico",
                   "Torres del Paine",
                   "Primavera",
                   "Chiguayante",
                   "Concepci�n",
                   "Coronel",
                   "Florida",
                   "Hualqui",
                   "Lota",
                   "San Pedro de la Paz",
                   "Santa Juana",
                   "San Gregorio",
                   "Porvenir",
                   "Cami�a",
                   "Colchane",
                   "Putre",
                   "Maria Elena",
                   "Talca",
                   "Colina",
                   "Santiago",
                   "Calama",
                   "Lampa",
                   "Pirque",
                   "Bulnes",
                   "Chill�n Viejo",
                   "Chill�n",
                   "Yumbel",
                   "Lonquimay",
                   "Puente Alto",
                   "Huechuraba",
                   "Yungay",
                   "Cabrero",
                   "Antuco",
                   "Quill�n",
                   "Alto Biob�o",
                   "San Bernardo",
                   "Curacav�",
                   "Maria Pinto",
                   "Cerrillos",
                   "Carahue",
                   "Tolt�n",
                   "Loncoche",
                   "Lanco",
                   "Cerro Navia",
                   "Vitacura",
                   "Conchal�",
                   "El Bosque",
                   "R�o Verde",
                   "Punta Arenas",
                   "Estaci�n Central",
                   "Curarrehue",
                   "Panguipulli",
                   "Puyehue",
                   "R�o Bueno",
                   "La Florida",
                   "La Granja",
                   "La Pintana",
                   "Cocham�",
                   "Lago Verde",
                   "Coyhaique",
                   "Cisnes",
                   "Tortel",
                   "Natales",
                   "O'Higgins",
                   "Cochrane",
                   "Las Condes",
                   "Lo Barnechea",
                   "Lo Espejo",
                   "Lo Prado",
                   "Macul",
                   "Ancud",
                   "Quell�n",
                   "Queil�n",
                   "Chonchi",
                   "Puerto Montt",
                   "R�o Negro",
                   "Castro",
                   "Dalcahue",
                   "Quemchi",
                   "Saavedra",
                   "Maip�",
                   "�u�oa",
                   "Pedro Aguirre Cerda",
                   "Tir�a",
                   "Hualp�n",
                   "Talcahuano",
                   "Penco",
                   "Chanco",
                   "Pelluhue",
                   "Constituci�n",
                   "Licant�n",
                   "Vichuqu�n",
                   "Paredones",
                   "Pudahuel",
                   "Quilicura",
                   "Pichilemu",
                   "Litueche",
                   "Navidad",
                   "Quinta Normal",
                   "Recoleta",
                   "Santo Domingo",
                   "San Antonio",
                   "El Tabo",
                   "El Quisco",
                   "Pozo Almonte",
                   "Alto del Carmen",
                   "San Carlos",
                   "Tom�",
                   "Algarrobo",
                   "Casablanca",
                   "Valparaiso",
                   "Renca",
                   "Vi�a del Mar",
                   "Con Con",
                   "Quintero",
                   "Puchuncav�",
                   "El Monte",
                   "Zapallar",
                   "Papudo",
                   "La Ligua",
                   "Los Vilos",
                   "Canela",
                   "Ovalle",
                   "Coquimbo",
                   "La Serena",
                   "Freirina",
                   "Huasco",
                   "Copiap�",
                   "Caldera",
                   "Huara",
                   "Camarones",
                   "San Juan de la Costa",
                   "Arica",
                   "Mariquina",
                   "Arauco",
                   "Tocopilla",
                   "Padre Hurtado",
                   "Pe�aflor",
                   "Antofagasta",
                   "Mejillones",
                   "Taltal",
                   "Cha�aral",
                   "Cauquenes",
                   "Juan Fern�ndez",
                   "Isla de Pascua",
                   "Cabo de Hornos",
                   "Talagante",
                   "Timaukel",
                   "Guaitecas",
                   "Alto Hospicio",
                   "Sierra Gorda",
                   "Combarbal�",
                   "Punitaqui",
                   "Paihuano",
                   "Illapel",
                   "Limache",
                   "Quillota",
                   "Ais�n",
                   "Calera",
                   "Nogales",
                   "Calle Larga",
                   "La Estrella",
                   "Paine",
                   "Calbuco",
                   "Chait�n",
                   "Corral",
                   "Valdivia",
                   "Iquique",
                   "La Higuera",
                   "Los Muermos",
                   "Maull�n",
                   "Quinchao",
                   "Curaco de V�lez",
                   "Treguaco",
                   "R�nquil",
                   "Portezuelo",
                   "Cobquecura",
                   "Quirihue",
                   "Coelemu",
                   "Ninhue",
                   "Do�ihue",
                   "Olivar",
                   "Isla de Maipo",
                   "Buin",
                   "Rengo",
                   "Santa Cruz",
                   "Palmilla",
                   "R�o Claro",
                   "Pelarco",
                   "San Javier",
                   "Villa Alegre",
                   "Yerbas Buenas",
                   "Maule",
                   "San Rafael",
                   "Santa B�rbara",
                   "Villarrica",
                   "Puc�n",
                   "Padre Las Casas",
                   "Puerto Octay",
                   "Frutillar",
                   "La Cruz",
                   "Sagrada Familia",
                   "Retiro",
                   "Quinta de Tilcoco",
                   "General Lagos",
                   "San Pedro de Atacama",
                   "Tierra Amarilla",
                   "Putaendo",
                   "Monte Patria",
                   "R�o Hurtado",
                   "Salamanca",
                   "San Esteban",
                   "Los Andes",
                   "San Jos� de Maipo",
                   "Curic�",
                   "Machal�",
                   "Molina",
                   "Colb�n",
                   "San Clemente",
                   "Melipeuco",
                   "Lago Ranco",
                   "Futrono",
                   "Puerto Varas",
                   "Hualaihu�",
                   "Palena",
                   "Futaleuf�",
                   "Ollag�e",
                   "Pica",
                   "Diego de Almagro",
                   "Vallenar",
                   "Vicu�a",
                   "Tiltil",
                   "Hijuelas",
                   "Quilpu�",
                   "Melipilla",
                   "Las Cabras",
                   "San Pedro",
                   "Alhu�",
                   "Chimbarongo",
                   "Lolol",
                   "Huala��",
                   "Rauco",
                   "Ch�pica",
                   "Teno",
                   "Romeral",
                   "San Fernando",
                   "Parral",
                   "Pinto",
                   "San Ignacio",
                   "Pemuco",
                   "San Nicol�s",
                   "San Fabi�n",
                   "�iqu�n",
                   "El Carmen",
                   "Coihueco")
                   

#cargamos pol�gono con las Masas Lacustres #### 
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




# paso 3. Importamos plan paso a paso por comunas
Hoja_1 <- read_csv("Paso/Hoja 1.csv")
class(Hoja_1)
Hoja_1 <- data.frame(Hoja_1)

Paso1 <- vector()
Paso1[Hoja_1$Paso == 1] <- 1
Paso1[Hoja_1$Paso == 2] <- 2
Paso1[Hoja_1$Paso == 3] <- 3
Paso1[Hoja_1$Paso == 4] <- 4
Paso1[Hoja_1$Paso == 5] <- 5

Hoja_1$Paso1 <- as.factor(Paso1)
levels(Hoja_1$Paso1)
levels(Hoja_1$Paso1) <- c( "Transici�n", "Preparaci�n", "Apertura Inicial", "Apertura Avanzada")
names(Hoja_1)

mps_paso <- Hoja_1 %>% 
  group_by(COMUNA) %>% #agrupamos casos totales
  dplyr::summarise(Paso1) %>%  
  ungroup()

sft_paso <- st_as_sf(shp_sf) %>% 
  inner_join(., y = mps_paso, by = c('Comuna' = 'COMUNA'))

# ubicamos los centroides
sft_paso <- sft_paso %>% mutate(centroid = map(geometry, st_centroid), 
                                  coords = map(centroid, st_coordinates), 
                                  coords_x = map_dbl(coords, 1), 
                                  coords_y = map_dbl(coords, 2))

shp_reg <- shp_reg %>% mutate(centroid = map(geometry, st_centroid), 
                                coords = map(centroid, st_coordinates), 
                                coords_x = map_dbl(coords, 1), 
                                coords_y = map_dbl(coords, 2))


#4. Ploteamos
colors <- c(#"Cuarentena" = "#f75c5c", 
            "Transici�n" = "#ffbf00", 
            "Preparaci�n" = "#fff200",
            "Apertura Inicial" = "#3389d0",
            "Apertura Avanzada" = "#a7d1f2")

gg <- 
  ggplot() +
  geom_sf(data = sft_paso, color= 'transparent', size=0.001, aes(fill = Paso1, colour = Paso1)) +
  scale_fill_manual(values = colors, name= "") +
  
  geom_sf(data = Lago, color= 'transparent', fill = '#D6F1FF', alpha =0.8) +
  geom_sf(data = Glaciar, color= 'transparent', fill = '#D6F1FF', alpha =0.8) +
  geom_sf(data = Ventisquero, color= 'transparent', size=0.5, fill = '#D6F1FF', alpha =0.8) +
  
  geom_sf(data = sft_paso, color= 'white', size=0.001, fill = 'transparent') +
  
  geom_text_repel(data=shp_reg, size= 3.5, color= 'black', fontface = 'bold',  segment.color = NA,
                  hjust = 0.5, vjust  = 0.51, aes(coords_x, coords_y, label= Region)) +
  
  coord_sf() +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic"),
        legend.position = c(0.5, 0.02), #"none" #c(0.5, 0.97) 
        legend.direction = "horizontal",
        legend.key.size = unit(0.5, "cm"), #alto leyenda
        legend.key.width = unit(0.5, "cm"), #ancho leyenda 
        legend.text= element_text(hjust = 0, size = 10, face = "bold"),
        legend.background = element_rect(fill = alpha('white', 0),colour = alpha('white', 0.0))) + 
  
  labs(x = NULL, 
       y = NULL, 
       title = "Etapa del Plan Paso a Paso, por Comunas",
       subtitle = as.character(max(producto19$Fecha), format="%B %d, %Y"), #Comentar si arroja error
       caption = "Fuente: Minsal.cl | gob.cl") +
  
  annotation_north_arrow(location = "tr", 
                         which_north = "true", 
                         pad_x = unit(0.1, "cm"), 
                         pad_y = unit(0.1, "cm"),
                         style = north_arrow_fancy_orienteering) +
  
  xlim(-9000000, -7000000)


ggsave(plot = gg, filename = './Gr�ficos/Paso_a_Paso.png', 
       units = 'mm', width = 216, height = 600, dpi = 300)