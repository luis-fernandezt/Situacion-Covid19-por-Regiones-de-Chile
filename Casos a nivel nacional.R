# Script guardado y codificado en Latin1 para evitar pérdida de caracteres
# cargamos librerías
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gganimate) 

# cargamos csv con los casos desde repositorio de MinCiencia
# producto5/TotalesNacionales_std.csv
TotalesNacionales_std <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto5/TotalesNacionales_std.csv",
                              col_names = T)
TotalesNacionales_std <- as.data.frame(TotalesNacionales_std)
names(TotalesNacionales_std)
# [1] "Dato"  "Fecha" "Total"

gg1 <- 
ggplot(TotalesNacionales_std, aes(x=Fecha, y=Total, group=Dato, color=Dato)) +
  # puntos
  geom_line(size = 1.1, data = TotalesNacionales_std) +
  # líneas
  geom_point(size=1.5, data = TotalesNacionales_std) +
  # etiquetas para el último dato
  geom_text(aes(Fecha, Total, label=Total),  size = 3, hjust = -0.2, 
            data=TotalesNacionales_std %>%  filter(Fecha >= max(Fecha))) +
  #ajustes visuales
  theme_classic() +
  #leyenda y eje x
  theme(legend.position = 'right',
        plot.title = element_text(hjust = 0, size = 14, face = "bold")) +
  #ejes x e y
        scale_x_date(date_breaks = '2 day', date_labels = "%b %d") +
        scale_y_continuous(trans = 'log10') +
  #nombres de títulos, ejes y leyenda
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle("Evolución de Casos Covid-19 en Chile") +
        ylab("Cantidad de casos positivos") +
        xlab("Fecha") +
        labs(caption = "Autor: L.Fernández - Data: Minsal.cl, github.com/MinCiencia") +
  scale_colour_viridis_d(name = "Casos", option = 'A', begin = 0,end = 0.8, direction = -1,alpha=0.8)

gg1
