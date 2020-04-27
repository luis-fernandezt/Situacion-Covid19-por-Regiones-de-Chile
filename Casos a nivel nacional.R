library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gganimate)

# casos totales
TotalesNacionales <- read_csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto5/TotalesNacionales.csv",
                              col_names = FALSE)

TotalesNacionales_t <- as.data.frame(t(TotalesNacionales[-1]))
colnames(TotalesNacionales_t) <- TotalesNacionales$X1

names(TotalesNacionales_t)
names(TotalesNacionales_t) <- c("Fecha", "Casos_nuevos", "Casos_totales", "Casos_recuperados", "Fallecidos", "Casos_activos")

TotalesNacionales_t$Fecha <- as.Date(TotalesNacionales_t$Fecha)
TotalesNacionales_t$Casos_nuevos <- as.numeric(as.character(TotalesNacionales_t$Casos_nuevos))
TotalesNacionales_t$Casos_totales <- as.numeric(as.character(TotalesNacionales_t$Casos_totales))
TotalesNacionales_t$Casos_recuperados <- as.numeric(as.character(TotalesNacionales_t$Casos_recuperados))
TotalesNacionales_t$Fallecidos <- as.numeric(as.character(TotalesNacionales_t$Fallecidos))
TotalesNacionales_t$Casos_activos <- as.numeric(as.character(TotalesNacionales_t$Casos_activos))

#color manual
cols <- c("Casos_nuevos"="darkblue",
          "Casos_totales"="red",
          "Casos_recuperados"="darkgreen",
          "Fallecidos"="darkred",
          "Casos_activos"="purple")

#plotear
ggplot(TotalesNacionales_t) +
  #point
  geom_point(size=1.5, aes(Fecha, Casos_nuevos, colour= 'Casos_nuevos'), data=TotalesNacionales_t %>%  filter(Fecha >= max(Fecha))) +
  geom_point(size=1.5, aes(Fecha, Casos_totales, colour= 'Casos_totales'), data=TotalesNacionales_t %>%  filter(Fecha >= max(Fecha))) +
  geom_point(size=1.5, aes(Fecha, Casos_recuperados, colour= 'Casos_recuperados'), data=TotalesNacionales_t %>%  filter(Fecha >= max(Fecha))) +
  geom_point(size=1.5, aes(Fecha, Fallecidos, colour= 'Fallecidos'), data=TotalesNacionales_t %>%  filter(Fecha >= max(Fecha))) +
  geom_point(size=1.5, aes(Fecha, Casos_activos, colour= 'Casos_activos'), data=TotalesNacionales_t %>%  filter(Fecha >= max(Fecha))) +
  #lineas
  geom_line(size=1, aes(Fecha, Casos_nuevos, colour= 'Casos_nuevos')) +
  geom_line(size=1, aes(Fecha, Casos_totales, colour= 'Casos_totales')) +
  geom_line(size=1, aes(Fecha, Casos_recuperados, colour= 'Casos_recuperados')) +
  geom_line(size=1, aes(Fecha, Fallecidos, colour= 'Fallecidos')) +
  geom_line(size=1, aes(Fecha, Casos_activos, colour= 'Casos_activos')) +
  # etiquetas
  geom_text(aes(Fecha, Casos_nuevos, label=Casos_nuevos),  size = 3, hjust = -0.2, data=TotalesNacionales_t %>%  filter(Fecha >= max(Fecha))) +
  geom_text(aes(Fecha, Casos_totales, label=Casos_totales),  size = 3, hjust = -0.2, data=TotalesNacionales_t %>%  filter(Fecha >= max(Fecha))) +
  geom_text(aes(Fecha, Casos_recuperados, label=Casos_recuperados), size = 3,  hjust = -0.2, data=TotalesNacionales_t %>%  filter(Fecha >= max(Fecha))) +
  geom_text(aes(Fecha, Fallecidos, label=Fallecidos),  size = 3, hjust = -0.2, data=TotalesNacionales_t %>%  filter(Fecha >= max(Fecha))) +
  geom_text(aes(Fecha, Casos_activos, label=Casos_activos),  size = 3, hjust = -0.2, data=TotalesNacionales_t %>%  filter(Fecha >= max(Fecha))) +
  #ajustes visuales
  theme_minimal() +
  ggtitle("Evolución de Casos Covid-19 en Chile") +
  #ejes x e y
  ylab("Número de casos") +
  xlab("Fecha") +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(trans = 'log10') +
  #leyenda
  scale_colour_manual(name="",values=cols) +
  theme(legend.position = 'right')