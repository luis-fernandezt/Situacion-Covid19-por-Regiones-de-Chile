## Situación Covid19 en Chile
En este repositorio puede encontrar varios scrips en lenguaje R, para visualizar la situación y evolución del Covid19 en Chile, con data disponible y descargable desde el repositorio del Ministerio Ciencias [github.com/MinCiencia](https://github.com/MinCiencia). Respecto a los polígonos de Chile, estan disponibles en formato shp en la mapoteca de [bcn.cl](https://www.bcn.cl/siit/mapas_vectoriales). Los scrips son los siguientes:

* [Casos a nivel nacional.R](https://github.com/luis-fernandezt/Situacion-Covid19-por-Regiones-de-Chile/blob/master/Casos%20a%20nivel%20nacional.R) - Script con código e instrucciones para construir un gráfico estilo serie de tiempo que incluye: los casos nuevos confirmados, totales o acumulados, recuperados, fallecidos a nivel nacional y activos según fecha de diagnóstico, reportados diariamente por el Ministerio de Salud. Corresponde al "Producto5" disponible en MinCiencia.
![grafico1](https://raw.githubusercontent.com/luis-fernandezt/Situacion-Covid19-por-Regiones-de-Chile/master/Gr%C3%A1ficos/Casos%20a%20nivel%20nacional.png)

* [Casos por regiones.R](https://github.com/luis-fernandezt/Situacion-Covid19-por-Regiones-de-Chile/blob/master/Casos%20por%20regiones.R) - Script con código orientado a contruir un mapa con las 16 regiones de chile, estilo choropleth map. Luego se guardan de forma independiente los casos activos, así como la tasa de contagiados y de personas fallecidas cada 100 mil  habitantes. Para este mapa se requirió el "Producto4" del repositorio de MinCiencia.
![grafico2](https://raw.githubusercontent.com/luis-fernandezt/Situacion-Covid19-por-Regiones-de-Chile/master/Gr%C3%A1ficos/Casos%20por%20regiones.png)

* [Casos r10.R](https://github.com/luis-fernandezt/Situacion-Covid19-por-Regiones-de-Chile/blob/master/Casos%20r10.R) - Apertura enfocada en visualizar la situación particular de las comunas afectadas con casos de covid en la Región de Los Lagos, así como la tasa de contagiados cada 100 mil habitantes. Tanto para la décima, así como para la decimocuarta región se utilizo el "Producto1" del repositorio de MinCiencia. 
![graficor10](https://raw.githubusercontent.com/luis-fernandezt/Situacion-Covid19-por-Regiones-de-Chile/master/Gr%C3%A1ficos/Casos%20r10.png)

* [Casos r14.R](https://github.com/luis-fernandezt/Situacion-Covid19-por-Regiones-de-Chile/blob/master/Casos%20r14.R) - Apertura enfocada en visualizar la situación particular de las comunas afectadas con casos de covid en la Región de Los Ríos, así como la tasa de contagiados cada 100 mil habitantes. 
![graficor10](https://raw.githubusercontent.com/luis-fernandezt/Situacion-Covid19-por-Regiones-de-Chile/master/Gr%C3%A1ficos/Casos%20r14.png)

Las imágenes fueron mejoradas utilizando Adobe Illustrator*
Fuente: Elaboración propia.

#### **Versión de Rstudio:**
"R version 3.6.3 (2020-02-29)"
