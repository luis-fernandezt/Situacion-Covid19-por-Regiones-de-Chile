## Situación Covid19 en Chile
En este repositorio puede encontrar varios scrips en lenguaje R, para visualizar la situación y evolución del Covid19 en Chile, con data disponible y descargable desde el repositorio del Ministerio Ciencias [github.com/MinCiencia](https://github.com/MinCiencia). Respecto a los polígonos de Chile, estan disponibles en formato shp en la mapoteca de [bcn.cl](https://www.bcn.cl/siit/mapas_vectoriales). Los scrips son los siguientes:

* [Casos por regiones.R](https://github.com/luis-fernandezt/Situacion-Covid19-por-Regiones-de-Chile/blob/master/Casos%20por%20regiones.R) - Script con código orientado a contruir un mapa con las 16 regiones de chile, estilo choropleth map. Luego se guardan de forma independiente los casos activos, así como la tasa de incidencia de casos activos y de personas fallecidas cada 100 mil  habitantes. Para este mapa se requirió el "Producto4" del repositorio de MinCiencia.

![grafico2](https://raw.githubusercontent.com/luis-fernandezt/Situacion-Covid19-por-Regiones-de-Chile/master/Gr%C3%A1ficos/Casos%20por%20regiones.png)

* [Evolucion de casos activos de covid 19 en la macro zona sur de chile.R](https://github.com/luis-fernandezt/Situacion-Covid19-por-Regiones-de-Chile/blob/master/Casos%20a%20nivel%20nacional.R) - Grafico construido agregando los datos correspondientes al "Producto19" disponible en MinCiencia.

![grafico1](https://raw.githubusercontent.com/luis-fernandezt/Situacion-Covid19-por-Regiones-de-Chile/master/Gr%C3%A1ficos/Activos%20macro%20sur.png)

* [Casos activos r10.R](https://github.com/luis-fernandezt/Situacion-Covid19-por-Regiones-de-Chile/blob/master/Casos%20r10.R) - Apertura enfocada en visualizar la situación particular de las comunas afectadas con casos activos de covid 19 en la Región de Los Lagos. Tanto para la décima, así como para la decimocuarta región se utilizo el "Producto19" del repositorio de MinCiencia. El código tambien permite estimar la tasa de incidencia de casos activos cada 100 mil habitantes por comuna.

![graficor10](https://raw.githubusercontent.com/luis-fernandezt/Situacion-Covid19-por-Regiones-de-Chile/master/Gr%C3%A1ficos/Casos%20r10.png)

* [Casos r14.R](https://github.com/luis-fernandezt/Situacion-Covid19-por-Regiones-de-Chile/blob/master/Casos%20r14.R) - Apertura enfocada en visualizar la situación particular de las comunas afectadas con casos activos de covid 19 en la Región de Los Ríos.  El código tambien permite estimar la tasa de incidencia de casos activos cada 100 mil habitantes por comuna. 

![graficor10](https://raw.githubusercontent.com/luis-fernandezt/Situacion-Covid19-por-Regiones-de-Chile/master/Gr%C3%A1ficos/Casos%20r14.png)

Las imágenes fueron mejoradas utilizando Adobe Illustrator*
Fuente: Elaboración propia.

#### **Versión de Rstudio:**
"R version 3.6.3 (2020-02-29)"
