##########################################################################################
####### Aves spp. data to occurrence presence: Ara militaris project ####################
#####Developed by: David A. Prieto-Torres (FES-Iztacala, UNAM)############################
library(rgbif)
library(TeachingDemos)
library(dismo)
library(biomod2)
library(sp)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(usdm)
library(ENMeval)
library(foreign)
library(spocc)
library(corrplot)
library(usdm)
library(XML)
library(dplyr)
library(reshape)
library(CoordinateCleaner)
library(readr)


rm(list=ls()) #eliminate the files in the environmental space
################################################################################
###########Step II: Dowloand of GBIF occurrence data############################
################################################################################
#1.To obtain the presence data
species1<- gbif("Ara", "militaris*", geo=FALSE)##permite vincularse a la p?gina GBIF y descargar datos, para ello es necesario escribir el nombre de la especie separando el g?nero del ep?teto (el aster?sco es para no descartar datos por las "subspecies").

#2.Limpieza automatica b?sica de los datos: quitar observaciones con informacion incompleta
data1 <- subset(species1, !is.na(lon) & !is.na(lat))#quita todos los datos que no tienen coordenadas geograficas
data2 <- subset(data1, !is.na(year))#quita todos los datos que no tienen informaci?n del a?o
data3 <- subset(data2, !is.na(adm1))#quita todos los datos que no tienen informaci?n del estado/localidad de colecta

gbif_download2 = data3 %>%
  setNames(tolower(names(.))) %>% # set lowercase column names to work with CoordinateCleaner
  filter(occurrencestatus  == "PRESENT") %>%
  filter(!basisofrecord %in% c("FOSSIL_SPECIMEN","LIVING_SPECIMEN")) %>%
  filter(!establishmentmeans %in% c("MANAGED", "INTRODUCED", "INVASIVE", "NATURALISED")) %>%
  filter(year >= 1970) %>% 
  filter(coordinateuncertaintyinmeters < 1000 | is.na(coordinateuncertaintyinmeters)) %>%
  filter(!lat == 0 | !lon == 0) %>%
  glimpse() # look at results of pipeline

data3<- gbif_download2##to obtain the occurrence data from gbif website

#3.Crear un archivo en formato ".CSV" con la informaci?n b?sica necesaria para cada registro, incluyendo: "c?digo GBIF", "instituci?n de procedencia", "n?mero de cat?logo/colecci?n", "taxonom?a" y "coordenadas geogr?ficas", as? como "a?o", "pa?s" y "regi?n" de colecta. NOTA: para este ejercicio solo se estan seleccionando 15 columnas de las 154 disponibles en GBIF, no obstante esto puede modificarse de acuerdo a la necesidades y preferencias de cada investigador
gbifID<-data3$gbifid###seleciona la informaci?n sobre el "c?digo GBIF" que corresponde a cada dato.
institutionCode<-data3$institutioncode###seleciona la informaci?n sobre el "instituci?n de procedencia" para cada dato.
catalogNumber<-data3$catalognumber###seleciona la informaci?n sobre el "n?mero de cat?logo" de cada dato.
kingdom<-data3$kingdom###seleciona la informaci?n tax?nomica del "Reino" de mi especie de inter?s.
phylum<-data3$phylum###seleciona la informaci?n tax?nomica del "Phylum" de mi especie de inter?s.
class<-data3$class###seleciona la informaci?n tax?nomica del "Clase" de mi especie de inter?s.
order<-data3$order###seleciona la informaci?n tax?nomica del "Orden" de mi especie de inter?s.
family<-data3$family###seleciona la informaci?n tax?nomica del "Familia" de mi especie de inter?s.
genus<-data3$genus###seleciona la informaci?n tax?nomica del "G?nero" de mi especie de inter?s.
species<-data3$species###seleciona la informaci?n tax?nomica del "Especie" de mi especie de inter?s.
lat<-data3$lat###seleciona la informaci?n sobre la "Latitud" que corresponde a la localidad de muestreo de cada dato.
lon<-data3$lon###seleciona la informaci?n sobre la "Longitud" que corresponde a la localidad de muestreo de cada dato.
year<-data3$year###seleciona la informaci?n sobre el "A?O" de colecta o muestreo de cada dato.
country<-data3$country###seleciona la informaci?n sobre el "pa?s" de la localidad de muestreo de cada dato.
region<-data3$adm1###seleciona la informaci?n sobre el "estado o provincia" de la localidad de muestreo de cada dato.

##Unir toda la informacion en un solo archivo
data_cleaned1 <-data.frame(gbifID,institutionCode,catalogNumber,kingdom,phylum,class,order,family,genus,species,lon,lat,year,country,region)

##Guardar el archivo creado para los datos datos descargados y filtrados
setwd("D:/proyectos_sigs/2023.Ara_militaris_paco/data/")##permite indicar el directorio dentro del computador donde queremos guardar el archivo ".csv" contentivo de la informaci?n
write.csv(data_cleaned1, file = "Ara_militaris_gbif_step1.csv") ###es el comando utilizado para guardar el archivo de nombre "Amazona_xantholora" con extensi?n ".csv"

points_occ <- SpatialPointsDataFrame(data_cleaned1[,11:12],data_cleaned1)#convertir a un archivo shp de puntos 

elevacion <- raster("D:/proyectos_sigs/2023.Ara_militaris_paco/clima_limpieza/dem.tif")
DEM <- stack(elevacion)

datacleaned2 <- data.frame(extract(DEM,points_occ[,11:12]))###extrae los valores para cada "presencia"
datacleaned3<-data.frame(points_occ,datacleaned2)##Tabla de datos con los valores
datacleaned4 <- na.omit(datacleaned3)## omite mis datos de presencia sin valores ambientales.

hist(datacleaned4$dem)
quantile(datacleaned4$dem, probs = 0.05, na.rm = TRUE)#nos da el valor minimo de elevaci?n
quantile(datacleaned4$dem, probs = 0.95, na.rm = TRUE)#nos da el valor m?ximo de elevacion

##calcular el rango de elevacion
dem_min= 0###cambiar a mano, colocar los valores de IUCN si existen y si no existen lo que salga en la linea 80
dem_max= 3200###cambiar a mano, colocar los valores de IUCN si existen y si no existen lo que salga en la linea 81

datacleaned4_2 <- subset(datacleaned4, dem > dem_min & dem < dem_max)#elimina datos con rango altitudinal extra?o

#1.To separate the data into two time series:
data4 <- subset(datacleaned4_2, year > 1969 & year < 2001)###to select the data between 1970 to 2010 (corresponding to environmental data)
data5 <- subset(datacleaned4_2, year > 2000)### to select the data despues de 2010

#3. Limpieza espacial de los datos: eliminiaci?n de duplicados
####las siguientes 4 lineas es una funcion que establece como usar los datos para hacer la limpieza (no modificar)###
clean_dup <- function(data,longitude,latitude,threshold=0.0){  data <- data[!is.na(data[,longitude]),]
dat_sp <- SpatialPointsDataFrame(data[,c(longitude ,latitude)],data)
dat_sp1 <- remove.duplicates(dat_sp, zero = threshold)
return(dat_sp1@data)}

###limpiar los dos set de datos creados: "datos6 = 1970-2010" y "data5 = 2011-2020"
data7 <- clean_dup(data4,longitude = "lon",latitude = "lat",threshold = 0.008333)###datos 1970-2010 espacialmente limpios
data8 <- clean_dup(data5,longitude = "lon",latitude = "lat",threshold = 0.008333)###datos 2011-2021 espacilamente limpios

##seleccionar datos 
muestreo1 <- sample_n(data7, size= 200)
muestreo2 <- SpatialPointsDataFrame(muestreo1[,11:12],muestreo1)


#4. Calcular la distancia buffer para seleccionar datos del 2011-2020
###convertir los archivos de datos en formato vectorial de puntos espaciales para el calculo de distancias geogr?ficas
points_occ2010 <- SpatialPointsDataFrame(data7[,11:12],data7)#convertir a un archivo shp de puntos 1970-2010
points_occ2020 <- SpatialPointsDataFrame(data8[,11:12],data8)##convertir a un archivo shp de puntos 2011-2021

DistanciaPuntos<-gDistance(muestreo2, byid=TRUE)##calcula la distancia promedio de los puntos de 1970 al 2010
buffer = mean(DistanciaPuntos)/10#Nos da el valor promedio de separaci?n de los datos

###aplicar la distancia buffer entre los puntos de 1970-2000 para seleccionar los datos del 2011-2021
buffer.points <- gBuffer(points_occ2010, width= buffer, byid=F)###aplica la distancia buffer a los puntos del 2011-2021

#Seleccionar los puntos del 2001-2020 que entran en el filtro buffer aplicado
data_poly_all <- over(points_occ2020,buffer.points, fn = NULL)###selecciona los puntos que si entran en el analisis
en_poligono_index <- which(!is.na(data_poly_all))
p_en_poligono <- points_occ2020[en_poligono_index ,]

selected_data <- as.data.frame(p_en_poligono)### para guardar el archivo de puntos

##################################################################
############# LIMPIEZA ECOLOGICA DE LOS DATOS ####################
##################################################################
#1.Verificar informacion ambiental en mis localidades de ocurrencia
bio_1 <- raster("D:/proyectos_sigs/2023.Ara_militaris_paco/clima_limpieza/bio_1.tif")
bio_12 <- raster("D:/proyectos_sigs/2023.Ara_militaris_paco/clima_limpieza/bio_12.tif")
bio_15 <- raster("D:/proyectos_sigs/2023.Ara_militaris_paco/clima_limpieza/bio_15.tif")

pca_path <- c(bio_1, bio_12, bio_15)
capas_presente<- stack(pca_path)

###Para los siguientes pasos, trabajaremos solo con las 3 columnas que nos interesan: nombre de la especie, longitud y latitud de los datos de 1970-2010
species<-data7$species ## seleccionar la columna del nombre de la especie para los datos 1970-2010
lat<-as.numeric(data7$lat)## seleccionar la columna del nombre de la latitud para los datos 1970-2010
lon<-as.numeric(data7$lon)## seleccionar la columna del nombre de la longitud para los datos 1970-2010
datos_2010<-data.frame(species,lon,lat) ## crear el archivo con solo esas tres variables para los datos 1970-2010

points_occ2010 <- SpatialPointsDataFrame(datos_2010[,2:3],datos_2010)#convertir el archivo csv 


#2. extraemos los valores ambientales para esas localidades
presencias_clima <- data.frame(extract(capas_presente, points_occ2010 [,2:3]))###extrae los valores climaticos 
presencias_clima2<-data.frame(points_occ2010,presencias_clima)##crear una tabla de datos con los valores climaticos 
presencias_clima3 <- na.omit(presencias_clima2)## omite mis datos de presencia sin valores ambientales.

#3. Calcular los intervalos de referencias para los valores ambientales de las variables m?s importantes en la distribuci?n de mi especie. Aqu? proponemos realizar este calculo a trav?s del valor promedio +/- 2 veces la desviaci?n estandar encontrada en los datos de 1970-2010. Estos valores ser?n utilizados para realizar un filtrado ambiental de los datos del 2011-2021.
points_occ2010_2 <- subset(presencias_clima3)


bio1_min= (mean(points_occ2010_2$bio_1)) - ((sd(points_occ2010_2$bio_1))*2)#valor m?nimo definido para bio1
bio1_max= (mean(points_occ2010_2$bio_1)) + ((sd(points_occ2010_2$bio_1))*2)#valor m?ximo definido para bio1

bio12_min= (mean(points_occ2010_2$bio_12)) - ((sd(points_occ2010_2$bio_12))*2)#valor m?nimo definido para bio12
bio12_max= (mean(points_occ2010_2$bio_12)) + ((sd(points_occ2010_2$bio_12))*2)#valor m?ximo definido para bio12

bio15_min= (mean(points_occ2010_2$bio_15)) - ((sd(points_occ2010_2$bio_15))*2)#valor m?nimo definido para bio15
bio15_max= (mean(points_occ2010_2$bio_15)) + ((sd(points_occ2010_2$bio_15))*2)#valor m?ximo definido para bio15


#4.realizar el filtrado ambiental de los datos para las localidades obtenidas entre 2011 y 2020
##extraemos los valores ambientales para esas localidad y omitimos los datos sin informacion
species<-data8$species ## seleccionar la columna del nombre de la especie para los datos >2011
lat<-as.numeric(data8$lat)## seleccionar la columna del nombre de la latitud para los datos >2011
lon<-as.numeric(data8$lon)## seleccionar la columna del nombre de la longitud para los datos >2011
datos_2020_1<-data.frame(species,lon,lat) ## crear el archivo con solo esas tres variables para los datos >2011

##antes tenemos que unir estos datos con los de la base de CONABIO
###datos de CONABIO
museo_data <- read.csv2("D:/proyectos_sigs/2023.Ara_militaris_paco/data/museo_data_spp.csv", sep = ",", header = TRUE)
museo_data2 <- museo_data[museo_data$TAXON =="Ara militaris",]
museo_data2$latitud <- as.numeric(as.character(museo_data2$latitud))###volver las variables en formato n?merico
museo_data2$longitud <- as.numeric(as.character(museo_data2$longitud))###volver las variables en formato n?merico

##unir los datos en un solo archivo
species <- c(datos_2020_1$species, museo_data2$TAXON)###"taxonomic information".
lat <- c(datos_2020_1$lat, museo_data2$latitud)###"geographical coordinates".
lon <- c(datos_2020_1$lon, museo_data2$longitud)###"geographical coordinates".

##Crear un solo archivo
datos_2020 <-data.frame(species,lon,lat)
points_occ2020 <- SpatialPointsDataFrame(datos_2020[,2:3],datos_2020)#convertir el archivo shapefile

presencias2020_clima <- data.frame(extract(capas_presente,points_occ2020[,2:3]))##extrae los valores climaticos de los puntos
presencias2020_clima2<-data.frame(points_occ2020,presencias2020_clima)##crear una tabla de datos con los valores climaticos 
presencias2020_clima3 <- na.omit(presencias2020_clima2)## omite mis datos de presencia sin valores ambientales


##Limpiar los datos de 2001-2020que estan fuera de los valores ambientales de distribuci?n definidos:
data2020_1 <- subset(presencias2020_clima3, bio_1 > bio1_min & bio_1 < bio1_max)#elimina datos con temperatura anual 
data2020_12 <- subset(data2020_1, bio_12 > bio12_min & bio_12 < bio12_max)#elimina datos con precipitaci?n anual 
data2020_15 <- subset(data2020_12, bio_15 > bio15_min & bio_15 < bio15_max)#elimina datos con precipitaci?n estacional 

data2020_15_1 <- SpatialPointsDataFrame(data2020_15[,2:3],data2020_15)#convertir el archivo shapefile

##verificar que esten igual a la elevaci?n correcta
elev_2020 <- data.frame(extract(DEM,data2020_15_1[,2:3]))###extrae los valores para cada "presencia"
elev_2020_2<-data.frame(data2020_15_1,elev_2020)##Tabla de datos con los valores
elev_2020_3 <- na.omit(elev_2020_2)## omite mis datos de presencia sin valores ambientales.

data2020_dem <- subset(elev_2020_3, dem > dem_min & dem < dem_max)#elimina datos con rango altitudinal extra?o

##generar los archivos finales
data_1970_2010_fin <- points_occ2010_2###corresponde al archivo final de datos para los a?os entre 1970 y 2000 
data_2011_2020_fin <- data2020_dem###corresponde al archivo final de datos para los a?os entre 2001 y 2020
data_buffer <- selected_data###corresponde al archivo final de datos seleccionados por buffer


#5. Armar el ?nico archivo .CSV con los registros validados y depurados de la especie 
species<- c(data_1970_2010_fin$species, data_2011_2020_fin$species, data_buffer$species)##selecciona y une las columnas "species" de ambos archivos
lat<-as.numeric(c(data_1970_2010_fin$lat, data_2011_2020_fin$lat, data_buffer$lat))##selecciona y une las columnas "latitud" de ambos archivos
lon<-as.numeric(c(data_1970_2010_fin$lon, data_2011_2020_fin$lon, data_buffer$lon))##selecciona y une las columnas "longitud" de ambos archivos

##Unir toda la informacion en un solo archivo
data_cleaned2 <-data.frame(species,lat,lon)###crea el archivo dataframe concatenado.
data_cleaned2$species_name <- "Ara_militaris"#crea una columna/variable colocando el nombre de la especie

species<-data_cleaned2$species_name###corresponding to "taxonomic information" to species.
lon<-as.numeric(data_cleaned2$lon)## seleccionar la columna del nombre de la longitud para los datos
lat<-as.numeric(data_cleaned2$lat)## seleccionar la columna del nombre de la latitud para los datos
data_cleaned4<-data.frame(species,lon,lat) ## crear el archivo con solo esas tres variables para los datos 

#6. Quitar datos duplicados (mismas coordenadas geograficas)
points_occs2 <- SpatialPointsDataFrame(data_cleaned4[,2:3],data_cleaned4)##convertir en archivo shp TODOS los puntos
#DistanciaPuntos<-gDistance(points_occs2, byid=TRUE)##calcula la distancia promedio de los puntos de 1970 al 2010
#buffer2 = quantile(DistanciaPuntos, probs = 0.09, na.rm = TRUE)###
#buffer2*100##nos dice el valor de buffer que va en el archivo excel.

data10 <- clean_dup(data_cleaned4,longitude = "lon",latitude = "lat",threshold = buffer)### datos espacialmente limpios (si son mas de 10mil datos cambiar "buffer2" por "0.1")

##Guardar el archivo en el PC: datos limpios
setwd("D:/proyectos_sigs/2023.Ara_militaris_paco/data/")##carpeta donde guardar el archivo final 
write.csv(data10, file = "Ara_militaris_data_cleaned.csv")###es el comando utilizado para guardar el archivo ".csv"


#5. Salvar el archivo shp = M spp.
points_final <- SpatialPointsDataFrame(data10[,2:3],data10)##convertir en archivo shp TODOS los puntos

setwd("D:/proyectos_sigs/2023.Ara_militaris_paco/data/point_qgis/")##carpeta donde guardar el archivo .shp
writeOGR(obj = points_final,layer = "Ara_militaris", dsn = "Ara_militaris",driver = "ESRI Shapefile")

##Fin