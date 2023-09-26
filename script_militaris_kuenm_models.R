#######################################################
######### Parte 2: Correr el modelo ###################
#######################################################
rm(list=ls()) #elimina TODOS los objetos del ambiente de trabajo
############################################
######### ETAPA 1 #########################
setwd("D:/proyectos_sigs/ara_militaris_paco/new_project_2023/kuenm_models_spp/ara_militaris/")### directorio donde estan todos los archivos 

############################################
######### The model creation ###############
############################################
occ_joint <- "ara_militaris_joint.csv"##archivo que tiene los datos completos
occ_tra <- "ara_militaris_train.csv"##archivo que tiene el 80% de los datos para entrenar
M_var_dir <- "M_variables"##carpeta donde estan las capas del presente
batch_cal <- "Candidate_models"##carpeta donde se guardaran los resultados de los 620 modelos candidatos
out_dir <- "Candidate_Models"##carpeta donde se guardaran los resultados de los 620 modelos candidatos
reg_mult <- c(seq(0.2, 2, 0.2), seq(2, 6, 0.5),8, 10)##indicar todas las combinaciones de RM que se usaran
f_clas <- "all"###indicar cuales combinaciones de "Feature" se utilizara
args <- NULL 

#Correr el programa para que haga 620 modelos simultaneos que leugo se usaran para definir al mejor.
maxent_path <- "C:/Users/ASUS/Documents/R/win-library/4.1/dismo/java" ##esto debe localizarlo en su computadora
wait <- FALSE
run <- TRUE
kuenm_cal(occ.joint = occ_joint, occ.tra = occ_tra, M.var.dir = M_var_dir, batch = batch_cal,
          out.dir = out_dir, reg.mult = reg_mult, f.clas = f_clas, args = args,
          maxent.path = maxent_path, wait = wait, run = run)
###Se abrir? una ventana negra con letras blancas que ir? indicando como va el avance para la elaboraci?n de los 620 modelos... esa ventana se cierra sola... cuando se cierre significa que ya termino y puede seguir ejecutando las otras partes del script.ESTO PUEDE TARDAR VARIOS MINUTOS E INCLUSO HORAS (DEPENDE DE LA CAPACIDAD DEL COMPUTADOR)...

############################################
######### The model evaluation #############
############################################
occ_test <- "ara_militaris_test.csv"##indicar donde esta el archivo para evaluar a los 620 modelos
out_eval <- "Calibration_results"##indicar donde guardar los resultados a obtener
threshold <- 5##cual es el criterio de seleccion en referencia al valor de error de omision
rand_percent <- 50##porcentaje a utilizar de los datos de forma aleatoria
iterations <- 500##numero de iteracciones
kept <- TRUE
selection <- "OR_AICc"##criterio de seleccion de modelos: omission error and Akaike criteria
paral_proc <- FALSE ##no correr en paralelo

cal_eval <- kuenm_ceval(path = out_dir, occ.joint = occ_joint, occ.tra = occ_tra, occ.test = occ_test, batch = batch_cal,
                        out.eval = out_eval, threshold = threshold, rand.percent = rand_percent, iterations = iterations,
                        kept = kept, selection = selection, parallel.proc = paral_proc)

###Aparecera una ventana en blanco con una barra de progreso.. esa barra se ir? colocando de color azul a medida que el programa avance en el an?lisis... al terminar (llegar al 100%) se cerrar? y entonces podr? avanzar en el script. ESTO IGUAL PUEDE TARDAR VARIOS MINUTOS Y/O HORAS, recuerde que esta evaluando 620 modelos (PERO NO es normal que no avance nada en m?s de 2 horas... si eso ocurre hay alg?n problema y debe revisarse).


############################################
######### The FINAL model creation ######### 
############################################
batch_fin <- "Final_models"#donde guardar el(los) modelos finales que se seleccionaron
mod_dir <- "Final_Models"#donde guardar el(los) modelos finales que se seleccionaron
rep_n <- 10#indicar cuantas replicas se haran
rep_type <- "Bootstrap"##cual es el criterio para hacer replicas
jackknife <- FALSE
out_format <- "cloglog"
project <- TRUE##indicar que queremos que haga modelos a futuro
G_var_dir <- "G_variables"##indicar donde estan las capas del futuro
ext_type <- "ext_clam"#indicar que queremos que permita extrapolacion y docampling
write_mess <- FALSE
write_clamp <- FALSE
wait1 <- FALSE
run1 <- TRUE
args <- NULL 
kuenm_mod(occ.joint = occ_joint, M.var.dir = M_var_dir, out.eval = out_eval, batch = batch_fin, rep.n = rep_n,
          rep.type = rep_type, jackknife = jackknife, out.dir = mod_dir, out.format = out_format, project = project,
          G.var.dir = G_var_dir, ext.type = ext_type, write.mess = write_mess, write.clamp = write_clamp, 
          maxent.path = maxent_path, args = args, wait = wait1, run = run1)
###Se abrira una ventana negra con letras blancas que ira indicando como va el avance para la elaboracion de (los) modelos finales... este sera el modelo utilizando los mejores parametros de configuracion esa ventana se cierra sola... cuando se cierre significa que ya termino y puede seguir ejecutando las otras partes del script.
#esto puede tardar un poco (quizas un par de horas)... depende de la PC, la cantidad de datos y el tamano del area


############################################
####### The model evaluation final #########
############################################
occ_ind <- "ara_militaris_ind.csv"##Elegir el archivo de 20% de los datos que nunca se usaron para evaluar el modelo
replicates <- TRUE##indicar que hay replicas
threshold <- 10
out_feval <- "Final_Models_evaluation"##nombre del archivo donde se guardaran los resultados

fin_eval <- kuenm_feval(path = mod_dir, occ.joint = occ_joint, occ.ind = occ_ind, replicates = replicates,
                        out.eval = out_feval, threshold = threshold, rand.percent = rand_percent,
                        iterations = iterations, parallel.proc = paral_proc)

best <- read.csv("Calibration_results/selected_models.csv")
knitr::kable(best, caption = "Models selected based on significance, omission rates, and AICc, in that order.")
##al terminar de leer esta parte le saldra una tabla final con resultados... esos resultados deben ser copiados en el archivo excel, llenando cada una de las cosas que se solitica.. Si en la tabla le sale m?s de una opci?n (como es este ejemplo) ustede debe solo copiar los datos del primer modelo... Debe copiar de ac? los valores de RM, feature y AICc


best <- read.csv("Final_Models_evaluation/fm_evaluation_results.csv")
knitr::kable(best, caption = "Models selected based on significance, omission rates, and AICc, in that order.")

########################################################
##########ANALISIS ESPACIALES DE MODELOS################
########################################################
rm(list=ls()) #elimina TODOS los objetos del ambiente de trabajo

########################################################################
######Primera PARTE: Calcular el mapa binario del presente #############
########################################################################
#1.llamar el modelo promedio que se obtivo para el presente
presente <- raster("D:/proyectos_sigs/ara_militaris_paco/new_project_2023/kuenm_models_spp/ara_militaris/Final_Models/M_2.5_F_lqt_Set_1_EC/Ara_militaris_median.asc")
plot(presente)

#2. Llamar al archivo .csv de los puntos utilizados para el training del modelo final.
data_spp <- read.csv2("D:/proyectos_sigs/ara_militaris_paco/new_project_2023/kuenm_models_spp/ara_militaris/ara_militaris_joint.csv", sep = ",", header = TRUE)
data_spp$lat <- as.numeric(as.character(data_spp$lat))###volver las variables en formato n?merico
data_spp$lon <- as.numeric(as.character(data_spp$lon))###volver las variables en formato n?merico

#1. Convertir los puntos en datos de presencia para un SIG
points_occ_fin <- SpatialPointsDataFrame(data_spp[,2:3],data_spp)#convertir a un archivo shp de puntos 

#3. Extraer los valores de idoneidad que el modelo predice para cada uno de los puntos de presencia (trainning)
presencia_model <- data.frame(raster::extract(presente, points_occ_fin [,2:3]))###extrae los valorespara cada uno de los puntos
presencia_model2 <- na.omit(presencia_model)## omite mis datos de presencia sin valores climaticos

#4. calcular el valor del 10% de los datos de presencia de la especie
umbral_models <- quantile(presencia_model2$raster..extract.presente..points_occ_fin...2.3.., prob= 0.09)##me indica el valor del umbral

#5. Transformar la capa del presente a un mapa binario
presente_bin <- presente >= umbral_models #convierte el mapa de idoneidad en presencia/ausencia

plot(presente_bin)###me grafica el mapa
plot(points_occ_fin, add= T, pch=19, col= "red")###grafica los puntos de entrenamiento

#6. Llamar al archivo .csv de los puntos utilizados para el testing del modelo final (es el archivo que se llama "IND").
data_testing_spp <- read.csv2("D:/proyectos_sigs/ara_militaris_paco/new_project_2023/kuenm_models_spp/ara_militaris/ara_militaris_ind.csv", sep = ",", header = TRUE)
data_testing_spp$lat <- as.numeric(as.character(data_testing_spp$lat))###volver las variables en formato n?merico
data_testing_spp$lon <- as.numeric(as.character(data_testing_spp$lon))###volver las variables en formato n?merico

#7. Convertir los puntos en datos de presencia para un SIG
points_test_fin <- SpatialPointsDataFrame(data_testing_spp[,2:3],data_testing_spp)#convertir a un archivo shp de puntos 

#8. Extraer los valores de idoneidad que el modelo predice para cada uno de los puntos de presencia (Testing)
presencia_model_test <- data.frame(raster::extract(presente_bin, points_test_fin [,2:3]))###extrae los valores para saber cuantos datos del TESTING son predichos correctamente y cuantos no.
presencia_model_test2 <- na.omit(presencia_model_test)## omite mis datos de presencia sin valores climaticos
sum(presencia_model_test2$raster..extract.presente_bin..points_test_fin...2.3..)###me indica en n?mero cuantos datos si fueron bien predichos


###Salvar el mapa de la especie
setwd("D:/proyectos_sigs/ara_militaris_paco/new_project_2023/mapas_finales_binarios/")#RUTA DEL DIRECTORIO DE DONDE Guardar las capas
writeRaster(presente_bin, filename= "Ara_militaris_m_present.asc", overwrite=T, suffix='names')

########################################################################
######Segunda PARTE: Calcular el mapa binario del PRESENTE TODO##############
########################################################################
#1.llamar las capas (modelos/replicas) que se hicieron del futuro 2040 (uno por cada laboratorio)
presente_todo <- raster("D:/proyectos_sigs/ara_militaris_paco/new_project_2023/kuenm_models_spp/ara_militaris/Final_Models/M_2.5_F_lqt_Set_1_EC/Ara_militaris_presente_completo_median.asc")##modelo del CCSM

#2.Transformar los mapas a mapas binarios
presente_todo_bin <- presente_todo >= umbral_models #convierte el mapa de idoneidad en presencia/ausencia
plot(presente_todo_bin)

#4.Guardar el mapa de 2040 DISPERSION 
setwd("D:/proyectos_sigs/ara_militaris_paco/new_project_2023/mapas_finales_binarios/")#RUTA DEL DIRECTORIO DE DONDE Guardar las capas
writeRaster(presente_todo_bin, filename= "Ara_militaris_present_todo.asc", overwrite=T, suffix='names')


########################################################################
######Segunda PARTE: Calcular el mapa binario del HOLOCENO##############
########################################################################
#1.llamar las capas (modelos/replicas) que se hicieron del futuro 2040 (uno por cada laboratorio)
Holoceno_1 <- raster("D:/proyectos_sigs/ara_militaris_paco/new_project_2023/kuenm_models_spp/ara_militaris/Final_Models/M_2.5_F_lqt_Set_1_EC/Ara_militaris_MH_CCSM_median.asc")##modelo del CCSM
Holoceno_2 <- raster("D:/proyectos_sigs/ara_militaris_paco/new_project_2023/kuenm_models_spp/ara_militaris/Final_Models/M_2.5_F_lqt_Set_1_EC/Ara_militaris_MH_MIROC_median.asc")##modelo del MIROC
Holoceno_3 <- raster("D:/proyectos_sigs/ara_militaris_paco/new_project_2023/kuenm_models_spp/ara_militaris/Final_Models/M_2.5_F_lqt_Set_1_EC/Ara_militaris_MH_MPI_ESM_median.asc")##modelo del MPI_ESM

#2.Transformar los mapas a mapas binarios
Holoceno_1_bin <- Holoceno_1 >= umbral_models #convierte el mapa de idoneidad en presencia/ausencia
Holoceno_2_bin <- Holoceno_2 >= umbral_models #convierte el mapa de idoneidad en presencia/ausencia
Holoceno_3_bin <- Holoceno_3 >= umbral_models #convierte el mapa de idoneidad en presencia/ausencia

#3. Sumar los 5 modelos para obtener el ?rea de coincidencia 
map_holoceno_total <- Holoceno_1_bin + Holoceno_2_bin + Holoceno_3_bin 
plot(map_holoceno_total)
map_holoceno_fin <- map_holoceno_total >= 2 ##selecciona como "presencia" aquellas ?reas donde al menos 3 modelos coinciden.

#4.Guardar el mapa de 2040 DISPERSION 
setwd("D:/proyectos_sigs/ara_militaris_paco/new_project_2023/mapas_finales_binarios/")#RUTA DEL DIRECTORIO DE DONDE Guardar las capas
writeRaster(map_holoceno_fin, filename= "Ara_militaris_holoceno.asc", overwrite=T, suffix='names')


###################################################################
######Tercera PARTE: Calcular el mapa binario del LGM##############
###################################################################
#1.llamar las capas (modelos/replicas) que se hicieron del futuro 2040 (uno por cada laboratorio)
LGM_1 <- raster("D:/proyectos_sigs/ara_militaris_paco/new_project_2023/kuenm_models_spp/ara_militaris/Final_Models/M_2.5_F_lqt_Set_1_EC/Ara_militaris_LGM_CCSM_median.asc")##modelo del CCSM
LGM_2 <- raster("D:/proyectos_sigs/ara_militaris_paco/new_project_2023/kuenm_models_spp/ara_militaris/Final_Models/M_2.5_F_lqt_Set_1_EC/Ara_militaris_LGM_MIROC_median.asc")##modelo del MIROC
LGM_3 <- raster("D:/proyectos_sigs/ara_militaris_paco/new_project_2023/kuenm_models_spp/ara_militaris/Final_Models/M_2.5_F_lqt_Set_1_EC/Ara_militaris_LGM_MPI_ESM_median.asc")##modelo del MPI_ESM

#2.Transformar los mapas a mapas binarios
LGM_1_bin <- LGM_1 >= umbral_models #convierte el mapa de idoneidad en presencia/ausencia
LGM_2_bin <- LGM_2 >= umbral_models #convierte el mapa de idoneidad en presencia/ausencia
LGM_3_bin <- LGM_3 >= umbral_models #convierte el mapa de idoneidad en presencia/ausencia

#3. Sumar los 5 modelos para obtener el ?rea de coincidencia 
map_LGM_total <- LGM_1_bin + LGM_2_bin + LGM_3_bin 
plot(map_LGM_total)
map_LGM_fin <- map_LGM_total >= 2 ##selecciona como "presencia" aquellas ?reas donde al menos 3 modelos coinciden.

#4.Guardar el mapa de 2040 DISPERSION 
setwd("D:/proyectos_sigs/ara_militaris_paco/new_project_2023/mapas_finales_binarios/")#RUTA DEL DIRECTORIO DE DONDE Guardar las capas
writeRaster(map_LGM_fin, filename= "Ara_militaris_LGM.asc", overwrite=T, suffix='names')


###################################################################
######Cuarta PARTE: Calcular el mapa binario del LIG##############
###################################################################
#1.llamar las capas (modelos/replicas) que se hicieron del futuro 2040 (uno por cada laboratorio)
LIG_1 <- raster("D:/proyectos_sigs/ara_militaris_paco/new_project_2023/kuenm_models_spp/ara_militaris/Final_Models/M_2.5_F_lqt_Set_1_EC/Ara_militaris_LIG_CCSM_median.asc")##modelo del CCSM


#2.Transformar los mapas a mapas binarios
LIG_1_bin <- LIG_1 >= umbral_models #convierte el mapa de idoneidad en presencia/ausencia
plot(LIG_1_bin)

#4.Guardar el mapa de 2040 DISPERSION 
setwd("D:/proyectos_sigs/ara_militaris_paco/new_project_2023/mapas_finales_binarios/")#RUTA DEL DIRECTORIO DE DONDE Guardar las capas
writeRaster(LIG_1_bin, filename= "Ara_militaris_LIG.asc", overwrite=T, suffix='names')
##FIN!