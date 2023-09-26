##Creating enmtools.species objects
setwd("D:/proyectos_sigs/ara_militaris_paco/new_project_2023/kuenm_models_spp/presente_completo/")###working directory with environmental variables
env.files <- list.files(".",pattern = "*.asc$",full.names = T)###read the environmental variables within folder.
env<- stack(env.files)##create the stack for environmental variables
env <- setMinMax(env)
plot(env[[1]])##plor the first environmetal variable in the stack.

##specie1: A.m.mexicana
mexicana <- enmtools.species()
mexicana

mexicana.path <- paste(system.file(package="ENMTools"), "/Ara_mexicanus_todos.csv", sep='')
mexicana <- enmtools.species(species.name = "mexicanus", 
                             presence.points = read.csv(mexicana.path))
mexicana$range <- background.raster.buffer(mexicana$presence.points, 50000, mask = env)
mexicana$background.points <- background.points.buffer(points = mexicana$presence.points,
                                                       radius = 20000, n = 1000, mask = env[[1]])
mexicana <- check.species(mexicana)
interactive.plot.enmtools.species(mexicana)


##specie2: A.m. militaris
militaris <- enmtools.species()
militaris

militaris.path <- paste(system.file(package="ENMTools"), "/Ara_militaris_todos.csv", sep='')
militaris <- enmtools.species(species.name = "militaris", 
                                presence.points = read.csv(militaris.path))
militaris$range <- background.raster.buffer(militaris$presence.points, 50000, mask = env)
militaris$background.points <- background.points.buffer(points = militaris$presence.points,
                                                          radius = 20000, n = 1000, mask = env[[1]])
militaris <- check.species(militaris)
interactive.plot.enmtools.species(militaris)


##Rangebreak tests
directorio = setwd("D:/proyectos_sigs/ara_militaris_paco/new_project_2023/")

rbl.glm <- rangebreak.linear(mexicana, militaris, env, type = "glm", nreps = 1000,
                             nback = 1000, rep.dir = directorio)

rbb.bc <- rangebreak.blob(mexicana, militaris, env, type = "bc", nreps = 1000,
                          nback = 1000)
rbl.glm
rbb.bc

rbl.glm$replicate.models

##Fin/end