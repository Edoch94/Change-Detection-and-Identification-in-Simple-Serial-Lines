rm(list = ls())
gc()

library(ggplot2)
library(simmer)
library(simmer.bricks)
library(simmer.plot)
library(magrittr)
library(tidyverse)
library(triangle)
library(lognorm)
library(slider)

##### Working directory #####
if(Sys.info()["user"] == "Edoardo pc"){
  general_path <- file.path("C:","Users","Edoardo pc","OneDrive","Tesi","Project")
} else {
  general_path <- file.path("C:","Users","edo_c","OneDrive","Tesi","Project")
}
dir_path <- file.path(general_path)
setwd(dir = dir_path)

##### Percorso script #####
script_path <- file.path("C:","Users","edo_c","Desktop","Politecnico","Master_thesis","work_in_progress")
script_file <- file.path(script_path,"Simulator_SingleRes_line_scripting9.R")

# script_path <- paste("C:","Users","edo_c","Desktop","Politecnico","Master_thesis","work_in_progress", sep = "/")
# script_file <- paste(script_path, "Simulator_SingleRes_line_scripting9.R", sep = "/")

##### Percorsi output ##### 
mixed_output_path <- file.path(general_path, "Plot_single_sim")
# output_path <- 
  
##### Parametri di setup #####
# Numero di replicazioni
tot_replications <- 1

# Changing point (numero di pezzi lavorati dalla macchina 1 prima del cambiamento)
change_value <- 5000

# Parametri di arrivo
arrival_param <- list(
  10000, # Numero totale di casi generato
  100 # Media di interrarrivo, nella funzione rexp viene trasformata in frequenza media
)

# Parametri di processo
# pinp <- matrix(
#   data = c(
#     c(80, 5),
#     c(85, 5),
#     c(90, 5),
#     c(95, 5),
#     c(100, 5),
#     c(110, 5)
#   ),
#   nrow = 6, ncol = 2, byrow = T
# )

process_param <- data.frame(
  c(getParmsLognormForMoments(mean = 75, var = 10)),  # 1
  c(getParmsLognormForMoments(mean = 80, var = 10)),  # 2
  c(getParmsLognormForMoments(mean = 85, var = 10)),  # 3
  c(getParmsLognormForMoments(mean = 90, var = 10)),  # 4
  c(getParmsLognormForMoments(mean = 95, var = 10)),  # 5
  c(getParmsLognormForMoments(mean = 100, var = 10)), # 6
  c(getParmsLognormForMoments(mean = 110, var = 10)), # 7
  c(getParmsLognormForMoments(mean = 115, var = 10)), # 8
  c(getParmsLognormForMoments(mean = 120, var = 10)), # 9
  c(getParmsLognormForMoments(mean = 125, var = 10)), # 10
  row.names = c("mean","sd")
)
colnames(process_param) <- as.character(c(1:10)) 

# Indici di delay
delay_ind_list <- list(
  1,  # 1
  2,  # 2
  3,  # 3
  7,  # 4
  2,  # 5
  3,  # 6
  1   # 7
)

delay_ind_new_list <- list(
  1,  # 1
  2,  # 2
  3,  # 3
  7,  # 4
  2,  # 5
  3,  # 6
  1   # 7
)

# Parametri limite code
q_lim <- list()
q_lim[[1]] <- 10 # Non utilizzato
q_lim[[2]] <- 10
q_lim[[3]] <- 10
q_lim[[4]] <- 10
q_lim[[5]] <- 10
q_lim[[6]] <- 10
q_lim[[7]] <- 10

# PROBLEMA: solo aumento di limite capacità
q_lim_new <- list()
q_lim_new[[1]] <- 10 # Non utilizzato
q_lim_new[[2]] <- 10
q_lim_new[[3]] <- 10
q_lim_new[[4]] <- 10
q_lim_new[[5]] <- 10
q_lim_new[[6]] <- 10
q_lim_new[[7]] <- 10


##### Lancio script #####
source(file = script_file)




