rm(list = ls())

library(ggplot2)
library(simmer)
library(simmer.bricks)
library(simmer.plot)
library(magrittr)
library(tidyverse)
library(triangle)
library(lognorm)
library(slider)
library(readr)

##### Working directory #####
if(Sys.info()["user"] == "Edoardo pc"){
  general_path <- file.path("D:","OneDrive","Tesi","Project")
} 
if(Sys.info()["user"] == "edoch") {
  general_path <- file.path("~","Documents","Polito","Project")
} 
if(Sys.info()["user"] == "edo_c") {
  general_path <- file.path("C:","Users","edo_c","OneDrive","Tesi","Project")
}
dir_path <- file.path(general_path)
setwd(dir = dir_path)

##### Percorso script #####
if(Sys.info()["user"] == "edo_c"){
  script_path <- file.path("C:","Users","edo_c","Desktop","Politecnico","Master_thesis","work_in_progress")
} 
if(Sys.info()["user"] == "edoch") {
  script_path <- file.path("~","Documents","Polito","master_thesis")
} 
if(Sys.info()["user"] == "Edoardo pc") {
  script_path <- file.path("D:","master_thesis","pm_thesis")
} 


script_sim_file <- file.path(script_path,"Simulator_SingleRes_script.R")
script_analysis_file <- file.path(script_path,"Analysis_SingleRes_script2.R")
script_plot_file <- file.path(script_path,"Plot_script3.R")

##### Percorsi output ##### 
# mixed_output_path <- file.path(general_path, "Plot_single_sim")
if(Sys.info()["user"] == "edo_c"){
  plot_general_output_path <- file.path("C:","Users","edo_c","Desktop","Politecnico","Tesi","Plot_sim_test")
} 
if(Sys.info()["user"] == "edoch") {
  plot_general_output_path <- file.path("~","Documents","Polito","Project", "Plot_sim_test")
} 
if(Sys.info()["user"] == "Edoardo pc") {
  plot_general_output_path <- file.path("D:","Politecnico","Plot_sim_test")
} 

##### Numero di replicazioni #####
tot_replications_ARRAY <- c(30)

##### Changing point #####
change_value_ARRAY <- c(5000)

##### Parametri di arrivo #####
arrival_param_LIST <- list()
arrival_param_LIST[[1]] <- c(10000, 15)

##### Indici di delay e parametri limite code #####
delay_ind_test_LIST <- list()
q_lim_test_LIST <- list()

# 1
delay_ind_test_LIST[[1]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 1, 1, 6, 2, 2, 1), 
  c(1, 1, 1, 9, 2, 2, 1)
))
q_lim_test_LIST[[1]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 3, 10, 10, 10),
  c(10, 10, 10, 3, 10, 10, 10)
))

##### Parametri finestre #####
window_param_LIST <- list()
window_param_LIST[[1]] <- c(400, 400)

##### Parametri di processo #####
std_value <- 5
process_param <- data.frame(
  c(getParmsLognormForMoments(mean = 6, var = std_value^2)),  # 1
  c(getParmsLognormForMoments(mean = 7, var = std_value^2)),  # 2
  c(getParmsLognormForMoments(mean = 8, var = std_value^2)),  # 3
  c(getParmsLognormForMoments(mean = 9, var = std_value^2)),  # 4
  c(getParmsLognormForMoments(mean = 10, var = std_value^2)), # 5
  c(getParmsLognormForMoments(mean = 11, var = std_value^2)), # 6
  c(getParmsLognormForMoments(mean = 12, var = std_value^2)), # 7
  c(getParmsLognormForMoments(mean = 13, var = std_value^2)), # 8
  c(getParmsLognormForMoments(mean = 14, var = std_value^2)), # 9
  c(getParmsLognormForMoments(mean = 15, var = std_value^2)), # 10
  row.names = c("mean","sd")
)
colnames(process_param) <- as.character(c(1:10))

#### Avvio #####

# Numero di replicazioni
start_time <- Sys.time()

tot_replications <- tot_replications_ARRAY

# Changing point (numero di pezzi lavorati dalla macchina 1 prima del cambiamento)
change_value <- change_value_ARRAY

# Parametri di arrivo
arrival_param <- as.list(arrival_param_LIST[[1]])

# Indici di delay
delay_ind_list <- as.list(delay_ind_test_LIST[[1]][1,])

delay_ind_new_list <- as.list(delay_ind_test_LIST[[1]][2,])

# Parametri limite code # PROBLEMA: solo aumento di limite capacità
q_lim <- as.list(q_lim_test_LIST[[1]][1,])

q_lim_new <- as.list(q_lim_test_LIST[[1]][2,])

# Parametri calcolo finestre di dati
ampiezza_finestra <- window_param_LIST[[1]][1]
frequenza_finestra <- window_param_LIST[[1]][2]

# Lancio script simulazione
source(file = script_sim_file)

# Lancio script analisi
source(file = script_analysis_file)

# Creazione cartelle plot
plot_specific_output_path <- plot_general_output_path

# Lancio script plotting
source(file = script_plot_file)

##### Stampa risultati #####
print(data_utilization %>% select(-Timestamp_res))
print(data_queue %>% select(-time))

end_time <- Sys.time()
cat(end_time - start_time)


