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
library(readr)
library(moments)

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

##### Percorsi script #####
if(Sys.info()["user"] == "edo_c"){
  script_path <- file.path("C:","Users","edo_c","Desktop","Politecnico","Master_thesis","work_in_progress")
} 
if(Sys.info()["user"] == "edoch") {
  script_path <- file.path("~","Documents","Polito","master_thesis")
} 
if(Sys.info()["user"] == "Edoardo pc") {
  script_path <- file.path("D:","master_thesis","pm_thesis")
} 
script_sim_file <- file.path(script_path,"Simulator_SingleRes_script2.R")
script_analysis_file <- file.path(script_path,"Analysis_SingleRes_script4.R")
script_plot_file <- file.path(script_path,"Plot_script5.R")

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
tot_replications_ARRAY <- c(1)

##### Changing point #####
change_value_ARRAY <- c(10000)

##### Parametri di arrivo #####
arrival_param_LIST <- list()
arrival_param_LIST[[1]] <- c(10000, 14)

##### Indici di delay e parametri limite code #####
delay_ind_test_LIST <- list()
q_lim_test_LIST <- list()

# 1
delay_ind_test_LIST[[1]] <- matrix(byrow = T, ncol = 7, c(
  c(7, 7, 7, 7, 8.5, 7, 7), 
  c(7, 7, 7, 7, 14, 7, 7)
))
q_lim_test_LIST[[1]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

##### Parametri finestre #####
window_param_LIST <- list()
window_param_LIST[[1]] <- c(100, 100)

##### Parametri di processo #####
std_log <- 5

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


