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
library(startup)
library(here)

##### Working directory #####
general_path <- file.path("~","Documents","Polito","Project")
dir_path <- file.path(general_path)
setwd(dir = dir_path)

##### Percorso file input #####
param_file_input_path <- file.path(general_path,"Param_files")
param_file_name_list <- list.files(param_file_input_path)

##### Percorso script #####
script_path <- here()
script_sim_file <- file.path(script_path,"Simulator_SingleRes_script.R")
script_analysis_file <- file.path(script_path,"Analysis_SingleRes_script.R")
script_plot_file <- file.path(script_path,"Plot_script.R")

##### Percorsi output ##### 
plot_general_output_path <- file.path("~","Documents","Polito","Project", "Plot_sim")
data_general_output_path <- file.path(general_path, "Data_out")

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

##### Caricamento dati da file input #####
param_file_input_list <- list()
param_file_input <- list()

for(k in 1:length(list.files(param_file_input_path))){
  param_file_input_list[[k]] <- read_lines(file.path(param_file_input_path, param_file_name_list[[k]]))
  param_file_input[[k]] <- list()
  
  for(k1 in 1:length(param_file_input_list[[k]])){
    param_file_input[[k]][[k1]] <- param_file_input_list[[k]][k1]
    param_file_input[[k]][[k1]] <- as.numeric((strsplit(param_file_input[[k]][[k1]], split = " "))[[1]])
  }
}
rm(param_file_input_list)

##### Lancio script #####
start_time_tot <- Sys.time()

for(k in 1:length(param_file_input)){
  
  cat(paste("\n\n\t\tMODELLO",k,"\n\n"))
  start_time <- Sys.time()
  
  # Numero di replicazioni
  tot_replications <- param_file_input[[k]][[1]]
  
  # Changing point (numero di pezzi lavorati dalla macchina 1 prima del cambiamento)
  change_value <- param_file_input[[k]][[2]]
  
  # Parametri di arrivo
  arrival_param <- as.list(param_file_input[[k]][[3]])
  
  # Indici di delay
  delay_ind_list <- as.list(param_file_input[[k]][[4]])
  
  delay_ind_new_list <- as.list(param_file_input[[k]][[5]])
  
  # Parametri limite code # PROBLEMA: solo aumento di limite capacità
  q_lim <- as.list(param_file_input[[k]][[6]])
  
  q_lim_new <- as.list(param_file_input[[k]][[7]])
  
  # Parametri calcolo finestre di dati
  ampiezza_finestra <- param_file_input[[k]][[8]][1]
  frequenza_finestra <- param_file_input[[k]][[8]][2]
  
  # Lancio script simulazione
  source(file = script_sim_file)
  
  # Lancio script analisi
  source(file = script_analysis_file)
  
  # Creazione cartelle data_out
  data_specific_output_path <-
    file.path(data_general_output_path, str_extract(param_file_name_list[k], "[^\\.]+"))
  if(!dir.exists(data_specific_output_path)){
    dir.create(data_specific_output_path)
  }
  
  # Salvataggio data_out 
  write.csv(data_out_standard, 
            file.path(data_specific_output_path, paste0("data_out_standard",".csv")),
            row.names = F)
  
  write.csv(data_windows, 
            file.path(data_specific_output_path, paste0("data_windows",".csv")),
            row.names = F)
  
  write.csv(data_queue_trend, 
            file.path(data_specific_output_path, paste0("data_queue_trend",".csv")),
            row.names = F)
  
  # Creazione cartelle plot
  plot_specific_output_path <-
    file.path(plot_general_output_path, str_extract(param_file_name_list[k], "[^\\.]+"))
  if(!dir.exists(plot_specific_output_path)){
    dir.create(plot_specific_output_path)
  }
  
  # Lancio script plotting
  source(file = script_plot_file)
  
  end_time <- Sys.time()
  cat(paste0("\nTempo computazione modello\t", end_time - start_time))
  
  if((sysinfo()$os == "unix") * (sysinfo()$rstudio == T) * (sysinfo()$rstudioterm == T)){
    restart()
  }
}

end_time_tot <- Sys.time()

cat(paste0("\nTempo computazione totale\t", end_time_tot - start_time_tot))


