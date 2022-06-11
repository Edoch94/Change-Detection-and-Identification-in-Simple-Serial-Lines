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
library(future)
library(future.apply)
# library(multidplyr)
# library(parallel)
# library(foreach)
# library(doParallel)
# library(reticulate)

##### Conda Environment #####
# use_condaenv(condaenv = "")

##### Working directory #####
if(Sys.info()["user"] == "Edoardo pc"){ #HP
  general_path <- file.path("D:","OneDrive","Articolo2020", "UlterioriTest","Project")
} 
if(Sys.info()["user"] == "edoardo" & Sys.info()["sysname"] == "Linux") { #Fisso Ubuntu
  general_path <- file.path("/home","edoardo","Documenti","ChangeDetectionAndIdentification","General_folder")
} 
if(Sys.info()["user"] == "edo_c") { #Fisso Windows
  general_path <- file.path("C:","Users","edo_c","OneDrive","Articolo2020", "UlterioriTest", "Project")
}
if(Sys.info()["user"] == "edo_c" & Sys.info()["nodename"] == "MININT-2C4GEGM") {
  general_path <- file.path("D:","ChangeDetectionIdentification_input_output","General_folder")
}
dir_path <- file.path(general_path)
setwd(dir = dir_path)

##### Percorso file input #####
param_file_input_path <- file.path(general_path,"Param_files")
param_file_name_list <- list.files(param_file_input_path)

##### Percorso script #####
if(Sys.info()["user"] == "edo_c"){ #HP
  script_path <- file.path("C:","Users","edo_c","Desktop","Politecnico","Master_thesis","work_in_progress")
} 
if(Sys.info()["user"] == "edoardo" & Sys.info()["sysname"] == "Linux") { #Fisso Ubuntu
  script_path <- file.path("/home","edoardo","Documenti","ChangeDetectionAndIdentification","pm_thesis")
} 
if(Sys.info()["user"] == "Edoardo pc"){ #Fisso Windows
  script_path <- file.path("D:","master_thesis","pm_thesis")
} 
if(Sys.info()["user"] == "edo_c" & Sys.info()["nodename"] == "MININT-2C4GEGM") {
  script_path <- file.path("D:","Documenti","ChangeDetectionIdentification","pm_thesis")
}

script_sim_file <- file.path(script_path,"Simulator_SingleRes_script2.R")
script_analysis_file <- file.path(script_path,"Analysis_SingleRes_script5.R")
script_plot_file <- file.path(script_path,"Plot_script5.R")

##### Percorsi output ##### 
# mixed_output_path <- file.path(general_path, "Plot_single_sim")
# if(Sys.info()["user"] == "edo_c"){
#   plot_general_output_path <- file.path("C:","Users","edo_c","Desktop","Politecnico","Tesi","Plot_sim")
# } 
if(Sys.info()["user"] == "edoch") { #HP
  plot_general_output_path <- file.path("~","Documents","Polito","Project", "Plot_sim")
} 
if(Sys.info()["user"] == "edoardo" & Sys.info()["sysname"] == "Linux") { #Fisso Ubuntu
  plot_general_output_path <- file.path(general_path,"Plot_sim")
} 
if(Sys.info()["user"] == "Edoardo pc"){ #Fisso Windows
  plot_general_output_path <- file.path("D:","Articolo2020","Plot_sim")
} 
if(Sys.info()["user"] == "edo_c" & Sys.info()["nodename"] == "MININT-2C4GEGM") {
  plot_general_output_path <- file.path("D:","ChangeDetectionIdentification_input_output","General_folder","Plot_sim")
}

data_general_output_path <- file.path(general_path, "Data_out")

##### Parametri di processo #####
std_log <- 5

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
isLocal <- F # Default deve essere F

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
  source(file = script_sim_file, local = isLocal)
  
  # Lancio script analisi
  source(file = script_analysis_file, local = isLocal)
  
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
  
  # Salvataggio dati non aggregati
  # data_specific_output_path_NOTaggr <-
  #   file.path(data_specific_output_path, "NOT_aggregated")
  # if(!dir.exists(data_specific_output_path_NOTaggr)){
  #   dir.create(data_specific_output_path_NOTaggr)
  # }
  # 
  # for (i in 1:length(data_out_list)) {
  #   write.csv(data_out_list[[i]],
  #             file.path(data_specific_output_path_NOTaggr, paste0("data_out_standard_not_aggr",i,".csv")),
  #             row.names = F)
  # }
  
  # Creazione cartelle plot
  plot_specific_output_path <-
    file.path(plot_general_output_path, str_extract(param_file_name_list[k], "[^\\.]+"))
  if(!dir.exists(plot_specific_output_path)){
    dir.create(plot_specific_output_path)
  }
  
  # Lancio script plotting
  source(file = script_plot_file, local = isLocal)
  
  end_time <- Sys.time()
  cat(paste0("\nTempo computazione modello\t", end_time - start_time))
  
  # if((sysinfo()$os == "unix") * (sysinfo()$rstudio == T) * (sysinfo()$rstudioterm == T)){
  #   restart()
  # }
}

end_time_tot <- Sys.time()

cat(paste0("\nTempo computazione totale\t", end_time_tot - start_time_tot))


