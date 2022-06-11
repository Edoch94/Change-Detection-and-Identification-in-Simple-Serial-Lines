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
library(moments)

##### Working directory #####
if(Sys.info()["user"] == "Edoardo pc"){
  general_path <- file.path("D:","OneDrive","Articolo2020", "UlterioriTest","Project")
} 
if(Sys.info()["user"] == "edoardo" & Sys.info()["sysname"] == "Linux") {
  general_path <- file.path("/home","edoardo","Documenti","ChangeDetectionAndIdentification","General_folder")
} 
if(Sys.info()["user"] == "edo_c") {
  general_path <- file.path("C:","Users","edo_c","OneDrive","Articolo2020", "UlterioriTest", "Project")
}
if(Sys.info()["user"] == "edo_c" & Sys.info()["nodename"] == "MININT-2C4GEGM") {
  general_path <- file.path("D:","ChangeDetectionIdentification_input_output","General_folder")
}
dir_path <- file.path(general_path)
setwd(dir = dir_path)

##### Percorso file txt #####
param_file_output_path <- file.path(general_path ,"Param_files")

##### Numero di replicazioni #####
tot_replications_ARRAY <- c(30)

##### Changing point #####
change_value_ARRAY <- c(5000)

##### Parametri di arrivo #####
arrival_param_LIST <- list()
arrival_param_LIST[[1]] <- c(10000, 10)

##### Indici di delay e parametri limite code #####
delay_ind_test_LIST <- list()
q_lim_test_LIST <- list()

##### Gruppo 1 #####
# CdB in posizione 4
# 1
delay_ind_test_LIST[[1]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 4, 3, 2, 1), 
  c(1, 2, 3, 4, 8, 2, 1)
))
q_lim_test_LIST[[1]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))


##### Parametri finestre #####
window_param_LIST <- list()
window_param_LIST[[1]] <- c(100, 100)

##### Punto di inizio e fine stampa param_files #####
print_start <- 1
# print_end <- length(delay_ind_test_LIST)
print_end <- 1

##### Stampa file #####
for (i1 in 1:length(tot_replications_ARRAY)) {
  for (i2 in 1:length(change_value_ARRAY)){
    for (i3 in 1:length(arrival_param_LIST)){
      for (i4 in print_start:print_end){
        for (i5 in 1:length(window_param_LIST)){
          
          param_file_output_name <- file.path(
            param_file_output_path, paste0(
              "Model", i4, "_",
              "n", arrival_param_LIST[[i3]][1], "_",
              "inter", arrival_param_LIST[[i3]][2], "_",
              "chv", change_value_ARRAY[i2], "_",
              "rep", tot_replications_ARRAY[i1], "_",
              "w", window_param_LIST[[i5]][1], "f", window_param_LIST[[i5]][2], 
              ".txt"
            )
          )
          
          cat(tot_replications_ARRAY[i1], file=param_file_output_name)
          
          cat("\n", file=param_file_output_name, append=T)
          cat(change_value_ARRAY[i2], file=param_file_output_name, append=T)
          
          cat("\n", file=param_file_output_name, append=T)
          cat(arrival_param_LIST[[i3]], file=param_file_output_name, append=T)
          
          cat("\n", file=param_file_output_name, append=T)
          cat(delay_ind_test_LIST[[i4]][1,], file=param_file_output_name, append=T)
          
          cat("\n", file=param_file_output_name, append=T)
          cat(delay_ind_test_LIST[[i4]][2,], file=param_file_output_name, append=T)
          
          cat("\n", file=param_file_output_name, append=T)
          cat(q_lim_test_LIST[[i4]][1,], file=param_file_output_name, append=T)
          
          cat("\n", file=param_file_output_name, append=T)
          cat(q_lim_test_LIST[[i4]][2,], file=param_file_output_name, append=T)
          
          cat("\n", file=param_file_output_name, append=T)
          cat(window_param_LIST[[i5]], file=param_file_output_name, append=T)
          
        }
      }
    }
  }
}
