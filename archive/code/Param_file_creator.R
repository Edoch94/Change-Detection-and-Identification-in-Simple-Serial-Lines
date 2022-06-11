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

param_file_output_path <- file.path("C:","Users","edo_c","OneDrive","Tesi","Project","Param_files")

##### Numero di replicazioni #####
tot_replications_ARRAY <- c(1)

##### Changing point #####
change_value_ARRAY <- c(5000)

##### Parametri di arrivo #####
arrival_param_LIST <- list()
arrival_param_LIST[[1]] <- c(10000, 10)

##### Indici di delay e parametri limite code #####
delay_ind_test_LIST <- list()
q_lim_test_LIST <- list()

# 1
delay_ind_test_LIST[[1]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 4, 8, 3, 1), 
  c(1, 2, 3, 4, 8, 3, 1)
))
q_lim_test_LIST[[1]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 40, 10, 10, 10, 10),
  c(10, 10, 20, 10, 10, 10, 10)
))

# 2
delay_ind_test_LIST[[2]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 4, 8, 3, 1), 
  c(1, 2, 3, 4, 8, 3, 1)
))
q_lim_test_LIST[[2]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 20, 10, 10, 10, 10),
  c(10, 10, 40, 10, 10, 10, 10)
))

#3
delay_ind_test_LIST[[3]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 4, 2, 3, 1), 
  c(1, 2, 3, 4, 2, 3, 1)
))
q_lim_test_LIST[[3]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 20, 10, 10, 10),
  c(10, 10, 10, 20, 10, 10, 10)
))

#4
delay_ind_test_LIST[[4]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 4, 7, 3, 1), 
  c(1, 2, 3, 4, 7, 3, 1)
))
q_lim_test_LIST[[4]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 50, 10, 10, 10, 10)
))

#5
delay_ind_test_LIST[[5]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 4, 2, 3, 1), 
  c(1, 2, 3, 4, 7, 3, 1)
))
q_lim_test_LIST[[5]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 50, 10, 10, 10, 10)
))

#6
delay_ind_test_LIST[[6]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 4, 7, 3, 1), 
  c(1, 2, 3, 4, 2, 3, 1)
))
q_lim_test_LIST[[6]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 50, 10, 10, 10, 10)
))


##### Stampa file #####
for (i1 in 1:length(tot_replications_ARRAY)) {
  for (i2 in 1:length(change_value_ARRAY)){
    for (i3 in 1:length(arrival_param_LIST)){
      for (i4 in 1:length(delay_ind_test_LIST)){
        
        param_file_output_name <- file.path(
          param_file_output_path, paste0(
            "Model", i4, "_",
            "n", arrival_param_LIST[[i3]][1], "_",
            "inter", arrival_param_LIST[[i3]][2], "_",
            "chv", change_value_ARRAY[i2], "_",
            "rep", tot_replications_ARRAY[i1],
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
        
      }
    }
  }
}
