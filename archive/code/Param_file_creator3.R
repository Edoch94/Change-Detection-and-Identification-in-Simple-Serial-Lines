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
# Benchmark
delay_ind_test_LIST[[1]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 4, 3, 2, 1), 
  c(1, 2, 3, 4, 3, 2, 1)
))
q_lim_test_LIST[[1]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 2
# Aumento tempo di processo in A5
delay_ind_test_LIST[[2]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 4, 3, 2, 1), 
  c(1, 2, 3, 4, 8, 2, 1)
))
q_lim_test_LIST[[2]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 3
# Riduzione tempo di processo in A5
delay_ind_test_LIST[[3]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 4, 3, 2, 1), 
  c(1, 2, 3, 4, 1, 2, 1)
))
q_lim_test_LIST[[3]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 4
# Aumento tempo di processo in A3
delay_ind_test_LIST[[4]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 4, 3, 2, 1), 
  c(1, 2, 8, 4, 3, 2, 1)
))
q_lim_test_LIST[[4]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 5
# Riduzione tempo di processo in A3
delay_ind_test_LIST[[5]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 4, 3, 2, 1), 
  c(1, 2, 1, 4, 3, 2, 1)
))
q_lim_test_LIST[[5]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 6
# Aumento limite buffer in A5
delay_ind_test_LIST[[6]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 4, 3, 2, 1), 
  c(1, 2, 3, 4, 3, 2, 1)
))
q_lim_test_LIST[[6]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 20, 10, 10)
))

# 7
# Riduzione limite buffer in A5
delay_ind_test_LIST[[7]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 4, 3, 2, 1), 
  c(1, 2, 3, 4, 3, 2, 1)
))
q_lim_test_LIST[[7]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 2, 10, 10)
))

# 8
# Aumento limite buffer in A3
delay_ind_test_LIST[[8]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 4, 3, 2, 1), 
  c(1, 2, 3, 4, 3, 2, 1)
))
q_lim_test_LIST[[8]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 20, 10, 10, 10, 10)
))

# 9
# Riduzione limite buffer in A3
delay_ind_test_LIST[[9]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 4, 3, 2, 1), 
  c(1, 2, 3, 4, 3, 2, 1)
))
q_lim_test_LIST[[9]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 2, 10, 10, 10, 10)
))

##### Gruppo 2 #####
# CdB in posizione 5
# 10
# Benchmark
delay_ind_test_LIST[[10]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 4, 2, 1), 
  c(1, 2, 3, 2, 4, 2, 1)
))
q_lim_test_LIST[[10]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 11
# Aumento tempo di processo in A5
delay_ind_test_LIST[[11]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 4, 2, 1), 
  c(1, 2, 3, 2, 8, 2, 1)
))
q_lim_test_LIST[[11]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 12
# Riduzione tempo di processo in A5
delay_ind_test_LIST[[12]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 4, 2, 1), 
  c(1, 2, 3, 2, 1, 2, 1)
))
q_lim_test_LIST[[12]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 13
# Aumento tempo di processo in A3
delay_ind_test_LIST[[13]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 4, 2, 1), 
  c(1, 2, 8, 2, 4, 2, 1)
))
q_lim_test_LIST[[13]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 14
# Riduzione tempo di processo in A3
delay_ind_test_LIST[[14]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 4, 2, 1), 
  c(1, 2, 1, 2, 4, 2, 1)
))
q_lim_test_LIST[[14]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 15
# Aumento limite buffer in A5
delay_ind_test_LIST[[15]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 4, 2, 1), 
  c(1, 2, 3, 2, 4, 2, 1)
))
q_lim_test_LIST[[15]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 20, 10, 10)
))

# 16
# Riduzione limite buffer in A5
delay_ind_test_LIST[[16]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 4, 2, 1), 
  c(1, 2, 3, 2, 4, 2, 1)
))
q_lim_test_LIST[[16]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 2, 10, 10)
))

# 17
# Aumento limite buffer in A3
delay_ind_test_LIST[[17]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 4, 2, 1), 
  c(1, 2, 3, 2, 4, 2, 1)
))
q_lim_test_LIST[[17]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 20, 10, 10, 10, 10)
))

# 18
# Riduzione limite buffer in A3
delay_ind_test_LIST[[18]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 4, 2, 1), 
  c(1, 2, 3, 2, 4, 2, 1)
))
q_lim_test_LIST[[18]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 2, 10, 10, 10, 10)
))

##### Gruppo 3 #####
# CdB in posizione 3
# 19
# Benchmark
delay_ind_test_LIST[[19]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 4, 2, 3, 2, 1), 
  c(1, 2, 4, 2, 3, 2, 1)
))
q_lim_test_LIST[[19]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 20
# Aumento tempo di processo in A5
delay_ind_test_LIST[[20]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 4, 2, 3, 2, 1), 
  c(1, 2, 4, 2, 8, 2, 1)
))
q_lim_test_LIST[[20]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 21
# Riduzione tempo di processo in A5
delay_ind_test_LIST[[21]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 4, 2, 3, 2, 1), 
  c(1, 2, 4, 2, 2, 2, 1)
))
q_lim_test_LIST[[21]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 22
# Aumento tempo di processo in A3
delay_ind_test_LIST[[22]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 4, 2, 3, 2, 1), 
  c(1, 2, 8, 2, 3, 2, 1)
))
q_lim_test_LIST[[22]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 23
# Riduzione tempo di processo in A3
delay_ind_test_LIST[[23]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 4, 2, 3, 2, 1), 
  c(1, 2, 2, 2, 3, 2, 1)
))
q_lim_test_LIST[[23]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 24
# Aumento limite buffer in A5
delay_ind_test_LIST[[24]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 4, 2, 3, 2, 1), 
  c(1, 2, 4, 2, 3, 2, 1)
))
q_lim_test_LIST[[24]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 20, 10, 10)
))

# 25
# Riduzione limite buffer in A5
delay_ind_test_LIST[[25]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 4, 2, 3, 2, 1), 
  c(1, 2, 4, 2, 3, 2, 1)
))
q_lim_test_LIST[[25]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 2, 10, 10)
))

# 26
# Aumento limite buffer in A3
delay_ind_test_LIST[[26]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 4, 2, 3, 2, 1), 
  c(1, 2, 4, 2, 3, 2, 1)
))
q_lim_test_LIST[[26]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 20, 10, 10, 10, 10)
))

# 27
# Riduzione limite buffer in A3
delay_ind_test_LIST[[27]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 4, 2, 3, 2, 1), 
  c(1, 2, 4, 2, 3, 2, 1)
))
q_lim_test_LIST[[27]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 2, 10, 10, 10, 10)
))

##### Gruppo 4 #####
# CdB in posizione 4
# 28
# Benchmark
delay_ind_test_LIST[[28]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 4, 2, 2, 1), 
  c(1, 2, 2, 4, 2, 2, 1)
))
q_lim_test_LIST[[28]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 29
# Aumento tempo di processo in A5
delay_ind_test_LIST[[29]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 4, 2, 2, 1), 
  c(1, 2, 2, 4, 3, 2, 1)
))
q_lim_test_LIST[[29]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 30
# Riduzione tempo di processo in A5
delay_ind_test_LIST[[30]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 4, 2, 2, 1), 
  c(1, 2, 2, 4, 1, 2, 1)
))
q_lim_test_LIST[[30]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 31
# Aumento tempo di processo in A3
delay_ind_test_LIST[[31]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 4, 2, 2, 1), 
  c(1, 2, 3, 4, 2, 2, 1)
))
q_lim_test_LIST[[31]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 32
# Riduzione tempo di processo in A3
delay_ind_test_LIST[[32]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 4, 2, 2, 1), 
  c(1, 2, 1, 4, 2, 2, 1)
))
q_lim_test_LIST[[32]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 33
# Aumento limite buffer in A5
delay_ind_test_LIST[[33]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 4, 2, 2, 1), 
  c(1, 2, 2, 4, 2, 2, 1)
))
q_lim_test_LIST[[33]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 20, 10, 10)
))

# 34
# Riduzione limite buffer in A5
delay_ind_test_LIST[[34]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 4, 2, 2, 1), 
  c(1, 2, 2, 4, 2, 2, 1)
))
q_lim_test_LIST[[34]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 2, 10, 10)
))

# 35
# Aumento limite buffer in A3
delay_ind_test_LIST[[35]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 4, 2, 2, 1), 
  c(1, 2, 2, 4, 2, 2, 1)
))
q_lim_test_LIST[[35]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 20, 10, 10, 10, 10)
))

# 36
# Riduzione limite buffer in A3
delay_ind_test_LIST[[36]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 4, 2, 2, 1), 
  c(1, 2, 2, 4, 2, 2, 1)
))
q_lim_test_LIST[[36]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 2, 10, 10, 10, 10)
))

##### Gruppo 5 #####
# CdB in posizione 5
# 37
# Benchmark
delay_ind_test_LIST[[37]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 4, 2, 1), 
  c(1, 2, 2, 2, 4, 2, 1)
))
q_lim_test_LIST[[37]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 38
# Aumento tempo di processo in A5
delay_ind_test_LIST[[38]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 4, 2, 1), 
  c(1, 2, 2, 2, 5, 2, 1)
))
q_lim_test_LIST[[38]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 39
# Riduzione tempo di processo in A5
delay_ind_test_LIST[[39]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 4, 2, 1), 
  c(1, 2, 2, 2, 1, 2, 1)
))
q_lim_test_LIST[[39]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 40
# Aumento tempo di processo in A3
delay_ind_test_LIST[[40]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 4, 2, 1), 
  c(1, 2, 3, 2, 4, 2, 1)
))
q_lim_test_LIST[[40]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 41
# Riduzione tempo di processo in A3
delay_ind_test_LIST[[41]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 4, 2, 1), 
  c(1, 2, 1, 2, 4, 2, 1)
))
q_lim_test_LIST[[41]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 42
# Aumento limite buffer in A5
delay_ind_test_LIST[[42]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 4, 2, 1), 
  c(1, 2, 2, 2, 4, 2, 1)
))
q_lim_test_LIST[[42]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 20, 10, 10)
))

# 43
# Riduzione limite buffer in A5
delay_ind_test_LIST[[43]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 4, 2, 1), 
  c(1, 2, 2, 2, 4, 2, 1)
))
q_lim_test_LIST[[43]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 2, 10, 10)
))

# 44
# Aumento limite buffer in A3
delay_ind_test_LIST[[44]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 4, 2, 1), 
  c(1, 2, 2, 2, 4, 2, 1)
))
q_lim_test_LIST[[44]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 20, 10, 10, 10, 10)
))

# 45
# Riduzione limite buffer in A3
delay_ind_test_LIST[[45]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 4, 2, 1), 
  c(1, 2, 2, 2, 4, 2, 1)
))
q_lim_test_LIST[[45]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 2, 10, 10, 10, 10)
))

##### Gruppo 6 #####
# CdB in posizione 3
# 46
# Benchmark
delay_ind_test_LIST[[46]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 4, 2, 2, 2, 1), 
  c(1, 2, 4, 2, 2, 2, 1)
))
q_lim_test_LIST[[46]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 47
# Aumento tempo di processo in A5
delay_ind_test_LIST[[47]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 4, 2, 2, 2, 1), 
  c(1, 2, 4, 2, 3, 2, 1)
))
q_lim_test_LIST[[47]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 48
# Riduzione tempo di processo in A5
delay_ind_test_LIST[[48]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 4, 2, 2, 2, 1), 
  c(1, 2, 4, 2, 1, 2, 1)
))
q_lim_test_LIST[[48]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 49
# Aumento tempo di processo in A3
delay_ind_test_LIST[[49]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 4, 2, 2, 2, 1), 
  c(1, 2, 5, 2, 2, 2, 1)
))
q_lim_test_LIST[[49]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 50
# Riduzione tempo di processo in A3
delay_ind_test_LIST[[50]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 4, 2, 2, 2, 1), 
  c(1, 2, 1, 2, 2, 2, 1)
))
q_lim_test_LIST[[50]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 51
# Aumento limite buffer in A5
delay_ind_test_LIST[[51]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 4, 2, 2, 2, 1), 
  c(1, 2, 4, 2, 2, 2, 1)
))
q_lim_test_LIST[[51]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 20, 10, 10)
))

# 52
# Riduzione limite buffer in A5
delay_ind_test_LIST[[52]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 4, 2, 2, 2, 1), 
  c(1, 2, 4, 2, 2, 2, 1)
))
q_lim_test_LIST[[52]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 2, 10, 10)
))

# 53
# Aumento limite buffer in A3
delay_ind_test_LIST[[53]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 4, 2, 2, 2, 1), 
  c(1, 2, 4, 2, 2, 2, 1)
))
q_lim_test_LIST[[53]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 20, 10, 10, 10, 10)
))

# 54
# Riduzione limite buffer in A3
delay_ind_test_LIST[[54]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 4, 2, 2, 2, 1), 
  c(1, 2, 4, 2, 2, 2, 1)
))
q_lim_test_LIST[[54]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 2, 10, 10, 10, 10)
))

##### Gruppo 7 #####
# CdB in posizione 4
# 55
# Benchmark
delay_ind_test_LIST[[55]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 3, 2, 2, 1), 
  c(1, 2, 2, 3, 2, 2, 1)
))
q_lim_test_LIST[[55]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 56
# Aumento tempo di processo in A5
delay_ind_test_LIST[[56]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 3, 2, 2, 1), 
  c(1, 2, 2, 3, 4, 2, 1)
))
q_lim_test_LIST[[56]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 57
# Riduzione tempo di processo in A5
delay_ind_test_LIST[[57]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 3, 2, 2, 1), 
  c(1, 2, 2, 3, 1, 2, 1)
))
q_lim_test_LIST[[57]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 58
# Aumento tempo di processo in A3
delay_ind_test_LIST[[58]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 3, 2, 2, 1), 
  c(1, 2, 4, 3, 2, 2, 1)
))
q_lim_test_LIST[[58]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 59
# Riduzione tempo di processo in A3
delay_ind_test_LIST[[59]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 3, 2, 2, 1), 
  c(1, 2, 1, 3, 2, 2, 1)
))
q_lim_test_LIST[[59]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 60
# Aumento limite buffer in A5
delay_ind_test_LIST[[60]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 3, 2, 2, 1), 
  c(1, 2, 2, 3, 2, 2, 1)
))
q_lim_test_LIST[[60]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 20, 10, 10)
))

# 61
# Riduzione limite buffer in A5
delay_ind_test_LIST[[61]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 3, 2, 2, 1), 
  c(1, 2, 2, 3, 2, 2, 1)
))
q_lim_test_LIST[[61]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 2, 10, 10)
))

# 62
# Aumento limite buffer in A3
delay_ind_test_LIST[[62]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 3, 2, 2, 1), 
  c(1, 2, 2, 3, 2, 2, 1)
))
q_lim_test_LIST[[62]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 20, 10, 10, 10, 10)
))

# 63
# Riduzione limite buffer in A3
delay_ind_test_LIST[[63]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 3, 2, 2, 1), 
  c(1, 2, 2, 3, 2, 2, 1)
))
q_lim_test_LIST[[63]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 2, 10, 10, 10, 10)
))

##### Gruppo 8 #####
# CdB in posizione 5
# 64
# Benchmark
delay_ind_test_LIST[[64]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 3, 2, 1), 
  c(1, 2, 2, 2, 3, 2, 1)
))
q_lim_test_LIST[[64]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 65
# Aumento tempo di processo in A5
delay_ind_test_LIST[[65]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 3, 2, 1), 
  c(1, 2, 2, 2, 4, 2, 1)
))
q_lim_test_LIST[[65]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 66
# Riduzione tempo di processo in A5
delay_ind_test_LIST[[66]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 3, 2, 1), 
  c(1, 2, 2, 2, 1, 2, 1)
))
q_lim_test_LIST[[66]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 67
# Aumento tempo di processo in A3
delay_ind_test_LIST[[67]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 3, 2, 1), 
  c(1, 2, 4, 2, 3, 2, 1)
))
q_lim_test_LIST[[67]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 68
# Riduzione tempo di processo in A3
delay_ind_test_LIST[[68]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 3, 2, 1), 
  c(1, 2, 1, 2, 3, 2, 1)
))
q_lim_test_LIST[[68]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 69
# Aumento limite buffer in A5
delay_ind_test_LIST[[69]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 3, 2, 1), 
  c(1, 2, 2, 2, 3, 2, 1)
))
q_lim_test_LIST[[69]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 20, 10, 10)
))

# 70
# Riduzione limite buffer in A5
delay_ind_test_LIST[[70]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 3, 2, 1), 
  c(1, 2, 2, 2, 3, 2, 1)
))
q_lim_test_LIST[[70]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 2, 10, 10)
))

# 71
# Aumento limite buffer in A3
delay_ind_test_LIST[[71]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 3, 2, 1), 
  c(1, 2, 2, 2, 3, 2, 1)
))
q_lim_test_LIST[[71]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 20, 10, 10, 10, 10)
))

# 72
# Riduzione limite buffer in A3
delay_ind_test_LIST[[72]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 3, 2, 1), 
  c(1, 2, 2, 2, 3, 2, 1)
))
q_lim_test_LIST[[72]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 2, 10, 10, 10, 10)
))

##### Gruppo 9 #####
# CdB in posizione 3
# 73
# Benchmark
delay_ind_test_LIST[[73]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 2, 2, 1), 
  c(1, 2, 3, 2, 2, 2, 1)
))
q_lim_test_LIST[[73]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 74
# Aumento tempo di processo in A5
delay_ind_test_LIST[[74]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 2, 2, 1), 
  c(1, 2, 3, 2, 4, 2, 1)
))
q_lim_test_LIST[[74]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 75
# Riduzione tempo di processo in A5
delay_ind_test_LIST[[75]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 2, 2, 1), 
  c(1, 2, 3, 2, 1, 2, 1)
))
q_lim_test_LIST[[75]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 76
# Aumento tempo di processo in A3
delay_ind_test_LIST[[76]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 2, 2, 1), 
  c(1, 2, 4, 2, 2, 2, 1)
))
q_lim_test_LIST[[76]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 77
# Riduzione tempo di processo in A3
delay_ind_test_LIST[[77]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 2, 2, 1), 
  c(1, 2, 1, 2, 2, 2, 1)
))
q_lim_test_LIST[[77]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 78
# Aumento limite buffer in A5
delay_ind_test_LIST[[78]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 2, 2, 1), 
  c(1, 2, 3, 2, 2, 2, 1)
))
q_lim_test_LIST[[78]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 20, 10, 10)
))

# 79
# Riduzione limite buffer in A5
delay_ind_test_LIST[[79]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 2, 2, 1), 
  c(1, 2, 3, 2, 2, 2, 1)
))
q_lim_test_LIST[[79]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 2, 10, 10)
))

# 80
# Aumento limite buffer in A3
delay_ind_test_LIST[[80]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 2, 2, 1), 
  c(1, 2, 3, 2, 2, 2, 1)
))
q_lim_test_LIST[[80]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 20, 10, 10, 10, 10)
))

# 81
# Riduzione limite buffer in A3
delay_ind_test_LIST[[81]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 2, 2, 1), 
  c(1, 2, 3, 2, 2, 2, 1)
))
q_lim_test_LIST[[81]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 2, 10, 10, 10, 10)
))

##### Gruppo 10 #####
# CdB in posizione 4: tempo di interarrivo 15
# 82
# Benchmark
delay_ind_test_LIST[[82]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 8, 3, 2, 1), 
  c(1, 2, 3, 8, 3, 2, 1)
))
q_lim_test_LIST[[82]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 83
# Aumento tempo di processo in A5
delay_ind_test_LIST[[83]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 8, 3, 2, 1), 
  c(1, 2, 3, 8, 6, 2, 1)
))
q_lim_test_LIST[[83]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 84
# Riduzione tempo di processo in A5
delay_ind_test_LIST[[84]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 8, 3, 2, 1), 
  c(1, 2, 3, 8, 1, 2, 1)
))
q_lim_test_LIST[[84]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 85
# Aumento tempo di processo in A3
delay_ind_test_LIST[[85]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 8, 3, 2, 1), 
  c(1, 2, 6, 8, 3, 2, 1)
))
q_lim_test_LIST[[85]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 86
# Riduzione tempo di processo in A3
delay_ind_test_LIST[[86]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 8, 3, 2, 1), 
  c(1, 2, 1, 8, 3, 2, 1)
))
q_lim_test_LIST[[86]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 87
# Aumento limite buffer in A5
delay_ind_test_LIST[[87]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 8, 3, 2, 1), 
  c(1, 2, 3, 8, 3, 2, 1)
))
q_lim_test_LIST[[87]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 20, 10, 10)
))

# 88
# Riduzione limite buffer in A5
delay_ind_test_LIST[[88]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 8, 3, 2, 1), 
  c(1, 2, 3, 8, 3, 2, 1)
))
q_lim_test_LIST[[88]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 2, 10, 10)
))

# 89
# Aumento limite buffer in A3
delay_ind_test_LIST[[89]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 8, 3, 2, 1), 
  c(1, 2, 3, 8, 3, 2, 1)
))
q_lim_test_LIST[[89]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 20, 10, 10, 10, 10)
))

# 90
# Riduzione limite buffer in A3
delay_ind_test_LIST[[90]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 8, 3, 2, 1), 
  c(1, 2, 3, 8, 3, 2, 1)
))
q_lim_test_LIST[[90]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 2, 10, 10, 10, 10)
))

##### Gruppo 11 #####
# CdB in posizione 5: tempo di interarrivo 15
# 91
# Benchmark
delay_ind_test_LIST[[91]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 6, 2, 1), 
  c(1, 2, 3, 2, 6, 2, 1)
))
q_lim_test_LIST[[91]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 92
# Aumento tempo di processo in A5
delay_ind_test_LIST[[92]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 6, 2, 1), 
  c(1, 2, 3, 2, 8, 2, 1)
))
q_lim_test_LIST[[92]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 93
# Riduzione tempo di processo in A5
delay_ind_test_LIST[[93]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 6, 2, 1), 
  c(1, 2, 3, 2, 4, 2, 1)
))
q_lim_test_LIST[[93]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 94
# Aumento tempo di processo in A3
delay_ind_test_LIST[[94]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 6, 2, 1), 
  c(1, 2, 5, 2, 6, 2, 1)
))
q_lim_test_LIST[[94]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 95
# Riduzione tempo di processo in A3
delay_ind_test_LIST[[95]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 6, 2, 1), 
  c(1, 2, 1, 2, 6, 2, 1)
))
q_lim_test_LIST[[95]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 96
# Aumento limite buffer in A5
delay_ind_test_LIST[[96]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 6, 2, 1), 
  c(1, 2, 3, 2, 6, 2, 1)
))
q_lim_test_LIST[[96]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 20, 10, 10)
))

# 97
# Riduzione limite buffer in A5
delay_ind_test_LIST[[97]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 6, 2, 1), 
  c(1, 2, 3, 2, 6, 2, 1)
))
q_lim_test_LIST[[97]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 2, 10, 10)
))

# 98
# Aumento limite buffer in A3
delay_ind_test_LIST[[98]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 6, 2, 1), 
  c(1, 2, 3, 2, 6, 2, 1)
))
q_lim_test_LIST[[98]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 20, 10, 10, 10, 10)
))

# 99
# Riduzione limite buffer in A3
delay_ind_test_LIST[[99]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 2, 6, 2, 1), 
  c(1, 2, 3, 2, 6, 2, 1)
))
q_lim_test_LIST[[99]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 2, 10, 10, 10, 10)
))

##### Gruppo 12 #####
# CdB in posizione 4: tempo di interarrivo 15, buffer con bassa capacità
# 100
# Benchmark
delay_ind_test_LIST[[100]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 9, 3, 2, 1), 
  c(1, 2, 3, 9, 3, 2, 1)
))
q_lim_test_LIST[[100]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 101
# Aumento tempo di processo in A5
delay_ind_test_LIST[[101]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 9, 3, 2, 1), 
  c(1, 2, 3, 9, 6, 2, 1)
))
q_lim_test_LIST[[101]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 102
# Riduzione tempo di processo in A5
delay_ind_test_LIST[[102]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 9, 3, 2, 1), 
  c(1, 2, 3, 9, 1, 2, 1)
))
q_lim_test_LIST[[102]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 103
# Aumento tempo di processo in A3
delay_ind_test_LIST[[103]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 9, 3, 2, 1), 
  c(1, 2, 6, 9, 3, 2, 1)
))
q_lim_test_LIST[[103]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 104
# Riduzione tempo di processo in A3
delay_ind_test_LIST[[104]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 9, 3, 2, 1), 
  c(1, 2, 1, 9, 3, 2, 1)
))
q_lim_test_LIST[[104]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 105
# Aumento limite buffer in A5
delay_ind_test_LIST[[105]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 9, 3, 2, 1), 
  c(1, 2, 3, 9, 3, 2, 1)
))
q_lim_test_LIST[[105]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 10, 3, 3)
))

# 106
# Riduzione limite buffer in A5
delay_ind_test_LIST[[106]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 9, 3, 2, 1), 
  c(1, 2, 3, 9, 3, 2, 1)
))
q_lim_test_LIST[[106]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 1, 3, 3)
))

# 107
# Aumento limite buffer in A3
delay_ind_test_LIST[[107]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 9, 3, 2, 1), 
  c(1, 2, 3, 9, 3, 2, 1)
))
q_lim_test_LIST[[107]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 10, 3, 3, 3, 3)
))

# 108
# Riduzione limite buffer in A3
delay_ind_test_LIST[[108]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 9, 3, 2, 1), 
  c(1, 2, 3, 9, 3, 2, 1)
))
q_lim_test_LIST[[108]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 1, 3, 3, 3, 3)
))

##### Gruppo 13 #####
# CdB in posizione 3: tempo di interarrivo 15, buffer con bassa capacità
# 109
# Benchmark
delay_ind_test_LIST[[109]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 9, 3, 6, 2, 1), 
  c(1, 2, 9, 3, 6, 2, 1)
))
q_lim_test_LIST[[109]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 110
# Aumento tempo di processo in A5
delay_ind_test_LIST[[110]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 9, 3, 6, 2, 1), 
  c(1, 2, 9, 3, 8, 2, 1)
))
q_lim_test_LIST[[110]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 111
# Riduzione tempo di processo in A5
delay_ind_test_LIST[[111]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 9, 3, 6, 2, 1), 
  c(1, 2, 9, 3, 4, 2, 1)
))
q_lim_test_LIST[[111]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 112
# Aumento tempo di processo in A3
delay_ind_test_LIST[[112]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 9, 3, 6, 2, 1), 
  c(1, 2, 10, 3, 6, 2, 1)
))
q_lim_test_LIST[[112]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 113
# Riduzione tempo di processo in A3
delay_ind_test_LIST[[113]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 9, 3, 6, 2, 1), 
  c(1, 2, 4, 3, 6, 2, 1)
))
q_lim_test_LIST[[113]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 114
# Aumento limite buffer in A5
delay_ind_test_LIST[[114]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 9, 3, 6, 2, 1), 
  c(1, 2, 9, 3, 6, 2, 1)
))
q_lim_test_LIST[[114]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 10, 3, 3)
))

# 115
# Riduzione limite buffer in A5
delay_ind_test_LIST[[115]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 9, 3, 6, 2, 1), 
  c(1, 2, 9, 3, 6, 2, 1)
))
q_lim_test_LIST[[115]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 1, 3, 3)
))

# 116
# Aumento limite buffer in A3
delay_ind_test_LIST[[116]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 9, 3, 6, 2, 1), 
  c(1, 2, 9, 3, 6, 2, 1)
))
q_lim_test_LIST[[116]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 10, 3, 3, 3, 3)
))

# 117
# Riduzione limite buffer in A3
delay_ind_test_LIST[[117]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 9, 3, 6, 2, 1), 
  c(1, 2, 9, 3, 6, 2, 1)
))
q_lim_test_LIST[[117]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 1, 3, 3, 3, 3)
))

##### Gruppo 14 #####
# CdB in posizione 4: tempo di interarrivo 15, buffer con variazioni 
# 118
# Aumento buffer: insaturo a insaturo
delay_ind_test_LIST[[118]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 9, 3, 2, 1), 
  c(1, 2, 3, 9, 3, 2, 1)
))
q_lim_test_LIST[[118]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 20, 10, 10, 10)
))

# 119
# Aumento buffer: saturo a saturo
delay_ind_test_LIST[[119]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 9, 3, 2, 1), 
  c(1, 2, 3, 9, 3, 2, 1)
))
q_lim_test_LIST[[119]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 8, 3, 3, 3)
))

# 120
# Aumento buffer: saturo a insaturo
delay_ind_test_LIST[[120]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 9, 3, 2, 1), 
  c(1, 2, 3, 9, 3, 2, 1)
))
q_lim_test_LIST[[120]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 5, 5, 5),
  c(5, 5, 5, 20, 5, 5, 5)
))

# 121
# Riduzione buffer: insaturo a insaturo
delay_ind_test_LIST[[121]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 9, 3, 2, 1), 
  c(1, 2, 3, 9, 3, 2, 1)
))
q_lim_test_LIST[[121]] <- matrix(byrow = T, ncol = 7, c(
  c(20, 20, 20, 20, 20, 20, 20),
  c(20, 20, 20, 10, 20, 20, 20)
))

# 122
# Riduzione buffer: saturo a saturo
delay_ind_test_LIST[[122]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 9, 3, 2, 1), 
  c(1, 2, 3, 9, 3, 2, 1)
))
q_lim_test_LIST[[122]] <- matrix(byrow = T, ncol = 7, c(
  c(8, 8, 8, 8, 8, 8, 8),
  c(8, 8, 8, 3, 8, 8, 8)
))

# 123
# Riduzione buffer: insaturo a saturo
delay_ind_test_LIST[[123]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 9, 3, 2, 1), 
  c(1, 2, 3, 9, 3, 2, 1)
))
q_lim_test_LIST[[123]] <- matrix(byrow = T, ncol = 7, c(
  c(20, 20, 20, 20, 20, 20, 20),
  c(20, 20, 20, 5, 20, 20, 20)
))

# 124
# Aumento buffer: insaturo a insaturo
delay_ind_test_LIST[[124]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 9, 3, 2, 1), 
  c(1, 2, 3, 9, 3, 2, 1)
))
q_lim_test_LIST[[124]] <- matrix(byrow = T, ncol = 7, c(
  c(2, 3, 4, 8, 2, 2, 2),
  c(2, 3, 4, 12, 2, 2, 2)
))

# 125
# Aumento buffer: saturo a saturo
delay_ind_test_LIST[[125]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 9, 3, 2, 1), 
  c(1, 2, 3, 9, 3, 2, 1)
))
q_lim_test_LIST[[125]] <- matrix(byrow = T, ncol = 7, c(
  c(2, 3, 4, 2, 2, 2, 2),
  c(2, 3, 4, 5, 2, 2, 2)
))

# 126
# Aumento buffer: saturo a insaturo
delay_ind_test_LIST[[126]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 9, 3, 2, 1), 
  c(1, 2, 3, 9, 3, 2, 1)
))
q_lim_test_LIST[[126]] <- matrix(byrow = T, ncol = 7, c(
  c(2, 3, 4, 2, 2, 2, 2),
  c(2, 3, 4, 10, 2, 2, 2)
))

# 127
# Riduzione buffer: insaturo a insaturo
delay_ind_test_LIST[[127]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 9, 3, 2, 1), 
  c(1, 2, 3, 9, 3, 2, 1)
))
q_lim_test_LIST[[127]] <- matrix(byrow = T, ncol = 7, c(
  c(2, 3, 4, 8, 2, 2, 2),
  c(2, 3, 4, 12, 2, 2, 2)
))

# 128
# Riduzione buffer: saturo a saturo
delay_ind_test_LIST[[128]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 9, 3, 2, 1), 
  c(1, 2, 3, 9, 3, 2, 1)
))
q_lim_test_LIST[[128]] <- matrix(byrow = T, ncol = 7, c(
  c(2, 3, 4, 5, 2, 2, 2),
  c(2, 3, 4, 2, 2, 2, 2)
))

# 129
# Riduzione buffer: insaturo a saturo
delay_ind_test_LIST[[129]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 9, 3, 2, 1), 
  c(1, 2, 3, 9, 3, 2, 1)
))
q_lim_test_LIST[[129]] <- matrix(byrow = T, ncol = 7, c(
  c(2, 3, 4, 10, 2, 2, 2),
  c(2, 3, 4, 2, 2, 2, 2)
))

##### Gruppo 15 #####
# 130
# Riduzione buffer: estremamente insaturo a estremamente insaturo
delay_ind_test_LIST[[130]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 6, 3, 2, 1), 
  c(1, 2, 3, 6, 3, 2, 1)
))
q_lim_test_LIST[[130]] <- matrix(byrow = T, ncol = 7, c(
  c(200, 200, 200, 200, 200, 200, 200),
  c(200, 200, 200, 100, 200, 200, 200)
))

# 131
# Aumento buffer: estremamente insaturo a estremamente insaturo
delay_ind_test_LIST[[131]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 6, 3, 2, 1), 
  c(1, 2, 3, 6, 3, 2, 1)
))
q_lim_test_LIST[[131]] <- matrix(byrow = T, ncol = 7, c(
  c(200, 200, 200, 200, 200, 200, 200),
  c(200, 200, 200, 300, 200, 200, 200)
))

# 132
# Aumento tempo di processo: buffer estremamente insaturo
delay_ind_test_LIST[[132]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 6, 3, 2, 1), 
  c(1, 2, 3, 9, 3, 2, 1)
))
q_lim_test_LIST[[132]] <- matrix(byrow = T, ncol = 7, c(
  c(200, 200, 200, 200, 200, 200, 200),
  c(200, 200, 200, 200, 200, 200, 200)
))

# 133
# Riduzione tempo di processo: buffer estremamente insaturo
delay_ind_test_LIST[[133]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 6, 3, 2, 1), 
  c(1, 2, 3, 2, 3, 2, 1)
))
q_lim_test_LIST[[133]] <- matrix(byrow = T, ncol = 7, c(
  c(200, 200, 200, 200, 200, 200, 200),
  c(200, 200, 200, 200, 200, 200, 200)
))

# 134
# Riduzione tempo di processo: buffer estremamente insaturo | Media variazione
delay_ind_test_LIST[[134]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 6, 8, 6, 2, 1), 
  c(1, 2, 6, 8, 2, 2, 1)
))
q_lim_test_LIST[[134]] <- matrix(byrow = T, ncol = 7, c(
  c(200, 200, 200, 200, 200, 200, 200),
  c(200, 200, 200, 200, 200, 200, 200)
))

# 135
# Riduzione tempo di processo: buffer estremamente insaturo | Media variazione
delay_ind_test_LIST[[135]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 6, 8, 6, 2, 1), 
  c(1, 2, 2, 8, 6, 2, 1)
))
q_lim_test_LIST[[135]] <- matrix(byrow = T, ncol = 7, c(
  c(200, 200, 200, 200, 200, 200, 200),
  c(200, 200, 200, 200, 200, 200, 200)
))

# 136
# Riduzione tempo di processo: buffer estremamente insaturo | Piccola variazione
delay_ind_test_LIST[[136]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 4, 3, 2, 1), 
  c(1, 2, 3, 4, 2, 2, 1)
))
q_lim_test_LIST[[136]] <- matrix(byrow = T, ncol = 7, c(
  c(200, 200, 200, 200, 200, 200, 200),
  c(200, 200, 200, 200, 200, 200, 200)
))

# 137
# Riduzione tempo di processo: buffer estremamente insaturo | Piccola variazione
delay_ind_test_LIST[[137]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 3, 4, 3, 2, 1), 
  c(1, 2, 2, 4, 3, 2, 1)
))
q_lim_test_LIST[[137]] <- matrix(byrow = T, ncol = 7, c(
  c(200, 200, 200, 200, 200, 200, 200),
  c(200, 200, 200, 200, 200, 200, 200)
))

# 138
# Riduzione tempo di processo: buffer estremamente insaturo | Media variazione
delay_ind_test_LIST[[138]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 6, 8, 6, 6, 1), 
  c(1, 2, 6, 8, 2, 6, 1)
))
q_lim_test_LIST[[138]] <- matrix(byrow = T, ncol = 7, c(
  c(200, 200, 200, 200, 200, 200, 200),
  c(200, 200, 200, 200, 200, 200, 200)
))

# 139
# Riduzione tempo di processo: buffer estremamente insaturo | Media variazione
delay_ind_test_LIST[[139]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 6, 8, 6, 6, 1), 
  c(1, 2, 2, 8, 6, 6, 1)
))
q_lim_test_LIST[[139]] <- matrix(byrow = T, ncol = 7, c(
  c(200, 200, 200, 200, 200, 200, 200),
  c(200, 200, 200, 200, 200, 200, 200)
))

##### Gruppo 16 #####
# 140
# Aumento buffer: saturo a saturo; cdb in corrispondenza 
delay_ind_test_LIST[[140]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 7, 7, 7, 9, 7, 1), 
  c(1, 7, 7, 7, 9, 7, 1)
))
q_lim_test_LIST[[140]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 5, 3, 3)
))

# 141
# Aumento buffer: saturo a saturo; cdb a monte
delay_ind_test_LIST[[141]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 7, 7, 7, 9, 7, 1), 
  c(1, 7, 7, 7, 9, 7, 1)
))
q_lim_test_LIST[[141]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 5, 3)
))

# 142
# Aumento buffer: saturo a saturo; cdb a valle
delay_ind_test_LIST[[142]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 7, 7, 7, 9, 7, 1), 
  c(1, 7, 7, 7, 9, 7, 1)
))
q_lim_test_LIST[[142]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 5, 3, 3, 3, 3)
))

# 143
# Aumento buffer: saturo a saturo; cdb a monte; tempo di processo dopo cdb molto simile
delay_ind_test_LIST[[143]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 7, 7, 7, 9, 8, 1), 
  c(1, 7, 7, 7, 9, 8, 1)
))
q_lim_test_LIST[[143]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 5, 3)
))

# 144
# Aumento buffer: estremamente saturo a saturo; cdb a monte; tempo di processo dopo cdb molto simile
delay_ind_test_LIST[[144]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 7, 7, 7, 9, 8, 1), 
  c(1, 7, 7, 7, 9, 8, 1)
))
q_lim_test_LIST[[144]] <- matrix(byrow = T, ncol = 7, c(
  c(2, 2, 2, 2, 2, 2, 2),
  c(2, 2, 2, 2, 2, 3, 2)
))

# 145
# Aumento buffer: estremamente saturo a saturo (solo in corrispondenza); cdb a monte; tempo di processo dopo cdb molto simile
delay_ind_test_LIST[[145]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 7, 7, 9, 8, 7, 1), 
  c(1, 7, 7, 9, 8, 7, 1)
))
q_lim_test_LIST[[145]] <- matrix(byrow = T, ncol = 7, c(
  c(100, 100, 100, 100, 1, 100, 100),
  c(100, 100, 100, 100, 2, 100, 100)
))

# 146
# Aumento buffer: estremamente saturo a saturo (solo in corrispondenza); cdb a monte; tempo di processo dopo cdb dissimile
delay_ind_test_LIST[[146]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 7, 7, 9, 5, 7, 1), 
  c(1, 7, 7, 9, 5, 7, 1)
))
q_lim_test_LIST[[146]] <- matrix(byrow = T, ncol = 7, c(
  c(100, 100, 100, 100, 1, 100, 100),
  c(100, 100, 100, 100, 2, 100, 100)
))

# 147
# Aumento buffer: estremamente saturo a saturo (solo in corrispondenza); cdb a monte; tempo di processo dopo cdb dissimile
delay_ind_test_LIST[[147]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 7, 7, 9, 2, 7, 1), 
  c(1, 7, 7, 9, 2, 7, 1)
))
q_lim_test_LIST[[147]] <- matrix(byrow = T, ncol = 7, c(
  c(100, 100, 100, 100, 1, 100, 100),
  c(100, 100, 100, 100, 2, 100, 100)
))

# 148
# Aumento buffer: estremamente saturo a saturo (solo in corrispondenza); cdb a monte; tempo di processo dopo cdb molto basso in corrispondenza
delay_ind_test_LIST[[148]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 7, 9, 7, 6, 7, 1), 
  c(1, 7, 9, 7, 6, 7, 1)
))
q_lim_test_LIST[[148]] <- matrix(byrow = T, ncol = 7, c(
  c(100, 100, 100, 100, 1, 100, 100),
  c(100, 100, 100, 100, 2, 100, 100)
))

# 149
# Aumento buffer: estremamente saturo a saturo (solo in corrispondenza); cdb a monte; tempo di processo dopo cdb molto basso in corrispondenza
delay_ind_test_LIST[[149]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 7, 9, 7, 4, 7, 1), 
  c(1, 7, 9, 7, 4, 7, 1)
))
q_lim_test_LIST[[149]] <- matrix(byrow = T, ncol = 7, c(
  c(100, 100, 100, 100, 1, 100, 100),
  c(100, 100, 100, 100, 2, 100, 100)
))

# 150
# Aumento buffer: estremamente saturo a saturo (solo in corrispondenza); cdb a monte; tempo di processo dopo cdb molto basso in corrispondenza
delay_ind_test_LIST[[150]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 7, 9, 7, 2, 7, 1), 
  c(1, 7, 9, 7, 2, 7, 1)
))
q_lim_test_LIST[[150]] <- matrix(byrow = T, ncol = 7, c(
  c(100, 100, 100, 100, 1, 100, 100),
  c(100, 100, 100, 100, 2, 100, 100)
))

##### Gruppo 17 #####
# 151
# Aumento tempo di processo: buffer insaturo a insaturo; cdb a valle
delay_ind_test_LIST[[151]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 1, 1, 5, 9, 2, 1), 
  c(1, 1, 1, 8, 9, 2, 1)
))
q_lim_test_LIST[[151]] <- matrix(byrow = T, ncol = 7, c(
  c(200, 200, 200, 200, 200, 200, 200),
  c(200, 200, 200, 200, 200, 200, 200)
))

# 152
# Aumento tempo di processo: buffer insaturo a insaturo; cdb a monte
delay_ind_test_LIST[[152]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 1, 9, 5, 2, 2, 1), 
  c(1, 1, 9, 8, 2, 2, 1)
))
q_lim_test_LIST[[152]] <- matrix(byrow = T, ncol = 7, c(
  c(200, 200, 200, 200, 200, 200, 200),
  c(200, 200, 200, 200, 200, 200, 200)
))

# 153
# Aumento tempo di processo: buffer insaturo a insaturo; cdb in corrispondenza
delay_ind_test_LIST[[153]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 1, 1, 7, 2, 2, 1), 
  c(1, 1, 1, 8, 2, 2, 1)
))
q_lim_test_LIST[[153]] <- matrix(byrow = T, ncol = 7, c(
  c(200, 200, 200, 200, 200, 200, 200),
  c(200, 200, 200, 200, 200, 200, 200)
))

# 154
# Aumento tempo di processo: buffer insaturo a insaturo; cdb a valle; spostamento cdb
delay_ind_test_LIST[[154]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 1, 1, 5, 7, 2, 1), 
  c(1, 1, 1, 9, 7, 2, 1)
))
q_lim_test_LIST[[154]] <- matrix(byrow = T, ncol = 7, c(
  c(200, 200, 200, 200, 200, 200, 200),
  c(200, 200, 200, 200, 200, 200, 200)
))

# 155
# Aumento tempo di processo: buffer saturo a saturo; cdb a valle
delay_ind_test_LIST[[155]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 1, 1, 7, 9, 2, 1), 
  c(1, 1, 1, 8, 9, 2, 1)
))
q_lim_test_LIST[[155]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 2, 10, 10, 10),
  c(10, 10, 10, 2, 10, 10, 10)
))

# 156
# Aumento tempo di processo: buffer saturo a saturo; cdb in corrispondenza
delay_ind_test_LIST[[156]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 1, 1, 7, 2, 2, 1), 
  c(1, 1, 1, 8, 2, 2, 1)
))
q_lim_test_LIST[[156]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 2, 10, 10, 10),
  c(10, 10, 10, 2, 10, 10, 10)
))

# 157
# Aumento tempo di processo: buffer saturo a saturo; cdb a monte (?)
delay_ind_test_LIST[[157]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 1, 9, 7, 2, 2, 1), 
  c(1, 1, 9, 8, 2, 2, 1)
))
q_lim_test_LIST[[157]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 2, 10, 10, 10),
  c(10, 10, 10, 2, 10, 10, 10)
))

# 158
# Aumento tempo di processo: buffer insaturo a saturo; cdb in corrispondenza
delay_ind_test_LIST[[158]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 1, 1, 6, 2, 2, 1), 
  c(1, 1, 1, 9, 2, 2, 1)
))
q_lim_test_LIST[[158]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 3, 10, 10, 10),
  c(10, 10, 10, 3, 10, 10, 10)
))

# 159
# Aumento tempo di processo: buffer insaturo a saturo; cdb a valle
delay_ind_test_LIST[[159]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 1, 1, 6, 8, 2, 1), 
  c(1, 1, 1, 9, 8, 2, 1)
))
q_lim_test_LIST[[159]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 3, 10, 10, 10),
  c(10, 10, 10, 3, 10, 10, 10)
))

##### Gruppo 18 #####
# 160
# Aumento buffer: saturo a saturo solo in corrispondenza; cdb in corrispondenza
delay_ind_test_LIST[[160]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 9, 2, 2, 1), 
  c(1, 2, 2, 9, 2, 2, 1)
))
q_lim_test_LIST[[160]] <- matrix(byrow = T, ncol = 7, c(
  c(100, 100, 100, 2, 100, 100, 100),
  c(100, 100, 100, 3, 100, 100, 100)
))

# 161
# Aumento buffer: saturo a insaturo solo in corrispondenza; cdb in corrispondenza
delay_ind_test_LIST[[161]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 9, 2, 2, 1), 
  c(1, 2, 2, 9, 2, 2, 1)
))
q_lim_test_LIST[[161]] <- matrix(byrow = T, ncol = 7, c(
  c(100, 100, 100, 2, 100, 100, 100),
  c(100, 100, 100, 100, 100, 100, 100)
))

# 162
# Aumento buffer: insaturo a insaturo solo in corrispondenza; cdb in corrispondenza
delay_ind_test_LIST[[162]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 9, 2, 2, 1), 
  c(1, 2, 2, 9, 2, 2, 1)
))
q_lim_test_LIST[[162]] <- matrix(byrow = T, ncol = 7, c(
  c(100, 100, 100, 100, 100, 100, 100),
  c(100, 100, 100, 200, 100, 100, 100)
))

# 163
# Riduzione buffer: saturo a saturo solo in corrispondenza; cdb in corrispondenza
delay_ind_test_LIST[[163]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 9, 2, 2, 1), 
  c(1, 2, 2, 9, 2, 2, 1)
))
q_lim_test_LIST[[163]] <- matrix(byrow = T, ncol = 7, c(
  c(100, 100, 100, 4, 100, 100, 100),
  c(100, 100, 100, 2, 100, 100, 100)
))

# 164
# Riduzione buffer: insaturo a saturo solo in corrispondenza; cdb in corrispondenza
delay_ind_test_LIST[[164]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 9, 2, 2, 1), 
  c(1, 2, 2, 9, 2, 2, 1)
))
q_lim_test_LIST[[164]] <- matrix(byrow = T, ncol = 7, c(
  c(100, 100, 100, 100, 100, 100, 100),
  c(100, 100, 100, 2, 100, 100, 100)
))

# 165
# Riduzione buffer: insaturo a insaturo solo in corrispondenza; cdb in corrispondenza
delay_ind_test_LIST[[165]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 9, 2, 2, 1), 
  c(1, 2, 2, 9, 2, 2, 1)
))
q_lim_test_LIST[[165]] <- matrix(byrow = T, ncol = 7, c(
  c(100, 100, 100, 100, 100, 100, 100),
  c(100, 100, 100, 50, 100, 100, 100)
))

##### Gruppo 19 #####
# 166
# Riduzione tempo di processo: buffer saturo a saturo; cdb in corrispondenza
delay_ind_test_LIST[[166]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 1, 1, 9, 2, 2, 1), 
  c(1, 1, 1, 7, 2, 2, 1)
))
q_lim_test_LIST[[166]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 3, 10, 10, 10),
  c(10, 10, 10, 3, 10, 10, 10)
))

# 167
# Riduzione tempo di processo: buffer saturo a insaturo; cdb in corrispondenza
delay_ind_test_LIST[[167]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 1, 1, 9, 2, 2, 1), 
  c(1, 1, 1, 3, 2, 2, 1)
))
q_lim_test_LIST[[167]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 3, 10, 10, 10),
  c(10, 10, 10, 3, 10, 10, 10)
))

# 168
# Riduzione tempo di processo: buffer saturo a insaturo; cdb in corrispondenza; spostamento cdb
delay_ind_test_LIST[[168]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 9, 2, 2, 1), 
  c(1, 2, 2, 1, 2, 2, 1)
))
q_lim_test_LIST[[168]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 3, 10, 10, 10),
  c(10, 10, 10, 3, 10, 10, 10)
))

##### Gruppo 20 #####
# 169
# Aumento tempo di processo: buffer insaturo a saturo; cdb in corrispondenza; saturazione sistema
delay_ind_test_LIST[[169]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 2, 2, 1), 
  c(1, 2, 2, 12, 2, 2, 1)
))
q_lim_test_LIST[[169]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 3, 10, 10, 10),
  c(10, 10, 10, 3, 10, 10, 10)
))

# 170
# Aumento tempo di processo: buffer insaturo a saturo; cdb in corrispondenza; saturazione sistema
delay_ind_test_LIST[[170]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 2, 2, 1), 
  c(1, 2, 2, 12, 2, 2, 1)
))
q_lim_test_LIST[[170]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 171
# Aumento tempo di processo: buffer insaturo a saturo; cdb in corrispondenza; saturazione sistema
delay_ind_test_LIST[[171]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 2, 2, 1), 
  c(1, 2, 2, 12, 2, 2, 1)
))
q_lim_test_LIST[[171]] <- matrix(byrow = T, ncol = 7, c(
  c(100, 100, 100, 100, 100, 100, 100),
  c(100, 100, 100, 100, 100, 100, 100)
))

# 172
# Riduzione tempo di processo: il cdb si sposta a valle; buffer limitati saturi
delay_ind_test_LIST[[172]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 1, 1, 9, 4, 4, 1), 
  c(1, 1, 1, 2, 4, 4, 1)
))
q_lim_test_LIST[[172]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 173
# Riduzione tempo di processo: il cdb non si sposta; buffer limitati saturi
delay_ind_test_LIST[[173]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 1, 1, 9, 4, 4, 1), 
  c(1, 1, 1, 5, 4, 4, 1)
))
q_lim_test_LIST[[173]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 174
# Riduzione tempo di processo: il cdb si sposta a monte; buffer limitati saturi
delay_ind_test_LIST[[174]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 4, 4, 9, 1, 1, 1), 
  c(1, 4, 4, 2, 1, 1, 1)
))
q_lim_test_LIST[[174]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 175
# Riduzione tempo di processo: il cdb si sposta a valle; buffer limitati insaturi
delay_ind_test_LIST[[175]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 1, 1, 9, 4, 4, 1), 
  c(1, 1, 1, 2, 4, 4, 1)
))
q_lim_test_LIST[[175]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 176
# Riduzione tempo di processo: il cdb non si sposta; buffer limitati insaturi
delay_ind_test_LIST[[176]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 1, 1, 9, 4, 4, 1), 
  c(1, 1, 1, 5, 4, 4, 1)
))
q_lim_test_LIST[[176]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 177
# Riduzione tempo di processo: il cdb si sposta a monte; buffer limitati insaturi
delay_ind_test_LIST[[177]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 4, 4, 9, 1, 1, 1), 
  c(1, 4, 4, 2, 1, 1, 1)
))
q_lim_test_LIST[[177]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 178
# Riduzione tempo di processo: il cdb si sposta a valle; buffer illimitati 
delay_ind_test_LIST[[178]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 1, 1, 9, 4, 4, 1), 
  c(1, 1, 1, 2, 4, 4, 1)
))
q_lim_test_LIST[[178]] <- matrix(byrow = T, ncol = 7, c(
  c(100, 100, 100, 100, 100, 100, 100),
  c(100, 100, 100, 100, 100, 100, 100)
))

# 179
# Riduzione tempo di processo: il cdb non si sposta; buffer illimitati
delay_ind_test_LIST[[179]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 1, 1, 9, 4, 4, 1), 
  c(1, 1, 1, 5, 4, 4, 1)
))
q_lim_test_LIST[[179]] <- matrix(byrow = T, ncol = 7, c(
  c(100, 100, 100, 100, 100, 100, 100),
  c(100, 100, 100, 100, 100, 100, 100)
))

# 180
# Riduzione tempo di processo: il cdb si sposta a monte; buffer illimitati
delay_ind_test_LIST[[180]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 4, 4, 9, 1, 1, 1), 
  c(1, 4, 4, 2, 1, 1, 1)
))
q_lim_test_LIST[[180]] <- matrix(byrow = T, ncol = 7, c(
  c(100, 100, 100, 100, 100, 100, 100),
  c(100, 100, 100, 100, 100, 100, 100)
))

# 181
# Aumento tempo di processo: buffer illimitato; cdb in corrispondenza
delay_ind_test_LIST[[181]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 2, 2, 1), 
  c(1, 2, 2, 12, 2, 2, 1)
))
q_lim_test_LIST[[181]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 182
# Aumento tempo di processo: buffer illimitato; cdb in corrispondenza
delay_ind_test_LIST[[182]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 2, 2, 2, 2, 2, 1), 
  c(1, 2, 2, 9, 2, 2, 1)
))
q_lim_test_LIST[[182]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 183
# Riduzione tempo di processo: buffer illimitato; cdb in corrispondenza
delay_ind_test_LIST[[183]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 5, 5, 5, 5, 5, 1), 
  c(1, 5, 5, 2, 5, 5, 1)
))
q_lim_test_LIST[[183]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 184
# Riduzione tempo di processo: buffer illimitato; cdb in corrispondenza
delay_ind_test_LIST[[184]] <- matrix(byrow = T, ncol = 7, c(
  c(1, 5, 5, 9, 5, 5, 1), 
  c(1, 5, 5, 2, 5, 5, 1)
))
q_lim_test_LIST[[184]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

##### Gruppo 21 #####
# 185
# Aumento tempo di processo: buffer illimitato; cdb in corrispondenza
delay_ind_test_LIST[[185]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 10, 5, 5, 5)
))
q_lim_test_LIST[[185]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 186
# Aumento tempo di processo: buffer illimitato; cdb in corrispondenza
delay_ind_test_LIST[[186]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 11, 5, 5, 5)
))
q_lim_test_LIST[[186]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 187
# Aumento tempo di processo: buffer illimitato; cdb in corrispondenza
delay_ind_test_LIST[[187]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 12, 5, 5, 5)
))
q_lim_test_LIST[[187]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 188
# Aumento tempo di processo: buffer illimitato; cdb in corrispondenza
delay_ind_test_LIST[[188]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 9.5, 5, 5, 5)
))
q_lim_test_LIST[[188]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 189
# Aumento tempo di processo: buffer illimitato; cdb a valle
delay_ind_test_LIST[[189]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 7, 9, 5, 5)
))
q_lim_test_LIST[[189]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 190
# Aumento tempo di processo: buffer illimitato; cdb a valle
delay_ind_test_LIST[[190]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 9, 9, 5, 5)
))
q_lim_test_LIST[[190]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 191
# Aumento tempo di processo: buffer illimitato; cdb a valle
delay_ind_test_LIST[[191]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 11, 9, 5, 5)
))
q_lim_test_LIST[[191]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 192
# Aumento tempo di processo: buffer illimitato; cdb a monte
delay_ind_test_LIST[[192]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 7, 5, 5, 5)
))
q_lim_test_LIST[[192]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 193
# Aumento tempo di processo: buffer illimitato; cdb a monte
delay_ind_test_LIST[[193]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 9, 5, 5, 5)
))
q_lim_test_LIST[[193]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 194
# Aumento tempo di processo: buffer illimitato; cdb a monte
delay_ind_test_LIST[[194]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 11, 5, 5, 5)
))
q_lim_test_LIST[[194]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 195
# Riduzione tempo di processo: buffer illimitato; cdb in corrispondenza
delay_ind_test_LIST[[195]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 7, 5, 5, 5)
))
q_lim_test_LIST[[195]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 196
# Riduzione tempo di processo: buffer illimitato; cdb in corrispondenza
delay_ind_test_LIST[[196]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 5, 5, 5, 5)
))
q_lim_test_LIST[[196]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 197
# Riduzione tempo di processo: buffer illimitato; cdb in corrispondenza
delay_ind_test_LIST[[197]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 3, 5, 5, 5)
))
q_lim_test_LIST[[197]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 198
# Riduzione tempo di processo: buffer illimitato; cdb a valle
delay_ind_test_LIST[[198]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 3, 9, 5, 5)
))
q_lim_test_LIST[[198]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 199
# Riduzione tempo di processo: buffer illimitato; cdb a valle
delay_ind_test_LIST[[199]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 1, 9, 5, 5)
))
q_lim_test_LIST[[199]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 200
# Riduzione tempo di processo: buffer illimitato; cdb a monte
delay_ind_test_LIST[[200]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 3, 5, 5, 5)
))
q_lim_test_LIST[[200]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 201
# Riduzione tempo di processo: buffer illimitato; cdb a monte
delay_ind_test_LIST[[201]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 1, 5, 5, 5)
))
q_lim_test_LIST[[201]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

##### Gruppo 22 #####
# 202
# Aumento tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[202]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 10, 5, 5, 5)
))
q_lim_test_LIST[[202]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 203
# Aumento tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[203]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 11, 5, 5, 5)
))
q_lim_test_LIST[[203]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 204
# Aumento tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[204]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 12, 5, 5, 5)
))
q_lim_test_LIST[[204]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 205
# Aumento tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[205]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 7, 9, 5, 5)
))
q_lim_test_LIST[[205]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 206
# Aumento tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[206]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 7, 9, 5, 5)
))
q_lim_test_LIST[[206]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 207
# Aumento tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[207]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 9, 9, 5, 5)
))
q_lim_test_LIST[[207]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 208
# Aumento tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[208]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 11, 9, 5, 5)
))
q_lim_test_LIST[[208]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 209
# Aumento tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[209]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 7, 5, 5, 5)
))
q_lim_test_LIST[[209]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 210
# Aumento tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[210]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 9, 5, 5, 5)
))
q_lim_test_LIST[[210]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 211
# Aumento tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[211]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 11, 5, 5, 5)
))
q_lim_test_LIST[[211]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 212
# Riduzione tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[212]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 7, 5, 5, 5)
))
q_lim_test_LIST[[212]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 213
# Riduzione tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[213]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 5, 5, 5, 5)
))
q_lim_test_LIST[[213]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 214
# Riduzione tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[214]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 3, 5, 5, 5)
))
q_lim_test_LIST[[214]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 215
# Riduzione tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[215]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 3, 9, 5, 5)
))
q_lim_test_LIST[[215]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 216
# Riduzione tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[216]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 1, 9, 5, 5)
))
q_lim_test_LIST[[216]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 217
# Riduzione tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[217]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 3, 5, 5, 5)
))
q_lim_test_LIST[[217]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 218
# Riduzione tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[218]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 1, 5, 5, 5)
))
q_lim_test_LIST[[218]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

##### Gruppo 23 #####
# 219
# Aumento tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[219]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 10, 5, 5, 5)
))
q_lim_test_LIST[[219]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 220
# Aumento tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[220]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 11, 5, 5, 5)
))
q_lim_test_LIST[[220]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 221
# Aumento tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[221]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 12, 5, 5, 5)
))
q_lim_test_LIST[[221]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 222
# Aumento tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[222]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 7, 9, 5, 5)
))
q_lim_test_LIST[[222]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 223
# Aumento tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[223]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 7, 9, 5, 5)
))
q_lim_test_LIST[[223]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 224
# Aumento tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[224]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 9, 9, 5, 5)
))
q_lim_test_LIST[[224]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 225
# Aumento tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[225]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 11, 9, 5, 5)
))
q_lim_test_LIST[[225]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 226
# Aumento tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[226]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 7, 5, 5, 5)
))
q_lim_test_LIST[[226]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 227
# Aumento tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[227]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 9, 5, 5, 5)
))
q_lim_test_LIST[[227]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 228
# Aumento tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[228]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 11, 5, 5, 5)
))
q_lim_test_LIST[[228]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 229
# Riduzione tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[229]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 7, 5, 5, 5)
))
q_lim_test_LIST[[229]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 230
# Riduzione tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[230]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 5, 5, 5, 5)
))
q_lim_test_LIST[[230]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 231
# Riduzione tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[231]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 3, 5, 5, 5)
))
q_lim_test_LIST[[231]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 232
# Riduzione tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[232]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 3, 9, 5, 5)
))
q_lim_test_LIST[[232]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 233
# Riduzione tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[233]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 1, 9, 5, 5)
))
q_lim_test_LIST[[233]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 234
# Riduzione tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[234]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 3, 5, 5, 5)
))
q_lim_test_LIST[[234]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 235
# Riduzione tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[235]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 1, 5, 5, 5)
))
q_lim_test_LIST[[235]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

##### Gruppo 24 #####
# 236
# Aumento tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[236]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 10, 5, 5, 5)
))
q_lim_test_LIST[[236]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 237
# Aumento tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[237]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 11, 5, 5, 5)
))
q_lim_test_LIST[[237]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 238
# Aumento tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[238]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 12, 5, 5, 5)
))
q_lim_test_LIST[[238]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 239
# Aumento tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[239]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 7, 9, 5, 5)
))
q_lim_test_LIST[[239]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 240
# Aumento tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[240]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 7, 9, 5, 5)
))
q_lim_test_LIST[[240]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 241
# Aumento tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[241]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 9, 9, 5, 5)
))
q_lim_test_LIST[[241]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 242
# Aumento tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[242]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 11, 9, 5, 5)
))
q_lim_test_LIST[[242]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 243
# Aumento tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[243]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 7, 5, 5, 5)
))
q_lim_test_LIST[[243]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 244
# Aumento tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[244]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 9, 5, 5, 5)
))
q_lim_test_LIST[[244]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 245
# Aumento tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[245]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 11, 5, 5, 5)
))
q_lim_test_LIST[[245]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 246
# Riduzione tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[246]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 7, 5, 5, 5)
))
q_lim_test_LIST[[246]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 247
# Riduzione tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[247]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 5, 5, 5, 5)
))
q_lim_test_LIST[[247]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 248
# Riduzione tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[248]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 3, 5, 5, 5)
))
q_lim_test_LIST[[248]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 249
# Riduzione tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[249]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 3, 9, 5, 5)
))
q_lim_test_LIST[[249]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 250
# Riduzione tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[250]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 1, 9, 5, 5)
))
q_lim_test_LIST[[250]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 251
# Riduzione tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[251]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 3, 5, 5, 5)
))
q_lim_test_LIST[[251]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 252
# Riduzione tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[252]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 1, 5, 5, 5)
))
q_lim_test_LIST[[252]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

##### Gruppo 25 #####
# 253
# Riduzione limite buffer: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[253]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 9, 5, 5, 5)
))
q_lim_test_LIST[[253]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 2, 3, 3, 3)
))

# 254
# Riduzione limite buffer: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[254]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 5, 9, 5, 5)
))
q_lim_test_LIST[[254]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 2, 3, 3, 3)
))

# 255
# Riduzione limite buffer: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[255]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 5, 5, 5, 5)
))
q_lim_test_LIST[[255]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 2, 3, 3, 3)
))

# 256
# Aumento limite buffer: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[256]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 9, 5, 5, 5)
))
q_lim_test_LIST[[256]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 4, 3, 3, 3)
))

# 257
# Aumento limite buffer: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[257]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 5, 9, 5, 5)
))
q_lim_test_LIST[[257]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 4, 3, 3, 3)
))

# 258
# Aumento limite buffer: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[258]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 5, 5, 5, 5)
))
q_lim_test_LIST[[258]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 4, 3, 3, 3)
))

# 259
# Aumento limite buffer: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[259]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 9, 5, 5, 5)
))
q_lim_test_LIST[[259]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 6, 3, 3, 3)
))

# 260
# Aumento limite buffer: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[260]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 5, 9, 5, 5)
))
q_lim_test_LIST[[260]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 6, 3, 3, 3)
))

# 261
# Aumento limite buffer: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[261]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 5, 5, 5, 5)
))
q_lim_test_LIST[[261]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 6, 3, 3, 3)
))

# 262
# Aumento limite buffer: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[262]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 9, 5, 5, 5), 
  c(5, 5, 5, 9, 5, 5, 5)
))
q_lim_test_LIST[[262]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 10, 3, 3, 3)
))

# 263
# Aumento limite buffer: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[263]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 5, 5, 9, 5, 5), 
  c(5, 5, 5, 5, 9, 5, 5)
))
q_lim_test_LIST[[263]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 10, 3, 3, 3)
))

# 264
# Aumento limite buffer: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[264]] <- matrix(byrow = T, ncol = 7, c(
  c(5, 5, 9, 5, 5, 5, 5), 
  c(5, 5, 9, 5, 5, 5, 5)
))
q_lim_test_LIST[[264]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 10, 3, 3, 3)
))

##### Gruppo 26 #####
# 265
# Aumento tempo di processo: buffer illimitato; cdb in corrispondenza
delay_ind_test_LIST[[265]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 9.5, 6, 6, 6)
))
q_lim_test_LIST[[265]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 266
# Aumento tempo di processo: buffer illimitato; cdb in corrispondenza
delay_ind_test_LIST[[266]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 11, 6, 6, 6)
))
q_lim_test_LIST[[266]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 267
# Aumento tempo di processo: buffer illimitato; cdb in corrispondenza
delay_ind_test_LIST[[267]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 12.5, 6, 6, 6)
))
q_lim_test_LIST[[267]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 268
# Aumento tempo di processo: buffer illimitato; cdb in corrispondenza
delay_ind_test_LIST[[268]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 14, 6, 6, 6)
))
q_lim_test_LIST[[268]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 269
# Aumento tempo di processo: buffer illimitato; cdb a valle
delay_ind_test_LIST[[269]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 8.5, 6, 6), 
  c(6, 6, 7, 6, 8.5, 6, 6)
))
q_lim_test_LIST[[269]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 270
# Aumento tempo di processo: buffer illimitato; cdb a valle
delay_ind_test_LIST[[270]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 8.5, 6, 6), 
  c(6, 6, 9.5, 6, 8.5, 6, 6)
))
q_lim_test_LIST[[270]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 271
# Aumento tempo di processo: buffer illimitato; cdb a valle
delay_ind_test_LIST[[271]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 8.5, 6, 6), 
  c(6, 6, 11, 6, 8.5, 6, 6)
))
q_lim_test_LIST[[271]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 272
# Aumento tempo di processo: buffer illimitato; cdb a monte
delay_ind_test_LIST[[272]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 8.5, 6, 6, 6, 6), 
  c(6, 6, 8.5, 6, 7, 6, 6)
))
q_lim_test_LIST[[272]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 273
# Aumento tempo di processo: buffer illimitato; cdb a monte
delay_ind_test_LIST[[273]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 8.5, 6, 6, 6, 6), 
  c(6, 6, 8.5, 6, 9.5, 6, 6)
))
q_lim_test_LIST[[273]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 274
# Aumento tempo di processo: buffer illimitato; cdb a monte
delay_ind_test_LIST[[274]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 8.5, 6, 6, 6, 6), 
  c(6, 6, 8.5, 6, 11, 6, 6)
))
q_lim_test_LIST[[274]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 275
# Riduzione tempo di processo: buffer illimitato; cdb in corrispondenza
delay_ind_test_LIST[[275]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6),  
  c(6, 6, 6, 7, 6, 6, 6)
))
q_lim_test_LIST[[275]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 276
# Riduzione tempo di processo: buffer illimitato; cdb in corrispondenza
delay_ind_test_LIST[[276]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6),  
  c(6, 6, 6, 5, 6, 6, 6)
))
q_lim_test_LIST[[276]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 277
# Riduzione tempo di processo: buffer illimitato; cdb in corrispondenza
delay_ind_test_LIST[[277]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6),  
  c(6, 6, 6, 4, 6, 6, 6)
))
q_lim_test_LIST[[277]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 278
# Riduzione tempo di processo: buffer illimitato; cdb a valle
delay_ind_test_LIST[[278]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 8.5, 6, 6),  
  c(6, 6, 5, 6, 8.5, 6, 6)
))
q_lim_test_LIST[[278]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 279
# Riduzione tempo di processo: buffer illimitato; cdb a valle
delay_ind_test_LIST[[279]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 8.5, 6, 6),  
  c(6, 6, 4, 6, 8.5, 6, 6)
))
q_lim_test_LIST[[279]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 280
# Riduzione tempo di processo: buffer illimitato; cdb a monte
delay_ind_test_LIST[[280]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 8.5, 6, 6, 6, 6),  
  c(6, 6, 8.5, 6, 5, 6, 6)
))
q_lim_test_LIST[[280]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

# 281
# Riduzione tempo di processo: buffer illimitato; cdb a monte
delay_ind_test_LIST[[281]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 8.5, 6, 6, 6, 6),  
  c(6, 6, 8.5, 6, 4, 6, 6)
))
q_lim_test_LIST[[281]] <- matrix(byrow = T, ncol = 7, c(
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000),
  c(10000, 10000, 10000, 10000, 10000, 10000, 10000)
))

##### Gruppo 27 #####
# 282
# Aumento tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[282]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 9.5, 6, 6, 6)
))
q_lim_test_LIST[[282]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 283
# Aumento tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[283]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 11, 6, 6, 6)
))
q_lim_test_LIST[[283]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 284
# Aumento tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[284]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 12.5, 6, 6, 6)
))
q_lim_test_LIST[[284]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 285
# Aumento tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[285]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 8.5, 6, 6), 
  c(6, 6, 7, 6, 8.5, 6, 6)
))
q_lim_test_LIST[[285]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 286
# Aumento tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[286]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 8.5, 6, 6), 
  c(6, 6, 9.5, 6, 8.5, 6, 6)
))
q_lim_test_LIST[[286]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 287
# Aumento tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[287]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 8.5, 6, 6), 
  c(6, 6, 11, 6, 8.5, 6, 6)
))
q_lim_test_LIST[[287]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 288
# Aumento tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[288]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 8.5, 6, 6, 6, 6),  
  c(6, 6, 8.5, 6, 7, 6, 6)
))
q_lim_test_LIST[[288]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 289
# Aumento tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[289]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 8.5, 6, 6, 6, 6),  
  c(6, 6, 8.5, 6, 9.5, 6, 6)
))
q_lim_test_LIST[[289]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 290
# Aumento tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[290]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 8.5, 6, 6, 6, 6),  
  c(6, 6, 8.5, 6, 11, 6, 6)
))
q_lim_test_LIST[[290]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 291
# Riduzione tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[291]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 7, 6, 6, 6)
))
q_lim_test_LIST[[291]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 292
# Riduzione tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[292]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 5, 6, 6, 6)
))
q_lim_test_LIST[[292]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 293
# Riduzione tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[293]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 4, 6, 6, 6)
))
q_lim_test_LIST[[293]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 294
# Riduzione tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[294]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 8.5, 6, 6), 
  c(6, 6, 5, 6, 8.5, 6, 6)
))
q_lim_test_LIST[[294]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 295
# Riduzione tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[295]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 8.5, 6, 6), 
  c(6, 6, 4, 6, 8.5, 6, 6)
))
q_lim_test_LIST[[295]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 296
# Riduzione tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[296]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 8.5, 6, 6, 6, 6), 
  c(6, 6, 8.5, 6, 5, 6, 6)
))
q_lim_test_LIST[[296]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

# 297
# Riduzione tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[297]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 8.5, 6, 6, 6, 6), 
  c(6, 6, 8.5, 6, 4, 6, 6)
))
q_lim_test_LIST[[297]] <- matrix(byrow = T, ncol = 7, c(
  c(3, 3, 3, 3, 3, 3, 3),
  c(3, 3, 3, 3, 3, 3, 3)
))

##### Gruppo 28 #####
# 298
# Aumento tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[298]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 9.5, 6, 6, 6)
))
q_lim_test_LIST[[298]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 299
# Aumento tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[299]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 11, 6, 6, 6)
))
q_lim_test_LIST[[299]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 300
# Aumento tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[300]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 12.5, 6, 6, 6)
))
q_lim_test_LIST[[300]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 301
# Aumento tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[301]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 8.5, 6, 6), 
  c(6, 6, 7, 6, 8.5, 6, 6)
))
q_lim_test_LIST[[301]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 302
# Aumento tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[302]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 8.5, 6, 6), 
  c(6, 6, 9.5, 6, 8.5, 6, 6)
))
q_lim_test_LIST[[302]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 303
# Aumento tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[303]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 8.5, 6, 6), 
  c(6, 6, 11, 6, 8.5, 6, 6)
))
q_lim_test_LIST[[303]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 304
# Aumento tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[304]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 8.5, 6, 6, 6, 6),  
  c(6, 6, 8.5, 6, 7, 6, 6)
))
q_lim_test_LIST[[304]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 305
# Aumento tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[305]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 8.5, 6, 6, 6, 6),  
  c(6, 6, 8.5, 6, 9.5, 6, 6)
))
q_lim_test_LIST[[305]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 306
# Aumento tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[306]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 8.5, 6, 6, 6, 6),  
  c(6, 6, 8.5, 6, 11, 6, 6)
))
q_lim_test_LIST[[306]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 307
# Riduzione tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[307]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 7, 6, 6, 6)
))
q_lim_test_LIST[[307]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 308
# Riduzione tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[308]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 5, 6, 6, 6)
))
q_lim_test_LIST[[308]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 309
# Riduzione tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[309]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 4, 6, 6, 6)
))
q_lim_test_LIST[[309]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 310
# Riduzione tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[310]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 8.5, 6, 6), 
  c(6, 6, 5, 6, 8.5, 6, 6)
))
q_lim_test_LIST[[310]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 311
# Riduzione tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[311]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 8.5, 6, 6), 
  c(6, 6, 4, 6, 8.5, 6, 6)
))
q_lim_test_LIST[[311]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 312
# Riduzione tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[312]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 8.5, 6, 6, 6, 6), 
  c(6, 6, 8.5, 6, 5, 6, 6)
))
q_lim_test_LIST[[312]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

# 313
# Riduzione tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[313]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 8.5, 6, 6, 6, 6), 
  c(6, 6, 8.5, 6, 4, 6, 6)
))
q_lim_test_LIST[[313]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 6, 6, 6),
  c(6, 6, 6, 6, 6, 6, 6)
))

##### Gruppo 29 #####
# 314
# Aumento tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[314]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 9.5, 6, 6, 6)
))
q_lim_test_LIST[[314]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 315
# Aumento tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[315]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 11, 6, 6, 6)
))
q_lim_test_LIST[[315]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 316
# Aumento tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[316]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 12.5, 6, 6, 6)
))
q_lim_test_LIST[[316]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 317
# Aumento tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[317]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 8.5, 6, 6), 
  c(6, 6, 7, 6, 8.5, 6, 6)
))
q_lim_test_LIST[[317]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 318
# Aumento tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[318]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 8.5, 6, 6), 
  c(6, 6, 9.5, 6, 8.5, 6, 6)
))
q_lim_test_LIST[[318]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 319
# Aumento tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[319]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 8.5, 6, 6), 
  c(6, 6, 11, 6, 8.5, 6, 6)
))
q_lim_test_LIST[[319]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 320
# Aumento tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[320]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 8.5, 6, 6, 6, 6),  
  c(6, 6, 8.5, 6, 7, 6, 6)
))
q_lim_test_LIST[[320]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 321
# Aumento tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[321]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 8.5, 6, 6, 6, 6),  
  c(6, 6, 8.5, 6, 9.5, 6, 6)
))
q_lim_test_LIST[[321]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 322
# Aumento tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[322]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 8.5, 6, 6, 6, 6),  
  c(6, 6, 8.5, 6, 11, 6, 6)
))
q_lim_test_LIST[[322]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 323
# Riduzione tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[323]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 7, 6, 6, 6)
))
q_lim_test_LIST[[323]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 324
# Riduzione tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[324]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 5, 6, 6, 6)
))
q_lim_test_LIST[[324]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 325
# Riduzione tempo di processo: buffer limitato; cdb in corrispondenza
delay_ind_test_LIST[[325]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 8.5, 6, 6, 6), 
  c(6, 6, 6, 4, 6, 6, 6)
))
q_lim_test_LIST[[325]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 326
# Riduzione tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[326]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 8.5, 6, 6), 
  c(6, 6, 5, 6, 8.5, 6, 6)
))
q_lim_test_LIST[[326]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 327
# Riduzione tempo di processo: buffer limitato; cdb a valle
delay_ind_test_LIST[[327]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 6, 6, 8.5, 6, 6), 
  c(6, 6, 4, 6, 8.5, 6, 6)
))
q_lim_test_LIST[[327]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 328
# Riduzione tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[328]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 8.5, 6, 6, 6, 6), 
  c(6, 6, 8.5, 6, 5, 6, 6)
))
q_lim_test_LIST[[328]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

# 329
# Riduzione tempo di processo: buffer limitato; cdb a monte
delay_ind_test_LIST[[329]] <- matrix(byrow = T, ncol = 7, c(
  c(6, 6, 8.5, 6, 6, 6, 6), 
  c(6, 6, 8.5, 6, 4, 6, 6)
))
q_lim_test_LIST[[329]] <- matrix(byrow = T, ncol = 7, c(
  c(10, 10, 10, 10, 10, 10, 10),
  c(10, 10, 10, 10, 10, 10, 10)
))

##### Gruppo 30 #####

##### Parametri finestre #####
window_param_LIST <- list()
window_param_LIST[[1]] <- c(100, 100)

##### Punto di inizio e fine stampa param_files #####
print_start <- 314
# print_end <- length(delay_ind_test_LIST)
print_end <- 329

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
