rm(list = ls())

library(tidyverse)
library(ggplot2)
library(magrittr)
# library(ggpubr)

##### WD #####
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
# setwd(dir = dir_path)

##### MOdello #####
model_name <- "Model184_n10000_inter15_chv5000_rep30_w100f100"

##### Percorso e file dati #####
data_in_path <- file.path(general_path,"Data_out"
                          ,model_name
                          ,"data_out_standard.csv"
                          # ,"data_windows.csv"
)

plot_out_path <- file.path(general_path,"Plot_single")

data_in <- read.csv(data_in_path) 

##### Calcolo autocorrelazione #####
a <- acf(data_in %>% filter(Activity == 3) %>% select(Waiting_time), lag.max = 1)
