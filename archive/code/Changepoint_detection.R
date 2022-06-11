rm(list = ls())

library(tidyverse)
library(ggplot2)
library(magrittr)
library(changepoint)
library(bcp)

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

##### Parametri #####
model_name <- "Model74_n10000_inter10_chv5000_rep30_w300f50"
field <- quo(Waiting_time)
actvty <- 5

##### calcoli #####
data_in_path <- file.path(general_path,"Data_out",
                          model_name,
                          "data_out_standard.csv")
plot_out_path <- file.path(general_path,"Plot_single")

data_in <- read.csv(data_in_path)

data_change_search <- data_in %>% filter(Activity == actvty) %>% select(!!field)

cpt.mean(as.numeric(data_change_search))

a <- bcp(as.vector(data_change_search), return.mcmc = T)
plot(a)

