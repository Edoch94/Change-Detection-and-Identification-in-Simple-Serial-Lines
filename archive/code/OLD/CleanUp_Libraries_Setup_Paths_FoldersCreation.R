##### Clean-up #####
# rm(list = ls())

##### Working directory #####
if(Sys.info()["user"] == "Edoardo pc"){
  general_path <- file.path("C:","Users","Edoardo pc","OneDrive","Tesi","Project")
} else {
  general_path <- file.path("C:","Users","edo_c","OneDrive","Tesi","Project")
}

dir_path <- file.path(general_path)

setwd(dir = dir_path)

##### Libraries #####
library(data.table)
library(ggplot2)
library(magrittr)
library(IC2)
library(moments)
library(tidyverse)

# Experimental library
# remotes::install_github("DavisVaughan/slide")

##### Setup #####
# Log
n <- "log90"

# Indexes
# ampiezza_tmp <- 10000
# distanza_tmp <- 500

ampiezza_qta <- 300
distanza_qta <- 50

##### Dataframe fields types and order #####
independent_variables <- c("CaseID","time_buff","time_res","time_end","time_prev")
dependent_variables <- c("buffer","resource","elapsed_time","int1","int2","int3","int4")
ordered_fields_names <- c(independent_variables,dependent_variables)

##### Paths #####
# Input file path (log path)
path <- file.path(general_path,"LOG")
input_path <- file.path(path, paste(n,".txt",sep=""))
# File existence control
if(!file.exists(input_path)){
  stop("NoFile")
}

# Output file path (plots path)
output_path <- file.path(general_path,"PLOTS",n)

index_path <- file.path(output_path,"Indici",paste("w",ampiezza_qta,"f",distanza_qta,sep = ""))
index_path_lvls_list <- list()
stat_ind_list_old <- c("media","varianza","gini","atkinson","entropia","skewness","kurtosis")
stat_ind_list <- c("media","varianza","skewness","kurtosis")

index_path_lvls_list[[1]] <- stat_ind_list
index_path_lvls_list[[2]] <- c("absolute","relative")
index_path_lvls_list[[3]] <- dependent_variables


##### Folders creation #####
# Creazione cartelle
if(!dir.exists(output_path)){
  dir.create(output_path)
}
if(!dir.exists(file.path(output_path,"InterarrivalTime_on_timestamp"))){
  dir.create(file.path(output_path,"InterarrivalTime_on_timestamp"))
}
if(!dir.exists(file.path(output_path,"InternalTime_on_caseID"))){
  dir.create(file.path(output_path,"InternalTime_on_caseID"))
}
if(!dir.exists(file.path(output_path,"InternalTime_on_timestamp"))){
  dir.create(file.path(output_path,"InternalTime_on_timestamp"))
}
if(!dir.exists(file.path(output_path,"Utilization"))){
  dir.create(file.path(output_path,"Utilization"))
}
if(!dir.exists(file.path(output_path,"TransferTime"))){
  dir.create(file.path(output_path,"TransferTime"))
}
if(!dir.exists(file.path(output_path,"Utilization",paste("w",ampiezza_qta,"f",distanza_qta,sep = "")))){
  dir.create(file.path(output_path,"Utilization",paste("w",ampiezza_qta,"f",distanza_qta,sep = "")))
}

output_util_path <- file.path(output_path,"Utilization",paste("w",ampiezza_qta,"f",distanza_qta,sep = ""))

if(!dir.exists(output_path)){
  stop("Non esiste percorso file")
} else {
  for(i1 in 1:length(index_path_lvls_list[[1]])){
    for(i2 in 1:length(index_path_lvls_list[[2]])){
      for(i3 in 1:length(index_path_lvls_list[[3]])){
        if(!dir.exists(file.path(index_path,index_path_lvls_list[[1]][i1],index_path_lvls_list[[2]][i2],index_path_lvls_list[[3]][i3]))){
          dir.create(file.path(index_path,index_path_lvls_list[[1]][i1],index_path_lvls_list[[2]][i2],index_path_lvls_list[[3]][i3]),
                     recursive = T)
        }
      }
    }
  }
}
