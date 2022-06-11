##### Clean-up #####
rm(list = ls())

##### Scripts path #####
script_path <- file.path("C:","Users","edo_c","OneDrive","Simulazione","Test_Variazioni","Analisi_Simulazioni","Script_2")

file_list <- list()

file_list[[1]] <- file.path(script_path,"CleanUp_Libraries_Setup_Paths_FoldersCreation.R")
file_list[[2]] <- file.path(script_path,"ComputationFunctions.R")
file_list[[3]] <- file.path(script_path,"PlottingFunctions.R")
# file_list[[4]] <- file.path(script_path,"ComputationCalls.R")
# file_list[[5]] <- file.path(script_path,"PlottingFunctions.R")

##### Scripts run #####
lapply(file_list, source)

