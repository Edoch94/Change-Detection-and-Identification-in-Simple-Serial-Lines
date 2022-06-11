cat("\nSTAMPA PLOT\n")

##### Funzione salvataggio immagine con ggplot (singola attività) #####
IMG_SAVE_GG <- function(plot_name, plot_to_save, DPI=100){
  ggsave(filename = plot_name, 
         plot = plot_to_save, 
         device = "png",
         scale = 1, width = 30, height = 15, units = "cm", 
         dpi = DPI, limitsize = TRUE)
}

##### Funzione costruzione plot con ggplot (singola attività) #####
# PLOT_BUILD_GG <- function(A, data_plot_in, field, save_path){
#   fld <- enquo(field)
#   fld_char <- quo_name(fld)
#   
#   data_plot_in %<>% filter(Activity == A)
#   name_plot <- paste0(fld_char,"_A",A)
#   this_plot <- ggplot(data_plot, aes(x = CaseID, y = !!fld)) +
#     geom_point() +
#     ggtitle(name_plot) +
#     theme(plot.title = element_text(hjust = 0.5),
#           axis.title.y = element_blank())
#   IMG_SAVE_GG(file.path(save_path,paste0(name_plot,".png")), this_plot)
# }


cat("Single Points\n")
##### Single Points - Same case intervals plots #####
plot_singlePoints_output_path_SCI <-
  file.path(plot_specific_output_path, "SinglePoints", "SCI")
if(!dir.exists(plot_singlePoints_output_path_SCI)){
  dir.create(plot_singlePoints_output_path_SCI, recursive = T)
}

# Waiting_time
cat("\tWaiting_time\n")
for(A in unique(data_out_standard$Activity)){
  data_plot <- data_out_standard %>% filter(Activity == A)
  name_plot <- paste0("Waiting_time_A",A)
  this_plot <- ggplot(data_plot, aes(x = CaseID, y = Waiting_time)) + 
    geom_point() + 
    ggtitle(name_plot) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank())
  IMG_SAVE_GG(file.path(plot_singlePoints_output_path_SCI,paste0(name_plot,".png")), this_plot)
}

# Processing_time
cat("\tProcessing_time\n")
for(A in unique(data_out_standard$Activity)){
  data_plot <- data_out_standard %>% filter(Activity == A)
  name_plot <- paste0("Processing_time_A",A)
  this_plot <- ggplot(data_plot, aes(x = CaseID, y = Processing_time)) + 
    geom_point() + 
    ggtitle(name_plot) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank())
  IMG_SAVE_GG(file.path(plot_singlePoints_output_path_SCI,paste0(name_plot,".png")), this_plot)
}

# Blocking_time
cat("\tBlocking_time\n")
for(A in unique(data_out_standard$Activity)){
  data_plot <- data_out_standard %>% filter(Activity == A)
  name_plot <- paste0("Blocking_time_A",A)
  this_plot <- ggplot(data_plot, aes(x = CaseID, y = Blocking_time)) + 
    geom_point() + 
    ggtitle(name_plot) +
    theme(plot.title = element_text(hjust = 0.5,),
          axis.title.y = element_blank())
  IMG_SAVE_GG(file.path(plot_singlePoints_output_path_SCI,paste0(name_plot,".png")), this_plot)
}

##### Single Points - Consecutive case intervals plots #####
plot_singlePoints_output_path_CCI <-
  file.path(plot_specific_output_path, "SinglePoints", "CCI")
if(!dir.exists(plot_singlePoints_output_path_CCI)){
  dir.create(plot_singlePoints_output_path_CCI, recursive = T)
}

# Input_diff
cat("\tInput_diff\n")
for(A in unique(data_out_standard$Activity)){
  data_plot <- data_out_standard %>% filter(Activity == A)
  name_plot <- paste0("Input_diff_A",A)
  this_plot <- ggplot(data_plot, aes(x = CaseID, y = Input_diff)) + 
    geom_point() + 
    ggtitle(name_plot) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank())
  IMG_SAVE_GG(file.path(plot_singlePoints_output_path_CCI,paste0(name_plot,".png")), this_plot)
}

# Mid_diff
cat("\tMid_diff\n")
for(A in unique(data_out_standard$Activity)){
  data_plot <- data_out_standard %>% filter(Activity == A)
  name_plot <- paste0("Mid_diff_A",A)
  this_plot <- ggplot(data_plot, aes(x = CaseID, y = Mid_diff)) + 
    geom_point() + 
    ggtitle(name_plot) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank())
  IMG_SAVE_GG(file.path(plot_singlePoints_output_path_CCI,paste0(name_plot,".png")), this_plot)
}

# Output_diff
cat("\tOutput_diff\n")
for(A in unique(data_out_standard$Activity)){
  data_plot <- data_out_standard %>% filter(Activity == A)
  name_plot <- paste0("Output_diff_A",A)
  this_plot <- ggplot(data_plot, aes(x = CaseID, y = Output_diff)) + 
    geom_point() + 
    ggtitle(name_plot) +
    theme(plot.title = element_text(hjust = 0.5,),
          axis.title.y = element_blank())
  IMG_SAVE_GG(file.path(plot_singlePoints_output_path_CCI,paste0(name_plot,".png")), this_plot)
}

cat("Windows\n")
##### Windows - Same case intervals plots #####
plot_windows_output_path_SCI <-
  file.path(plot_specific_output_path, "Windows", "SCI")
if(!dir.exists(plot_windows_output_path_SCI)){
  dir.create(plot_windows_output_path_SCI, recursive = T)
}
plot_windows_output_path_SCI_mean <-
  file.path(plot_specific_output_path, "Windows", "SCI", "mean")
if(!dir.exists(plot_windows_output_path_SCI_mean)){
  dir.create(plot_windows_output_path_SCI_mean)
}
plot_windows_output_path_SCI_var <-
  file.path(plot_specific_output_path, "Windows", "SCI", "var")
if(!dir.exists(plot_windows_output_path_SCI_var)){
  dir.create(plot_windows_output_path_SCI_var)
}

# Waiting_time MEAN
cat("\tWaiting_time mean\n")
for(A in unique(data_windows$Activity)){
  data_plot <- data_windows %>% filter(Activity == A)
  name_plot <- paste0("Waiting_time_MEAN_A",A)
  this_plot <- ggplot(data_plot, aes(x = CaseID, y = Waiting_time_MEAN)) + 
    geom_point() + 
    ggtitle(name_plot) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank())
  IMG_SAVE_GG(file.path(plot_windows_output_path_SCI_mean,paste0(name_plot,".png")), this_plot)
}

# Waiting_time VAR
cat("\tWaiting_time var\n")
for(A in unique(data_windows$Activity)){
  data_plot <- data_windows %>% filter(Activity == A)
  name_plot <- paste0("Waiting_time_VAR_A",A)
  this_plot <- ggplot(data_plot, aes(x = CaseID, y = Waiting_time_VAR)) + 
    geom_point() + 
    ggtitle(name_plot) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank())
  IMG_SAVE_GG(file.path(plot_windows_output_path_SCI_var,paste0(name_plot,".png")), this_plot)
}

# Processing_time MEAN
cat("\tProcessing_time mean\n")
for(A in unique(data_windows$Activity)){
  data_plot <- data_windows %>% filter(Activity == A)
  name_plot <- paste0("Processing_time_MEAN_A",A)
  this_plot <- ggplot(data_plot, aes(x = CaseID, y = Processing_time_MEAN)) + 
    geom_point() + 
    ggtitle(name_plot) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank())
  IMG_SAVE_GG(file.path(plot_windows_output_path_SCI_mean,paste0(name_plot,".png")), this_plot)
}

# Processing_time VAR
cat("\tProcessing_time var\n")
for(A in unique(data_windows$Activity)){
  data_plot <- data_windows %>% filter(Activity == A)
  name_plot <- paste0("Processing_time_VAR_A",A)
  this_plot <- ggplot(data_plot, aes(x = CaseID, y = Processing_time_VAR)) + 
    geom_point() + 
    ggtitle(name_plot) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank())
  IMG_SAVE_GG(file.path(plot_windows_output_path_SCI_var,paste0(name_plot,".png")), this_plot)
}

# Blocking_time MEAN
cat("\tBlocking_time mean\n")
for(A in unique(data_windows$Activity)){
  data_plot <- data_windows %>% filter(Activity == A)
  name_plot <- paste0("Blocking_time_MEAN_A",A)
  this_plot <- ggplot(data_plot, aes(x = CaseID, y = Blocking_time_MEAN)) + 
    geom_point() + 
    ggtitle(name_plot) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank())
  IMG_SAVE_GG(file.path(plot_windows_output_path_SCI_mean,paste0(name_plot,".png")), this_plot)
}

# Blocking_time VAR
cat("\tBlocking_time var\n")
for(A in unique(data_windows$Activity)){
  data_plot <- data_windows %>% filter(Activity == A)
  name_plot <- paste0("Blocking_time_VAR_A",A)
  this_plot <- ggplot(data_plot, aes(x = CaseID, y = Blocking_time_VAR)) + 
    geom_point() + 
    ggtitle(name_plot) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank())
  IMG_SAVE_GG(file.path(plot_windows_output_path_SCI_var,paste0(name_plot,".png")), this_plot)
}

##### Windows - Consecutive case intervals plots #####
# plot_windows_output_path_CCI <-
#   file.path(plot_specific_output_path, "Windows", "CCI")
# if(!dir.exists(plot_windows_output_path_CCI)){
#   dir.create(plot_windows_output_path_CCI)
# }
# 
# # Input_diff
# cat("\tInput_diff\n")
# for(A in unique(data_out_standard$Activity)){
#   data_plot <- data_out_standard %>% filter(Activity == A)
#   name_plot <- paste0("Input_diff_A",A)
#   this_plot <- ggplot(data_plot, aes(x = CaseID, y = Input_diff)) + 
#     geom_point() + 
#     ggtitle(name_plot) +
#     theme(plot.title = element_text(hjust = 0.5),
#           axis.title.y = element_blank())
#   IMG_SAVE_GG(file.path(plot_windows_output_path_CCI,paste0(name_plot,".png")), this_plot)
# }
# 
# # Mid_diff
# cat("\tMid_diff\n")
# for(A in unique(data_out_standard$Activity)){
#   data_plot <- data_out_standard %>% filter(Activity == A)
#   name_plot <- paste0("Mid_diff_A",A)
#   this_plot <- ggplot(data_plot, aes(x = CaseID, y = Mid_diff)) + 
#     geom_point() + 
#     ggtitle(name_plot) +
#     theme(plot.title = element_text(hjust = 0.5),
#           axis.title.y = element_blank())
#   IMG_SAVE_GG(file.path(plot_windows_output_path_CCI,paste0(name_plot,".png")), this_plot)
# }
# 
# # Output_diff
# cat("\tOutput_diff\n")
# for(A in unique(data_out_standard$Activity)){
#   data_plot <- data_out_standard %>% filter(Activity == A)
#   name_plot <- paste0("Output_diff_A",A)
#   this_plot <- ggplot(data_plot, aes(x = CaseID, y = Output_diff)) + 
#     geom_point() + 
#     ggtitle(name_plot) +
#     theme(plot.title = element_text(hjust = 0.5,),
#           axis.title.y = element_blank())
#   IMG_SAVE_GG(file.path(plot_windows_output_path_CCI,paste0(name_plot,".png")), this_plot)
# }







