cat("\nSTAMPA PLOT\n")

##### Funzione salvataggio immagine con ggplot (singola attività) #####
IMG_SAVE_GG <- function(plot_name, plot_to_save, DPI=100){
  ggsave(filename = plot_name, 
         plot = plot_to_save, 
         device = "png",
         scale = 1, width = 30, height = 15, units = "cm", 
         dpi = DPI, limitsize = TRUE)
}

##### Funzioni costruzione geom_point e geom_line plot con ggplot (singola attività) #####
PLOT_BUILD_GG <- function(A, data_plot_in, field, save_path){
  cat(paste0("\t",A))
  fld <- enquo(field)
  fld_char <- quo_name(fld)
  
  data_plot_in %<>% filter(Activity == A)
  name_plot <- paste0(fld_char,"_A",A)
  this_plot <- ggplot(data_plot_in, aes(x = CaseID, y = !!fld)) +
    geom_point() +
    ggtitle(name_plot) +
    expand_limits(y = 0) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank())
  IMG_SAVE_GG(file.path(save_path,paste0(name_plot,".png")), this_plot)
}

PLOT_BUILD_Q_GG <- function(A, data_plot_in, field, save_path){
  cat(paste0("\t",A))
  fld <- enquo(field)
  fld_char <- quo_name(fld)
  
  data_plot_in %<>% filter(Activity == A)
  name_plot <- paste0(fld_char,"_A",A)
  this_plot <- ggplot(data_plot_in, aes(x = Time, y = !!fld)) +
    geom_line() +
    ggtitle(name_plot) +
    expand_limits(y = 0) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank())
  IMG_SAVE_GG(file.path(save_path,paste0(name_plot,".png")), this_plot)
}

PLOT_BUILD_U_GG <- function(A, data_plot_in, field, save_path){
  cat(paste0("\t",A))
  fld <- enquo(field)
  fld_char <- quo_name(fld)
  
  data_plot_in %<>% filter(Activity == A)
  name_plot <- paste0(fld_char,"_A",A)
  this_plot <- ggplot(data_plot_in, aes(x = CaseID, y = !!fld)) +
    geom_point() +
    ggtitle(name_plot) +
    expand_limits(x = 0, y = c(0,1)) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank())
  IMG_SAVE_GG(file.path(save_path,paste0(name_plot,".png")), this_plot)
}

PLOT_BUILD_T_GG <- function(A, data_plot_in, field, save_path){
  cat(paste0("\t",A))
  fld <- enquo(field)
  fld_char <- quo_name(fld)
  
  data_plot_in %<>% filter(Activity == A)
  name_plot <- paste0(fld_char,"_A",A)
  this_plot <- ggplot(data_plot_in, aes(x = CaseID, y = !!fld)) +
    geom_point() +
    ggtitle(name_plot) +
    expand_limits(x = 0, y = 1) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_blank())
  IMG_SAVE_GG(file.path(save_path,paste0(name_plot,".png")), this_plot)
}

cat("Single Points\n")
##### Single Points - Same case intervals plots #####
plot_singlePoints_output_path_SCI <-
  file.path(plot_specific_output_path, "SinglePoints", "SCI")
if(!dir.exists(plot_singlePoints_output_path_SCI)){
  dir.create(plot_singlePoints_output_path_SCI, recursive = T)
}

# Waiting_time
cat("\tWaiting_time\t")
for(actvty in unique(data_out_standard$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_out_standard, 
                field = Waiting_time, 
                save_path = plot_singlePoints_output_path_SCI)
}
cat("\n")

# Processing_time
cat("\tProcessing_time\t")
for(actvty in unique(data_out_standard$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_out_standard, 
                field = Processing_time, 
                save_path = plot_singlePoints_output_path_SCI)
}
cat("\n")

# Blocking_time
cat("\tBlocking_time\t")
for(actvty in unique(data_out_standard$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_out_standard, 
                field = Blocking_time, 
                save_path = plot_singlePoints_output_path_SCI)
}
cat("\n")

# Starving_time
cat("\tStarving_time\t")
for(actvty in unique(data_out_standard$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_out_standard, 
                field = Starving_time, 
                save_path = plot_singlePoints_output_path_SCI)
}
cat("\n")

# Idle_time
cat("\tIdle_time\t")
for(actvty in unique(data_out_standard$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_out_standard, 
                field = Idle_time, 
                save_path = plot_singlePoints_output_path_SCI)
}
cat("\n")
cat("\n")

##### Single Points - Consecutive case intervals plots #####
plot_singlePoints_output_path_CCI <-
  file.path(plot_specific_output_path, "SinglePoints", "CCI")
if(!dir.exists(plot_singlePoints_output_path_CCI)){
  dir.create(plot_singlePoints_output_path_CCI, recursive = T)
}

# Input_diff
cat("\tInput_diff\t")
for(actvty in unique(data_out_standard$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_out_standard, 
                field = Input_diff, 
                save_path = plot_singlePoints_output_path_CCI)
}
cat("\n")

# Mid_diff
cat("\tMid_diff\t")
for(actvty in unique(data_out_standard$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_out_standard, 
                field = Mid_diff, 
                save_path = plot_singlePoints_output_path_CCI)
}
cat("\n")

# Output_diff
cat("\tOutput_diff\t")
for(actvty in unique(data_out_standard$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_out_standard, 
                field = Output_diff, 
                save_path = plot_singlePoints_output_path_CCI)
}
cat("\n")
cat("\n")

##### Single Points - Trend plots #####
plot_singlePoints_output_path_T <-
  file.path(plot_specific_output_path, "SinglePoints", "Trend")
if(!dir.exists(plot_singlePoints_output_path_T)){
  dir.create(plot_singlePoints_output_path_T, recursive = T)
}

# Waiting_trend
cat("\tWaiting_trend\t")
for(actvty in unique(data_out_standard$Activity)){
  PLOT_BUILD_T_GG(A = actvty, 
                  data_plot_in = data_out_standard, 
                  field = Waiting_trend, 
                  save_path = plot_singlePoints_output_path_T)
}
cat("\n")

# Processing_trend
cat("\tProcessing_trend")
for(actvty in unique(data_out_standard$Activity)){
  PLOT_BUILD_T_GG(A = actvty, 
                  data_plot_in = data_out_standard, 
                  field = Processing_trend, 
                  save_path = plot_singlePoints_output_path_T)
}
cat("\n")

# Blocking_trend
cat("\tBlocking_trend\t")
for(actvty in unique(data_out_standard$Activity)){
  PLOT_BUILD_T_GG(A = actvty, 
                  data_plot_in = data_out_standard, 
                  field = Blocking_trend, 
                  save_path = plot_singlePoints_output_path_T)
}
cat("\n")
cat("\n")

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
plot_windows_output_path_SCI_skew <-
  file.path(plot_specific_output_path, "Windows", "SCI", "skew")
if(!dir.exists(plot_windows_output_path_SCI_skew)){
  dir.create(plot_windows_output_path_SCI_skew)
}

# Waiting_time MEAN
cat("\tWaiting_time mean")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_windows, 
                field = Waiting_time_MEAN, 
                save_path = plot_windows_output_path_SCI_mean)
}
cat("\n")

# Waiting_time VAR
cat("\tWaiting_time var")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_windows, 
                field = Waiting_time_VAR, 
                save_path = plot_windows_output_path_SCI_var)
}
cat("\n")

# Waiting_time SKEW
cat("\tWaiting_time skew")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_windows, 
                field = Waiting_time_SKEW, 
                save_path = plot_windows_output_path_SCI_skew)
}
cat("\n")

# Processing_time MEAN
cat("\tProcessing_time mean")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_windows, 
                field = Processing_time_MEAN, 
                save_path = plot_windows_output_path_SCI_mean)
}
cat("\n")

# Processing_time VAR
cat("\tProcessing_time var")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_windows, 
                field = Processing_time_VAR, 
                save_path = plot_windows_output_path_SCI_var)
}
cat("\n")

# Blocking_time MEAN
cat("\tBlocking_time mean")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_windows, 
                field = Blocking_time_MEAN, 
                save_path = plot_windows_output_path_SCI_mean)
}
cat("\n")

# Blocking_time VAR
cat("\tBlocking_time var")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_windows, 
                field = Blocking_time_VAR, 
                save_path = plot_windows_output_path_SCI_var)
}
cat("\n")

# Starving_time MEAN
cat("\tStarving_time mean")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_windows, 
                field = Starving_time_MEAN, 
                save_path = plot_windows_output_path_SCI_mean)
}
cat("\n")

# Starving_time VAR
cat("\tStarving_time var")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_windows, 
                field = Starving_time_VAR, 
                save_path = plot_windows_output_path_SCI_var)
}
cat("\n")

# Idle_time MEAN
cat("\tIdle_time mean\t")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_windows, 
                field = Idle_time_MEAN, 
                save_path = plot_windows_output_path_SCI_mean)
}
cat("\n")

# Idle_time VAR
cat("\tIdle_time var\t")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_windows, 
                field = Idle_time_VAR, 
                save_path = plot_windows_output_path_SCI_var)
}
cat("\n")
cat("\n")

##### Windows - Consecutive case intervals plots #####
plot_windows_output_path_CCI <-
  file.path(plot_specific_output_path, "Windows", "CCI")
if(!dir.exists(plot_windows_output_path_CCI)){
  dir.create(plot_windows_output_path_CCI, recursive = T)
}
plot_windows_output_path_CCI_mean <-
  file.path(plot_specific_output_path, "Windows", "CCI", "mean")
if(!dir.exists(plot_windows_output_path_CCI_mean)){
  dir.create(plot_windows_output_path_CCI_mean)
}
plot_windows_output_path_CCI_var <-
  file.path(plot_specific_output_path, "Windows", "CCI", "var")
if(!dir.exists(plot_windows_output_path_CCI_var)){
  dir.create(plot_windows_output_path_CCI_var)
}

# Input_diff MEAN
cat("\tInput_diff mean\t")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_windows, 
                field = Input_diff_MEAN, 
                save_path = plot_windows_output_path_CCI_mean)
}
cat("\n")

# Input_diff VAR
cat("\tInput_diff var\t")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_windows, 
                field = Input_diff_VAR, 
                save_path = plot_windows_output_path_CCI_var)
}
cat("\n")

# Mid_diff MEAN
cat("\tMid_diff mean\t")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_windows, 
                field = Mid_diff_MEAN, 
                save_path = plot_windows_output_path_CCI_mean)
}
cat("\n")

# Mid_diff VAR
cat("\tMid_diff var\t")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_windows, 
                field = Mid_diff_VAR, 
                save_path = plot_windows_output_path_CCI_var)
}
cat("\n")

# Output_diff MEAN
cat("\tOutput_diff mean")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_windows, 
                field = Output_diff_MEAN, 
                save_path = plot_windows_output_path_CCI_mean)
}
cat("\n")

# Output_diff VAR
cat("\tOutput_diff var\t")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_windows, 
                field = Output_diff_VAR, 
                save_path = plot_windows_output_path_CCI_var)
}
cat("\n")
cat("\n")

##### Windows - Trend plots #####
plot_windows_output_path_T <-
  file.path(plot_specific_output_path, "Windows", "Trend")
if(!dir.exists(plot_windows_output_path_T)){
  dir.create(plot_windows_output_path_T, recursive = T)
}
plot_windows_output_path_T_mean <-
  file.path(plot_specific_output_path, "Windows", "Trend", "mean")
if(!dir.exists(plot_windows_output_path_T_mean)){
  dir.create(plot_windows_output_path_T_mean)
}
plot_windows_output_path_T_var <-
  file.path(plot_specific_output_path, "Windows", "Trend", "var")
if(!dir.exists(plot_windows_output_path_T_var)){
  dir.create(plot_windows_output_path_T_var)
}

# Waiting_trend MEAN
cat("\tWaiting_trend mean")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_T_GG(A = actvty, 
                  data_plot_in = data_windows, 
                  field = Waiting_trend_MEAN, 
                  save_path = plot_windows_output_path_T_mean)
}
cat("\n")

# Waiting_trend VAR
cat("\tWaiting_trend var")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_windows, 
                field = Waiting_trend_VAR, 
                save_path = plot_windows_output_path_T_var)
}
cat("\n")

# Processing_trend MEAN
cat("\tProcessing_trend mean")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_T_GG(A = actvty, 
                  data_plot_in = data_windows, 
                  field = Processing_trend_MEAN, 
                  save_path = plot_windows_output_path_T_mean)
}
cat("\n")

# Processing_trend VAR
cat("\tProcessing_trend var")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_windows, 
                field = Processing_trend_VAR, 
                save_path = plot_windows_output_path_T_var)
}
cat("\n")

# Blocking_trend MEAN
cat("\tBlocking_trend mean")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_T_GG(A = actvty, 
                  data_plot_in = data_windows, 
                  field = Blocking_trend_MEAN, 
                  save_path = plot_windows_output_path_T_mean)
}
cat("\n")

# Blocking_trend VAR
cat("\tBlocking_trend var")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_windows, 
                field = Blocking_trend_VAR, 
                save_path = plot_windows_output_path_T_var)
}
cat("\n")
cat("\n")

##### Utilization plots #####
plot_windows_output_path_UTIL <-
  file.path(plot_specific_output_path, "Windows", "Utilization")
if(!dir.exists(plot_windows_output_path_UTIL)){
  dir.create(plot_windows_output_path_UTIL, recursive = T)
}

# Utilization
cat("\tUtilization\t")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_U_GG(A = actvty, 
                  data_plot_in = data_windows, 
                  field = Utilization, 
                  save_path = plot_windows_output_path_UTIL)
}
cat("\n")

# Blocking probability
cat("\tBlocking_prob\t")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_U_GG(A = actvty, 
                  data_plot_in = data_windows, 
                  field = Blocking_prob, 
                  save_path = plot_windows_output_path_UTIL)
}
cat("\n")

# Starving probability
cat("\tStarving_prob\t")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_U_GG(A = actvty, 
                  data_plot_in = data_windows, 
                  field = Starving_prob, 
                  save_path = plot_windows_output_path_UTIL)
}
cat("\n")

# Idle probability
cat("\tIdle_prob\t")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_U_GG(A = actvty, 
                  data_plot_in = data_windows, 
                  field = Idle_prob, 
                  save_path = plot_windows_output_path_UTIL)
}
cat("\n")

##### Queue plots #####
plot_windows_output_path_QUEUE <-
  file.path(plot_specific_output_path, "Queue")
if(!dir.exists(plot_windows_output_path_QUEUE)){
  dir.create(plot_windows_output_path_QUEUE, recursive = T)
}

# Queue
cat("\nQueue\t\t\t")
for(actvty in unique(data_windows$Activity)){
  PLOT_BUILD_GG(A = actvty, 
                data_plot_in = data_windows, 
                field = Queue_MEAN, 
                save_path = plot_windows_output_path_QUEUE)
}
cat("\n")

cat("\nQueue_CUM\t\t")
for(actvty in unique(data_queue_trend$Activity)){
  PLOT_BUILD_Q_GG(A = actvty, 
                  data_plot_in = data_queue_trend, 
                  field = Queue_Cum, 
                  save_path = plot_windows_output_path_QUEUE)
}
cat("\n")


