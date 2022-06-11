##### Funzioni distribuzioni delay #####
# Distribuzione triangolare
# pt_distr <- list()
# pt_distr[[1]] <- function(){return(rtriangle(a = 10, b = 30))}
# pt_distr[[2]] <- function(){return(rtriangle(a = 20, b = 40))}
# pt_distr[[3]] <- function(){return(rtriangle(a = 30, b = 50))}
# pt_distr[[4]] <- function(){return(rtriangle(a = 40, b = 60))}
# pt_distr[[5]] <- function(){return(rtriangle(a = 50, b = 70))}
# pt_distr[[6]] <- function(){return(rtriangle(a = 60, b = 80))}

# Distribuzione lognormale
pt_distr <- list()
pt_distr[[1]] <- function(){return(rlnorm(n = 1, meanlog = process_param$`1`[1], sdlog = process_param$`1`[2]))}
pt_distr[[2]] <- function(){return(rlnorm(n = 1, meanlog = process_param$`2`[1], sdlog = process_param$`2`[2]))}
pt_distr[[3]] <- function(){return(rlnorm(n = 1, meanlog = process_param$`3`[1], sdlog = process_param$`3`[2]))}
pt_distr[[4]] <- function(){return(rlnorm(n = 1, meanlog = process_param$`4`[1], sdlog = process_param$`4`[2]))}
pt_distr[[5]] <- function(){return(rlnorm(n = 1, meanlog = process_param$`5`[1], sdlog = process_param$`5`[2]))}
pt_distr[[6]] <- function(){return(rlnorm(n = 1, meanlog = process_param$`6`[1], sdlog = process_param$`6`[2]))}
pt_distr[[7]] <- function(){return(rlnorm(n = 1, meanlog = process_param$`7`[1], sdlog = process_param$`7`[2]))}
pt_distr[[8]] <- function(){return(rlnorm(n = 1, meanlog = process_param$`8`[1], sdlog = process_param$`8`[2]))}
pt_distr[[9]] <- function(){return(rlnorm(n = 1, meanlog = process_param$`9`[1], sdlog = process_param$`9`[2]))}
pt_distr[[10]] <- function(){return(rlnorm(n = 1, meanlog = process_param$`10`[1], sdlog = process_param$`10`[2]))}


# Distribuzione degli interarrivi
arrival_distr <- function() {c(rexp(arrival_param[[1]], 1/arrival_param[[2]]),-1)}

##### Stage builder #####
cat("\nCostruzione funzioni\n")

builder_first_act <- function(env_name = env_single, delay_ind = 1){
  act <- trajectory("A1") %>%
    set_attribute("Act", 1) %>%
    set_attribute("Pos", 1) %>%
    seize("R_A1") %>%
    set_attribute("Act", 1) %>%
    set_attribute("Pos", 2) %>%
    timeout(pt_distr[[delay_ind]]) %>% 
    
    set_global("count_in", 1, mod = c("+")) %>% # changing in base a quanti pezzi sono stati lavorati dalla prima macchina
    
    set_attribute("Act", 1) %>%
    set_attribute("Pos", 3) %>%
    branch(option = function()(get_queue_count(env_name, "R_A2") >= q_lim[[2]]),
           continue = T,
           trajectory() %>%
             wait_until("Signal_A2")) %>%
    release("R_A1")
  return(act)
}

builder_last_act <- function(env_name = env_single, delay_ind = 1){
  act <- trajectory("A7") %>%
    set_attribute("Act", 7) %>%
    set_attribute("Pos", 1) %>%
    seize("R_A7") %>%
    set_attribute("Act", 7) %>%
    set_attribute("Pos", 2) %>%
    timeout(pt_distr[[delay_ind]]) %>% 
    set_attribute("Act", 7) %>%
    set_attribute("Pos", 3) %>%
    release("R_A7") %>%
    send("Signal_A7")
  return(act)
}

builder_middle_act <- function(env_name = env_single, num = 1, delay_ind = 1){
  trj_name <- paste0("A", as.character(num))
  res_name <- paste0("R_A", as.character(num))
  res_name_next <- paste0("R_A", as.character(num+1))
  signal_in <- paste0("Signal_A", as.character(num+1))
  signal_out <- paste0("Signal_A", as.character(num))
  
  act <- trajectory(trj_name) %>%
    set_attribute("Act", num) %>%
    set_attribute("Pos", 1) %>%
    seize(res_name) %>%
    
    set_attribute("Act", num) %>%
    set_attribute("Pos", 2) %>%
    timeout(pt_distr[[delay_ind]]) %>% 
    
    set_attribute("Act", num) %>%
    set_attribute("Pos", 3) %>%
    branch(option = function()(get_queue_count(env_name, res_name_next) >= q_lim[[num + 1]]),
           continue = T,
           trajectory() %>%
             wait_until(signal_in)) %>%
    release(res_name) %>%
    send(signal_out)
  
  return(act)
}

builder_changing_PT_act <- function(env_name = env_single, num = 1, delay_ind = 1, changing_point = 5000, new_delay_ind = 1){
  trj_name <- paste0("A", as.character(num))
  res_name <- paste0("R_A", as.character(num))
  res_name_next <- paste0("R_A", as.character(num+1))
  signal_in <- paste0("Signal_A", as.character(num+1))
  signal_out <- paste0("Signal_A", as.character(num))
  
  act <- trajectory(trj_name) %>%
    set_attribute("Act", num) %>%
    set_attribute("Pos", 1) %>%
    seize(res_name) %>%
    
    set_attribute("Act", num) %>%
    set_attribute("Pos", 2) %>%
    branch(
      option = function(){
        if(get_global(env_name, "count_in") < changing_point){ return(1) }
        else{ return(2) } 
      },
      continue = T,
      trajectory() %>% timeout(pt_distr[[delay_ind]]),
      trajectory() %>% timeout(pt_distr[[new_delay_ind]])
    ) %>%
    
    set_attribute("Act", num) %>%
    set_attribute("Pos", 3) %>%
    branch(option = function()(get_queue_count(env_name, res_name_next) >= q_lim[[num + 1]]),
           continue = T,
           trajectory() %>%
             wait_until(signal_in)) %>%
    release(res_name) %>%
    send(signal_out)
  
  return(act)
}

builder_changing_BL_act <- function(env_name = env_single, num = 1, delay_ind = 1, changing_point = 5000, new_delay_ind = 1){
  trj_name <- paste0("A", as.character(num))
  res_name <- paste0("R_A", as.character(num))
  res_name_next <- paste0("R_A", as.character(num+1))
  signal_in <- paste0("Signal_A", as.character(num+1))
  signal_out <- paste0("Signal_A", as.character(num))
  
  act <- trajectory(trj_name) %>%
    set_attribute("Act", num) %>%
    set_attribute("Pos", 1) %>%
    seize(res_name) %>%
    
    set_attribute("Act", num) %>%
    set_attribute("Pos", 2) %>%
    timeout(pt_distr[[delay_ind]]) %>% 
    
    set_attribute("Act", num) %>%
    set_attribute("Pos", 3) %>%
    branch(
      option = function(){
        if(get_global(env_name, "count_in") < changing_point) {
          return(get_queue_count(env_name, res_name_next) >= q_lim[[num + 1]])
        }
        else {
          return(get_queue_count(env_name, res_name_next) >= q_lim_new[[num + 1]])
        }
      },
      continue = T,
      trajectory() %>%
        wait_until(signal_in)
    ) %>%
    release(res_name) %>%
    send(signal_out)
  
  return(act)
}

builder_general_act <- function(env_name = env_single, 
                                num = 1, 
                                delay_ind = 1, 
                                new_delay_ind = 1,
                                changing_point = 5000){
  
  trj_name <- paste0("A", as.character(num))
  res_name <- paste0("R_A", as.character(num))
  res_name_next <- paste0("R_A", as.character(num+1))
  signal_in <- paste0("Signal_A", as.character(num+1))
  signal_out <- paste0("Signal_A", as.character(num))
  
  act <- trajectory(trj_name) %>%
    
    # Ingresso nel buffer
    set_attribute("Act", num) %>%
    set_attribute("Pos", 1) %>%
    seize(res_name) %>%
    
    # Ingresso nella risorsa
    set_attribute("Act", num) %>%
    set_attribute("Pos", 2) %>%
    branch(
      option = function(){
        if(get_global(env_name, "count_in") < changing_point){
          return(1)
        }
        else{
          return(2)
        } 
      },
      continue = T,
      trajectory() %>% timeout(pt_distr[[delay_ind]]),
      trajectory() %>% timeout(pt_distr[[new_delay_ind]])
    ) %>%
    
    # Fine lavorazione
    set_attribute("Act", num) %>%
    set_attribute("Pos", 3) %>%
    branch(
      option = function(){
        if(get_global(env_name, "count_in") < changing_point) {
          return(get_queue_count(env_name, res_name_next) >= q_lim[[num + 1]])
        }
        else {
          return(get_queue_count(env_name, res_name_next) >= q_lim_new[[num + 1]])
        }
      },
      continue = T,
      trajectory() %>%
        wait_until(signal_in)
    ) %>%
    release(res_name) %>%
    send(signal_out)
  
  return(act)
}

##### Environment builder #####
cat("\nCreo ambiente di simulazione\n")
builder_env <- function(){
  traj_complete <- 
    join(builder_first_act(delay_ind = delay_ind_list[[1]]),
         
         builder_general_act(num = 2, 
                             delay_ind = delay_ind_list[[2]], 
                             new_delay_ind = delay_ind_new_list[[2]],
                             changing_point = change_value),
         
         builder_general_act(num = 3, 
                             delay_ind = delay_ind_list[[3]], 
                             new_delay_ind = delay_ind_new_list[[3]],
                             changing_point = change_value),
         
         builder_general_act(num = 4, 
                             delay_ind = delay_ind_list[[4]], 
                             new_delay_ind = delay_ind_new_list[[4]],
                             changing_point = change_value),
         
         builder_general_act(num = 5, 
                             delay_ind = delay_ind_list[[5]], 
                             new_delay_ind = delay_ind_new_list[[5]],
                             changing_point = change_value),
         
         builder_general_act(num = 6, 
                             delay_ind = delay_ind_list[[6]], 
                             new_delay_ind = delay_ind_new_list[[6]],
                             changing_point = change_value),
         
         builder_last_act(delay_ind = delay_ind_list[[7]])
    )
  
  sim <-
    simmer("sim") %>%
    add_resource("R_A1", capacity = 1, preempt_order = "fifo") %>%
    add_resource("R_A2", capacity = 1, preempt_order = "fifo") %>%
    add_resource("R_A3", capacity = 1, preempt_order = "fifo") %>%
    add_resource("R_A4", capacity = 1, preempt_order = "fifo") %>%
    add_resource("R_A5", capacity = 1, preempt_order = "fifo") %>%
    add_resource("R_A6", capacity = 1, preempt_order = "fifo") %>%
    add_resource("R_A7", capacity = 1, preempt_order = "fifo") %>%
    add_generator(name_prefix = "",
                  trajectory = traj_complete,
                  mon = 2,
                  # distribution = function() {c(rexp(15000),-1)}
                  distribution = arrival_distr
    ) %>%
    add_global("count_in", 0)
  return(sim)
}

env_single <- builder_env()

##### Run #####
cat("\nAvvio simulazioni\n")

# Run1 <- env_single %>%
#   reset() %>%
#   run()
# 
# env_single

env_replication <- list()
for(i in 1:tot_replications){
  if(i == 1) {cat("\nRun delle simulazioni\n")}
  env_single <- builder_env()
  env_replication[[i]] <- env_single %>% reset() %>% run()
  cat(paste("\nReplicazione",i,"\n"))
  print(env_replication[[i]])
}

##### Raccolta dati della simulazione #####
data_sim_in_list <- list()
for(i in 1:tot_replications){
  if(i == 1) {cat(paste("\nRaccolta dati ambiente di simulazione\n", "Replicazioni completate: \n", sep = ""))}
  
  # b <- a %>% as_tibble() %>% filter(key != "count_in")
  
  data_sim_in_list[[i]] <- get_mon_attributes(env_replication[[i]]) %>%
    filter(key != "count_in") %>%
    select(-replication) %>%
    mutate(name = as.numeric(name)) %>%
    mutate(name = name + 1) %>%
    # filter(name <= 15000) %>%
    mutate(eventID = row_number()) %>%
    group_by(name, key) %>%
    arrange(eventID, value) %>%
    mutate(rank_col = row_number()) %>%
    ungroup() %>%
    arrange(name,rank_col,key) %>%
    ungroup() %>%
    select(-eventID) %>%
    group_by(name, rank_col) %>%
    pivot_wider(names_from = key, values_from = value) %>%
    ungroup() %>%
    select(-rank_col) %>%
    rename(CaseID = name,
           Time = time,
           Activity = Act,
           Position = Pos) %>%
    arrange(Time, Activity) 
  
  cat(paste("\t", i))
  if(i == tot_replications) {cat("\n")}
}

cat("\nCostruzione unico dataframe\n")
data_sim_in <- bind_rows(data_sim_in_list) %>%
  group_by(CaseID, Activity, Position) %>%
  summarise(Time = mean(Time)) 

##### Ristrutturazione dataframe della simulazione #####
cat("\nRistrutturazione dataframe\n")

data_out <- data_sim_in %>%
  # mutate(Resource = rep(1)) %>% # Eliminato: inutile in linea con stage single resource
  group_by(Activity, CaseID) %>%
  # mutate(Position = order(Time)) %>% # Eliminato: Position viene definito dal simulatore
  # mutate(Resource = replace(Resource, Resource == 0, max(Resource))) %>%
  group_by(Activity) %>%
  mutate(EventID = NULL) %>%
  pivot_wider(names_from = Position, values_from = Time, names_prefix = "Time") %>% 
  mutate(Waiting = Time2 - Time1,
         Processing = Time3 - Time2) %>%
  mutate(Diff1 = Time1 - lag(Time1, n = 1)) %>% 
  mutate(Diff1 = coalesce(Diff1, 0)) %>%
  group_by(Activity) %>% 
  arrange(Activity, Time2) %>%
  mutate(Diff2 = Time2 - lag(Time2, n = 1)) %>%
  mutate(Diff2 = coalesce(Diff2, 0)) %>%
  arrange(Activity, Time3) %>%
  mutate(Diff3 = Time3 - lag(Time3, n = 1)) %>%
  mutate(Diff3 = coalesce(Diff3, 0)) %>%
  group_by(Activity) %>%
  mutate(Diff2_shift = lead(Diff2)) %>%
  filter(!is.na(Diff2_shift)) %>%
  group_by(CaseID) %>%
  arrange(CaseID,Time1) %>%
  mutate(TimeNext = lead(Time1, n = 1, default = max(Time3))) %>%
  mutate(Blocking = TimeNext - Time3) 

# Dataframe con nomi standardizzati
cat("\nStandardizzazione nomi campi dataframe\n")

data_out_standard <- data_out %>%
  ungroup() %>%
  select(CaseID, Activity, Time1, Time2, Time3, Waiting, Processing, Blocking, Diff1, Diff2, Diff3) %>%
  rename(Timestamp_buff = Time1,
         Timestamp_res = Time2,
         Timestamp_end = Time3,
         Waiting_time = Waiting,
         Processing_time= Processing,
         Blocking_time = Blocking,
         Input_diff = Diff1,
         Mid_diff = Diff2,
         Output_diff = Diff3)

# Plot #
# cosidered_data_list <- data_out %>%
#   filter(Activity == 3)
# data_plot <- cosidered_data_list
# plot_1 <- ggplot(data_plot,
#                  aes(x = CaseID))+
#   geom_point(aes(y = Waiting), size = 0.5)+
#   expand_limits(x = 0, y = 0)+
#   theme(legend.position = "none",
#         axis.title = element_blank())+
#   labs(x = "CaseID",
#        y = "Blocking_time")
# plot_1
# 
# a <- data_out %>% filter(CaseID > 1000, Activity == 3)
# b <- a$Waiting
# mean(b)


##### Calcolo dataframe delle finestre #####
cat("\nCostruzione dataframe finestre\n")

ampiezza_finestra <- 300
frequenza_finestra <- 50

data_windows <- data_out_standard %>%
  group_by(Activity) %>%
  arrange(Timestamp_buff) %>%
  mutate(Waiting_time_MEAN = slide_dbl(Waiting_time, 
                                       mean, 
                                       .before = ampiezza_finestra - 1, 
                                       .step = frequenza_finestra,
                                       .complete = T),
         Waiting_time_VAR = slide_dbl(Waiting_time, 
                                      var, 
                                      .before = ampiezza_finestra - 1, 
                                      .step = frequenza_finestra,
                                      .complete = T)
  ) %>%
  mutate(Processing_time_MEAN = slide_dbl(Processing_time, 
                                          mean, 
                                          .before = ampiezza_finestra - 1, 
                                          .step = frequenza_finestra,
                                          .complete = T),
         Processing_time_VAR = slide_dbl(Processing_time, 
                                         var, 
                                         .before = ampiezza_finestra - 1, 
                                         .step = frequenza_finestra,
                                         .complete = T)
  ) %>%
  mutate(Blocking_time_MEAN = slide_dbl(Blocking_time, 
                                        mean, 
                                        .before = ampiezza_finestra - 1, 
                                        .step = frequenza_finestra,
                                        .complete = T),
         Blocking_time_VAR = slide_dbl(Blocking_time, 
                                       var,
                                       .before = ampiezza_finestra - 1, 
                                       .step = frequenza_finestra,
                                       .complete = T)
  ) %>%
  mutate(Input_diff_MEAN = slide_dbl(Input_diff, 
                                     mean, 
                                     .before = ampiezza_finestra - 1, 
                                     .step = frequenza_finestra,
                                     .complete = T),
         Input_diff_VAR = slide_dbl(Input_diff, 
                                    var,
                                    .before = ampiezza_finestra - 1, 
                                    .step = frequenza_finestra,
                                    .complete = T)
  ) %>%
  mutate(Mid_diff_MEAN = slide_dbl(Mid_diff, 
                                   mean, 
                                   .before = ampiezza_finestra - 1, 
                                   .step = frequenza_finestra,
                                   .complete = T),
         Mid_diff_VAR = slide_dbl(Mid_diff, 
                                  var,
                                  .before = ampiezza_finestra - 1, 
                                  .step = frequenza_finestra,
                                  .complete = T)
  ) %>%
  mutate(Output_diff_MEAN = slide_dbl(Output_diff, 
                                      mean, 
                                      .before = ampiezza_finestra - 1, 
                                      .step = frequenza_finestra,
                                      .complete = T),
         Output_diff_VAR = slide_dbl(Output_diff, 
                                     var,
                                     .before = ampiezza_finestra - 1, 
                                     .step = frequenza_finestra,
                                     .complete = T)
  ) %>%
  # mutate(a = lead(Mid_diff)) %>%
  mutate(lagged_Mid_diff_MEAN = slide_dbl(lead(Mid_diff),
                                          mean,
                                          .before = ampiezza_finestra - 1,
                                          .step = frequenza_finestra,
                                          .complete = T)
  ) %>%
  ungroup() %>%
  filter(!is.na(Waiting_time_MEAN)) %>%
  mutate(Utilization = Processing_time_MEAN/lagged_Mid_diff_MEAN) %>%
  arrange(Activity, Timestamp_buff) 

# Plot utilizzo #
cat("\nPlot utilizzo\n")

for(i in 1:n_distinct(data_windows$Activity)){
  data_plot <- data_windows %>%
    filter(Activity == i)
  
  plot_1 <- ggplot(data_plot,
                   aes(x = CaseID))+
    geom_point(aes(y = Utilization), size = 1)+
    expand_limits(x = 1, y = c(0,1))+
    theme(legend.position = "none",
          plot.title = element_text(lineheight=1, face="bold", hjust = 0.5))+
    labs(x = "CaseID", y = "Utilization")+
    ggtitle(paste("Utilization_",i))
  
  ggsave(filename = paste("Utilization_", i, ".png", sep = ""), 
         path = mixed_output_path,
         plot = plot_1, 
         device = "png",
         scale = 1, width = 30, height = 15, units = "cm", 
         dpi = 100, limitsize = TRUE)
}

# data_plot <- data_windows %>%
#   filter(Activity == 3)
# 
# plot_1 <- ggplot(data_plot,
#                  aes(x = CaseID))+
#   geom_point(aes(y = Waiting_time_MEAN), size = 0.5)+
#   expand_limits(x = 0, y = 0)+
#   theme(legend.position = "none",
#         axis.title = element_blank())+
#   labs(x = "CaseID")
# plot_1

##### Calcolo utilizzo massimo e medio di ogni attività #####
cat("\nCalcolo utilizzi massimi e medi\n")

data_utilization <- data_windows %>%
  group_by(Activity) %>%
  mutate(Mean_Utilization = mean(Utilization)) %>%
  filter(Utilization == max(Utilization)) %>%
  filter(Timestamp_res == max(Timestamp_res)) %>%
  rename(Max_Utilization = Utilization) %>%
  select(Activity, Max_Utilization, Mean_Utilization, Timestamp_res) %>%
  ungroup() %>%
  arrange(Activity)

##### plot con simmer.plot #####
# util_plot <- plot(x = get_mon_resources(env_replication[[2]]),
#                   metric = "usage", 
#                   c("R_A3"))
# 
# # utilization
# util_plot_data_list <- list()
# for(i in 1:tot_replications){
#   util_plot <- plot(x = get_mon_resources(env_replication[[i]]),
#                     metric = "utilization")
#   
#   util_plot_data_list[[i]] <- util_plot$data %>% select(resource, Q50)
# }
# util_plot_data <- bind_rows(util_plot_data_list) %>% 
#   group_by(resource) %>%
#   summarise(Q50 = mean(Q50))

# usage
# util_plot_data_list <- list()
# for(i in 1:tot_replications){
#   util_plot <- plot(x = get_mon_resources(env_replication[[i]]),
#                     metric = "usage")
#   
#   util_plot_data_list[[i]] <- util_plot$data %>% 
#     filter(item == "server") %>%
#     select(resource, mean)
# }
# util_plot_data <- bind_rows(util_plot_data_list) %>% 
#   group_by(resource) %>%
#   summarise(MEAN = mean(mean))

##### Monitoraggio code #####
cat("\nCalcolo code massime e medie\n")

data_queue_list <- list()
for(i in 1:tot_replications){
  data_queue_list[[i]] <- get_mon_resources(env_replication[[i]]) %>%
    select(resource, time, queue) %>%
    group_by(resource) %>%
    mutate(Mean_Queue = mean(queue)) %>%
    filter(queue == max(queue)) %>%
    filter(time == max(time)) %>%
    rename(Max_Queue = queue) %>%
    arrange(resource)
}
data_queue <- bind_rows(data_queue_list) %>%
  group_by(resource) %>%
  summarise(Max_Queue = mean(Max_Queue),
            Mean_Queue = mean(Mean_Queue), 
            time = mean(time))

# Dataframe del trend delle code #
data_queue_trend <- data_sim_in %>%
  mutate(Queue = case_when(
    Position == 1 ~ 1,
    Position == 2 ~ -1,
    Position == 3 ~ 0
  )) %>%
  group_by(Activity) %>%
  arrange(Time) %>%
  mutate(Queue_Cum = cumsum(Queue)) %>%
  arrange(Activity) %>%
  ungroup()

# Plot code #
cat("\nPlot code\n")

for(i in 1:n_distinct(data_queue_trend$Activity)){
  data_plot <- data_queue_trend %>%
    filter(Activity == i, Position == 1)
  
  plot_1 <- ggplot(data_plot,
                   aes(x = CaseID))+
    geom_line(aes(y = Queue_Cum), size = 1)+
    expand_limits(x = 1, y = c(0,1))+
    theme(legend.position = "none",
          plot.title = element_text(lineheight=1, face="bold", hjust = 0.5))+
    labs(x = "CaseID", y = "Queue_Cumul")+
    ggtitle(paste("Queue_Cumul",i))
  
  ggsave(filename = paste("Queue_Cumul_", i, ".png", sep = ""), 
         path = mixed_output_path,
         plot = plot_1, 
         device = "png",
         scale = 1, width = 30, height = 15, units = "cm", 
         dpi = 100, limitsize = TRUE)
}


##### Stampa risultati #####
cat("\nDati utilizzo\n")
print(data_utilization %>% select(-Timestamp_res))
cat("\nDati code\n")
print(data_queue %>% select(-time))

