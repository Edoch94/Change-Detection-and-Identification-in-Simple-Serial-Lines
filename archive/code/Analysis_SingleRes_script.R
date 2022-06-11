##### Raccolta dati della simulazione #####
data_sim_in_list <- list()
for(i in 1:tot_replications){
  if(i == 1) {cat(paste("\nRaccolta dati ambiente di simulazione\n", "Replicazioni completate: \n", sep = ""))}
  
  data_sim_in_list[[i]] <- get_mon_attributes(env_replication[[i]]) %>%
    filter(key != "count_in") %>%
    select(-replication) %>%
    mutate(name = as.numeric(name)) %>%
    mutate(name = name + 1) %>%
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
  group_by(Activity, CaseID) %>%
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

##### Calcolo dataframe delle finestre #####
cat("\nCostruzione dataframe finestre\n")

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
data_queue_trend <-
  bind_rows(
    data_sim_in %>%
      filter(Position == 1) %>%
      mutate(Queue = 1),
    data_sim_in %>%
      filter(Position == 2) %>%
      mutate(Queue = -1),
    data_sim_in %>%
      filter(Position == 3) %>%
      mutate(Queue = 0)
  ) %>%
  ungroup() %>%
  arrange(Activity) %>%
  group_by(Activity) %>%
  arrange(Time, CaseID) %>%
  mutate(Queue_Cum = cumsum(Queue)) %>%
  arrange(Activity) %>%
  ungroup() %>%
  filter(Position == 1) %>%
  select(-Queue, -Position)

##### Stampa risultati #####
# cat("\nDati utilizzo\n")
# print(data_utilization %>% select(-Timestamp_res))
# cat("\nDati code\n")
# print(data_queue %>% select(-time))