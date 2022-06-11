##### Raccolta dati della simulazione #####
data_sim_in_list <- list()

cat(paste("\nRaccolta dati ambiente di simulazione\n", "Replicazioni completate: \n", sep = ""))
tictoc::tic("inizio")

for(i in 1:tot_replications){
  data_sim_in_list[[i]] <- get_mon_attributes(env_replication[[i]])
}

plan(multisession, gc = TRUE, workers = min(5, (parallel::detectCores())/2))
data_sim_in_list <-
  future_lapply(
    X = data_sim_in_list,
    FUN = function(x) {
      # library(magrittr)
      x %<>%
        dplyr::filter(key != "count_in") %>%
        dplyr::select(-replication) %>%
        dplyr::mutate(name = as.numeric(name),
                      name = name + 1,
                      eventID = dplyr::row_number()) %>%
        dplyr::group_by(name, key) %>%
        dplyr::arrange(eventID, value) %>%
        dplyr::mutate(rank_col = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(name,rank_col,key) %>%
        dplyr::select(-eventID) %>%
        dplyr::group_by(name, rank_col) %>%
        tidyr::pivot_wider(names_from = key, values_from = value) %>%
        dplyr::ungroup() %>%
        dplyr::select(-rank_col) %>%
        dplyr::rename(CaseID = name,
                      Time = time,
                      Activity = Act,
                      Position = Pos) %>%
        dplyr::arrange(Time, Activity)
      return(x)})

# OLD: parallelizzato con cluster
# cluster_1 <- makeCluster(min(5, (parallel::detectCores())/2))
# data_sim_in_list <-
#   parLapply(
#     cl = cluster_1,
#     X = data_sim_in_list,
#     fun = function(x) {
#       library(magrittr)
#       x %<>%
#         dplyr::filter(key != "count_in") %>%
#         dplyr::select(-replication) %>%
#         dplyr::mutate(name = as.numeric(name),
#                       name = name + 1,
#                       eventID = dplyr::row_number()) %>%
#         dplyr::group_by(name, key) %>%
#         dplyr::arrange(eventID, value) %>%
#         dplyr::mutate(rank_col = dplyr::row_number()) %>%
#         dplyr::ungroup() %>%
#         dplyr::arrange(name,rank_col,key) %>%
#         dplyr::select(-eventID) %>%
#         dplyr::group_by(name, rank_col) %>%
#         tidyr::pivot_wider(names_from = key, values_from = value) %>%
#         dplyr::ungroup() %>%
#         dplyr::select(-rank_col) %>%
#         dplyr::rename(CaseID = name,
#                       Time = time,
#                       Activity = Act,
#                       Position = Pos) %>%
#         dplyr::arrange(Time, Activity)
#       return(x)})
# stopCluster(cluster_1)
tictoc::toc()

# OLD_ non parallelizzato
# for(i in 1:tot_replications){
#   if(i == 1) {cat(paste("\nRaccolta dati ambiente di simulazione\n", "Replicazioni completate: \n", sep = ""))}
#   
#   data_sim_in_list[[i]] <- get_mon_attributes(env_replication[[i]]) %>%
#     filter(key != "count_in") %>%
#     select(-replication) %>%
#     mutate(name = as.numeric(name)) %>%
#     mutate(name = name + 1) %>%
#     mutate(eventID = row_number()) %>%
#     group_by(name, key) %>%
#     arrange(eventID, value) %>%
#     mutate(rank_col = row_number()) %>%
#     ungroup() %>%
#     arrange(name,rank_col,key) %>%
#     ungroup() %>%
#     select(-eventID) %>%
#     group_by(name, rank_col) %>%
#     pivot_wider(names_from = key, values_from = value) %>%
#     ungroup() %>%
#     select(-rank_col) %>%
#     rename(CaseID = name,
#            Time = time,
#            Activity = Act,
#            Position = Pos) %>%
#     arrange(Time, Activity) 
#   
#   cat(paste("\t", i))
#   if(i == tot_replications) {cat("\n")}
# }

cat("\nCostruzione unico dataframe\n")

# data_sim_in_NOTaggr <- data_sim_in_list %>% 
#   bind_rows(.id = "Replication") %>% 
#   mutate(Replication = as_factor(Replication)) 

data_sim_in <- data_sim_in_list %>%
  bind_rows() %>%
  group_by(CaseID, Activity, Position) %>%
  summarise(Time = mean(Time))

##### Ristrutturazione dataframe della simulazione #####
cat("\nRistrutturazione dataframe\n")

# tictoc::tic("inizio")
# data_out_list <-
#   future_lapply(
#     X = data_sim_in_list,
#     FUN = function(x) {
#       library(magrittr)
#       x %<>%
#         dplyr::group_by(Activity) %>%
#         dplyr::mutate(EventID = NULL) %>%
#         tidyr::pivot_wider(names_from = Position, values_from = Time, names_prefix = "Time") %>%
#         dplyr::mutate(Waiting = Time2 - Time1,
#                       Processing = Time3 - Time2,
#                       Diff1 = Time1 - lag(Time1, n = 1),
#                       Diff1 = dplyr::coalesce(Diff1, 0)) %>%
#         dplyr::group_by(Activity) %>%
#         dplyr::arrange(Activity, Time2) %>%
#         dplyr::mutate(Diff2 = Time2 - lag(Time2, n = 1),
#                       Diff2 = dplyr::coalesce(Diff2, 0)) %>%
#         dplyr::arrange(Activity, Time3) %>%
#         dplyr::mutate(Diff3 = Time3 - lag(Time3, n = 1),
#                       Diff3 = dplyr::coalesce(Diff3, 0)) %>%
#         dplyr::group_by(Activity) %>%
#         dplyr::mutate(Diff2_shift = dplyr::lead(Diff2)) %>%
#         dplyr::filter(!is.na(Diff2_shift)) %>%
#         dplyr::group_by(CaseID) %>%
#         dplyr::arrange(CaseID,Time1) %>%
#         dplyr::mutate(TimeNext = dplyr::lead(Time1, n = 1, default = max(Time3)),
#                       Blocking = TimeNext - Time3,
#                       Waiting_trend = Diff2 / Diff1,
#                       Processing_trend = Diff3 / Diff2,
#                       Blocking_trend = dplyr::lead(Diff1, n = 1, default = dplyr::last(Diff3)) / Diff3,
#                       Waiting_trend = dplyr::case_when(is.nan(Waiting_trend) ~ 1, TRUE ~ Waiting_trend),
#                       Processing_trend = dplyr::case_when(is.nan(Processing_trend) ~ 1, TRUE ~ Processing_trend),
#                       Blocking_trend = dplyr::case_when(is.nan(Blocking_trend) ~ 1, TRUE ~ Blocking_trend)) %>%
#         dplyr::group_by(Activity) %>%
#         dplyr::arrange(Diff2) %>%
#         dplyr::mutate(Starvation = Diff2_shift - (Processing + Blocking),
#                       Idle = Blocking + Starvation) %>%
#         dplyr::ungroup() %>%
#         dplyr::select(CaseID, Activity, Time1, Time2, Time3, Waiting, Processing, Blocking, Starvation, Idle, Diff1, Diff2, Diff3, Waiting_trend, Processing_trend, Blocking_trend) %>%
#         dplyr::rename(Timestamp_buff = Time1,
#                       Timestamp_res = Time2,
#                       Timestamp_end = Time3,
#                       Waiting_time = Waiting,
#                       Processing_time= Processing,
#                       Blocking_time = Blocking,
#                       Starving_time = Starvation,
#                       Idle_time = Idle,
#                       Input_diff = Diff1,
#                       Mid_diff = Diff2,
#                       Output_diff = Diff3)
#       return(x)})
# tictoc::toc()

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
  mutate(Blocking = TimeNext - Time3) %>%
  mutate(Waiting_trend = Diff2 / Diff1,
         Processing_trend = Diff3 / Diff2,
         Blocking_trend = lead(Diff1, n = 1, default = last(Diff3)) / Diff3) %>%
  mutate(Waiting_trend = case_when(is.nan(Waiting_trend) ~ 1, TRUE ~ Waiting_trend),
         Processing_trend = case_when(is.nan(Processing_trend) ~ 1, TRUE ~ Processing_trend),
         Blocking_trend = case_when(is.nan(Blocking_trend) ~ 1, TRUE ~ Blocking_trend)) %>%
  group_by(Activity) %>%
  arrange(Diff2) %>%
  mutate(Starvation = Diff2_shift - (Processing + Blocking)) %>%
  mutate(Idle = Blocking + Starvation) 


# Dataframe con nomi standardizzati
cat("\nStandardizzazione nomi campi dataframe\n")

data_out_standard <- data_out %>%
  ungroup() %>%
  select(CaseID, Activity, Time1, Time2, Time3, Waiting, Processing, Blocking, Starvation, Idle, Diff1, Diff2, Diff3, Waiting_trend, Processing_trend, Blocking_trend) %>%
  rename(Timestamp_buff = Time1,
         Timestamp_res = Time2,
         Timestamp_end = Time3,
         Waiting_time = Waiting,
         Processing_time= Processing,
         Blocking_time = Blocking,
         Starving_time = Starvation,
         Idle_time = Idle,
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
                                      .complete = T),
         Waiting_time_SKEW = slide_dbl(Waiting_time, 
                                       skewness, 
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
  mutate(Starving_time_MEAN = slide_dbl(Starving_time, 
                                        mean, 
                                        .before = ampiezza_finestra - 1, 
                                        .step = frequenza_finestra,
                                        .complete = T),
         Starving_time_VAR = slide_dbl(Starving_time, 
                                       var, 
                                       .before = ampiezza_finestra - 1, 
                                       .step = frequenza_finestra,
                                       .complete = T)
  ) %>%
  mutate(Idle_time_MEAN = slide_dbl(Idle_time, 
                                    mean, 
                                    .before = ampiezza_finestra - 1, 
                                    .step = frequenza_finestra,
                                    .complete = T),
         Idle_time_VAR = slide_dbl(Idle_time, 
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
  mutate(Waiting_trend_MEAN = slide_dbl(Waiting_trend, 
                                        mean, 
                                        .before = ampiezza_finestra - 1, 
                                        .step = frequenza_finestra,
                                        .complete = T),
         Waiting_trend_VAR = slide_dbl(Waiting_trend, 
                                       var,
                                       .before = ampiezza_finestra - 1, 
                                       .step = frequenza_finestra,
                                       .complete = T)
  ) %>%
  mutate(Processing_trend_MEAN = slide_dbl(Processing_trend, 
                                           mean, 
                                           .before = ampiezza_finestra - 1, 
                                           .step = frequenza_finestra,
                                           .complete = T),
         Processing_trend_VAR = slide_dbl(Processing_trend, 
                                          var,
                                          .before = ampiezza_finestra - 1, 
                                          .step = frequenza_finestra,
                                          .complete = T)
  ) %>%
  mutate(Blocking_trend_MEAN = slide_dbl(Blocking_trend, 
                                         mean, 
                                         .before = ampiezza_finestra - 1, 
                                         .step = frequenza_finestra,
                                         .complete = T),
         Blocking_trend_VAR = slide_dbl(Blocking_trend, 
                                        var,
                                        .before = ampiezza_finestra - 1, 
                                        .step = frequenza_finestra,
                                        .complete = T)
  ) %>%
  mutate(lagged_Input_diff_MEAN = slide_dbl(lead(Input_diff),
                                            mean,
                                            .before = ampiezza_finestra - 1,
                                            .step = frequenza_finestra,
                                            .complete = T)
  ) %>%
  mutate(lagged_Mid_diff_MEAN = slide_dbl(lead(Mid_diff),
                                          mean,
                                          .before = ampiezza_finestra - 1,
                                          .step = frequenza_finestra,
                                          .complete = T)
  ) %>%
  mutate(lagged_Output_diff_MEAN = slide_dbl(lead(Output_diff),
                                             mean,
                                             .before = ampiezza_finestra - 1,
                                             .step = frequenza_finestra,
                                             .complete = T)
  ) %>%
  ungroup() %>%
  filter(!is.na(Waiting_time_MEAN)) %>%
  mutate(Queue_MEAN = Waiting_time_MEAN/lagged_Input_diff_MEAN) %>%
  mutate(Utilization = Processing_time_MEAN/lagged_Mid_diff_MEAN) %>%
  mutate(Blocking_prob = Blocking_time_MEAN/lagged_Mid_diff_MEAN) %>%
  mutate(Starving_prob = Starving_time_MEAN/lagged_Mid_diff_MEAN) %>%
  mutate(Idle_prob = Idle_time_MEAN/lagged_Mid_diff_MEAN) %>%
  arrange(Activity, Timestamp_buff) %>%
  select(-lagged_Input_diff_MEAN, -lagged_Mid_diff_MEAN, -lagged_Output_diff_MEAN)

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
  filter(Position != 3) %>%
  select(-Queue, -Position)

##### Stampa risultati #####
cat("\nDati utilizzo\n")
print(data_utilization %>% select(-Timestamp_res))
cat("\nDati code\n")
print(data_queue %>% select(-time))