rm(list = ls())
gc()

library(simmer)
library(simmer.bricks)
library(magrittr)
library(tidyverse)
library(triangle)
library(lognorm)

# set.seed(1234)

##### Parametri di setup #####
# Numero di replicazioni
tot_replications <- 2

# Parametri di arrivo
arrival_param <- list(
  10000, # Numero totale di casi generato
  20 # Media di interrarrivo, nella funzione rexp viene trasformata in frequenza media
)

# Parametri di processo
process_param <- data.frame(
  c(getParmsLognormForMoments(mean = 10, var = 5)),
  c(getParmsLognormForMoments(mean = 15, var = 5)),
  c(getParmsLognormForMoments(mean = 20, var = 5)),
  c(getParmsLognormForMoments(mean = 25, var = 5)),
  c(getParmsLognormForMoments(mean = 30, var = 5)),
  c(getParmsLognormForMoments(mean = 35, var = 5)),
  row.names = c("mean","sd")
)
colnames(process_param) <- as.character(c(1:6)) 

# Changing point (numero di pezzi lavorati dalla macchina 1 prima del cambiamento)
change_value <- 5000

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

# Distribuzione degli interarrivi
arrival_distr <- function() {c(rexp(arrival_param[[1]], 1/arrival_param[[2]]),-1)}

# Limite code
q_lim <- list()
q_lim[[1]] <- 20
q_lim[[2]] <- 40
q_lim[[3]] <- 30
q_lim[[4]] <- 60
q_lim[[5]] <- 20
q_lim[[6]] <- 50
q_lim[[7]] <- 80
q_lim[[8]] <- 20


##### Stage builder #####
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
        if(get_global(env_name, "count_in") < changing_point){return(1)}
        else{return(2)} },
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


##### Environment builder #####
builder_env <- function(){
  traj_complete <- join(builder_first_act(),
                        builder_middle_act(num = 2, delay_ind = 3),
                        builder_changing_PT_act(num = 3, 
                                                delay_ind = 1, new_delay_ind = 6, 
                                                changing_point = change_value),
                        builder_middle_act(num = 4, delay_ind = 3),
                        builder_middle_act(num = 5, delay_ind = 4),
                        builder_middle_act(num = 6, delay_ind = 2),
                        builder_last_act())
  
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
# Run1 <- env_single %>%
#   reset() %>%
#   run()
# 
# env_single

env_replication <- list()
for(i in 1:tot_replications){
  env_single <- builder_env()
  env_replication[[i]] <- env_single %>% reset() %>% run()
  print(env_replication[[i]])
}


data_sim_in_list <- list()
for(i in 1:tot_replications){
  data_sim_in_list[[i]] <- get_mon_attributes(env_replication[[i]]) %>%
    filter(key != "count_in") %>%
    select(-replication) %>%
    mutate(name = as.numeric(name)) %>%
    mutate(name = name + 1) %>%
    filter(name <= 15000) %>%
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
  
  # data_sim_in_list[[i]]$replication <- rep(i) 
}

data_sim_in <- bind_rows(data_sim_in_list) %>%
  group_by(CaseID, Activity, Position) %>%
  summarise(Time = mean(Time)) 

##### continua in Computation, qui riportato il codice #####
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

##### Plot #####
cosidered_data_list <- data_out %>%
  filter(Activity == 3)
data_plot <- cosidered_data_list
plot_1 <- ggplot(data_plot, 
                 aes(x = CaseID))+
  geom_point(aes(y = Waiting), size = 0.5)+
  expand_limits(x = 0, y = 0)+
  theme(legend.position = "none",
        axis.title = element_blank())+
  labs(x = "CaseID",
       y = "Blocking_time")
plot_1

a <- data_out %>% filter(CaseID > 1000, Activity == 3)
b <- a$Waiting
mean(b)
