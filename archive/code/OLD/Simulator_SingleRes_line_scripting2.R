rm(list = ls())

library(simmer)
library(simmer.bricks)
library(magrittr)
library(tidyverse)
library(triangle)

# set.seed(1234)

##### Setup #####
# Distribuzioni delay
tr_distr <- list()
tr_distr[[1]] <- function(){return(rtriangle(a = 10, b = 30))}
tr_distr[[2]] <- function(){return(rtriangle(a = 20, b = 40))}
tr_distr[[3]] <- function(){return(rtriangle(a = 30, b = 50))}
tr_distr[[4]] <- function(){return(rtriangle(a = 40, b = 60))}
tr_distr[[5]] <- function(){return(rtriangle(a = 50, b = 70))}
tr_distr[[6]] <- function(){return(rtriangle(a = 60, b = 80))}

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
builder_first_act <- function(env_name = env_single){
  act <- trajectory("A1") %>%
    set_attribute("Act", 1) %>%
    set_attribute("Pos", 1) %>%
    seize("R_A1") %>%
    set_attribute("Act", 1) %>%
    set_attribute("Pos", 2) %>%
    timeout(tr_distr[[1]]) %>% 
    
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

builder_last_act <- function(env_name = env_single){
  act <- trajectory("A7") %>%
    set_attribute("Act", 7) %>%
    set_attribute("Pos", 1) %>%
    seize("R_A7") %>%
    set_attribute("Act", 7) %>%
    set_attribute("Pos", 2) %>%
    timeout(tr_distr[[1]]) %>% 
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
    timeout(tr_distr[[delay_ind]]) %>% 
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
      trajectory() %>% timeout(tr_distr[[delay_ind]]),
      trajectory() %>% timeout(tr_distr[[new_delay_ind]])
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
                                                changing_point = 7500),
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
                  distribution = function() {c(rexp(15000),-1)}) %>%
    add_global("count_in", 0)
  return(sim)
}

env_single <- builder_env()

##### Run #####
Run1 <- env_single %>%
  reset() %>%
  run()

env_single

env_replication <- list()
for(i in 1:2){
  env_replication[[i]] <- builder_env() %>% reset() %>% run()
  print(env_replication[[i]])
}

data_sim_in_list <- list()
for(i in 1:2){
  data_sim_in_list[[i]] <- get_mon_attributes(env_replication[[i]])
  data_sim_in_list[[i]] %<>% filter(key!="count_in")
  data_sim_in_list[[i]]$replication <- rep(i)
}

data_sim_in <- bind_rows(data_sim_in_list)

##### Ristrutturazione dati #####
data_sim_in <- get_mon_attributes(Run1) %>%
  filter(key != "count_in") %>%
  select(-replication) %>%
  mutate(name = as.numeric(name)) %>%
  mutate(name = name + 1) %>%
  filter(name <= 15000) %>%
  mutate(eventID = row_number()) %>%
  group_by(name) %>% 
  mutate(n = row_number()) %>%
  mutate(n = max(n)) %>%
  ungroup() %>%
  filter(n == 7*3*n_distinct(select(.,key))) %>%
  select(-n) %>%
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
  select(-rank_col, - Pos) %>%
  rename(CaseID = name,
         Time = time,
         Activity = Act) %>%
  mutate(Resource = rep(1)) %>%
  arrange(Time, Activity)


# data_sim <- data_sim_in %>%
#   mutate(name = as.numeric(name)) %>% 
#   mutate(name = name + 1) %>%
#   filter(name <= 15000) %>%
#   
#   group_by(name, replication, key, value) %>%
#   arrange(time) %>%
#   mutate(event_in_case = row_number())
#   
#   # group_by(replication) %>%
#   # arrange(name, time, key, value) %>%
#   # mutate(eventID = row_number()) %>%
#   # 
#   # group_by(name, key, value, eventID) %>%
#   # summarise(time = mean(time))
#   
#   group_by(name, key, replication) %>%
#   arrange(eventID, value) %>%
#   mutate(rank_col = row_number()) 
# 
#   ungroup() %>%
#   arrange(name,rank_col,key) %>%
#   # ungroup() %>%
#   select(-eventID) %>%
#   group_by(name, rank_col) %>%
#   pivot_wider(names_from = key, values_from = value) %>%
#   ungroup() %>%
#   select(-rank_col, - Pos) %>%
#   rename(CaseID = name,
#          Time = time,
#          Activity = Act) %>%
#   mutate(Resource = rep(1)) %>%
#   arrange(Time, Activity)

##### continua in Computation, qui riportato il codice #####
data_out <- data_sim_in %>%
  group_by(Activity, CaseID) %>%
  mutate(Position = order(Time)) %>%
  mutate(Resource = replace(Resource, Resource == 0, max(Resource))) %>%
  group_by(Activity) %>%
  mutate(EventID = NULL) %>%
  pivot_wider(names_from = Position, values_from = Time, names_prefix = "Time") %>% 
  mutate(Waiting = Time2 - Time1,
         Processing = Time3 - Time2) %>%
  mutate(Diff1 = Time1 - lag(Time1, n = 1)) %>% 
  mutate(Diff1 = coalesce(Diff1, 0)) %>%
  group_by(Activity, Resource) %>% 
  arrange(Activity, Resource, Time2) %>%
  mutate(Diff2 = Time2 - lag(Time2, n = 1)) %>%
  mutate(Diff2 = coalesce(Diff2, 0)) %>%
  arrange(Activity, Resource, Time3) %>%
  mutate(Diff3 = Time3 - lag(Time3, n = 1)) %>%
  mutate(Diff3 = coalesce(Diff3, 0)) %>%
  group_by(Activity, Resource) %>%
  mutate(Diff2_shift = lead(Diff2)) %>%
  filter(!is.na(Diff2_shift)) %>%
  group_by(CaseID) %>%
  arrange(CaseID,Time1) %>%
  mutate(TimeNext = lead(Time1, n = 1, default = max(Time3))) %>%
  mutate(Blocking = TimeNext - Time3) 

# plot
cosidered_data_list <- data_out %>%
  filter(Activity == 2)
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
