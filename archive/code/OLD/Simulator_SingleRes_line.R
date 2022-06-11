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


##### Stage #####
Attivita1 <- trajectory("A1") %>%
  set_attribute("Act", 1) %>%
  set_attribute("Pos", 1) %>%
  seize("R_A1") %>%
  set_attribute("Act", 1) %>%
  set_attribute("Pos", 2) %>%
  timeout(tr_distr[[1]]) %>% 
  set_attribute("Act", 1) %>%
  set_attribute("Pos", 3) %>%
  branch(option = function()(get_queue_count(Env_5, "R_A2") >= q_lim[[2]]),
         continue = T,
         trajectory() %>%
           wait_until("Signal_A2")) %>%
  release("R_A1")

Attivita8 <- trajectory("A8") %>%
  set_attribute("Act", 8) %>%
  set_attribute("Pos", 1) %>%
  seize("R_A8") %>%
  set_attribute("Act", 8) %>%
  set_attribute("Pos", 2) %>%
  timeout(tr_distr[[1]]) %>% 
  set_attribute("Act", 8) %>%
  set_attribute("Pos", 3) %>%
  release("R_A8") %>%
  send("Signal_A8")

##### Stage builder #####
act_builder_singleRes <- function(env_name = Env_5, num = 1, delay_ind = 1){
  trj_name <- paste0("A", as.character(num))
  res_name <- paste0("R_A", as.character(num))
  res_name_next <- paste0("R_A", as.character(num+1))
  signal_in <- paste0("Signal_A", as.character(num+1))
  signal_out <- paste0("Signal_A", as.character(num))
  
  AttivitaX <- trajectory(trj_name) %>%
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
  
  return(AttivitaX)
}

Attivita2 <- act_builder_singleRes(num = 2, delay_ind = 3)
Attivita3 <- act_builder_singleRes(num = 3, delay_ind = 2)
Attivita4 <- act_builder_singleRes(num = 4, delay_ind = 5)
Attivita5 <- act_builder_singleRes(num = 5, delay_ind = 3)
Attivita6 <- act_builder_singleRes(num = 6, delay_ind = 4)
Attivita7 <- act_builder_singleRes(num = 7, delay_ind = 1)


##### Risorse e generatore #####
traj_complete <- join(Attivita1,
                      Attivita2,
                      Attivita3,
                      Attivita4,
                      Attivita5,
                      Attivita6,
                      Attivita7,
                      Attivita8)

Env_5 <- 
  simmer("Sim_5") %>%
  add_resource("R_A1", capacity = 1, preempt_order = "fifo") %>%
  add_resource("R_A2", capacity = 1, preempt_order = "fifo") %>%
  add_resource("R_A3", capacity = 1, preempt_order = "fifo") %>%
  add_resource("R_A4", capacity = 1, preempt_order = "fifo") %>%
  add_resource("R_A5", capacity = 1, preempt_order = "fifo") %>%
  add_resource("R_A6", capacity = 1, preempt_order = "fifo") %>%
  add_resource("R_A7", capacity = 1, preempt_order = "fifo") %>%
  add_resource("R_A8", capacity = 1, preempt_order = "fifo") %>%
  add_generator(name_prefix = "",
                trajectory = traj_complete,
                mon = 2,
                distribution = function() {c(rexp(15000),-1)})


##### Run #####
Run1 <- Env_5 %>%
  reset() %>%
  run(1000000)

Env_5


##### Ristrutturazione dati #####
data_sim_in <- get_mon_attributes(Env_5) %>%
  select(-replication) %>%
  mutate(name = as.numeric(name)) %>%
  mutate(name = name + 1) %>%
  filter(name <= 15000) %>%
  mutate(eventID = row_number()) %>%
  group_by(name) %>% 
  mutate(n = row_number()) %>%
  mutate(n = max(n)) %>%
  ungroup() %>%
  filter(n == 8*3*n_distinct(select(.,key))) %>%
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

# continua in Computation






