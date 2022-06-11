rm(list = ls())

library(simmer)
library(simmer.bricks)
library(magrittr)
library(tidyverse)
library(triangle)
library(ggplot2)
library(plotly)


##### Setup #####
# Distribuzioni delay triangolari
tr_distr <- list()
tr_distr[[1]] <- function(){return(rtriangle(a = 100, b = 300))}
tr_distr[[2]] <- function(){return(rtriangle(a = 200, b = 400))}
tr_distr[[3]] <- function(){return(rtriangle(a = 300, b = 500))}
tr_distr[[4]] <- function(){return(rtriangle(a = 400, b = 600))}
tr_distr[[5]] <- function(){return(rtriangle(a = 500, b = 700))}
tr_distr[[6]] <- function(){return(rtriangle(a = 600, b = 800))}

# Distribuzioni delay lognormali
# tr_distr <- list()
# tr_distr[[1]] <- function(){return(rlnorm(n = 1, meanlog = 0, sdlog = 1))}
# tr_distr[[2]] <- function(){return(rlnorm(n = 1, meanlog = 0, sdlog = 1))}
# tr_distr[[3]] <- function(){return(rlnorm(n = 1, meanlog = 0, sdlog = 1))}
# tr_distr[[4]] <- function(){return(rlnorm(n = 1, meanlog = 0, sdlog = 1))}
# tr_distr[[5]] <- function(){return(rlnorm(n = 1, meanlog = 0, sdlog = 1))}
# tr_distr[[6]] <- function(){return(rlnorm(n = 1, meanlog = 0, sdlog = 1))}
# 
# mediaLN <- 1
# sigmaLN <- 2
# npoints <- 10000
# vec_rgen <- rlnorm(npoints, meanlog = mediaLN, sdlog = sigmaLN)
# mean(vec_rgen) # media campionaria
# var(vec_rgen) # varianza campionaria
# exp(mediaLN + (sigmaLN^2)/2) # media distribuzione
# (exp(sigmaLN^2)-1)*exp(2*mediaLN+sigmaLN^2) # varianza distribuzione
# (exp(sigmaLN^2)-1)*exp(2*mediaLN+sigmaLN^2)/sqrt(npoints)
# 
# 
# ggplotly(
#   ggplot(data.frame(x = vec_rgen), aes(x)) +
#     geom_density() +
#     stat_function(fun = dlnorm, args = list(mean = mediaLN, sd = sigmaLN), colour = "red")
# )



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


##### Stage builders #####
first_act_builder <- function(){
  AttivitaFirst <- trajectory("A1") %>%
    set_attribute("Act", 1) %>%
    set_attribute("Pos", 1) %>%
    seize("R_A1") %>%
    set_attribute("Act", 1) %>%
    set_attribute("Pos", 2) %>%
    timeout(tr_distr[[1]]) %>% 
    set_attribute("Act", 1) %>%
    set_attribute("Pos", 3) %>%
    branch(option = function()(get_queue_count(Env_single, "R_A2") >= q_lim[[2]]),
           continue = T,
           trajectory() %>%
             wait_until("Signal_A2")) %>%
    release("R_A1")
  return(AttivitaFirst)
}

last_act_builder <- function(){
  AttivitaLast <- trajectory("A8") %>%
    set_attribute("Act", 8) %>%
    set_attribute("Pos", 1) %>%
    seize("R_A7") %>%
    set_attribute("Act", 8) %>%
    set_attribute("Pos", 2) %>%
    timeout(tr_distr[[1]]) %>% 
    set_attribute("Act", 8) %>%
    set_attribute("Pos", 3) %>%
    release("R_A8") %>%
    send("Signal_A8")
  return(AttivitaLast)
}

act_builder <- function(env_name = Env_single, num = 1, delay_ind = 1){
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

changing_PT_act_builder <- function(env_name = Env_single, num = 1, delay_ind = 1, changing_point = 5000, new_delay_ind = 1){
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
    branch(
      option = function()(get_n_generated(env_name, "") >= changing_point),
      continue = T,
      trajectory() %>% 
        timeout(tr_distr[[new_delay_ind]])
    ) %>%
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

# Ancora da sviluppare il change di buffer limit
changing_BL_act_builder <- function(env_name = Env_single, num = 1, delay_ind = 1){
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




##### Environment builder #####
Env_builder <- function(){
  traj_complete <- 
    join(
      first_act_builder(),
      act_builder(num = 2, delay_ind = 3),
      act_builder(num = 3, delay_ind = 2),
      # changing_PT_act_builder(num = 1, 
      #                         delay_ind = 1, new_delay_ind = 5,
      #                         changing_point = 5000),
      act_builder(num = 5, delay_ind = 3),
      act_builder(num = 5, delay_ind = 3),
      act_builder(num = 6, delay_ind = 4),
      act_builder(num = 7, delay_ind = 4),
      last_act_builder()
    )
  
  en <- simmer("Sim") %>%
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
  return(en)
}


##### Launcher #####
Env_Out_list <- list()
for(i in 1:2){
  Env_single <- Env_builder()
  Env_Out_list[[i]] <- Env_single %>% run()
  print(Env_Out_list[[i]])
}

# Env_Out_list[[1]] <- Env_single %>% run()
# Env_single <- Env_builder()
# Env_Out_list[[2]] <- Env_single %>% run()
# Env_Out_list[[1]]
# Env_Out_list[[2]]

# Test differenza repliche
tot_1 <- get_mon_attributes(Env_Out_list[[1]])

tot_2 <- get_mon_attributes(Env_Out_list[[2]])


##### Run #####
# Run1 <- Env_5 %>%
#   reset() %>%
#   run()
# 
# Env_5


tot_1 <- get_mon_attributes(Env_Out_list[[1]]) %>%
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
  # arrange(name,rank_col,key) %>%
  # ungroup() %>%
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

data_sim_in <- tot_1

##### Ristrutturazione dati #####
# data_sim_in <- get_mon_attributes(Env_Out) %>%
#   # select(-replication) %>%
#   mutate(name = as.numeric(name)) %>%
#   mutate(name = name + 1) %>%
#   filter(name <= 15000) %>%
#   group_by(replication) %>%
#   mutate(eventID = row_number()) %>%
#   ungroup() %>%
#   
#   group_by(name, key, value, eventID)%>%
#   summarise(time = mean(time)) %>%
#   arrange(eventID)
#   
#   group_by(name, replication) %>% 
#   mutate(n = row_number()) %>%
#   mutate(n = max(n)) %>%
#   ungroup() %>%
#   filter(n == 8*3*n_distinct(select(.,key))) %>%
#   select(-n) %>%
#   
#   group_by(name, key) %>%
#   arrange(eventID, value) %>%
#   mutate(rank_col = row_number()) %>%
#   # ungroup() %>%
#   # arrange(name,rank_col,key) %>%
#   ungroup() %>%
#   select(-eventID) %>%
#   
#   group_by(name, rank_col) %>%
#   pivot_wider(names_from = key, values_from = value) %>%
#   ungroup() %>%
#   select(-rank_col, - Pos) %>%
#   rename(CaseID = name,
#          Time = time,
#          Activity = Act) %>%
#   mutate(Resource = rep(1)) %>%
#   arrange(Time, Activity)

# continua in Computation





