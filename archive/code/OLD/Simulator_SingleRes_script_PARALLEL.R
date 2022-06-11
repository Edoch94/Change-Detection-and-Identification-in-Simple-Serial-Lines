##### Funzioni distribuzioni delay #####
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
    
    
    branch(
      option = function(){ # il segnale parte quando la coda è strettamente minore o solo minore del limite del buffer?
        return(get_queue_count(env_name, "R_A7") < q_lim[[7]])
      },
      continue = T,
      trajectory() %>% send("Signal_A7")
    )
  
  
  # send("Signal_A7")
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
        if(get_global(env_name, "count_in") < changing_point) { return((get_queue_count(env_name, res_name_next) >= q_lim[[num + 1]])) }
        else { return((get_queue_count(env_name, res_name_next) >= q_lim_new[[num + 1]])) }
      },
      continue = T,
      trajectory() %>% wait_until(signal_in)
    ) %>%
    release(res_name) %>%
    branch(
      option = function(){ # il segnale parte quando la coda è strettamente minore o solo minore del limite del buffer?
        if(get_global(env_name, "count_in") < changing_point) { return((get_queue_count(env_name, res_name) < q_lim[[num]])) }
        else { return((get_queue_count(env_name, res_name) < q_lim_new[[num]])) }
      },
      continue = T,
      trajectory() %>% send(signal_out)
    )
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
# 
# env_replication <- list()
# for(i in 1:tot_replications){
#   if(i == 1) {cat("\nRun delle simulazioni\n")}
#   env_single <- builder_env()
#   env_replication[[i]] <- env_single %>% reset() %>% run()
#   cat(paste("\nReplicazione",i,"\n"))
#   print(env_replication[[i]])
# }


env_replication <- list()
for(i in 1:tot_replications){
  env_replication[[i]] <- builder_env()
}

# system.time(
#   env_replication <- mclapply(env_replication, function(i){
#     # env_single <- builder_env()
#     # cat(paste("\nReplicazione",i,"\n"))
#     # print(env_replication[[i]])
#     return(i %>% reset() %>% run())
#   }, mc.cores = 4)
# )

env_replication1 <- mclapply(env_replication, function(i){
  # env_single <- builder_env()
  # cat(paste("\nReplicazione",i,"\n"))
  # print(env_replication[[i]])
  return(i %>% reset() %>% run())
}, mc.cores = 2)






