# Distribuzione degli interarrivi
arrival_distr <- function() {c(rexp(arrival_param[[1]], 1/arrival_param[[2]]),-1)}

##### Stage builder #####
cat("\nCostruzione funzioni\n")

builder_first_act <- function(env_name = env_single, 
                              delay_ind = 1){
  act <- trajectory("A1") %>%
    set_attribute("Act", 1) %>%
    set_attribute("Pos", 1) %>%
    seize("R_A1") %>%
    set_attribute("Act", 1) %>%
    set_attribute("Pos", 2) %>%
    timeout(function() rlnorm(n = 1, 
                              meanlog = getParmsLognormForMoments(mean = delay_ind, var = std_log^2)[[1]], 
                              sdlog = getParmsLognormForMoments(mean = delay_ind, var = std_log^2)[[2]])) %>% 
    
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

builder_last_act <- function(env_name = env_single, 
                             delay_ind = 1){
  act <- trajectory("A7") %>%
    set_attribute("Act", 7) %>%
    set_attribute("Pos", 1) %>%
    seize("R_A7") %>%
    set_attribute("Act", 7) %>%
    set_attribute("Pos", 2) %>%
    timeout(function()rlnorm(n = 1, 
                             meanlog = getParmsLognormForMoments(mean = delay_ind, var = std_log^2)[[1]], 
                             sdlog = getParmsLognormForMoments(mean = delay_ind, var = std_log^2)[[2]])) %>% 
    set_attribute("Act", 7) %>%
    set_attribute("Pos", 3) %>%
    release("R_A7") %>%
    
    
    branch(
      option = function(){ # il segnale parte quando la coda è strettamente minore o solo minore del limite del buffer
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
      trajectory() %>% timeout(function()rlnorm(n = 1, 
                                                meanlog = getParmsLognormForMoments(mean = delay_ind, var = std_log^2)[[1]], 
                                                sdlog = getParmsLognormForMoments(mean = delay_ind, var = std_log^2)[[2]])),
      trajectory() %>% timeout(function()rlnorm(n = 1, 
                                                meanlog = getParmsLognormForMoments(mean = new_delay_ind, var = std_log^2)[[1]], 
                                                sdlog = getParmsLognormForMoments(mean = new_delay_ind, var = std_log^2)[[2]]))
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
      option = function(){ # il segnale parte quando la coda è strettamente minore o solo minore del limite del buffer
        if(get_global(env_name, "count_in") < changing_point) { return((get_queue_count(env_name, res_name) < q_lim[[num]])) }
        else { return((get_queue_count(env_name, res_name) < q_lim_new[[num]])) }
      },
      continue = T,
      trajectory() %>% send(signal_out)
    )
  return(act)
}

##### Environment builder #####
cat("\nCreazione ambiente di simulazione\n")
builder_env <- function(env_in_builder = env_single,
                        delay_ind_list_in_builder = delay_ind_list,
                        delay_ind_new_list_in_builder = delay_ind_new_list,
                        changing_point_in_builder = change_value){
  traj_complete <- 
    join(builder_first_act(env_name = env_in_builder,
                           delay_ind = delay_ind_list_in_builder[[1]]),
         
         builder_general_act(env_name = env_in_builder,
                             num = 2, 
                             delay_ind = delay_ind_list_in_builder[[2]], 
                             new_delay_ind = delay_ind_new_list_in_builder[[2]],
                             changing_point = changing_point_in_builder),
         
         builder_general_act(env_name = env_in_builder,
                             num = 3, 
                             delay_ind = delay_ind_list_in_builder[[3]], 
                             new_delay_ind = delay_ind_new_list_in_builder[[3]],
                             changing_point = changing_point_in_builder),
         
         builder_general_act(env_name = env_in_builder,
                             num = 4,
                             delay_ind = delay_ind_list_in_builder[[4]],
                             new_delay_ind = delay_ind_new_list_in_builder[[4]],
                             changing_point = changing_point_in_builder),
         
         builder_general_act(env_name = env_in_builder,
                             num = 5, 
                             delay_ind = delay_ind_list_in_builder[[5]], 
                             new_delay_ind = delay_ind_new_list_in_builder[[5]],
                             changing_point = changing_point_in_builder),
         
         builder_general_act(env_name = env_in_builder,
                             num = 6, 
                             delay_ind = delay_ind_list_in_builder[[6]], 
                             new_delay_ind = delay_ind_new_list_in_builder[[6]],
                             changing_point = changing_point_in_builder),
         
         builder_last_act(env_name = env_in_builder,
                          delay_ind = delay_ind_list_in_builder[[7]])
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

# plan(multisession, gc = TRUE, workers = min(5, (parallel::detectCores())/2))
# env_replication <- vector(mode = "list", length = 30)
# 
# env_replication <-
#   future_lapply(
#     X = env_replication,
#     FUN = function(x) {
#       env_single <- builder_env()
#       x <- env_single %>% simmer::reset() %>% simmer::run()
#       return(x)
#     },
#     future.seed = TRUE
#   )

env_replication <- list()

for(i in 1:tot_replications){
  env_single <- builder_env()
  env_replication[[i]] <- env_single %>% reset() %>% simmer::run()
  cat(paste("\nReplicazione",i,"\n"))
  print(env_replication[[i]])
}