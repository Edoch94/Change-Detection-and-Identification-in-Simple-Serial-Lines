# Controllo rapporto massimo fra process_time e mid_diff (workload)
controllo1 <- c()
for(i in 1:length(data_out_list)){
  controllo1 <- c(controllo1, all(data_out_list[[i]]$resource[1:nrow(data_out_list[[i]])-1] <=
                                  data_out_list[[i]]$int2[2:nrow(data_out_list[[i]])]))
}
unique(controllo1)

a <- subset(data_out_list[[4]],
            data_out_list[[i]]$resource[1:nrow(data_out_list[[i]])-1] >
              data_out_list[[i]]$int2[2:nrow(data_out_list[[i]])])

rap_pos <- 4
util <- data_out_list[[rap_pos]]$resource[1:nrow(data_out_list[[rap_pos]])-1] / 
  data_out_list[[rap_pos]]$int2[2:nrow(data_out_list[[rap_pos]])]

rapporto <- c(0,rapporto)

data_test <- as.data.frame(matrix(ncol = 2, nrow = length(rapporto)))

colnames(data_test) <- c("CaseID", "util")
data_test$util <- rapporto
data_test$CaseID <- seq_along(data_test$CaseID)

plot_util <- ggplot(data_test, 
                 aes(x = CaseID, 
                     y = util))+
  geom_point(size = 0.1)+
  geom_vline(xintercept = c(5000,7500,10000,12500),
             linetype = "dotted")+
  expand_limits(x = 0, y = 0)+
  theme(legend.position = "none",
        axis.title = element_blank())+
  labs(x = "CaseID",
       y = "util")
plot_util

controllo2 <- c()
for(i in 1:length(data_out_list)){
  controllo2 <- c(controllo2, any(data_out_list[[i]]$buffer[1:nrow(data_out_list[[i]])-1] >
                                    data_out_list[[i]]$int1[2:nrow(data_out_list[[i]])]))
}
controllo2

# utilization in windows
data_utilization_list <- FCSIqta(data_out_list,
                                 dependent_variables,
                                 ampiezza_qta,
                                 distanza_qta,
                                 stat_ind = "media")
data_utilization_list_SUB <- lapply(data_utilization_list, FSIL)

controllo <- c()
for(i in seq_along(data_utilization_list_SUB)){
  controllo <- c(controllo, all(data_utilization_list_SUB[[i]]$media_resource[1:nrow(data_utilization_list_SUB[[i]])-1] <=
                                  data_utilization_list_SUB[[i]]$media_int2[2:nrow(data_utilization_list_SUB[[i]])]))
}
controllo

data_test_2 <- data_utilization_list_SUB[[6]] %>% filter(media_resource > media_int2)

plot_util <- ggplot(data_utilization_list_SUB[[6]], aes(x = CaseID))+
  geom_point(size = 0.1, aes(y = media_int1), colour = "red")+
  geom_point(size = 0.1, aes(y = media_int2))+
  geom_vline(xintercept = c(5000,7500,10000,12500),
             linetype = "dotted")+
  expand_limits(x = 0, y = 0)+
  theme(legend.position = "none",
        axis.title = element_blank())+
  labs(x = "CaseID",
       y = "util")
plot_util

rap_pos <- 6
rapporto <- data_utilization_list_SUB[[rap_pos]]$media_resource[1:nrow(data_utilization_list_SUB[[rap_pos]])-1] / 
  data_utilization_list_SUB[[rap_pos]]$media_int2[2:nrow(data_utilization_list_SUB[[rap_pos]])]

rapporto <- c(0,rapporto)

data_test <- as.data.frame(matrix(ncol = 2, nrow = length(rapporto)))

colnames(data_test) <- c("CaseID", "util")
data_test$util <- rapporto
data_test$CaseID <- data_utilization_list_SUB[[1]]$CaseID

plot_util <- ggplot(data_test, 
                    aes(x = CaseID, 
                        y = util))+
  geom_point(size = 1)+
  geom_vline(xintercept = c(5000,7500,10000,12500),
             linetype = "dotted")+
  expand_limits(x = 0, y = 0)+
  theme(legend.position = "none",
        axis.title = element_blank())+
  labs(x = "CaseID",
       y = "util")
plot_util

data_test_2 <- data_utilization_list_SUB[[6]] %>% filter(media_resource > media_int2) %>% select(CaseID,media_resource,media_int2)
data_test_3 <- data_test_2 %>% mutate(ch_bool = case_when(media_resource <= media_int2 ~ 1,
                                                          T ~ 0))
data_test_3 %<>% mutate(ch_bool2 = case_when(media_resource/media_int2 > 1 ~ 0, T ~ 1))

# data_test_3 <- data_test %>% filter(util > 1)
data_test_3 <- data_test_3 %>% mutate(util_perc = (util-1))


data_util_min_1 <- data_utilization_list_SUB[[4]]
data_util_min_1 %<>% filter(media_resource > media_int2)

plot_util <- ggplot(data_util_min_1, 
                    aes(x = CaseID))+
  geom_point(size = 1, aes(y = media_resource), colour = "red")+
  geom_point(size = 1, aes(y = media_int2))+
  theme(legend.position = "none",
        axis.title = element_blank())+
  labs(x = "CaseID",
       y = "util")
plot_util

case_filter <- 11000
data_test_4 <- data_out_list[[3]] %>% filter(CaseID > case_filter-300 &
                                               CaseID <= case_filter)
data_test_4 %<>% select(CaseID,resource,int2)

data_test_4_mean <- data_test_4 %>% c(mean(.$resource),mean(.$int2))

##### Valori in data_out_list_util #####
subset_util_sup_one <- lapply(data_out_list_util, 
                              FUN = function(x){
                                x %<>% filter(utilization > 1)
                              })

subset_util_sup_one %<>% lapply(FUN = function(x){x %<>% select(CaseID,media_resource,media_int2,utilization)})

##### Funzione calcolo media su finestre #####
FCMW <- function(data_struct, ampiezza = ampiezza_qta, distanza = distanza_qta, sfasamento = F){
  sequenza <- seq(from = ampiezza,
                  to = length(data_struct), 
                  by = distanza)
  sequenza <- c(0,sequenza)
  sequenza[length(sequenza)] <- sequenza[length(sequenza)] - 1
  
  if(sfasamento){
    sequenza <- sequenza + 1
  }
  
  data_struct_out <- numeric(length = length(sequenza) - 1)
  
  for (i in 2:length(sequenza)) {
    data_struct_out[i-1] <- mean(data_struct[(sequenza[i-1] + 1):sequenza[i]])
  }
  
  return(data_struct_out)
}

# a <- FCMW(data_out_list[[3]]$int2,ampiezza_qta,distanza_qta, T)
# b <- FCMW(data_out_list[[3]]$resource,ampiezza_qta,distanza_qta, F)
# 
# c <- as.data.frame(matrix(data = c(a,b), ncol = 2, byrow = F))
# colnames(c) <- c("a","b")
# filter(c, b > a)

##### Funzione calcolo dataframe con medie utilizzo #####
FCUR <- function(data_struct, ampiezza = ampiezza_qta, distanza = distanza_qta){
  
  data_struct_out_util <- lapply(data_struct_out_util, )
  
  return(data_struct_out_util)
}


##### Funzione che associa a CaseID l'utilizzo della risorsa (dataframe con attività e risorsa univoche) #####
FAUR <- function(data_struct, ampiezza = ampiezza_qta, distanza = distanza_qta){
  sequenza <- seq(from = ampiezza,
                  to = nrow(data_struct), 
                  by = distanza)
  sequenza <- c(0,sequenza)
  sequenza[length(sequenza)] <- sequenza[length(sequenza)] - 1
  
  data_struct_out <- data_struct[sequenza,]
  
  data_struct_out$util <- FCMW(data_struct$resource,ampiezza,distanza,F)/FCMW(data_struct$int2,ampiezza,distanza,T)
  
  return(data_struct_out)
}

# Funzione split su ResourceID 
lapply_ResourceID <- function(data_struct){
  data_out <- split(data_struct, data_struct$ResourceID)
  return(data_out)
}

data_out_util_list <- data_out_list %>% lapply(lapply_ResourceID) %>% lapply(function(x) {return(lapply(x,FAUR))})


# ##### Funzione calcolo medie utilizzo (multi risorse) #####
# FCUR_multi <- function(data_struct, ampiezza, distanza){
#   data_out <- split(data_struct,ResourceID)
#   
# }
# 
# ##### Funzione calcolo medie utilizzo (singole risorse) #####
# FCUR_single <- function(data_struct, ampiezza, distanza){
#   utilizzo <- FCMW(data_struct$resource,ampiezza,distanza,F)/FCMW(data_struct$int2,ampiezza,distanza,T)
#   return(utilizzo)
# }





