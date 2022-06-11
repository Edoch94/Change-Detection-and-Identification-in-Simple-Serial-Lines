# floor(runif(1)*20) + 1
# b <- as.data.frame(matrix(data = c(floor(runif(10000)*20) + 1)), 
#                    ncol = 1,nrow = 10000, byrow = T)
# d <- ggplot(b, aes(x = V1)) + geom_bar()
# last_plot()
# t.test(floor(runif(10^6)*20)+1, mu = 10.5)

a <- data_out_util_list[[4]][[2]]

b <- data_out_list

d <- b$`4`

f <- d %>% 
  filter(ResourceID == 3) %>%
  mutate(resource = lag(resource,n = 1, default = 0)) %>%
  arrange(time_res) %>% 
  filter(resource > int2) %>% 
  select(CaseID,resource,int2) %>% 
  mutate(util = resource/int2) %>%
  filter(util > 2)


f <- data_trans_split[[3]] %>% mutate(resource = lag(resource,n = 1, default = 0)) %>% arrange(time_res) %>% filter(resource > int2) %>% select(CaseID,ResourceID,resource,int2,time_res)

f %<>% mutate(util = resource/int2)

g <- data_trans_split[[3]]

h <- g %>% FCI2() %>% 
  mutate(resource = lag(resource,n = 1, default = 0)) %>% 
  arrange(time_res) %>% 
  filter(resource <= int2) %>% 
  select(CaseID,ResourceID,resource,int2,time_res) %>%
  mutate(util = resource/int2)


k <- data_trans %>% 
  mutate(resource = lag(resource,n = 1, default = 0)) %>% 
  filter(ResourceID == 1) %>%
  arrange(time_res) %>% 
  filter(resource > int2) %>%
  select(CaseID,ResourceID,resource,int2,time_res)




##### Funzione salvataggio immagine con ggplot (multiattività in funzione di CaseID) #####
MULTI_IMAGE_SAVE_GG_CaseID_2 <- function(data_list, p){
  
  for(i in 1:length(data_list)){
    data_list[[i]] <- arrange(data_list[[i]], CaseID)
    ggplot_buffer <- ggplot(data_list[[i]], aes(x = CaseID, y = Waiting))+
      geom_point()
    
    ggplot_resource <- ggplot(data_list[[i]], aes(x = CaseID, y = Processing))+
      geom_point()
    
    IMG_SAVE_GG(paste(p,"\\BUFF_case_buff-",i,".png",sep = ""),
                ggplot_buffer)
    IMG_SAVE_GG(paste(p,"\\RES_case_res-",i,".png",sep = ""),
                ggplot_resource)
  }
}
##### Funzione salvataggio immagine con ggplot (int in funzione di time) #####
MULTI_IMAGE_SAVE_GG_int_2 <- function(data_list,p){
  
  for(i in 1:length(data_list)){
    data_list[[i]] <- data_list[[i]][order(data_list[[i]]$Time1),]
    ggplot_buffer <- ggplot(data_list[[i]], aes(x = Time1, y = Diff1))+
      geom_point()
    
    data_list[[i]] <- data_list[[i]][order(data_list[[i]]$Time2),]
    ggplot_resource <- ggplot(data_list[[i]], aes(x = Time2, y = Diff2))+
      geom_point()
    
    data_list[[i]] <- data_list[[i]][order(data_list[[i]]$Time1),]
    ggplot_elapsed_time <- ggplot(data_list[[i]], aes(x = Time1, y = Diff3))+
      geom_point()
    
    IMG_SAVE_GG(paste(p,"\\BUFF_timeBuff_int1-",i,".png",sep = ""),
                ggplot_buffer)
    IMG_SAVE_GG(paste(p,"\\RES_timeRes_int2-",i,".png",sep = ""),
                ggplot_resource)
    IMG_SAVE_GG(paste(p,"\\ET_timeBuff_int3-",i,".png",sep = ""),
                ggplot_elapsed_time)
  }
}
##### Funzione salvataggio immagine con ggplot (int4 in funzione di time_buff e CaseID) #####
MULTI_IMAGE_SAVE_GG_int4_2 <- function(data_list,p){
  
  for(i in 1:length(data_list)){
    
    data_list[[i]] <- data_list[[i]][order(data_list[[i]]$CaseID),]
    ggplot_buffer2 <- ggplot(data_list[[i]], aes(x = CaseID, y = Blocking))+
      geom_point()

    IMG_SAVE_GG(paste(p,"\\BUFF_case_int4-",i,".png", sep = ""),
                ggplot_buffer2)
  }
}


##### Codice prototipo #####

data_in <- read.csv(file = input_path, header = F, sep = " ", dec = ".")
data_in$V6 <- NULL
colnames(data_in) <- c("CaseID", "EventID", "Activity", "Resource", "Time")

# data_list_test <- data_in %>%
#   group_by(Activity, CaseID) %>%
#   mutate(Position = order(Time)) %>% 
#   mutate(Resource = replace(Resource, Resource == 0, max(Resource))) %>%
#   group_by(Activity) %>%
#   mutate(EventID = NULL) %>%
#   pivot_wider(names_from = Position, values_from = Time, names_prefix = "Time") %>%
#   arrange(Time2) %>%
#   mutate(CaseID_succ = lead(CaseID),
#          Time3_succ = lead(Time2)) %>%
#   arrange(Activity, Resource, CaseID) %>%
#   filter(CaseID_succ < CaseID)


data_list_test <- data_in %>%
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
  group_by(Activity, Resource) %>%
  mutate(Diff2_shift = lead(Diff2)) %>%
  filter(!is.na(Diff2_shift)) %>%
  group_by(CaseID) %>%
  arrange(CaseID,Time1) %>%
  mutate(TimeNext = lead(Time1, n = 1, default = max(Time3))) %>%
  mutate(Blocking = TimeNext - Time3) %>%
  # filter(Processing > Diff2_shift) %>% 
  group_by(Activity) %>%
  group_split()



library(tsibble)

a <- data_list_test[[3]]
a %<>% filter(Resource == 1)
slide(rep_len(1,10), sum,.size = 3,.step = 2)



# MULTI_IMAGE_SAVE_GG_CaseID_2(a, p = file.path("C:","Users","edo_c","Desktop","Test_plots"))
# 
# MULTI_IMAGE_SAVE_GG_int_2(a, p = file.path("C:","Users","edo_c","Desktop","Test_plots"))
# 
# MULTI_IMAGE_SAVE_GG_int4_2(a, p = file.path("C:","Users","edo_c","Desktop","Test_plots"))

b <- a %>% 
  lapply(lapply_ResourceID) %>%
  lapply(function(x) {return(lapply(x,FAUR))})




  



  
