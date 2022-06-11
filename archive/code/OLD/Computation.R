##### Data import #####
data_in <- read.csv(file = input_path, header = F, sep = " ", dec = ".")

if(ncol(data_in) == 5){
  data_in$V5 <- NULL
  colnames(data_in) <- c("CaseID", "EventID", "Activity", "Time")
  data_in$Resource <- rep(1,nrow(data_in))
}
if(ncol(data_in) == 6){
  data_in$V6 <- NULL
  colnames(data_in) <- c("CaseID", "EventID", "Activity", "Resource", "Time")
}

##### Case list construction #####
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

##### Case list standard #####
data_out_standard <- data_out %>%
  select(CaseID, Activity, Resource, Time1, Time2, Time3, Waiting, Processing, Blocking, Diff1, Diff2, Diff3) %>%
  rename(Timestamp_buff = Time1,
         Timestamp_res = Time2,
         Timestamp_end = Time3,
         Waiting_time = Waiting,
         Process_time= Processing,
         Blocking_time = Blocking,
         Input_diff = Diff1,
         Mid_diff = Diff2,
         Output_diff = Diff3)

##### Windows #####
data_out_windows <- data_out



