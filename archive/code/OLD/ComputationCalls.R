##### Prototipo codice modificato #####
# data_list_test <- data_in %>%
#   group_by(Activity, CaseID) %>%
#   mutate(Position = order(Time)) %>%
#   mutate(Resource = replace(Resource, Resource == 0, max(Resource))) %>%
#   group_by(Activity) %>%
#   mutate(EventID = NULL) %>%
#   pivot_wider(names_from = Position, values_from = Time, names_prefix = "Time") %>%
#   mutate(Waiting = Time2 - Time1,
#          Processing = Time3 - Time2) %>%
#   mutate(Diff1 = Time1 - lag(Time1, n = 1)) %>%
#   mutate(Diff1 = coalesce(Diff1, 0)) %>%
#   group_by(Activity, Resource) %>%
#   mutate(Diff2 = Time2 - lag(Time2, n = 1),
#          Diff3 = Time3 - lag(Time3, n = 1)) %>%
#   mutate(Diff2 = coalesce(Diff2, 0),
#          Diff3 = coalesce(Diff3, 0)) %>%
#   arrange(CaseID,Time1) %>%
#   group_by(CaseID) %>%
#   mutate(TimeNext = lead(Time1, n = 1, default = max(Time3))) %>%
#   mutate(Blocking = TimeNext - Time3)

##### Caricamento dati in memoria #####
data_in <- read.csv(file = input_path, header = F, sep = " ", dec = ".")

if(ncol(data_in) == 5){
  data_in$V5 <- NULL
  colnames(data_in) <- c("CaseID", "EventID", "Activity", "Time")
}
if(ncol(data_in) == 6){
  data_in$V6 <- NULL
  colnames(data_in) <- c("CaseID", "EventID", "Activity", "Resource", "Time")
}

##### Trattamento dati senza considerare Resource (Inserimento valore di default) ######
if(ncol(data_in) == 4){
  data_in$Resource <- rep(1,nrow(data_in))
}

##### Ristrutturazione dati #####
data_out_list  <- data_in 

# Colonne categoriche
data_out_list$Activity <- as.factor(data_out_list$Activity)
data_out_list$Resource <- as.factor(data_out_list$Resource)

data_out_list <- FSA(data_in)

data_out_list <- lapply(data_out_list, FRD2)

data_out_list <- lapply(data_out_list, FCET)

data_out_list <- FTAP(data_out_list)

data_out_list <- lapply(data_out_list, FIAP)

##### Calcolo indici #####
data_out_stat_ind_Superlist <- list()
data_out_stat_ind_Superlist_SUB <- list()

for(i in 1:length(stat_ind_list)){
  data_out_stat_ind_Superlist[[i]] <- FCSIqta(data_out_list,
                                              dependent_variables,
                                              ampiezza_qta,
                                              distanza_qta,
                                              stat_ind = stat_ind_list[i])
  data_out_stat_ind_Superlist_SUB[[i]] <- lapply(data_out_stat_ind_Superlist[[i]], FSIL)
}

names(data_out_stat_ind_Superlist) <- stat_ind_list
names(data_out_stat_ind_Superlist_SUB) <- stat_ind_list

##### Calcolo utilizzo #####
data_out_util_list <- data_out_list %>% 
  lapply(lapply_ResourceID) %>%
  lapply(function(x) {return(lapply(x,FAUR))})






