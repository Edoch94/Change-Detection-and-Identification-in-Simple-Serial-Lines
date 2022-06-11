##### Funzione che restituisce l'indice della colonna di un dataframe avente un certo nome #####
FCM <- function(data_fr, nome_colonna){
  indice <- match(nome_colonna, colnames(data_fr))
  return(indice)
}

##### Funzione separazione attività #####
FSA <- function(data_input){
  # data_input$Activity <- as.factor(data_input$Activity)
  return(split(data_input, data_input$Activity))
}

##### Funzione calcolo intervalli di tempo #####
FCI <- function(data_struct){
  t_d <- diff(as.vector(data_struct$Time))
  return(as.data.frame(c(data_struct$CaseID[1], t_d, data_struct$Time[1:3])))
}

##### Funzione associatrice CaseID - Resource #####
FAR <- function(data_struct){
  t_d <- unique(data_struct$Resource)[-(1)]
  return(c(data_struct$CaseID[1], t_d))
}

##### Funzione calcolo int2 considerando ResourceID #####
FCI2 <- function(data_struct){
  if(nrow(data_struct)>1){
    data_struct <- arrange(data_struct, time_res)
    data_struct$int2[2:nrow(data_struct)] <- data_struct$time_res[2:nrow(data_struct)] - data_struct$time_res[1:(nrow(data_struct)-1)]
  }
  return(data_struct)
}

##### Funzione calcolo int3 considerando ResourceID #####
FCI3 <- function(data_struct){
  if(nrow(data_struct)>1){
    data_struct <- arrange(data_struct, time_end)
    data_struct$int3[2:nrow(data_struct)] <- data_struct$time_end[2:nrow(data_struct)] - data_struct$time_end[1:(nrow(data_struct)-1)]
  }
  return(data_struct)
}

##### Funzione ristrutturazione dati (singola attività) #####
FRD2 <- function(data_file){
  cases <- split(data_file, data_file$CaseID) #Separazione dati in base al caso e trasformazione in lista
  cases_res <- lapply(cases, FAR)
  # cases_res <- bind_rows(cases_res)
  cases_res <- data.frame(matrix(unlist(cases_res),ncol = 2, byrow = T))
  colnames(cases_res) <- c("CaseID", "ResourceID")
  
  cases_diff <- lapply(cases, FCI) #Applicazione funzione di calcolo intervalli di tempo
  data_diff <- as.data.frame(cases_diff) #Trasformazione della lista in dataframe
  colnames(data_diff) <- seq_len(ncol(data_diff)) #Numerazione casi
  data_trans <- data.table::transpose(data_diff) #Trasposizione dataframe
  colnames(data_trans) <- c("CaseID", "buffer", "resource", "time_buff", "time_res", "time_end") #Denominazione colonne
  
  data_trans <- merge(data_trans,cases_res, by = "CaseID")
  data_trans$ActivityID <- rep(unique(data_file$Activity),nrow(data_trans))
  
  data_trans$int1 <- rep_len(0,nrow(data_trans))
  # data_trans$int1test <- rep_len(0,nrow(data_trans))
  data_trans$int2 <- rep_len(0,nrow(data_trans))
  # data_trans$int2test <- rep_len(0,nrow(data_trans))
  data_trans$int3 <- rep_len(0,nrow(data_trans))
  # data_trans$int3test <- rep_len(0,nrow(data_trans))
  
  data_trans <- arrange(data_trans,time_buff)
  for (i in 2:nrow(data_trans)) {
    data_trans$int1[i] <- data_trans$time_buff[i]-data_trans$time_buff[i-1]
  }
  data_trans$int1[2:nrow(data_trans)] <- data_trans$time_buff[2:nrow(data_trans)] - data_trans$time_buff[1:(nrow(data_trans)-1)]
  
  # data_trans <- arrange(data_trans,time_res)
  # for (i in 2:nrow(data_trans)) {
  #   data_trans$int2[i] <- data_trans$time_res[i]-data_trans$time_res[i-1]
  # }
  
  data_trans_split <- split(data_trans, data_trans$ResourceID)
  data_trans_split <- lapply(data_trans_split, FCI2)
  data_trans_split <- bind_rows(data_trans_split)
  data_trans$int2 <- data_trans_split$int2
  
  # data_trans <- arrange(data_trans,time_end)
  # for (i in 2:nrow(data_trans)) {
  #   data_trans$int3[i] <- data_trans$time_end[i]-data_trans$time_end[i-1]
  # }
  
  data_trans_split <- split(data_trans, data_trans$ResourceID)
  data_trans_split <- lapply(data_trans_split, FCI3)
  data_trans_split <- bind_rows(data_trans_split)
  data_trans$int3 <- data_trans_split$int3
  
  data_trans <- arrange(data_trans,CaseID)
  return(data_trans)
}

FRD <- function(data_file){
  cases <- split(data_file, data_file$CaseID) #Separazione dati in base al caso e trasformazione in lista
  cases_diff <- lapply(cases, FCI) #Applicazione funzione di calcolo intervalli di tempo
  data_diff <- as.data.frame(cases_diff) #Trasformazione della lista in dataframe
  colnames(data_diff) <- seq_len(ncol(data_diff)) #Numerazione casi
  data_trans <- transpose(data_diff) #Trasposizione dataframe
  colnames(data_trans) <- c("CaseID", "buffer", "resource", "time_buff", "time_res", "time_end") #Denominazione colonne
  data_trans$int1 <- rep_len(0,nrow(data_trans))
  data_trans$int2 <- rep_len(0,nrow(data_trans))
  data_trans$int3 <- rep_len(0,nrow(data_trans))
  
  data_trans$ActivityID <- rep(unique(data_file$Activity),nrow(data_trans))
  
  data_trans <- data_trans[order(data_trans$time_buff),]
  for (i in 2:nrow(data_trans)) {
    data_trans$int1[i] <- data_trans$time_buff[i]-data_trans$time_buff[i-1]
  }
  
  data_trans <- data_trans[order(data_trans$time_res),]
  for (i in 2:nrow(data_trans)) {
    data_trans$int2[i] <- data_trans$time_res[i]-data_trans$time_res[i-1]
  }
  
  data_trans <- data_trans[order(data_trans$time_end),]
  for (i in 2:nrow(data_trans)) {
    data_trans$int3[i] <- data_trans$time_end[i]-data_trans$time_end[i-1]
  }
  
  data_trans <- data_trans[order(data_trans$CaseID),]
  return(data_trans)
}

##### Funzione calcolo elapsed time #####
FCET <- function(data_list){
  data_list$elapsed_time <- data_list$buffer + data_list$resource
  return(data_list)
}

##### Funzione tempo attività precedente #####
FTAP <- function(data_list){
  for(i in 1:length(data_list)){
    data_list[[i]]$time_prev <- rep_len(0,nrow(data_list[[i]]))
    # data_list[[i]] <- data_list[[i]][order(data_list[[i]]$CaseID),]
    data_list[[i]] <- arrange(data_list[[i]],CaseID)
  }
  
  for(i in 2:length(data_list)){
    for(j in 1:nrow(data_list[[i]])){
      data_list[[i]]$time_prev[j] <- data_list[[i-1]]$time_end[j]
    }
  }
  
  return(data_list)
}

##### Funzione calcolo intervallo con fine attività precedente #####
FIAP <- function(data_input){
  data_input$int4 <- rep_len(0, nrow(data_input))
  
  data_input$int4 <- data_input$time_buff - data_input$time_prev
  
  return(data_input)
}

##### Funzione calcolo valore medio (buffer, resource, int1, int2, int3, int4) su subset dati #####
FCMSD <- function(data_list, job, lim_list){
  out_list <- list()
  length(out_list) <- 6
  names(out_list) <- c("buffer","resource","int1", "int2", "int3", "int4")
  
  for (i in 1:6) {
    out_list[[i]] <- as.data.frame(matrix(ncol = length(lim_list)))
  }
  
  for(i in 1:length(lim_list)){
    data_mat <- subset(data_list[[job]], 
                       data_list[[job]]$CaseID > lim_list[[i]][1] &
                         data_list[[job]]$CaseID < lim_list[[i]][2])
    
    out_list[[1]][i] <- mean(data_mat$buffer)
    out_list[[2]][i] <- mean(data_mat$resource)
    out_list[[3]][i] <- mean(data_mat$int1)
    out_list[[4]][i] <- mean(data_mat$int2)
    out_list[[5]][i] <- mean(data_mat$int3)
    out_list[[6]][i] <- mean(data_mat$int4)
  }
  
  return(out_list)
}

##### Funzione calcolo deviazione standard (int1, int2, int3, int4) su subset dati #####
FCDSD <- function(data_list, job, lim_list){
  out_list <- list()
  length(out_list) <- 4
  names(out_list) <- c("buffer","resource","int1", "int2", "int3", "int4")
  
  for (i in 1:4) {
    out_list[[i]] <- as.data.frame(matrix(ncol = length(lim_list)))
  }
  
  for(i in 1:length(lim_list)){
    data_mat <- subset(data_list[[job]], 
                       data_list[[job]]$CaseID > lim_list[[i]][1] &
                         data_list[[job]]$CaseID < lim_list[[i]][2])
    
    out_list[[1]][i] <- sd(data_mat$int1)
    out_list[[2]][i] <- sd(data_mat$int2)
    out_list[[3]][i] <- sd(data_mat$int3)
    out_list[[4]][i] <- sd(data_mat$int4)
  }
  
  return(out_list)
}

##### Funzioni che restituiscono indici di concentrazione e statistiche #####
# Funzione restituzione Indice di Atkinson
return_AtkinsonIndex <- function(data_col){
  ris <- calcAtkinson(data_col, epsilon = 1)
  if(is.null(ris)){
    ris <- 0
  } else {
    ris <- as.numeric(ris[[1]][1])
  }
  return(ris)
}

# Funzione restituzione Indice di Gini
return_GiniIndex <- function(data_col){
  ris <- calcSGini(data_col, param = 2)
  if(is.null(ris)){
    ris <- 0
  } else {
    ris <- as.numeric(ris[[1]][1])
  }
  return(ris)
}

# Funzione restituzione Entropia
return_Entropy <- function(data_col){
  ris <- calcGEI(data_col, alpha = 1)
  if(is.null(ris)){
    ris <- 0
  } else {
    ris <- as.numeric(ris[[1]][1])
  }
  return(ris)
}

# Funzione restituzione skewness
return_skewness <- function(data_col){
  ris <- skewness(data_col)
  if(is.nan(ris)){
    ris <- 0
  }
  return(ris)
}

# Funzione restituzione kurtosis
return_kurtosis <- function(data_col){
  ris <- kurtosis(data_col)
  if(is.nan(ris)){
    ris <- 0
  }
  return(ris)
}

##### Funzione Subset dati per escludere transitorio #####
FSIL <- function(data_fr, end_trans = 2500){
  data_out <- subset(data_fr,
                     subset = (CaseID >= end_trans))
  return(data_out)
}

##### Funzione calcolo statistiche o indici su finestre basate su CaseID (qta) #####
FCSIqta <- function(data_list, col_list, ampiezza, distanza, stat_ind = "media"){
  lista_indici <- list()
  for(i in 1:length(data_list)){
    sequenza <- seq(from = ampiezza,
                    to = nrow(data_list[[i]]), 
                    by = distanza)
    lista_indici[[i]] <- as.data.frame(matrix(ncol = length(col_list), nrow = nrow(data_list[[i]])))
    
    nomi_colonne <- switch(stat_ind,
                           "media" = rep("media", length(col_list)),
                           "varianza" = rep("varianza", length(col_list)),
                           "gini" = rep("gini", length(col_list)),
                           "atkinson" = rep("atkinson", length(col_list)),
                           "entropia" = rep("entropia", length(col_list)),
                           "skewness" = rep("skewness", length(col_list)),
                           "kurtosis" = rep("kurtosis", length(col_list)),
                           stop("Inserire nome statistica o indice corretto"))
    
    
    for(q in 1:length(col_list)){
      nomi_colonne[q] <- paste(nomi_colonne[q],col_list[q],sep = "_")
    }
    colnames(lista_indici[[i]]) <- nomi_colonne
    
    lista_indici[[i]] <- bind_cols(data_list[[i]],lista_indici[[i]])
    
    for(j in 1:length(sequenza)){
      data_sub <- subset(data_list[[i]], 
                         data_list[[i]]$CaseID > (sequenza[j] - ampiezza) &
                           data_list[[i]]$CaseID <= sequenza[j])
      
      data_sub <- select(data_sub, one_of(col_list))
      
      trasl_col <- ncol(lista_indici[[i]]) - ncol(data_sub)
      
      for(k in 1:ncol(data_sub)){
        # lista_indici[[i]][sequenza[j],k+trasl_col] <- return_GiniIndex(data_sub[,k])
        
        lista_indici[[i]][sequenza[j],k+trasl_col] <- 
          switch(stat_ind,
                 "media" = mean(data_sub[,k]),
                 "varianza" = var(data_sub[,k]),
                 "gini" = return_GiniIndex(data_sub[,k]),
                 "atkinson" = return_AtkinsonIndex(data_sub[,k]),
                 "entropia" = return_Entropy(data_sub[,k]),
                 "skewness" = return_skewness(data_sub[,k]),
                 "kurtosis" = return_kurtosis(data_sub[,k]),
                 stop("Inserire nome statistica o indice corretto"))
      }
    }
    lista_indici[[i]] <- slice(lista_indici[[i]], sequenza)
  }
  return(lista_indici)
}

##### Funzione calcolo variazione percentuale statistiche o indici su finestre basate su CaseID (qta)#####
FCVPSIqta <- function(data_fr, col_list = dependent_variables){
  data_frA <- data_fr[,(ncol(data_fr)-length(col_list)+1):ncol(data_fr)]
  data_frB <- rbind(rep(0), data_frA[1:(nrow(data_frA)-1),])
  data_frC <- (data_frA - data_frB)/ data_frB
  data_frC[1,] <- rep(0)
  
  data_fr[,(ncol(data_fr)-length(col_list)+1):ncol(data_fr)] <- data_frC
  data_fr <- as.matrix(data_fr)
  data_fr[!is.finite(data_fr)] <- 0 # Cercare metodo alternativo: in questo modo un passaggio dal 0 a valore diverso da 0 risulta come una variazione nulla
  data_fr <- as.data.frame(data_fr)
  
  return(data_fr)
}

##### Funzione calcolo media su finestre indicando sfasamento #####
# FCMW <- function(data_struct, ampiezza = ampiezza_qta, distanza = distanza_qta, sfasamento = F){
#   sequenza <- seq(from = ampiezza,
#                   to = length(data_struct), 
#                   by = distanza)
#   sequenza <- c(0,sequenza)
#   sequenza[length(sequenza)] <- sequenza[length(sequenza)] - 1
#   
#   if(sfasamento){
#     sequenza <- sequenza + 1
#   }
#   
#   data_struct_out <- numeric(length = length(sequenza) - 1)
#   
#   for (i in 2:length(sequenza)) {
#     data_struct_out[i-1] <- mean(data_struct[(sequenza[i-1] + 1):sequenza[i]])
#   }
#   
#   return(data_struct_out)
# }

FCMW <- function(data_struct, ampiezza = ampiezza_qta, distanza = distanza_qta, sfasamento = F){
  
  # if(nrow(data_struct) >= ampiezza){
  #   sequenza <- seq(from = ampiezza,
  #                   to = nrow(data_struct), 
  #                   by = distanza)
  #   sequenza <- c(0,sequenza)
  #   sequenza[length(sequenza)] <- sequenza[length(sequenza)] - 1
  #   
  # } else {
  #   sequenza <- c(0, nrow(data_struct)-1)
  # }
  
  sequenza <- FSqnz(length(data_struct),ampiezza,distanza)
  
  if(sfasamento){
    sequenza <- sequenza + 1
  }
  
  data_struct_out <- numeric(length = length(sequenza) - 1)
  
  for (i in 2:length(sequenza)) {
    data_struct_out[i-1] <- mean(data_struct[(sequenza[i-1] + 1):sequenza[i]])
  }
  
  return(data_struct_out)
}

##### Funzione che associa a CaseID l'utilizzo della risorsa (dataframe con attività e risorsa univoche) #####
# FAUR <- function(data_struct, ampiezza = ampiezza_qta, distanza = distanza_qta){
#   # Condizione 1: il dataframe deve essere composto almeno da 2 casi
#   if(nrow(data_struct)>1){
#     
#     sequenza <- seq(from = ampiezza,
#                     to = nrow(data_struct), 
#                     by = distanza)
#     sequenza <- c(0,sequenza)
#     sequenza[length(sequenza)] <- sequenza[length(sequenza)] - 1
#     
#     data_struct_out <- data_struct[sequenza,]
#     
#     data_struct_out$util <- FCMW(data_struct$resource,ampiezza,distanza,F)/FCMW(data_struct$int2,ampiezza,distanza,T)
#   } else {
#     data_struct_out <- data_struct
#     data_struct_out$util <- c(0)
#   }
#   return(data_struct_out)
# }

FAUR <- function(data_struct, ampiezza = ampiezza_qta, distanza = distanza_qta){
  # Condizione 1: il dataframe deve essere composto almeno da 2 casi
  if(nrow(data_struct)>1){
    
    # Condizione 2: per il calcolo della sequenza il dataframe deve essere composto da un numero di casi maggiore-uguale all'ampiezza della finestra
    # if(nrow(data_struct) >= ampiezza){
    #   sequenza <- seq(from = ampiezza,
    #                   to = nrow(data_struct), 
    #                   by = distanza)
    #   sequenza <- c(0,sequenza)
    #   sequenza[length(sequenza)] <- sequenza[length(sequenza)] - 1
    #   
    # } else {
    #   sequenza <- c(0, nrow(data_struct)-1)
    # }
    
    sequenza <- FSqnz(nrow(data_struct),ampiezza,distanza)
    
    data_struct_out <- data_struct[sequenza,]
    data_struct_out$mean_resource <- FCMW(data_struct$resource,ampiezza,distanza,F)
    data_struct_out$mean_int2 <- FCMW(data_struct$int2,ampiezza,distanza,T)
    data_struct_out$util <- FCMW(data_struct$resource,ampiezza,distanza,F)/FCMW(data_struct$int2,ampiezza,distanza,T)
  } else {
    data_struct_out <- data_struct
    data_struct_out$util <- c(0)
  }
  return(data_struct_out)
}

# Funzione split su ResourceID 
lapply_ResourceID <- function(data_struct){
  data_out <- split(data_struct, data_struct$ResourceID)
  return(data_out)
}

# Sequenziatore
FSqnz <- function(lng, ampiezza = ampiezza_qta, distanza = distanza_qta){
  if(lng >= ampiezza){
    sequenza <- seq(from = ampiezza,
                    to = lng, 
                    by = distanza)
    sequenza <- c(0,sequenza)
    sequenza[length(sequenza)] <- sequenza[length(sequenza)] - 1
    
  } else {
    sequenza <- c(0, lng-1)
  }
  return(sequenza)
}
