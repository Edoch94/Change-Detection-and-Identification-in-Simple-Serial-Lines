##### Funzione salvataggio immagine con ggplot (singola attività) #####
IMG_SAVE_GG <- function(plot_name,plot_to_save,DPI=100){
  ggsave(filename = plot_name, 
         plot = plot_to_save, 
         device = "png",
         scale = 1, width = 30, height = 15, units = "cm", 
         dpi = DPI, limitsize = TRUE)
}

##### Funzione salvataggio immagine con ggplot (multiattività in funzione di CaseID) #####
MULTI_IMAGE_SAVE_GG_CaseID <- function(data_list, p){
  
  for(i in 1:length(data_list)){
    data_list[[i]] <- arrange(data_list[[i]], CaseID)
    ggplot_buffer <- ggplot(data_list[[i]], aes(x = CaseID, y = buffer))+
      geom_point()
    
    ggplot_resource <- ggplot(data_list[[i]], aes(x = CaseID, y = resource))+
      geom_point()
    
    IMG_SAVE_GG(paste(p,"\\InternalTime_on_caseID","\\BUFF_case_buff-",i,".png",sep = ""),
                ggplot_buffer)
    IMG_SAVE_GG(paste(p,"\\InternalTime_on_caseID","\\RES_case_res-",i,".png",sep = ""),
                ggplot_resource)
  }
}

##### Funzione salvataggio immagine con ggplot (multiattività in funzione di time_buff o time_res) #####
MULTI_IMAGE_SAVE_GG_Time <- function(data_list,p){
  
  for(i in 1:length(data_list)){
    data_list[[i]] <- data_list[[i]][order(data_list[[i]]$time_buff),]
    ggplot_buffer <- ggplot(data_list[[i]], aes(x = time_buff, y = buffer))+
      geom_point()
    
    data_list[[i]] <- data_list[[i]][order(data_list[[i]]$time_res),]
    ggplot_resource <- ggplot(data_list[[i]], aes(x = time_res, y = resource))+
      geom_point()
    
    IMG_SAVE_GG(paste(p,"\\InternalTime_on_timestamp","\\BUFF_timeBuff_buff-",i,".png",sep = ""),
                ggplot_buffer)
    IMG_SAVE_GG(paste(p,"\\InternalTime_on_timestamp","\\RES_timeRes_res-",i,".png",sep = ""),
                ggplot_resource)
  }
}

##### Funzione salvataggio immagine con ggplot (int in funzione di time) #####
MULTI_IMAGE_SAVE_GG_int <- function(data_list,p){
  
  for(i in 1:length(data_list)){
    data_list[[i]] <- data_list[[i]][order(data_list[[i]]$time_buff),]
    ggplot_buffer <- ggplot(data_list[[i]], aes(x = time_buff, y = int1))+
      geom_point()
    
    data_list[[i]] <- data_list[[i]][order(data_list[[i]]$time_res),]
    ggplot_resource <- ggplot(data_list[[i]], aes(x = time_res, y = int2))+
      geom_point()
    
    data_list[[i]] <- data_list[[i]][order(data_list[[i]]$time_buff),]
    ggplot_elapsed_time <- ggplot(data_list[[i]], aes(x = time_buff, y = int3))+
      geom_point()
    
    IMG_SAVE_GG(paste(p,"\\InterarrivalTime_on_timestamp","\\BUFF_timeBuff_int1-",i,".png",sep = ""),
                ggplot_buffer)
    IMG_SAVE_GG(paste(p,"\\InterarrivalTime_on_timestamp","\\RES_timeRes_int2-",i,".png",sep = ""),
                ggplot_resource)
    IMG_SAVE_GG(paste(p,"\\InterarrivalTime_on_timestamp","\\ET_timeBuff_int3-",i,".png",sep = ""),
                ggplot_elapsed_time)
  }
}

##### Funzione salvataggio immagine con ggplot (int4 in funzione di time_buff e CaseID) #####
MULTI_IMAGE_SAVE_GG_int4 <- function(data_list,p){
  
  for(i in 1:length(data_list)){
    data_list[[i]] <- data_list[[i]][order(data_list[[i]]$time_buff),]
    ggplot_buffer1 <- ggplot(data_list[[i]], aes(x = time_buff, y = int4))+
      geom_point()
    
    data_list[[i]] <- data_list[[i]][order(data_list[[i]]$CaseID),]
    ggplot_buffer2 <- ggplot(data_list[[i]], aes(x = CaseID, y = int4))+
      geom_point()
    
    IMG_SAVE_GG(paste(p,"\\TransferTime","\\BUFF_timeBuff_int4-",i,".png", sep = ""),
                ggplot_buffer1)
    IMG_SAVE_GG(paste(p,"\\TransferTime","\\BUFF_case_int4-",i,".png", sep = ""),
                ggplot_buffer2)
  }
}

##### Funzione salvataggio immagine con ggplot (indici) #####
MULTI_IMAGE_SAVE_GG_indice <- function(data_list, p, stat_ind, tipo){
  
  for(i in 1:length(data_list)){
    for(j in 15:21) { #riscrivere limiti ciclo: se cambia ordine o aumenta numero di colonne l'algoritmo si rompe
      data_list[[i]] <- data_list[[i]][order(data_list[[i]]$CaseID),]
      ggplot_index <- ggplot(data_list[[i]], aes_string(x = "CaseID", 
                                                        y = names(data_list[[i]][j])))+
        geom_point()
      
      save_path <- file.path(p,
                             stat_ind,
                             tipo,
                             gsub("^.*?_","",colnames(data_list[[i]][j])),
                             paste(colnames(data_list[[i]][j]),
                                   "-",
                                   i,
                                   ".png",
                                   sep = ""))
      IMG_SAVE_GG(save_path,
                  ggplot_index,
                  DPI = 100)
    }
  }
}

##### Funzione salvataggio immagine con ggplot (Utilizzo) #####
MULTI_IMAGE_SAVE_GG_utilizzo <- function(data_list, p, ampiezza = ampiezza_qta, distanza = distanza_qta){
  
  for(i in 1:length(data_list)){
    for(j in 1:length(data_list[[i]])){
      ggplot_util <- ggplot(data_list[[i]][[j]], aes(x = CaseID, y = util))+
        geom_point()+
        expand_limits(x = c(0,1), y = c(0,1))
      
      save_path <- file.path(p, paste("Util_A",i,"_R",j,".png", sep = ""))
      
      IMG_SAVE_GG(save_path,
                  ggplot_util,
                  DPI = 100)
    }
  }
}






