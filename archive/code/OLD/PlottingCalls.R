##### GGPLOT molteplici attività: single-log #####
MULTI_IMAGE_SAVE_GG_CaseID(data_out_list,output_path)

MULTI_IMAGE_SAVE_GG_Time(data_out_list,output_path)

MULTI_IMAGE_SAVE_GG_int(data_out_list,output_path)

MULTI_IMAGE_SAVE_GG_int4(data_out_list,output_path)

##### GGPLOT indici: single-log #####
# Calcolo dati e stampa plots
for(i in 1:length(data_out_stat_ind_Superlist_SUB)){
  MULTI_IMAGE_SAVE_GG_indice(data_out_stat_ind_Superlist_SUB[[i]],
                             index_path,
                             stat_ind = names(data_out_stat_ind_Superlist_SUB)[i],
                             tipo = "absolute")
}

# for(i in 1:length(data_out_stat_ind_perc_Superlist_SUB)){
#   MULTI_IMAGE_SAVE_GG_indice(data_out_stat_ind_perc_Superlist_SUB[[i]],
#                              index_path,
#                              stat_ind = names(data_out_stat_ind_perc_Superlist_SUB)[i],
#                              tipo = "relative")
# }

##### GGPLOT utilizzo: single-log #####
MULTI_IMAGE_SAVE_GG_utilizzo(data_out_util_list, output_util_path)



