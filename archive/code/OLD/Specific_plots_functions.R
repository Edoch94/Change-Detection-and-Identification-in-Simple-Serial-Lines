# output_single_plots_path <- file.path(general_path,"PLOTS","Single_plots")

cosidered_data_list <- data_out %>%
  filter(Activity == 4)

# considered_act <- 1

data_plot <- cosidered_data_list
# data_plot <- subset(cosidered_data_list[[considered_act]], ResourceID == 1)
# data_plot$ResourceID <- as.factor(data_plot$ResourceID)

plot_1 <- ggplot(data_plot, 
                 aes(x = CaseID))+
  geom_point(aes(y = Waiting), size = 0.5)+
  # geom_vline(xintercept = c(5000,7500,10000,12500),linetype = "dotted")+
  expand_limits(x = 0, y = 0)+
  theme(legend.position = "none",
        axis.title = element_blank())+
  labs(x = "CaseID",
       y = "Blocking_time")

# , aes(colour = factor(ResourceID))
plot_1

ggsave(filename = "Blocking_unstable_BeforeChangepoint_log78.png",
       path = output_single_plots_path,
       plot = plot_1,
       device = "png",
       scale = 1, width = 10, height = 10, units = "cm", 
       dpi = 500, limitsize = TRUE)

##### Plot differenze di consecutive cases intervals #####
cosidered_data_list <- data_out_stat_ind_Superlist
considered_metric <- 1
considered_act <- 6

data_plot <- cosidered_data_list[[considered_metric]][[considered_act]]

plot_1 <- ggplot(filter(data_plot), 
                 aes(x = CaseID))+
  geom_point(aes(y = media_int2 - media_int1), size = 0.5)+
  geom_vline(xintercept = c(5000,7500,10000,12500),linetype = "dotted")+
  expand_limits(x = 0, y = 0)+
  theme(legend.position = "none",
        axis.title = element_blank())+
  labs(x = "CaseID",
       y = "Waiting_time")

# , aes(colour = factor(ResourceID))
plot_1

ggsave(filename = "InOutDiff_stable_BN_log79.png",
       path = output_single_plots_path,
       plot = plot_1,
       device = "png",
       scale = 1, width = 10, height = 10, units = "cm", 
       dpi = 500, limitsize = TRUE)

