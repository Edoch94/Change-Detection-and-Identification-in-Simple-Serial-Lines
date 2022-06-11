output_single_plots_path <- file.path(general_path,"PLOTS","Single_plots")

cosidered_data_list <- data_out_list_util
considered_act <- 5

data_plot <- cosidered_data_list[[considered_act]]
# data_plot <- subset(cosidered_data_list[[considered_act]], ResourceID == 1)
data_plot$ResourceID <- as.factor(data_plot$ResourceID)

plot_util <- ggplot(data_plot, 
                    aes(x = CaseID))+
  # geom_point(size = 0.1, aes(y = media_int1), colour = "red")+
  geom_point(size = 0.1, aes(y = media_int2 - media_int1))+
  geom_vline(xintercept = c(5000,7500,10000,12500),
             linetype = "dotted")+
  expand_limits(x = 0, y = 0)+
  theme(legend.position = "none",
        axis.title = element_blank())+
  labs(x = "CaseID",
       y = "util")
plot_util
