rm(list = ls())

library(tidyverse)
library(ggplot2)
library(magrittr)
library(grid)
library(slider)
# library(latex2exp)
# library(ggpubr)

##### WD #####
if(Sys.info()["user"] == "Edoardo pc"){
  general_path <- file.path("D:","OneDrive","Tesi","Project")
} 
if(Sys.info()["user"] == "edoch") {
  general_path <- file.path("~","Documents","Polito","Project")
} 
if(Sys.info()["user"] == "edo_c") {
  general_path <- file.path("C:","Users","edo_c","OneDrive","Tesi","Project")
}
dir_path <- file.path(general_path)
# setwd(dir = dir_path)

plot_out_path <- file.path(general_path,"Plot_single")

##### Modello #####
# ProcessingIncrease8_Starving_time_MEAN
model_list <- list (
  list(
    "Model375_n10000_inter10_chv5000_rep30_w100f100"
    ,"Model366_n10000_inter10_chv5000_rep30_w100f100"
    ,"Model367_n10000_inter10_chv5000_rep30_w100f100"
  ),
  list(
    "Model375_n10000_inter10_chv5000_rep30_w100f100"
    ,"Model366_n10000_inter10_chv5000_rep30_w100f100"
    ,"Model367_n10000_inter10_chv5000_rep30_w100f100"
  )
)

##### Percorso e file dati #####
data_in_list <- list()
for(i in 1:length(model_list)){
  data_in_list[[i]] <- list()
  for(j in 1:length(model_list[[i]])){
    data_in_list[[i]][[j]] <- read.csv(
      file.path(general_path,"Data_out",
                model_list[[i]][[j]],
                "data_windows.csv")
    )
  }
}

data_in <- data_in_list %>%
  map(.%>% bind_rows(.id = "model_in")) %>%
  bind_rows(.id = "model_out") %>%
  rename(Average_Queue = Queue_MEAN)

# data_queue_trend <- read.csv(file.path(general_path,"Data_out","Model363_n10000_inter10_chv5000_rep30_w100f100","data_queue_trend.csv"))
# data_queue_trend %<>% 
#   mutate(Queue_Cum = as.numeric(Queue_Cum)) %>%
#   group_by(CaseID, Activity) %>%
#   summarise(Queue_Cum = mean(Queue_Cum)) %>%
#   group_by(Activity) %>%
#   arrange(CaseID) %>%
#   mutate(Job_Count = slide_dbl(Queue_Cum,
#                                .f = mean, 
#                                .before = 9, 
#                                .step = 10,
#                                .complete = T)) %>%
#   filter(!is.na(Job_Count))
# 
# data_in <- right_join(data_in, data_queue_trend, by = c("CaseID", "Activity"), suffix = c("",""))


##### Costruzione dati multi_plot multi_field (STESSO DATASET) #####
# fields <- list(
#   quo(Input_diff_MEAN)
# )

# fields <- list(
#   quo(Mid_diff_MEAN)
# )

# fields <- list(
#   quo(Waiting_time_MEAN)
# )

# fields <- list(
#   quo(Waiting_time_VAR)
# )

# fields <- list(
#   quo(Average_Queue)
# )

# fields <- list(
#   quo(Processing_time_MEAN)
# )

# fields <- list(
#   quo(Utilization)
# )

# fields <- list(
#   quo(Blocking_time_MEAN)
# )

fields <- list(
  quo(Starving_time_MEAN)
)

# fields <- list(
#   quo(Blocking_prob)
# )

fields <- list(
  quo(Starving_prob)
)

# fields <- list(
#   quo(Blocking_time_MEAN),
#   quo(Starving_time_MEAN)
# )

# fields <- list(
#   quo(Blocking_prob),
#   quo(Starving_prob)
# )

# fields <- list(
#   quo(Processing_trend_MEAN)
# )

# fields <- list(
#   quo(Starving_time_MEAN),
#   quo(Starving_prob)
# )

fields <- list(
  quo(Waiting_time_MEAN),
  quo(Average_Queue)
)



# data_in %<>% filter(Waiting_time_VAR < 25)

data_in %<>%
  mutate(across(-c(model_in, model_out, CaseID, Activity),as.numeric))%>%
  select(model_in, model_out, CaseID, Activity, !!!fields) %>%
  pivot_longer(cols = c(!!!fields), names_to = "fld_nm", values_to = "int_prob") %>%
  mutate(fld_nm = as_factor(fld_nm))

# data_in %<>%
#   mutate(across(-c(model_in, model_out, CaseID, Activity),as.numeric))%>%
#   select(model_in, model_out, CaseID, Activity, !!!fields) %>%
#   mutate(Blocking_prop = Blocking_time_MEAN / (Blocking_time_MEAN + Starving_time_MEAN),
#          Starving_prop = Starving_time_MEAN / (Blocking_time_MEAN + Starving_time_MEAN))
# fields <- list(
#   quo(Blocking_prop),
#   quo(Starving_prop)
# )
# data_in %<>%
#   pivot_longer(cols = c(!!!fields), names_to = "fld_nm", values_to = "int_prob") %>%
#   mutate(fld_nm = as_factor(fld_nm))

# data_in %<>% 
#   mutate(across(-c(model_in, model_out, CaseID, Activity),as.numeric))%>%
#   select(model_in, model_out, CaseID, Activity, !!!fields) %>%
#   mutate(I_field = Blocking_time_MEAN+Starving_time_MEAN)  %>%
#   mutate(I_field_Block = Blocking_time_MEAN/I_field)  %>%
#   mutate(I_field_Starv = Starving_time_MEAN/I_field)  %>%
#   pivot_longer(cols = c(I_field_Block,I_field_Starv), names_to = "fld_nm", values_to = "int_prob") %>%
#   mutate(fld_nm = as_factor(fld_nm))

##### Stampa data_in #####
data_in %<>% filter(Activity %in% 2:4) %>%
  mutate(Activity = as.character(Activity)) %>%
  mutate(Activity = case_when(Activity == "2" ~ "A~monte",
                              Activity == "3" ~ "In~corrispondenza",
                              Activity == "4" ~ "A~valle",
                              T ~ model_in)
  ) %>%
  mutate(Activity = as_factor(Activity))

# data_in %<>% filter(CaseID < 5000)

# data_in %<>% filter((model_out == 1 & fld_nm == "Blocking_time_MEAN")|
#                       (model_out == 2 & fld_nm == "Starving_time_MEAN"))

# data_in %<>% filter((model_out == 1 & fld_nm == "Starving_time_MEAN")|
#                       (model_out == 2 & fld_nm == "Starving_prob"))

data_in %<>% filter((model_out == 1 & fld_nm == "Waiting_time_MEAN")|
                      (model_out == 2 & fld_nm == "Average_Queue"))

data_in %<>%
  mutate(model_out = as.numeric(model_out)) %>%
  mutate(model_out = case_when(model_out == 1 ~ "Waiting_time"
                               ,model_out == 2 ~ "Average_Queue")
  ) %>%
  mutate(model_out = as_factor(model_out))

# data_in %<>% filter((model_out == 1 & fld_nm == "Blocking_prop")|
#                       (model_out == 2 & fld_nm == "Starving_prop"))

# legend_labels <- list(expression(mu*second[s[CH]]==11),
#                       expression(mu*second[s[CH]]==12.5))

# legend_labels <- list(expression(mu*second[s[CH]]==7.5),
#                       expression(mu*second[s[CH]]==6),
#                       expression(mu*second[s[CH]]==4.5))

# legend_labels <- list(expression(mu*second[s[CH]]==8),
#                       expression(mu*second[s[CH]]==9.5),
#                       expression(mu*second[s[CH]]==11))

# legend_labels <- list(expression(mu*second[s[CH]]==6),
#                       expression(mu*second[s[CH]]==8),
#                       expression(mu*second[s[CH]]==11))

# legend_labels <- list(expression(mu*second[s[CH]]==4.5),
#                       expression(mu*second[s[CH]]==9.5),
#                       expression(mu*second[s[CH]]==11))

# legend_labels <- list(expression(mu*second[s[CH]]==6.5),
#                       expression(mu*second[s[CH]]==7.5),
#                       expression(mu*second[s[CH]]==8.5),
#                       expression(mu*second[s[CH]]==9.5))

# legend_labels <- list(expression(cl[s]==3),
#                       expression(cl[s]==6),
#                       expression(cl[s]==10))

# legend_labels <- list(expression(cl[s]==1), 
#                       expression(cl[s]==3),
#                       expression(cl[s]==10),
#                       expression(cl[s]==20))

legend_labels <- list(expression("Riduzione"), 
                      expression("Aumento"),
                      expression("Instabile"))

# legend_labels <- list(expression(cl*second[s] == 10),
#                       expression(cl*second[s] == 30))

# legend_labels <- list(expression(cl[s]==3),
#                       expression(cl[s]==6),
#                       expression(cl[s]==10),
#                       expression(cl[s]==infinite))

# legend_labels <- list(bquote(cl*second[s[CH]] == 6),
#                       bquote(cl*second[s[CH]] == 3),
#                       bquote(cl*second[s[CH]] == 1))

# legend_labels <- list(bquote(cl*second[s[CH]] == 6),
#                       bquote(cl*second[s[CH]] == 10),
#                       bquote(cl*second[s[CH]] == 20))

# legend_labels <- list(bquote(cl*minute[s[CH]] == 3),
#                       bquote(cl*minute[s[CH]] == 6),
#                       bquote(cl*minute[s[CH]] == 10))

# legend_labels <- list(expression(cl[3]==300),
#                       expression(cl[3]==600),
#                       expression(cl[3]==900))

# data_in %<>%
#   mutate(model_in = as.numeric(model_in)) %>%
#   mutate(model_in = case_when(model_out == 2 & model_in == 1 ~ 2,
#                               model_out == 2 & model_in == 2 ~ 3,
#                               model_out == 3 & model_in == 1 ~ 3,
#                               T ~ model_in)
#   ) %>%
#   mutate(model_in = as_factor(model_in))

# data_in %<>%
#   mutate(model_in = as.numeric(model_in)) %>%
#   mutate(model_in = case_when(model_out == 2 & model_in == 1 ~ 2,
#                               model_out == 2 & model_in == 2 ~ 3,
#                               T ~ model_in)
#   ) %>%
#   mutate(model_in = as_factor(model_in))

# data_in %<>%
#   mutate(model_in = as.character(model_in)) %>%
#   mutate(model_in = case_when(fld_nm == "Average_Queue" ~ "Average_Queue",
#                               fld_nm == "Job_Count" ~ "Job_Count",
#                               T ~ model_in)
#   ) %>%
#   mutate(model_in = as_factor(model_in))

# data_in %<>%
#   mutate(model_out = as.character(model_out)) %>%
#   mutate(model_out = case_when(model_out == 1 ~ "(list(s[BN],s[CH]))==(list(5,3))"
#                                ,model_out == 2 ~ "(list(s[BN],s[CH]))==(list(4,4))"
#                                ,model_out == 3 ~ "(list(s[BN],s[CH]))==(list(3,5))"
#   )
#   ) %>%
#   mutate(model_out = as_factor(model_out))

# data_in %<>%
#   mutate(model_out = as.character(model_out)) %>%
#   mutate(model_out = case_when(model_out == 1 ~ "(list(s[BN],s[CH]))==(list(5,3))"
#                                ,model_out == 2 ~ "(list(s[BN],s[CH]))==(list(4,4))"
#                                ,model_out == 3 ~ "(list(s[BN],s[CH]))==(list(3,5))"
#   )
#   ) %>%
#   mutate(model_out = as_factor(model_out))

# data_in %<>%
#   mutate(model_out = as.character(model_out)) %>%
#   mutate(model_out = case_when(model_out == 1 ~ "cl*second[s] == 3"
#                                ,model_out == 2 ~ "cl*second[s] == 6"
#                                ,model_out == 3 ~ "cl*second[s] == 10"
#   )
#   ) %>%
#   mutate(model_out = as_factor(model_out))

# data_in %<>%
#   mutate(model_out = as.character(model_out)) %>%
#   mutate(model_out = case_when(model_out == 1 ~ "mu*second[s[CH]]==8"
#                                ,model_out == 2 ~ "mu*second[s[CH]]==9.5"
#                                ,model_out == 3 ~ "mu*second[s[CH]]==11"
#   )
#   ) %>%
#   mutate(model_out = as_factor(model_out))

# data_in %<>%
#   mutate(model_out = as.character(model_out)) %>%
#   mutate(model_out = case_when(model_out == 1 ~ "Blocking_time"
#                                ,model_out == 2 ~ "Starving_time"
#   )
#   ) %>%
#   mutate(model_out = as_factor(model_out))

# data_in %<>%
#   mutate(model_out = as.character(model_out)) %>%
#   mutate(model_out = case_when(model_out == 1 ~ "cl*second[s] == 30"
#                                ,model_out == 2 ~ "cl*second[s] == 10"
#   )
#   ) %>%
#   mutate(model_out = as_factor(model_out))

# data_in %<>%
#   mutate(model_out = as.numeric(model_out)) %>%
#   mutate(model_out = case_when(model_out == 1 ~ "PT increase"
#                                ,model_out == 2 ~ "PT decrease")
#   ) %>%
#   mutate(model_out = as_factor(model_out))

# data_in %<>%
#   mutate(model_out = as.numeric(model_out)) %>%
#   mutate(model_out = "(list(s[BN],s[CH]))==(list(5,3))") %>%
#   mutate(model_out = as_factor(model_out))

# data_in %<>%
#   mutate(model_out = as.numeric(model_out)) %>%
#   mutate(model_out = "cl[s]==infinity") %>%
#   mutate(model_out = as_factor(model_out))

# data_in %<>%
#   mutate(model_out = as.numeric(model_out)) %>%
#   mutate(model_out = "Waiting_time_MEAN") %>%
#   mutate(model_out = as_factor(model_out))

multi_plot_multi_field <- ggplot(data_in, aes(x = CaseID, y = int_prob)) +
  geom_point(size = 1, aes(colour = factor(model_in)), show.legend = T) +
  # geom_hline(data = data.frame(yint = 10, fld_nm = "Processing_time_MEAN"), aes(yintercept=yint), linetype="dashed") + # attivare per Processing_time_MEAN
  # geom_hline(yintercept=10, linetype="dashed") + # attivare per Diff
  scale_color_brewer(type = "qual", palette = "Set1"
                     ,labels = legend_labels
  ) +
  # labs(subtitle = expression(Model~with~cl[s]==6)) +
  facet_grid(model_out~Activity, switch = "y",
             # scales = "fixed",
             scales = "free_y",
             labeller = label_parsed) +
  scale_x_continuous(breaks=c(1, 5000, 10000), labels = c(expression(1), expression(5%*%10^{"3"}), expression(10^{"4"}))) +
  # scale_y_continuous(trans = "log10") +
  expand_limits(x = 0, y = 0) + # attivare per time
  # expand_limits(x = 0, y = c(0,1)) + # attivare per utilization
  # expand_limits(y = 1) + # attivare per trend
  # ylim(0,10) + # attivare per time con limitazione superiore
  guides(colour = guide_legend(override.aes = list(size=10))) +
  theme(text = element_text(size = 13),
        strip.placement = "outside",
        plot.title = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(vjust=0),
        panel.spacing.y = unit(0.7, "cm"),
        panel.spacing.x = unit(0.3, "cm"),
        legend.position = "left",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_rect(fill = "transparent", colour = "transparent")
  )


# facet_wrap(vars(Activity), 
#            # switch = "y",
#            # scales = "fixed",
#            # scales = "free_y",
#            labeller = label_parsed) +


# last_plot()

# multi_plot_multi_field_grid <- ggplot_gtable(ggplot_build(multi_plot_multi_field))

plot_subtitle <- paste0(map_chr(fields,quo_name),collapse = "_")

plot_name <- paste0("Presentazione14",
                    "_",plot_subtitle,
                    ".png")

ggsave(filename = file.path(plot_out_path, plot_name),
       plot = multi_plot_multi_field,
       device = "png",
       # scale = 1, width = 20, height = 9, units = "cm", # usato per 1 riga di immagini
       scale = 1, width = 20, height = 12, units = "cm", # usato per 2 righe di immagini
       # scale = 1, width = 20, height = 15, units = "cm", # usato per 3 righe di immagini
       # scale = 1, width = 25, height = 25, units = "cm", # usato per 4 righe di immagini
       # scale = 1, width = 35, height = 19, units = "cm", # usato per landscape
       # scale = 1, width = 10, height = 6.51, units = "cm", # usato per 1 plot facet singolo 
       dpi = 500, limitsize = TRUE)

cat(str_replace_all(plot_name,"_","\\\\_"))
cat("\n")
cat(str_replace_all(plot_name,"_"," "))
