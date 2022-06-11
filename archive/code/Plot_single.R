rm(list = ls())

library(tidyverse)
library(ggplot2)
library(magrittr)
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
# model_name <- "Model235_n10000_inter10_chv5000_rep30_w100f100"

model_list <- list(
  "Model367_n10000_inter10_chv5000_rep30_w100f100"
)
model_name <- paste0(as_vector(model_list),collapse = "_")


##### Percorso e file dati #####
data_in_list <- list()
for(i in 1:length(model_list)){
  data_in_list[[i]] <- read.csv(
    file.path(general_path,"Data_out",
              model_list[[i]],
              "data_windows.csv")
  )
}

data_in <- bind_rows(data_in_list, .id = "model") %>% 
  rename(Average_Queue = Queue_MEAN)

##### Costruzione dati multi_plot multi_field (STESSO DATASET) #####
# data_in %<>% rename(Starving_prob = Starvation_prob
#                     ,Average_Queue = Queue_MEAN)
# fields <- list(
#   quo(Waiting_trend_MEAN)
#   ,quo(Processing_trend_MEAN)
#   ,quo(Blocking_trend_MEAN)
# )
# fields <- list(
#   quo(Processing_time_MEAN)
#   ,quo(Blocking_time_MEAN)
#   ,quo(Starving_time_MEAN)
# )
# fields <- list(
#   quo(Waiting_time_MEAN)
# )
# fields <- list(
#   quo(Utilization)
#   ,quo(Blocking_prob)
#   ,quo(Starving_prob)
# )
# fields <- list(
#   quo(Average_Queue)
# )
fields <- list(
  quo(Input_diff_MEAN)
  ,quo(Mid_diff_MEAN)
  ,quo(Output_diff_MEAN)
)

data_in_3field <- data_in %>%
  mutate(across(-c(model, CaseID, Activity),as.numeric))%>%
  select(model, CaseID, Activity, !!!fields) %>%
  pivot_longer(cols = c(!!!fields), names_to = "fld_nm", values_to = "int_prob") %>%
  mutate(fld_nm = as_factor(fld_nm))

# data_in_3field %<>%
#   mutate(point_or_not = case_when(int_prob > plot_max_y ~ 1, TRUE ~ 0)) %>% # situazioni in cui valori crescono all'infinito e deve essere imposto un limite superiore
#   mutate(int_prob = case_when(int_prob > plot_max_y ~ plot_max_y, TRUE ~ int_prob))

# data_in_3field %<>%
#   mutate(point_color_group = case_when(CaseID >= 5000 ~ 1, TRUE ~ 0))

##### Stampa data_in_3field #####
data_in_3field %<>% filter(Activity %in% 1:7)

legend_labels <- list(bquote(paste("Input_diff")),
                      bquote(paste("Mid_diff")),
                      bquote(paste("Output_diff")))

# legend_labels <- list(bquote(c[s] == 3),
#                       bquote(c[s] == 6),
#                       bquote(c[s] == 10))

data_in_3field %<>%
  mutate(model = as.numeric(model)) %>%
  mutate(model = "(list(s[BN],s[CH]))==(list(5,3))") %>%
  mutate(model = as_factor(model))

multi_plot_multi_field <- ggplot(data_in_3field, aes(x = CaseID, y = int_prob)) +
  geom_point(size = 1, aes(colour = factor(fld_nm)), show.legend = T) +
  labs(colour = expression(mu_p)) +
  # geom_hline(data = data.frame(yint = 10, fld_nm = "Processing_time_MEAN"), aes(yintercept=yint), linetype="dashed") + # attivare per Processing_time_MEAN
  geom_hline(yintercept=10, linetype="dashed") + # attivare per Diff
  scale_color_brewer(type = "qual", palette = "Set1"
                     ,labels = legend_labels
  ) +
  # scale_size_manual(values = c(1.2,1,0.8)) +
  facet_grid(model~Activity, switch = "y", scales = "free_y", labeller = label_parsed) +
  scale_x_continuous(breaks=c(1, 5000, 10000), labels = c(expression(1^{" "}), expression(5~x~10^{"3"}), expression(10^{"4"}))) +
  # scale_y_continuous(trans = "log10") +
  # expand_limits(x = 0, y = 0) + # attivare per time
  # expand_limits(x = 0, y = c(0,1)) + # attivare per utilization
  # expand_limits(y = 1) + # attivare per trend
  # ylim(0,10) + # attivare per time con limitazione superiore
  theme(text = element_text(size = 11),
        strip.placement = "outside",
        plot.title = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(vjust=0),
        panel.spacing.y = unit(0.7, "cm"),
        panel.spacing.x = unit(0.3, "cm"),
        legend.position = "bottom",
        legend.title = element_blank()
        )

# last_plot()

plot_subtitle <- paste0(map_chr(fields,quo_name),collapse = "_")

plot_name <- paste0("ProcessingIncrease3",
                    "_",plot_subtitle,
                    ".pdf")

ggsave(filename = file.path(plot_out_path, plot_name),
       plot = multi_plot_multi_field,
       device = "pdf",
       scale = 1, width = 20, height = 6.51, units = "cm", # usato per 1 riga di immagini
       # scale = 1, width = 20, height = 10.88, units = "cm", # usato per 2 righe di immagini
       # scale = 1, width = 20, height = 15, units = "cm", # usato per 3 righe di immagini
       # scale = 1, width = 25, height = 25, units = "cm", # usato per 4 righe di immagini
       # scale = 1, width = 35, height = 19, units = "cm", # usato per landscape
       dpi = 500, limitsize = TRUE)

cat(str_replace_all(plot_name,"_","\\\\_"))
cat("\n")
cat(str_replace_all(plot_name,"_"," "))
