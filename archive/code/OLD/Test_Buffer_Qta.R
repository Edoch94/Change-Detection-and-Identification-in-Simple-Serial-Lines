data_out_list_2 <- FSA(data_in)

data_out_frame <- data_out_list_2[[3]]
data_out_frame$CaseID <- as.factor(data_out_frame$CaseID)

data_out_frame$pos <- ave(data_out_frame$EventID, data_out_frame$CaseID, FUN = rank)

data_out_frame$buff_change <- data_out_frame$pos

data_out_frame <-  data_out_frame %>% 
  mutate(buff_change = replace(buff_change, pos == 1, 1)) %>%
  mutate(buff_change = replace(buff_change, pos == 2, -1)) %>%
  mutate(buff_change = replace(buff_change, pos == 3, 0))

data_out_frame$buff_qta <- cumsum(data_out_frame$buff_change)
max(data_out_frame$buff_qta)
min(data_out_frame$buff_qta)

##### Buffer con tidyverse #####
data_buffer_test <- data_sim_in %>%
  # filter(CaseID == 100) %>%
  group_by(CaseID, Activity) %>%
  arrange(Time) %>%
  mutate(compl_time = max(Time)) %>%
  mutate(Pos = row_number()) %>%
  group_by(Activity) %>% 
  arrange(Time) %>%
  mutate(buffInOut = case_when(Pos == 1 ~ 1,
                               Pos == 2 ~ -1,
                               Pos == 3 ~ 0)) %>%
  mutate(buff = cumsum(buffInOut)) %>%
  ungroup()


data_plot <- data_buffer_test %>%
  filter(Activity == 2, Pos == 3) %>%
  ggplot(aes(x = CaseID, y = buff)) +
  geom_point(size = 0.5)

data_plot


a <- filter(data_buffer_test, Activity == 3) %>%
  arrange(CaseID)
