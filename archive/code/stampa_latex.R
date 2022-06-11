rm(list = ls())

library(tidyverse)
library(ggplot2)
library(magrittr)
library(xtable)

data_in <- data_sim_in %>%
  ungroup() %>%
  mutate(EventID = row_number()) %>%
  filter(CaseID %in% c(7500)) %>%
  rename(ActivityID = Activity, PositionID = Position, Timestamp = Time) %>%
  relocate(EventID)

data_in_xtable <- xtable(data_in)
print(data_in_xtable, include.rownames=FALSE)

data_in <- data_out_standard %>%
  select(CaseID, Activity, Timestamp_buff, Timestamp_res, Timestamp_end) %>%
  filter(CaseID %in% c(7500)) %>%
  arrange(Activity)

data_in_xtable <- xtable(data_in)
print(data_in_xtable, include.rownames=FALSE)
