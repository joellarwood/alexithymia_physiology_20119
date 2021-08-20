
#################################################################
##                    Model Emotion Ratings                    ##
#################################################################


# Load packages -----------------------------------------------------------

library(tidyverse)
library(lmerTest)
library(irr)


# Load Data ---------------------------------------------------------------

data <- here::here(
  "data",
  "all_data.Rds"
) %>% 
  read_rds() %>% 
  drop_na(liking) %>% 
  # filter(
  #   valence_target == "negative"
  # ) %>% 
  select(
    pid,
    age,
    gender, 
    depression_z,
    contains("tas"),
    contains("ddf"),
    contains("dif"),
    anxious:excited
  )

glimpse(data)

length(unique(data$pid))
# Emotion DIfferntiation --------------------------------------------------

# Nest responses ----------------------------------------------------------


data_nested_icc <- data %>% 
  group_by(pid) %>% 
  select(pid,
         anxious,
         ashamed,
         scared,
         angry,
         sad,
         guilty,
         frustrated) %>% 
  nest(data = -pid) %>% 
  mutate(icc = map_dbl(data, function(df) irr::icc(df, model= "twoway")$value),
         icc_r = 1 - icc,
         positive = case_when(
           icc > 0 ~ "positive",
           icc < 0 ~ "negative"
         ))

table(data_nested_icc$positive) # 105 usable cases

data_icc_filter <- data_nested_icc %>% 
  filter(
    positive == "positive"
  ) 

outliers_icc <- boxplot(data_icc_filter$icc_r)$out %>% 
  as.data.frame() %>% 
  rename(
    icc_r = "."
  )

data_icc <- data_icc_filter %>% 
   # anti_join(
   #   outliers_icc,
   #   by = "icc_r") %>% 
  left_join(
    select(
      data,
      contains("tas"),
      contains("dif"),
      contains("ddf"),
      pid
    ), 
    by = "pid"
  ) %>% 
  mutate(
    ddf_dif = ddf + dif
  ) %>% 
  distinct(pid, .keep_all = TRUE)

boxplot(data_icc$icc_r)

nrow(data_icc)
# Model Emotion differentiation -------------------------------------------

em_d_1 <- lm(icc_r ~ tas,
             data = data_icc)

summary(em_d_1)

apa::cor_apa(cor.test(data_icc$tas, data_icc$icc_r))
