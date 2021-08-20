
##################################################################
##                        Model Physiology                      ##
##################################################################

# Load packages -----------------------------------------------------------

library(emmeans) # Estimated Marginal Means, aka Least-Squares Means
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(lmerTest) # Linear Mixed-Effects Models using 'Eigen' and S4

source("code/apa_f.r")
# Import Data -------------------------------------------------------------

data <- here::here(
  "data",
  "all_data.Rds"
) %>%
  read_rds() %>%
  drop_na(liking, tas_z) %>% 
  mutate(
    liking = liking - 3
  ) 

length(unique(data$pid))

log <- here::here(
  "data",
  "log.csv"
) %>% 
  read_csv() %>% 
  rename(
    pid = P
  )



# Zygomaticus -------------------------------------------------------------


# Create data -------------------------------------------------------------

# Remove bad recordings ---------------------------------------------------


bad_zygo <- log %>% 
  filter(grepl("bad", Zygo)) %>% 
  transmute(
    pid =
  )

zygo_subset <- data %>% 
  drop_na(zygo) %>% 
  anti_join(bad_zygo, 
            by = "pid")

length(unique(zygo_subset$pid))

# Z score outliers --------------------------------------------------------

outlier_zygo_z <- filter(zygo_subset,
                       zygo_z < 3 & zygo_z > -3
) %>% 
  select(id_trial, zygo_z)  %>% 
  left_join(zygo_subset, 
            by = "id_trial") # data set of Z score zygo with outliers removed

length(unique(outlier_zygo_z$pid))

# Raw score outliers ------------------------------------------------------
outlier_zygo_raw_val <- boxplot(zygo_subset$zygo)$out %>% 
  as.data.frame() %>% 
  rename(
    zygo = "."
  ) # data frame of outlier raw zygo numbers 

outlier_zygo_raw <- zygo_subset %>% 
  anti_join(
    outlier_zygo_raw_val,
    bt = "zygo"
  ) # data frame with raw score outliers removed

length(unique(outlier_zygo_raw$pid))



# Inspect data ------------------------------------------------------------


# Fit models --------------------------------------------------------------

zygo_1 <- lmerTest::lmer(
  zygo_z ~ affect_label + liking + (1 | pid) + (1 | song),
  data = outlier_zygo_raw,
  contrasts = list(
    arousal_target = contr.sum,
    valence_target = contr.sum
  ),
  REML = FALSE
)


zygo_2 <- update(
  zygo_1,
  . ~ . + affect_label * liking
)

zygo_3 <- update(
  zygo_2, 
  . ~ . + tas_z * affect_label
)

## Compare models
sapply(
  paste0(
    "zygo_",
    c(1:3)
  ),
  function(x) {
    AIC(get(x))
  }
) %>%
  data.frame() %>%
  rename(
    AIC = "."
  ) %>% 
  mutate(
    AICdiff = AIC - AIC(zygo_3)
  )

anova(zygo_1, zygo_2, zygo_3)

## Model zygo_1 is best model 

apa_f(
  zygo_1
)

emmeans::emmeans(
  zygo_1,
  pairwise ~ valence_target
)

# Model Corrugator --------------------------------------------------------

# Create data -------------------------------------------------------------

bad_cor <- log %>% 
  filter(!is.na(Corr)) %>% 
  select(pid) 

outlier_corr <- filter(data,
                       corr_z > 3 | corr_z < -3
) %>% 
  select(id_trial, corr_z)


corr_subset <- data %>% 
  drop_na(corr) %>% 
  anti_join(bad_cor,
            by = "pid")# %>% 
  anti_join(outlier_corr,
            by = "id_trial") 

length(unique(corr_subset$pid))
# Fit Models --------------------------------------------------------------

corr_1 <- lmerTest::lmer(
  corr_z ~ arousal_target + valence_target + liking + (1 | pid),
  data = corr_subset,
  contrasts = list(
    valence_target = contr.sum,
    arousal_target = contr.sum
  ),
  REML = FALSE
)


corr_2 <- update(
  corr_1,
  . ~ . + arousal_target * valence_target + arousal_target * liking + valence_target * liking
)

corr_3 <- update(
  corr_2, 
  . ~ . + tas_z * arousal_target + tas_z * valence_target 
)


## Compare models
sapply(
  paste0(
    "corr_",
    c(1:3)
  ),
  function(x) {
    AIC(get(x))
  }
) %>%
  data.frame() %>%
  rename(
    AIC = "."
  ) %>% 
  mutate(
    AICdiff = AIC - AIC(corr_3)
  )

anova(corr_1, corr_2, corr_3)

## Model corr_1 is best model 

apa_f(
  corr_1
)
  
## Mean comparisons 

emmeans::emmeans(
  corr_1,
  pairwise ~ valence_target
)



# Model Tonic --------------------------------------------------------------

# Create data -------------------------------------------------------------

bad_tonic <- log %>% 
  filter(!is.na(Eda)) %>% 
  select(pid) 

outlier_tonic <- filter(data,
                        tonic_z > 3 | tonic_z < -3
) %>% 
  select(id_trial, tonic_z)


tonic_subset <- data %>% 
  drop_na(tonic) %>% 
  anti_join(bad_tonic,
            by = "pid") %>% 
  anti_join(outlier_tonic,
            by = "id_trial") 

length(unique(tonic_subset$pid))


# Fit Models --------------------------------------------------------------

tonic_1 <- lmerTest::lmer(
  tonic_z ~ arousal_target + valence_target + liking + (1 | pid),
  data = tonic_subset,
  contrasts = list(
    valence_target = contr.sum,
    arousal_target = contr.sum
  ),
  REML = FALSE
)


tonic_2 <- update(
  tonic_1,
  . ~ . + arousal_target * valence_target + arousal_target * liking + valence_target * liking
)

tonic_3 <- update(
  tonic_2, 
  . ~ . + tas_z * arousal_target + tas_z * valence_target 
)

## Compare models
sapply(
  paste0(
    "tonic_",
    c(1:3)
  ),
  function(x) {
    AIC(get(x))
  }
) %>%
  data.frame() %>%
  rename(
    AIC = "."
  ) %>% 
  mutate(
    AICdiff = AIC - AIC(tonic_3)
  )

## Model tonic_1 is best model 

apa_f(
  tonic_1
)








