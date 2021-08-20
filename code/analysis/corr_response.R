
#################################################################
##                        Model corr                        ##
#################################################################

# Load packages -----------------------------------------------------------

library(emmeans) # Estimated Marginal Means, aka Least-Squares Means
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(lmerTest) # Linear Mixed-Effects Models using 'Eigen' and S4
library(AICcmodavg) # Model Selection and Multimodel Inference Based on (Q)AIC(c)
source("code/apa_f.r")

# Import Data -------------------------------------------------------------

data <-here::here(
  "data",
  "all_data.Rds"
) %>%
  read_rds() 

corr_log <- here::here(
  "data",
  "log.csv"
) %>%
  read_csv() %>%
  mutate(
    pid = as.factor(P)
  ) %>% 
  filter(
    grepl("bad", Corr)
  ) %>%
  select(pid)

corr <- data %>% 
  anti_join(corr_log) %>% 
  drop_na(liking, tas_z) %>% 
  filter(abs(corr) < mean(corr, na.rm = TRUE) + 3*sd(corr, na.rm = TRUE)) 


message(
  paste0(nrow(data) - nrow(corr), " Observations removed out of ", nrow(data), " or ", round((nrow(data) - nrow(corr))/nrow(data)*100, 3), "%")
)

message(
  paste0(length(unique(data$pid)) - length(unique(corr$pid)), " Observations removed")
)

message(
  paste0("There are ", nrow(corr), " observations from ", length(unique(corr$pid)), " participants")
)

# Fit data to null model 

corr_null <- lmerTest::lmer(
  corr_z ~ 1 + (1 | pid),
  data = corr,
  REML = FALSE
)

corr_item <- lmerTest::lmer(
  corr_z ~ 1 + (1 | pid) + (1 | song),
  data = corr,
  REML = FALSE
)

corr_rand <- lmerTest::lmer(
  corr_z ~ 1 + (1 + factor | pid) + (1 | song),
  data = corr,
  REML = FALSE
)

anova(corr_null, corr_item, corr_rand) 

# Random structure is best 


# Model 1: Factor only ----------------------------------------------------

corr_factor <- update(
  corr_item, 
  . ~ . + factor, 
  contrasts = list(factor = contr.sum)
)


# Model 2: Liking  --------------------------------------------------------

corr_liking <- update(
  corr_factor, 
  . ~ . + liking_z + liking_z : factor
)


# Model 3: Alexithymia ----------------------------------------------------

corr_tas <- update(
  corr_liking, 
  . ~ . + tas_z + factor : tas_z
)


# Model 4: Three way ------------------------------------------------------

corr_all <- update(
  corr_tas,
  . ~ . + factor : tas_z : liking_z
)


# Compare Models ----------------------------------------------------------

anova(corr_factor, 
      corr_liking, 
      corr_tas, 
      corr_all) %>% 
  apa_lrt(caption = "Model comparison: corr")

# Follow up model ---------------------------------------------------------

apa_f(corr_factor, 
      caption = "Type 3 ANOVA table for best fitting corr model")

## Get main effect

corr_main <- emmeans::emmeans(corr_factor, 
                              pairwise ~ factor,
                              infer = TRUE)

corr_est <- as.data.frame(corr_main$emmeans)

corr_est %>%  
  transmute(
    `Song target affect` = corr_est[,1],
    trend_CI = glue::glue("Est = {round(corr_est[,2], 2)},  95% CI [{round(lower.CL, 2)}, {round(upper.CL, 2)}]")
  ) %>% 
  kableExtra::kable() %>% 
  kableExtra::kable_styling()

corr_main$contrasts
