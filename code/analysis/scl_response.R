
#################################################################
##                        Model tonic                        ##
#################################################################

# Load packages -----------------------------------------------------------

library(emmeans) # Estimated Marginal Means, aka Least-Squares Means
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(lmerTest) # Linear Mixed-Effects Models using 'Eigen' and S4
library(AICcmodavg) # Model Selection and Multimodel Inference Based on (Q)AIC(c)
source("code/apa_f.r")

# Import Data -------------------------------------------------------------

tonic_all <-here::here(
  "data",
  "all_data.Rds"
) %>%
  read_rds() %>%
  mutate(
    liking = liking - 3
  ) %>% 
  filter(
    abs(tonic_z) < 3
  )

tonic_log <- here::here(
  "data",
  "log.csv"
) %>%
  read_csv() %>%
  rename(
    pid = P
  ) %>% 
  filter(
    grepl("bad", Eda)
  ) %>%
  select(pid)

tonic <- tonic_all %>% 
  anti_join(tonic_log) %>% 
  filter(abs(tonic_z) < 3) %>% 
  drop_na(liking, tas_z) %>% 
  mutate(
    log_scl = log(tonic + 1)
  ) 


length(unique(tonic$pid))
# Model Responses ---------------------------------------------------------

# Fit data to null model 

tonic_null <- lmerTest::lmer(
  tonic ~ 1 + (1 | pid),
  data = tonic,
  REML = FALSE
)

tonic_rand <- lmerTest::lmer(
  tonic ~ 1 + (1 | pid) + (1 | song),
  data = tonic,
  REML = FALSE
)

anova(tonic_null, tonic_rand) # null model is prefered

# Random structure is best 
# Model 1: Liking ------------------------------------------


tonic_1 <- update(
  tonic_null, 
  . ~ . + liking
)
# Model 2: Add factor -----------------------------------------------------
tonic_2 <- update(
  tonic_1,
  . ~ . + factor
)

# Model 4 Add interaction ------------------------------------------------

tonic_3 <- update(
  tonic_2,
  . ~ . + liking:factor
)
# Model 5 add TAS ---------------------------------------------------------

tonic_4 <- update(
  tonic_3,
  . ~ . + tas_z
)


tonic_5 <- update(
  tonic_4,
  . ~ . + tas_z:factor + tas_z:liking
)
# Model 6 add depression --------------------------------------------------

tonic_5 <- update(
  tonic_4,
  . ~ . + depression_z 
)

tonic_6 <- update(
  tonic_5,
  . ~ . + depression_z:factor + depression_z:liking
)


# LRT of model fit ------------------------------------------------------

anova(tonic_1, tonic_2, tonic_3, tonic_4, tonic_5, tonic_6)

# ANOVA of Model 3 --------------------------------------------------------

apa_f(tonic_3)



# Estimated Marginal Means ------------------------------------------------

emmeans::emmeans(
  tonic_3, 
  pairwise ~ factor, 
  infer = TRUE
)



# Simple Slopes -----------------------------------------------------------

emmeans::emtrends(
  tonic_3,
  pairwise ~ factor, 
  var = "liking"
)
# Plot --------------------------------------------------------------------

tonic_plot <- emmeans::emmip(
  tonic_3,
  factor ~ liking,
  CIs = TRUE,
  at = list(liking = c(-2, 0 , 2))
) 


# Discordance -------------------------------------------------------------

discordacne_null <- lmerTest::lmer(
  arousal_rating ~ 1 + (1 | pid),
  data = tonic,
  REML = FALSE
)

discordance_random <- lmerTest::lmer(
  arousal_rating ~ 1 + (1 + factor | pid) + (1 | song),
  data = tonic,
  REML = FALSE
)
anova(discordacne_null, discordance_random) # random is better

discordance_1 <- update(
  discordance_random, 
  . ~ . + tonic
)

discordance_2 <- update(
  discordance_1, 
  . ~ . + tonic:liking
)

discordance_3 <- update(
  discordance_2,
  . ~ . + tas_z
) 

discordance_4 <- update(
  discordance_3, 
  . ~ . + tas_z:tonic + tas_z:liking
)

anova(discordance_1, discordance_2, discordance_3, discordance_4)


emmip(
  discordance_4,
  tas_z ~ liking, 
  at = list(liking = c(-2, 0, 2),
            tas_z = c(-1, 0, 1))
)
