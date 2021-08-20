
#################################################################
##                        Model zygo                        ##
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

zygo_log <- here::here(
  "data",
  "log.csv"
) %>%
  read_csv() %>%
  mutate(
    pid = as.factor(P)
  ) %>% 
  filter(
    grepl("bad", Zygo)
  ) %>%
  select(pid)

zygo <- data %>% 
  anti_join(zygo_log) %>% 
  drop_na(liking, tas_z) %>% 
  filter(abs(corr) < mean(corr, na.rm = TRUE) + 3*sd(corr, na.rm = TRUE)) 

message(
  paste0(nrow(data) - nrow(zygo), " Observations removed out of ", nrow(data), " or ", round((nrow(data) - nrow(zygo))/nrow(data)*100, 3), "%")
)

message(
  paste0(length(unique(data$pid)) - length(unique(zygo$pid)), " Observations removed")
)

message(
  paste0("There are ", nrow(zygo), " observations from ", length(unique(zygo$pid)), " participants")
)

# Fit data to null model 

zygo_null <- lmerTest::lmer(
  zygo_z ~ 1 + (1 | pid),
  data = zygo,
  REML = FALSE
)

zygo_item <- lmerTest::lmer(
  zygo_z ~ 1 + (1 | pid) + (1 | song),
  data = zygo,
  REML = FALSE
)

zygo_rand <- lmerTest::lmer(
  zygo_z ~ 1 + (1 + factor | pid) + (1 | song),
  data = zygo,
  REML = FALSE
)

anova(zygo_null, zygo_item, zygo_rand) # random model is prefered

# Random structure is best 


# Model 1: Factor only ----------------------------------------------------

zygo_factor <- update(
  zygo_item, 
  . ~ . + factor, 
  contrasts = list(factor = contr.sum)
)


# Model 2: Liking  --------------------------------------------------------

zygo_liking <- update(
  zygo_factor, 
  . ~ . + liking_z + liking_z : factor
)


# Model 3: Alexithymia ----------------------------------------------------

zygo_tas <- update(
  zygo_liking, 
  . ~ . + tas_z + factor : tas_z
)


# Model 4: Three way ------------------------------------------------------

zygo_all <- update(
  zygo_tas,
  . ~ . + factor : tas_z : liking_z
)


# Compare Models ----------------------------------------------------------

anova(zygo_factor, 
      zygo_liking, 
      zygo_tas, 
      zygo_all) %>% 
  apa_lrt(caption = "Model comparison: zygo")

# Follow up model ---------------------------------------------------------

## Best model is the two way interaction with liking 

apa_f(zygo_liking, 
      caption = "Type 3 ANOVA table for best fitting zygo model")

## Get main effect

zygo_main <- emmeans::emmeans(zygo_factor, 
                              pairwise ~ factor,
                              infer = TRUE)

apa_post_hoc(zygo_main, caption = "Affect category differences: Zygo")

zygo_main %>% apa_post_hoc_contrast()

# Follow up interactions ---------------------------------------------------

## Two way interaction 
### Liking

zygo_liking_emmeans <- emmeans::emtrends(
  zygo_liking, 
  pairwise ~ factor,
  var = "liking_z",
  infer = TRUE
)

apa_trends(zygo_liking_emmeans)

zygo_liking_emmeans$contrasts



# Discordance -------------------------------------------------------------

zygo_discord <- lmerTest::lmer(
  zygo_z ~ factor * valence_rating * liking + factor * tas_z * valence_rating + (1 | pid) + (1 | song),
  data = zygo
)

emmeans::emmip(
  zygo_discord,
  at =  list(
    valence_rating = c(-1, 0, 1),
    liking = c(-1,0,1)
  ),
   valence_rating ~ liking |factor
)
