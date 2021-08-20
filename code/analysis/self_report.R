
##################################################################
##                      Model Self Report                       ##
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
  mutate(
    liking = liking - 3
  ) %>% 
  drop_na(liking, tas_z)

# Model Valence -----------------------------------------------------------

## Factors only

v_1 <- lmerTest::lmer(
  valence_rating ~ arousal_target + valence_target + liking + (1 + arousal_target + valence_target | pid) + (1 | song),
  data = data,
  contrasts = list(
    valence_target = contr.sum,
    arousal_target = contr.sum
  ),
  REML = FALSE
)

## Factor interaction


v_2 <- update(
  v_1,
  . ~ . + arousal_target * valence_target + arousal_target * liking + valence_target * liking
)

v_3 <- update(
  v_2, 
  . ~ . + tas_z + tas_z * arousal_target + tas_z * valence_target
)


## Compare models
sapply(
  paste0(
    "v_",
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
    AICdiff = AIC - AIC(v_3)
  )

## Model v_3 is best model 

apa_f(
  v_3
)


# Follow up selected valence model ----------------------------------------

emmeans::emtrends(
  v_3,
  pairwise ~ valence_target,
  var = "liking",
  infer = TRUE
)




# Model Arousal -----------------------------------------------------------

a_1 <- lmerTest::lmer(
  arousal_rating ~ arousal_target + valence_target + liking + (1 + arousal_target * valence_target | pid) + (1 | song),
  data = data,
  contrasts = list(
    valence_target = contr.sum,
    arousal_target = contr.sum
  ),
  REML = FALSE
)

## Factor interaction


a_2 <- update(
  a_1,
  . ~ . + arousal_target * valence_target + arousal_target * liking + valence_target * liking
)

a_3 <- update(
  a_2, 
  . ~ . + tas_z + tas_z * arousal_target + tas_z * valence_target
)

a_4 <- update(
  a_3,
  . ~ . + depression_z * arousal_target + depression_z * valence_target
)
## Compare models
sapply(
  paste0(
    "a_",
    c(1:4)
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
    AICdiff = AIC - AIC(a_4)
  )

## Model a_4 is best model 

apa_f(
  a_3
)

## Model arousal by liking

emmeans::emtrends(
  a_2,
  pairwise ~ arousal_target,
  var = "liking"
)

emmeans::emtrends(
  a_2,
  pairwise ~ valence_target,
  var = "liking"
)


