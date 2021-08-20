
#################################################################
##       Effect of Alexithymia on Target Emotion Ratings       ##
#################################################################

# Load packages -----------------------------------------------------------

library(emmeans) # Estimated Marginal Means, aka Least-Squares Means
library(tidyverse) # Easily Install and Load the 'Tidyverse'
library(lmerTest) # Linear Mixed-Effects Models using 'Eigen' and S4
library(AICcmodavg) # Model Selection and Multimodel Inference Based on (Q)AIC(c)
source("code/apa_f.r")

# Import Data -------------------------------------------------------------

data <- here::here(
  "data",
  "all_data.Rds"
) %>%
  read_rds() 

emotion_data <- data %>% 
  mutate(
    emotion_rating = case_when(
      emotion_target == "tender" ~ tender,
      emotion_target == "happy" ~ happy,
      emotion_target == "fear" ~ scared,
      emotion_target == "sad" ~ sad
    ),
    experimental_rating = case_when(
      emotion_target == "tender" ~ tender - rowMeans(select(data, anxious:excited, -tender)),
      emotion_target == "happy" ~ happy - rowMeans(select(data, anxious:excited, -happy)),
      emotion_target == "fear" ~ scared - rowMeans(select(data, anxious:excited, -scared)),
      emotion_target == "sad" ~ sad - rowMeans(select(data, anxious:excited, -sad))
    )
    )%>% 
  rowwise() %>% 
  mutate(
    max = max(anxious:excited),
    min = min(anxious:excited),
    bad = if_else(
      min - max == 0,
      TRUE,
      FALSE
    )
  ) %>% 
  select(
    pid,
    song, 
    bad,
    emotion_target,
    emotion_rating,
    experimental_rating,
    liking_z,
    tas_z,
  ) 

## Describe Means
emotion_data %>% 
  group_by(emotion_target) %>% 
  summarise(
    mean = mean(emotion_rating),
    sd = sd(emotion_rating)
  )

# Model results -----------------------------------------------------------

emotion_null <- lmerTest::lmer(
  emotion_rating ~ 1 + (1 | pid),
  data = emotion_data,
  # data = filter(emotion_data, bad == FALSE),
  REML = FALSE
)

emotion_item <- lmerTest::lmer(
  emotion_rating ~ 1 + (1 | pid) + (1 | song),
  data = emotion_data,
  # data = filter(emotion_data, bad == FALSE),
  REML = FALSE
)

emotion_rand <- lmerTest::lmer(
  emotion_rating ~ 1 + (1 + emotion_target | pid) + (1 | song),
  data = emotion_data,
  # data = filter(emotion_data, bad == FALSE),
  REML = FALSE
)

anova(emotion_null, emotion_item, emotion_rand) # random model is prefered

# Random structure is best 


# Model 1: Factor only ----------------------------------------------------

emotion_factor <- update(
  emotion_rand, 
  . ~ . + emotion_target, 
  contrasts = list(emotion_target = contr.sum)
)


# Model 2: Liking  --------------------------------------------------------

emotion_liking <- update(
  emotion_factor, 
  . ~ . + liking_z + liking_z : emotion_target
)


# Model 3: Alexithymia ----------------------------------------------------

emotion_tas <- update(
  emotion_liking, 
  . ~ . + tas_z + emotion_target : tas_z
)


# Model 4: Three way ------------------------------------------------------

emotion_all <- update(
  emotion_tas,
  . ~ . + emotion_target : tas_z : liking_z
)


# Compare Models ----------------------------------------------------------

anova(emotion_factor, 
      emotion_liking, 
      emotion_tas, 
      emotion_all) %>% 
  apa_lrt(caption = "Model comparison: emotion")

# Follow up model ---------------------------------------------------------

## Best model is the three way interaction 

apa_f(emotion_tas, 
      caption = "Type 3 ANOVA table for best fitting emotion model")

## Get main effect

emotion_main <- emmeans::emmeans(emotion_tas, 
                                 pairwise ~ emotion_target,
                                 infer = TRUE)

apa_post_hoc(emotion_main, caption = "Affect category differences: emotion")

emotion_main %>% apa_post_hoc_contrast()

# Follow up interactions ---------------------------------------------------

## Two way interaction 
### Liking

emotion_liking_emmeans <- emmeans::emtrends(
  emotion_tas, 
  pairwise ~ emotion_target,
  var = "liking_z",
  infer = TRUE
)

apa_trends(emotion_liking_emmeans)

emotion_liking_emmeans$contrasts

emmip(emotion_tas, emotion_target ~ liking_z, at = list(liking_z = c(-1, 0,1)))
### Alexithymia

emotion_tas_emmeans <- emmeans::emtrends(
  emotion_tas, 
  pairwise ~ emotion_target,
  var = "tas_z",
  infer = TRUE
)

apa_trends(emotion_tas_emmeans)

emotion_liking_emmeans$contrasts

emmip(emotion_tas, emotion_target ~ tas_z, at = list(tas_z = c(-1, 0, 1)))


