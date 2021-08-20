
#################################################################
##                        Model arousal                        ##
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



# Model Responses ---------------------------------------------------------


# Fit data to null model 

arousal_null <- lmerTest::lmer(
  arousal_rating ~ 1 + (1 | pid),
  data = data,
  REML = FALSE
)

arousal_item <- lmerTest::lmer(
  arousal_rating ~ 1 + (1 | pid) + (1 | song),
  data = data,
  REML = FALSE
)

arousal_rand <- lmerTest::lmer(
  arousal_rating ~ 1 + (1 + factor | pid) + (1 | song),
  data = data,
  REML = FALSE
)

anova(arousal_null, arousal_item, arousal_rand) # random model is prefered

# Random structure is best 


# Model 1: Factor only ----------------------------------------------------

arousal_factor <- update(
  arousal_rand, 
  . ~ . + factor, 
  contrasts = list(factor = contr.sum)
)


# Model 2: Liking  --------------------------------------------------------

arousal_liking <- update(
  arousal_factor, 
  . ~ . + liking_z + liking_z : factor
)


# Model 3: Alexithymia ----------------------------------------------------

arousal_tas <- update(
  arousal_liking, 
  . ~ . + tas_z + factor : tas_z
)


# Model 4: Three way ------------------------------------------------------

arousal_all <- update(
  arousal_tas,
  . ~ . + factor : tas_z : liking_z
)


# Compare Models ----------------------------------------------------------

anova(arousal_factor, 
      arousal_liking, 
      arousal_tas, 
      arousal_all) %>% 
  apa_lrt(caption = "Model comparison: arousal")


# Follow up model ---------------------------------------------------------

## Best model is the three way interaction 

apa_f(arousal_all, 
      caption = "Type 3 ANOVA table for best fitting arousal model")

## Get main effect

arousal_main <- emmeans::emmeans(arousal_all, 
                                 pairwise ~ factor,
                                 infer = TRUE)

apa_post_hoc(arousal_main, caption = "Affect category differences: arousal")

arousal_main %>% apa_post_hoc_contrast()

# Follow up interactions ---------------------------------------------------

## Two way interaction 
### Liking

arousal_liking_emmeans <- emmeans::emtrends(
  arousal_all, 
  pairwise ~ factor,
  var = "liking_z",
  infer = TRUE
)

apa_trends(arousal_liking_emmeans)

arousal_liking_emmeans$contrasts

### Alexithymia

arousal_tas_emmeans <- emmeans::emtrends(
  arousal_all, 
  pairwise ~ factor,
  var = "tas_z",
  infer = TRUE
)

apa_trends(arousal_tas_emmeans)

arousal_tas_emmeans$contrasts

## Three way interaction

arousal_simple_simple <- emmeans::emtrends(
  arousal_all,
  pairwise ~ liking_z | factor,
  at = list(liking_z = c(-1, 0, 1)),
  var = "tas_z",
  infer = TRUE)


arousal_simple_simple$emtrends %>% 
  as.data.frame() %>% 
  arrange(factor, liking_z) %>% 
  distinct(factor, liking_z, .keep_all = TRUE) %>% 
  mutate(
    statistic = glue::glue("trend = {round(tas_z.trend, 2)}, p = {round(p.value, 3)}, 95% CI [{round(asymp.UCL, 2)}, {round(asymp.LCL, 2)}]")
  ) %>% 
  select(factor, liking_z, statistic) %>% 
  knitr::kable() %>% 
  kableExtra::kable_styling()


arousal_simple_simple$contrasts %>% 
  as.data.frame() %>% 
  arrange(contrast) %>% 
  tidyr::separate(contrast, 
                  into = c("ref", "contr"),
                  sep = " - ") %>% 
  tidyr::separate(ref,
                  into = c("Liking", "Tas"),
                  sep = "\\s") %>% 
  tidyr::separate(contr,
                  into = c("Liking.c", "Tas.c"),
                  sep = "\\s") %>% 
  arrange(factor, Liking) %>% 
  # select(
  #   contains(c(
  #     "factor",
  #     "Liking",
  #     "Tas",
  #     "p.value")
  #   )
  # ) %>% 
  filter(p.value < .05)

plot_arousal_3way <- emmeans::emmip(
  arousal_all,
  tas_z ~ liking_z | factor, 
  at = list(
    tas_z = c(-1, 0, 1), 
    liking_z = c(-1, 0, 1)
  ),
  plotit = FALSE
) %>% 
  mutate(
    tas_z = as.factor(tas_z),
    factor = recode(factor,
                    "Positive/High" = "Happy", 
                    "Positive/Low" = "Tender",
                    "Negative/High" = "Fear",
                    "Negative/Low" = "Sad"
    )
  ) %>% 
  ggplot2::ggplot(
    aes(
      x = liking_z,
      y = yvar,
      color = tas_z,
      group = tas_z
    )
  ) + 
  geom_path() +
  facet_wrap(~factor) +
  ylim(-2, 2) +
  labs(
    x = "Song liking (Z transformed)",
    y = "Rated arousal",
    color = "Alexithymia \n(Z transformed)"
  ) +
  theme_classic() +
  theme(text=element_text(family="Times New Roman", face="bold", size=12))

ggsave("output/plot/arousal_3way.svg", height = 10, width = 15, units = "cm")

